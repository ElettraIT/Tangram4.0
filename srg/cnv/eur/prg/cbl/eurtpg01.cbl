       Identification Division.
       Program-Id.                                 eurtpg01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    eur                 *
      *                                   Fase:    eurtpg01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/00    *
      *                       Ultima revisione:    NdK del 17/05/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per EURO - Tipografia Rumor     *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Copia archivi                               *
      *                                                                *
      * {gct}   [had]      Tabella addetti                             *
      * {gct}   [han]      Tabella annotazioni                         *
      * {gct}   [har]      Righe di annotazione per le commesse        *
      * {gct}   [hba]      Bancali per commessa                        *
      * {gct}   [hce]      Tabella centri di costo                     *
      * {gct}   [hcn]      Movimenti per gestione controlli            *
      * {gct}   [hcr]      Movimenti per gestione lavorazioni : righe  *
      * {gct}   [hct]      Testate commesse di lavorazione             *
      * {gct}   [hcx]      Movimenti gestione commesse : estensioni    *
      * {gct}   [hma]      Tabella macchine                            *
      * {gct}   [hmn]      Movimenti per gestione manutenzioni         *
      * {gct}   [hms]      Saldi quantita' per gestione impegnato      *
      * {gct}   [hmx]      Commesse di lavorazione : estensioni        *
      * {gct}   [hnc]      Non conformita' commesse                    *
      * {gct}   [hnu]      Numerazioni per 'gct'                       *
      * {gct}   [hot]      Ordini di lavoro : testate                  *
      * {gct}   [hox]      Ordini di lavoro : estensioni per le righe  *
      * {gct}   [hpc]      Tabella parametri di controllo              *
      * {gct}   [hpd]      Tabelle descrizioni per preventivi          *
      * {gct}   [hpl]      Assegnazione lavorazioni previste           *
      * {gct}   [hpm]      Tabella parametri di manutenzione           *
      * {gct}   [hpr]      Tabella % media di ricarico                 *
      * {gct}   [hpv]      Tabelle descrizioni per preventivi          *
      * {gct}   [hrc]      Tabella responsabili delle commesse         *
      * {gct}   [hsr]      Scheda tecnica commessa : righe impianti    *
      * {gct}   [hst]      Scheda tecnica commessa : testata           *
      * {gct}   [hsx]      Scheda tecnica commessa : estensioni test.  *
      * {gct}   [hsy]      Scheda tecnica commessa : estensioni riga   *
      * {gct}   [htl]      Tabella tipi di lavoro                      *
      * {gct}   [htn]      Tabella Tipi di non conformita'             *
      * {gct}   [hut]      Ordini di lavoro preventivi : testate       *
      * {gct}   [hux]      Ordini di lavoro preventivi : estensioni    *
      * {gct}   [hvr]      Schede di lavorazione preventivi : righe    *
      * {gct}   [hvt]      Scheda tecnica preventivo                   *
      * {gct}   [hvx]      Scheda tecnica preventivo : estensioni      *
      * {gct}   [hvy]      Scheda tecnica preventivo : estensioni riga *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * {gct} v [hla]      Archivio dati lavorazioni                   *
      * {gct} v [hls]      Listino scanner                             *
      * {gct} v [hmc]      Movimenti per gestione commesse             *
      * {gct} v [hor]      Ordini di lavoro : righe                    *
      * {gct} v [hpt]      Testate perventivi                          *
      * {gct} v [hrp]      Righe di servizi aggiuntivi preventivi      *
      * {gct} v [hur]      Ordini di lavoro preventivi : righe         *
      * {gct} v [hyf]      Fotolito - scanner commesse : righe         *
      * {gct} v [hzf]      Fotolito - scanner preventivi : righe       *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.        N-d-K-Sia-PD .
       Object-Computer.        N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [hla]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hla   assign to disk           f-hla-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hla-k01
                   alternate record key   is hla-k02
                   alternate record key   is hla-k03
                   alternate record key   is hla-k04
                   alternate record key   is hla-k05
                   alternate record key   is hla-k06
                   alternate record key   is hla-k07
                             file status  is                f-hla-sts .

      *    *===========================================================*
      *    * File Control [hls]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hls   assign to disk           f-hls-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hls-k01
                   alternate record key   is hls-k02
                             file status  is                f-hls-sts .

      *    *===========================================================*
      *    * File Control [hmc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hmc   assign to disk           f-hmc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hmc-k01
                   alternate record key   is hmc-k02
                   alternate record key   is hmc-k03
                   alternate record key   is hmc-k04
                   alternate record key   is hmc-k05
                   alternate record key   is hmc-k06
                   alternate record key   is hmc-k07
                             file status  is                f-hmc-sts .

      *    *===========================================================*
      *    * File Control [hor]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hor   assign to disk           f-hor-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hor-k01
                   alternate record key   is hor-k02
                   alternate record key   is hor-k03
                             file status  is                f-hor-sts .

      *    *===========================================================*
      *    * File Control [hpt]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hpt   assign to disk           f-hpt-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hpt-k01
                   alternate record key   is hpt-k02
                   alternate record key   is hpt-k03
                   alternate record key   is hpt-k04
                   alternate record key   is hpt-k05
                             file status  is                f-hpt-sts .

      *    *===========================================================*
      *    * File Control [hrp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hrp   assign to disk           f-hrp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hrp-k01
                   alternate record key   is hrp-k02
                             file status  is                f-hrp-sts .

      *    *===========================================================*
      *    * File Control [hur]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hur   assign to disk           f-hur-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hur-k01
                   alternate record key   is hur-k02
                   alternate record key   is hur-k03
                             file status  is                f-hur-sts .

      *    *===========================================================*
      *    * File Control [hyf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hyf   assign to disk           f-hyf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hyf-k01
                             file status  is                f-hyf-sts .

      *    *===========================================================*
      *    * File Control [hzf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hzf   assign to disk           f-hzf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hzf-k01
                             file status  is                f-hzf-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [hla]                                    *
      *    *-----------------------------------------------------------*
       fd  hla       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hla-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hla-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMLAV                         *
      *            *---------------------------------------------------*
               10  hla-k01.
                   15  hla-num-lav        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hla-k02.
                   15  hla-ide-dat        pic  9(07)       comp-3     .
                   15  hla-num-lav-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  hla-k03.
                   15  hla-des-key        pic  x(40)                  .
                   15  hla-num-lav-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFLAV                         *
      *            *---------------------------------------------------*
               10  hla-k04.
                   15  hla-alf-lav        pic  x(14)                  .
                   15  hla-num-lav-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNLAV                         *
      *            *---------------------------------------------------*
               10  hla-k05.
                   15  hla-syn-lav        pic  x(13)                  .
                   15  hla-num-lav-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  hla-k06.
                   15  hla-cla-lav        pic  9(05)       comp-3     .
                   15  hla-gru-lav        pic  9(05)       comp-3     .
                   15  hla-sgr-lav        pic  9(05)       comp-3     .
                   15  hla-des-key-6      pic  x(40)                  .
                   15  hla-num-lav-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  hla-k07.
                   15  hla-cla-lav-7      pic  9(05)       comp-3     .
                   15  hla-gru-lav-7      pic  9(05)       comp-3     .
                   15  hla-sgr-lav-7      pic  9(05)       comp-3     .
                   15  hla-alf-lav-7      pic  x(14)                  .
                   15  hla-num-lav-7      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hla-dat.
               10  hla-inf-gen.
                   15  hla-ide-ute        pic  x(08)                  .
                   15  hla-ide-fas        pic  x(06)                  .
                   15  hla-des-lav        pic  x(40)                  .
                   15  hla-tip-lav        pic  9(02)                  .
               10  hla-inf-prd.
                   15  hla-umi-prd        pic  x(03)                  .
                   15  hla-dec-qta        pic  9(01)                  .
                   15  hla-snx-2qt        pic  9(01)                  .
                   15  hla-dec-2qt        pic  9(01)                  .
                   15  hla-snx-3qt        pic  9(01)                  .
                   15  hla-dec-3qt        pic  9(01)                  .
                   15  hla-tip-vpr        pic  x(03)                  .
                   15  hla-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  hla-coe-div        pic  9(04)v9(03) comp-3     .
               10  hla-inf-val.
                   15  hla-ann-rif        pic  9(03)       comp-3     .
                   15  hla-val-med        pic  9(09)       comp-3     .
               10  hla-inf-pcs.
                   15  hla-cod-s01        pic  9(05)       comp-3     .
                   15  hla-cod-s02        pic  9(05)       comp-3     .
                   15  hla-cod-s03        pic  9(05)       comp-3     .
               10  hla-inf-bdg.
                   15  hla-cla-bdg        pic  9(05)       comp-3     .
               10  hla-inf-aps.
                   15  hla-alx-exp.
                       20  filler occurs 80
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hls]                                    *
      *    *-----------------------------------------------------------*
       fd  hls       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hls-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hls-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTCMQ                         *
      *            *---------------------------------------------------*
               10  hls-k01.
                   15  hls-cod-lst        pic  x(03)                  .
                   15  hls-rif-cmq        pic  9(07)       comp-3     .
                   15  hls-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hls-k02.
                   15  hls-ide-dat        pic  9(07)       comp-3     .
                   15  hls-cod-lst-2      pic  x(03)                  .
                   15  hls-rif-cmq-2      pic  9(07)       comp-3     .
                   15  hls-num-prg-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hls-dat.
               10  hls-ide-ute            pic  x(08)                  .
               10  hls-ide-fas            pic  x(06)                  .
               10  hls-dec-prz            pic  9(01)                  .
               10  hls-prz-00a            pic  9(09)       comp-3     .
               10  hls-prz-00b            pic  9(09)       comp-3     .
               10  hls-prz-00c            pic  9(09)       comp-3     .
               10  hls-alx-exp.
                   15  filler occurs 200  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hmc]                                    *
      *    *-----------------------------------------------------------*
       fd  hmc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hmc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hmc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hmc-k01.
                   15  hmc-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hmc-k02.
                   15  hmc-ide-dat        pic  9(07)       comp-3     .
                   15  hmc-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATREG                         *
      *            *---------------------------------------------------*
               10  hmc-k03.
                   15  hmc-dat-reg        pic  9(07)       comp-3     .
                   15  hmc-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CMLPRT                         *
      *            *---------------------------------------------------*
               10  hmc-k04.
                   15  hmc-prt-cml        pic  9(09)       comp-3     .
                   15  hmc-dat-reg-4      pic  9(07)       comp-3     .
                   15  hmc-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CPMPRT                         *
      *            *---------------------------------------------------*
               10  hmc-k05.
                   15  hmc-dat-reg-5      pic  9(07)       comp-3     .
                   15  hmc-tip-cpm        pic  9(02)                  .
                   15  hmc-num-cpm        pic  9(07)       comp-3     .
                   15  hmc-num-prt-5      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DATCDC                         *
      *            *---------------------------------------------------*
               10  hmc-k06.
                   15  hmc-dat-reg-6      pic  9(07)       comp-3     .
                   15  hmc-cod-cdc        pic  9(05)       comp-3     .
                   15  hmc-cod-mac        pic  9(05)       comp-3     .
                   15  hmc-cod-adt        pic  9(05)       comp-3     .
                   15  hmc-num-prt-6      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CMLCDC                         *
      *            *---------------------------------------------------*
               10  hmc-k07.
                   15  hmc-prt-cml-7      pic  9(11)       comp-3     .
                   15  hmc-cod-cdc-7      pic  9(05)       comp-3     .
                   15  hmc-tip-cpm-7      pic  9(02)                  .
                   15  hmc-num-cpm-7      pic  9(07)       comp-3     .
                   15  hmc-dat-reg-7      pic  9(07)       comp-3     .
                   15  hmc-num-prt-7      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hmc-dat.
               10  hmc-ide-ute            pic  x(08)                  .
               10  hmc-ide-fas            pic  x(06)                  .
               10  hmc-cod-dpz            pic  9(02)                  .
               10  hmc-tip-cml            pic  9(02)                  .
               10  hmc-prt-mag            pic  9(11)       comp-3     .
               10  hmc-prt-orl            pic  9(09)       comp-3     .
               10  hmc-prt-orf            pic  9(11)       comp-3     .
               10  hmc-snx-mac            pic  x(01)                  .
               10  hmc-alf-cpm            pic  x(14)                  .
               10  hmc-var-cpm            pic  x(14)                  .
               10  hmc-udm-cpm            pic  x(03)                  .
               10  hmc-cod-fnt            pic  9(07)       comp-3     .
               10  hmc-dpz-fnt            pic  x(04)                  .
               10  hmc-ddo-rif            pic  9(07)       comp-3     .
               10  hmc-ndo-rif            pic  x(10)                  .
               10  hmc-qta-prv            pic s9(08)v9(03) comp-3     .
               10  hmc-qta-eff            pic s9(08)v9(03) comp-3     .
               10  hmc-snx-ivc            pic  x(01)                  .
               10  hmc-val-mag            pic  9(09)       comp-3     .
               10  hmc-val-std            pic  9(09)       comp-3     .
               10  hmc-per-ric            pic s9(05)v9(02) comp-3     .
               10  hmc-val-prv            pic  9(09)       comp-3     .
               10  hmc-val-eff            pic  9(09)       comp-3     .
               10  hmc-not-mov            pic  x(40)                  .
               10  hmc-snx-agg            pic  x(01)                  .
               10  hmc-flg-ela.
                   15  hmc-flg-blo.
                       20  hmc-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hmc-flg-nbl.
                       20  hmc-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hmc-flg-pul            pic  x(01)                  .
               10  hmc-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hor]                                    *
      *    *-----------------------------------------------------------*
       fd  hor       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hor-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hor-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hor-k01.
                   15  hor-num-prt        pic  9(09)       comp-3     .
                   15  hor-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : CNSCDC                         *
      *            *---------------------------------------------------*
               10  hor-k02.
                   15  hor-dat-cns        pic  9(07)       comp-3     .
                   15  hor-cod-cdc        pic  9(05)       comp-3     .
                   15  hor-prt-cml        pic  9(09)       comp-3     .
                   15  hor-tip-mag        pic  9(02)                  .
                   15  hor-num-mag        pic  9(07)       comp-3     .
                   15  hor-num-prt-2      pic  9(09)       comp-3     .
                   15  hor-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CMLPRT                         *
      *            *---------------------------------------------------*
               10  hor-k03.
                   15  hor-prt-cml-3      pic  9(09)       comp-3     .
                   15  hor-cod-cdc-3      pic  9(05)       comp-3     .
                   15  hor-tip-mag-3      pic  9(02)                  .
                   15  hor-num-mag-3      pic  9(07)       comp-3     .
                   15  hor-num-prt-3      pic  9(09)       comp-3     .
                   15  hor-num-prg-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hor-dat.
               10  hor-cod-dpz            pic  9(02)                  .
               10  hor-tmo-orl            pic  x(05)                  .
               10  hor-tip-rig            pic  x(05)                  .
               10  hor-alf-mag            pic  x(14)                  .
               10  hor-umi-lav            pic  x(03)                  .
               10  hor-dec-qta            pic  9(01)                  .
               10  hor-des-ext            pic  9(01)                  .
               10  hor-des-rig            pic  x(40)                  .
               10  hor-qta-ord            pic s9(06)v9(03) comp-3     .
               10  hor-prz-uni            pic  9(09)       comp-3     .
               10  hor-prt-mcl            pic  9(09)       comp-3     .
               10  hor-flg-ela.
                   15  hor-flg-blo.
                       20  hor-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hor-flg-nbl.
                       20  hor-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hor-flg-pul            pic  x(01)                  .
               10  hor-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hpt]                                    *
      *    *-----------------------------------------------------------*
       fd  hpt       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hpt-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hpt-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hpt-k01.
                   15  hpt-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hpt-k02.
                   15  hpt-ide-dat        pic  9(07)       comp-3     .
                   15  hpt-dat-reg        pic  9(07)       comp-3     .
                   15  hpt-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  hpt-k03.
                   15  hpt-dat-reg-3      pic  9(07)       comp-3     .
                   15  hpt-cod-tpr        pic  x(05)                  .
                   15  hpt-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CLIDAT                         *
      *            *---------------------------------------------------*
               10  hpt-k04.
                   15  hpt-cod-cli        pic  9(07)       comp-3     .
                   15  hpt-dat-reg-4      pic  9(07)       comp-3     .
                   15  hpt-cod-tpr-4      pic  x(05)                  .
                   15  hpt-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CMLPRT                         *
      *            *---------------------------------------------------*
               10  hpt-k05.
                   15  hpt-prt-cml        pic  9(09)       comp-3     .
                   15  hpt-num-prt-5      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hpt-dat.
               10  hpt-ide-ute            pic  x(08)                  .
               10  hpt-ide-fas            pic  x(06)                  .
               10  hpt-cod-dpz            pic  9(02)                  .
               10  hpt-tip-prv            pic  9(02)                  .
               10  hpt-dat-chi            pic  9(07)       comp-3     .
               10  hpt-dat-cop            pic  9(07)       comp-3     .
               10  hpt-dat-doc            pic  9(07)       comp-3     .
               10  hpt-num-doc            pic  x(10)                  .
               10  hpt-dpz-cli            pic  x(04)                  .
               10  hpt-cod-fop            pic  9(07)       comp-3     .
               10  hpt-cod-age            pic  9(07)       comp-3     .
               10  hpt-cod-dst            pic  9(07)       comp-3     .
               10  hpt-dpz-dst            pic  x(04)                  .
               10  hpt-prt-ftr            pic  9(09)       comp-3     .
               10  hpt-cod-rdp            pic  x(03)                  .
               10  hpt-dat-rcl            pic  9(07)       comp-3     .
               10  hpt-num-rcl            pic  x(10)                  .
               10  hpt-cst-cpp occurs 06.
                   15  hpt-num-cpp        pic  9(09)       comp-3     .
                   15  hpt-vmn-cpp        pic  9(09)       comp-3     .
               10  hpt-des-prv.
                   15  hpt-rig-des occurs 2
                                          pic  x(40)                  .
               10  hpt-ann-prv.
                   15  hpt-rig-ann occurs 3
                                          pic  x(40)                  .
               10  hpt-nom-int            pic  x(30)                  .
               10  hpt-num-tel            pic  x(20)                  .
               10  hpt-num-cdr            pic  x(15)                  .               
               10  hpt-cod-cns            pic  9(02)                  .
               10  hpt-des-cns            pic  x(30)                  .
               10  hpt-cst-ccs            pic  9(09)       comp-3     .
               10  hpt-cat-lav            pic  9(02)                  .
               10  hpt-flg-ela.
                   15  hpt-flg-blo.
                       20  hpt-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hpt-flg-nbl.
                       20  hpt-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hpt-flg-pul            pic  x(01)                  .
               10  hpt-mod-pag            pic  9(02)                  .
               10  hpt-num-fax            pic  x(20)                  .
               10  hpt-iem-ail            pic  x(40)                  .
               10  hpt-num-pro            pic  9(07)                  .
               10  hpt-vde-prv occurs 12.
                   15  hpt-vde-cod        pic  x(03)                  .
               10  hpt-ppv-age            pic  9(02)v9(01)            .
               10  hpt-per-scp            pic  9(02)v9(01)            .
               10  hpt-fir-map            pic  x(40)                  .
               10  hpt-cod-iva            pic  9(05)                  .
               10  hpt-alx-exp.
                   15  filler occurs 044  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hrp]                                    *
      *    *-----------------------------------------------------------*
       fd  hrp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hrp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hrp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRTPRV                         *
      *            *---------------------------------------------------*
               10  hrp-k01.
                   15  hrp-prt-prv        pic  9(09)       comp-3     .
                   15  hrp-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : LAVPRV                         *
      *            *---------------------------------------------------*
               10  hrp-k02.
                   15  hrp-cod-lav        pic  9(07)       comp-3     .
                   15  hrp-prt-prv-2      pic  9(09)       comp-3     .
                   15  hrp-num-prg-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hrp-dat.
               10  hrp-ide-dat            pic  9(07)       comp-3     .
               10  hrp-ide-ute            pic  x(08)                  .
               10  hrp-ide-fas            pic  x(06)                  .
               10  hrp-tip-rig            pic  x(05)                  .
               10  hrp-ind-prz            pic  x(01)                  .
               10  hrp-qta-rig            pic  9(09)v9(02) comp-3     .
               10  hrp-prz-lav            pic  9(13)       comp-3     .
               10  hrp-stp-not            pic  x(40)                  .
               10  hrp-dat-prv            pic  9(07)       comp-3     .
               10  hrp-dat-eff            pic  9(07)       comp-3     .
               10  hrp-tst-400.
                   15  hrp-rig-400 occurs 10
                                          pic  x(40)                  .
               10  hrp-alx-exp.
                   15  filler      occurs 200
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hur]                                    *
      *    *-----------------------------------------------------------*
       fd  hur       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hur-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hur-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hur-k01.
                   15  hur-num-prt        pic  9(09)       comp-3     .
                   15  hur-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : CNSCDC                         *
      *            *---------------------------------------------------*
               10  hur-k02.
                   15  hur-dat-cns        pic  9(07)       comp-3     .
                   15  hur-cod-cdc        pic  9(05)       comp-3     .
                   15  hur-prt-prv        pic  9(09)       comp-3     .
                   15  hur-ver-prv        pic  9(03)       comp-3     .
                   15  hur-tip-mag        pic  9(02)                  .
                   15  hur-num-mag        pic  9(07)       comp-3     .
                   15  hur-num-prt-2      pic  9(09)       comp-3     .
                   15  hur-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PRVPRT                         *
      *            *---------------------------------------------------*
               10  hur-k03.
                   15  hur-prt-prv-3      pic  9(09)       comp-3     .
                   15  hur-ver-prv-3      pic  9(03)       comp-3     .
                   15  hur-cod-cdc-3      pic  9(05)       comp-3     .
                   15  hur-tip-mag-3      pic  9(02)                  .
                   15  hur-num-mag-3      pic  9(07)       comp-3     .
                   15  hur-num-prt-3      pic  9(09)       comp-3     .
                   15  hur-num-prg-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hur-dat.
               10  hur-cod-dpz            pic  9(02)                  .
               10  hur-tmo-orl            pic  x(05)                  .
               10  hur-tip-rig            pic  x(05)                  .
               10  hur-alf-mag            pic  x(14)                  .
               10  hur-umi-lav            pic  x(03)                  .
               10  hur-dec-qta            pic  9(01)                  .
               10  hur-des-ext            pic  9(01)                  .
               10  hur-des-rig            pic  x(40)                  .
               10  hur-qta-ord            pic s9(06)v9(03) comp-3     .
               10  hur-prz-uni            pic  9(09)       comp-3     .
               10  hur-prt-mcl            pic  9(09)       comp-3     .
               10  hur-flg-ela.
                   15  hur-flg-blo.
                       20  hur-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hur-flg-nbl.
                       20  hur-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hur-flg-pul            pic  x(01)                  .
               10  hur-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hyf]                                    *
      *    *-----------------------------------------------------------*
       fd  hyf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hyf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hyf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRTPRV                         *
      *            *---------------------------------------------------*
               10  hyf-k01.
                   15  hyf-prt-prv        pic  9(09)       comp-3     .
                   15  hyf-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hyf-dat.
               10  hyf-tip-rig            pic  x(05)                  .
               10  hyf-fmt-bas            pic  9(05)v9(02) comp-3     .
               10  hyf-fmt-alt            pic  9(05)v9(02) comp-3     .
               10  hyf-fmt-cmq            pic  9(09)v9(02) comp-3     .
               10  hyf-per-scn            pic  9(03)v9(02) comp-3     .
               10  hyf-cod-lst            pic  x(03)                  .
               10  hyf-dec-prz            pic  9(01)                  .
               10  hyf-prz-uni            pic  9(09)       comp-3     .
               10  hyf-num-fff            pic  9(09)v9(02) comp-3     .
               10  hyf-tot-rig            pic  9(13)       comp-3     .
               10  hyf-alx-exp.
                   15  filler  occurs 200 pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hzf]                                    *
      *    *-----------------------------------------------------------*
       fd  hzf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hzf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hzf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRTCML                         *
      *            *---------------------------------------------------*
               10  hzf-k01.
                   15  hzf-prt-cml        pic  9(09)       comp-3     .
                   15  hzf-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hzf-dat.
               10  hzf-tip-rig            pic  x(05)                  .
               10  hzf-fmt-bas            pic  9(05)v9(02) comp-3     .
               10  hzf-fmt-alt            pic  9(05)v9(02) comp-3     .
               10  hzf-fmt-cmq            pic  9(09)v9(02) comp-3     .
               10  hzf-per-scn            pic  9(03)v9(02) comp-3     .
               10  hzf-cod-lst            pic  x(03)                  .
               10  hzf-dec-prz            pic  9(01)                  .
               10  hzf-prz-uni            pic  9(09)       comp-3     .
               10  hzf-num-fff            pic  9(09)v9(02) comp-3     .
               10  hzf-tot-rig            pic  9(13)       comp-3     .
               10  hzf-alx-exp.
                   15  filler  occurs 200 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [hla]                                       *
      *    *-----------------------------------------------------------*
       01  f-hla.
           05  f-hla-nam                  pic  x(04)                  .
           05  f-hla-pat                  pic  x(40)                  .
           05  f-hla-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hls]                                       *
      *    *-----------------------------------------------------------*
       01  f-hls.
           05  f-hls-nam                  pic  x(04)                  .
           05  f-hls-pat                  pic  x(40)                  .
           05  f-hls-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hmc]                                       *
      *    *-----------------------------------------------------------*
       01  f-hmc.
           05  f-hmc-nam                  pic  x(04)                  .
           05  f-hmc-pat                  pic  x(40)                  .
           05  f-hmc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hor]                                       *
      *    *-----------------------------------------------------------*
       01  f-hor.
           05  f-hor-nam                  pic  x(04)                  .
           05  f-hor-pat                  pic  x(40)                  .
           05  f-hor-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hpt]                                       *
      *    *-----------------------------------------------------------*
       01  f-hpt.
           05  f-hpt-nam                  pic  x(04)                  .
           05  f-hpt-pat                  pic  x(40)                  .
           05  f-hpt-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hrp]                                       *
      *    *-----------------------------------------------------------*
       01  f-hrp.
           05  f-hrp-nam                  pic  x(04)                  .
           05  f-hrp-pat                  pic  x(40)                  .
           05  f-hrp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hur]                                       *
      *    *-----------------------------------------------------------*
       01  f-hur.
           05  f-hur-nam                  pic  x(04)                  .
           05  f-hur-pat                  pic  x(40)                  .
           05  f-hur-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hyf]                                       *
      *    *-----------------------------------------------------------*
       01  f-hyf.
           05  f-hyf-nam                  pic  x(04)                  .
           05  f-hyf-pat                  pic  x(40)                  .
           05  f-hyf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hzf]                                       *
      *    *-----------------------------------------------------------*
       01  f-hzf.
           05  f-hzf-nam                  pic  x(04)                  .
           05  f-hzf-pat                  pic  x(40)                  .
           05  f-hzf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [hla]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhla"                          .
      *        *-------------------------------------------------------*
      *        * [hls]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhls"                          .
      *        *-------------------------------------------------------*
      *        * [hmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhmc"                          .
      *        *-------------------------------------------------------*
      *        * [hor]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhor"                          .
      *        *-------------------------------------------------------*
      *        * [hpt]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhpt"                          .
      *        *-------------------------------------------------------*
      *        * [hrp]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhrp"                          .
      *        *-------------------------------------------------------*
      *        * [hur]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhur"                          .
      *        *-------------------------------------------------------*
      *        * [hyf]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhyf"                          .
      *        *-------------------------------------------------------*
      *        * [hzf]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhzf"                          .

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "eur"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "eurtpg"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "eurtpg01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "CONVERSIONI PER EURO - Tipografia Rumor "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

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
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-nam                  pic  x(04)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-des                  pic  x(46)                  .
           05  f-xxx-sts                  pic  x(02)                  .
           05  f-xxx-obj                  pic  x(40)                  .
           05  f-xxx-nrl                  pic  9(09)                  .
           05  f-xxx-nrs                  pic  9(09)                  .
           05  f-xxx-nrc                  pic  9(02)                  .
           05  f-xxx-nrd                  pic  9(01)                  .
           05  f-xxx-ope                  pic  x(02)                  .
           05  f-xxx-fas                  pic  x(01)                  .

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
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
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
      *            * Si/No richieste pre esecuzione interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico interrogazione        *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento automatico interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-aut      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Si/No primo giro di esecuzione            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-prm-gir      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento qry-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-qry.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-qry-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-liv.
                   15  w-cnt-qry-sav-l05  pic  x(64)                  .
                   15  w-cnt-qry-sav-l04  pic  x(64)                  .
                   15  w-cnt-qry-sav-l03  pic  x(64)                  .
                   15  w-cnt-qry-sav-l02  pic  x(64)                  .
                   15  w-cnt-qry-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Literals                                              *
      *        *-------------------------------------------------------*
           05  w-cnt-lit.
               10  w-cnt-lit-t80          pic  x(80) value all "="    .
      *        *-------------------------------------------------------*
      *        * Work per padding campi alfanumerici con 'z'           *
      *        *-------------------------------------------------------*
           05  w-pad-zzz.
               10  w-pad-zzz-alf.
                   15  w-pad-zzz-alf-chr
                                   occurs 20       
                                          pic  x(01)                  .
               10  w-pad-zzz-ctr          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Conversione automatica o manuale                      *
      *        *  - A1 : Automatica                                    *
      *        *  - M1 : Manuale                                       *
      *        *  - A2 : Automatica, riscrittura                       *
      *        *  - M2 : Manuale, riscrittura                          *
      *        *-------------------------------------------------------*
           05  rr-aut-man                 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice azienda                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-azi                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per presa visione                              *
      *        *-------------------------------------------------------*
           05  rr-pre-vis                 pic  x(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo conversione                           *
      *        *-------------------------------------------------------*
           05  w-exp-aut-man.
               10  w-exp-aut-man-num      pic  9(02)       value 04   .
               10  w-exp-aut-man-lun      pic  9(02)       value 40   .
               10  w-exp-aut-man-tbl.
                   15  filler             pic  x(40) value
                          "Automatica, per tutti gli archivi       "  .
                   15  filler             pic  x(40) value
                          "Manuale, un archivio alla volta         "  .
                   15  filler             pic  x(40) value
                          "Automatica, riscrittura archivi         "  .
                   15  filler             pic  x(40) value
                          "Manuale, riscrittura archivi            "  .
      *        *-------------------------------------------------------*
      *        * Work per : Risposta Si/No                             *
      *        *-------------------------------------------------------*
           05  w-exp-ris-snx.
               10  w-exp-ris-snx-num      pic  9(02)       value 02   .
               10  w-exp-ris-snx-lun      pic  9(02)       value 02   .
               10  w-exp-ris-snx-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Work-area specifica del modulo di i-o                     *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Indici di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-inx                      pic  9(03)                  .
           05  w-c01                      pic  9(03)                  .
           05  w-c02                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per messaggi errore                              *
      *        *-------------------------------------------------------*
           05  w-ide-dat                  pic  x(08)                  .
           05  w-ide-saa                  pic  x(03)                  .
           05  w-ide-mes                  pic  x(02)                  .
           05  w-ide-arc                  pic  x(07)                  .
           05  w-ide-mag                  pic  x(07)                  .
           05  w-ide-prt                  pic  x(11)                  .
           05  w-ide-prg                  pic  x(09)                  .

      *    *===========================================================*
      *    * Work per conversione in Euro                              *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/prg/cpy/cnveur01.wkl"                   .

      *    *===========================================================*
      *    * Work area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatori generici                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-001              pic  9(06)v9(03)            .
           05  w-wrk-ctr-002              pic  9(03)v9(05)            .
           05  w-wrk-ctr-003              pic  9(06)                  .

      *    *===========================================================*
      *    * Work area per tabella archivi                             *
      *    *-----------------------------------------------------------*
       01  w-arc.
           05  w-arc-ele.
               10  w-arc-ele-max          pic  9(03) value 036        .
               10  w-arc-ele-tbl.
                   15  filler             pic  x(55) value
             "[had]      Tabella addetti                             ".
                   15  filler             pic  x(55) value
             "[han]      Tabella annotazioni                         ".
                   15  filler             pic  x(55) value
             "[har]      Righe di annotazione per le commesse        ".
                   15  filler             pic  x(55) value
             "[hba]      Bancali per commessa                        ".
                   15  filler             pic  x(55) value
             "[hce]      Tabella centri di costo                     ".
                   15  filler             pic  x(55) value
             "[hcn]      Movimenti per gestione controlli            ".
                   15  filler             pic  x(55) value
             "[hcr]      Movimenti per gestione lavorazioni : righe  ".
                   15  filler             pic  x(55) value
             "[hct]      Testate commesse di lavorazione             ".
                   15  filler             pic  x(55) value
             "[hcx]      Movimenti gestione commesse : estensioni    ".
                   15  filler             pic  x(55) value
             "[hma]      Tabella macchine                            ".
                   15  filler             pic  x(55) value
             "[hmn]      Movimenti per gestione manutenzioni         ".
                   15  filler             pic  x(55) value
             "[hms]      Saldi quantita' per gestione impegnato      ".
                   15  filler             pic  x(55) value
             "[hmx]      Commesse di lavorazione : estensioni        ".
                   15  filler             pic  x(55) value
             "[hnc]      Non conformita' commesse                    ".
                   15  filler             pic  x(55) value
             "[hnu]      Numerazioni per 'gct'                       ".
                   15  filler             pic  x(55) value
             "[hot]      Ordini di lavoro : testate                  ".
                   15  filler             pic  x(55) value
             "[hox]      Ordini di lavoro : estensioni per le righe  ".
                   15  filler             pic  x(55) value
             "[hpc]      Tabella parametri di controllo              ".
                   15  filler             pic  x(55) value
             "[hpd]      Tabelle descrizioni per preventivi          ".
                   15  filler             pic  x(55) value
             "[hpl]      Assegnazione lavorazioni previste           ".
                   15  filler             pic  x(55) value
             "[hpm]      Tabella parametri di manutenzione           ".
                   15  filler             pic  x(55) value
             "[hpr]      Tabella % media di ricarico                 ".
                   15  filler             pic  x(55) value
             "[hpv]      Tabelle descrizioni per preventivi          ".
                   15  filler             pic  x(55) value
             "[hrc]      Tabella responsabili delle commesse         ".
                   15  filler             pic  x(55) value
             "[hsr]      Scheda tecnica commessa : righe impianti    ".
                   15  filler             pic  x(55) value
             "[hst]      Scheda tecnica commessa : testata           ".
                   15  filler             pic  x(55) value
             "[hsx]      Scheda tecnica commessa : estensioni test.  ".
                   15  filler             pic  x(55) value
             "[hsy]      Scheda tecnica commessa : estensioni riga   ".
                   15  filler             pic  x(55) value
             "[htl]      Tabella tipi di lavoro                      ".
                   15  filler             pic  x(55) value
             "[htn]      Tabella Tipi di non conformita'             ".
                   15  filler             pic  x(55) value
             "[hut]      Ordini di lavoro preventivi : testate       ".
                   15  filler             pic  x(55) value
             "[hux]      Ordini di lavoro preventivi : estensioni    ".
                   15  filler             pic  x(55) value
             "[hvr]      Schede di lavorazione preventivi : righe    ".
                   15  filler             pic  x(55) value
             "[hvt]      Scheda tecnica preventivo                   ".
                   15  filler             pic  x(55) value
             "[hvx]      Scheda tecnica preventivo : estensioni      ".
                   15  filler             pic  x(55) value
             "[hvy]      Scheda tecnica preventivo : estensioni riga ".
               10  w-arc-ele-tbr redefines
                   w-arc-ele-tbl.
                   15  w-arc-ele-ele occurs 036.
                       20  filler         pic  x(01)                  .
                       20  w-arc-ele-nam  pic  x(03)                  .
                       20  filler         pic  x(05)                  .
                       20  w-arc-ele-des  pic  x(46)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
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
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Se no richieste : a esecuzione conversione      *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-500.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-500.
      *              *-------------------------------------------------*
      *              * Esecuzione conversione file                     *
      *              *-------------------------------------------------*
           perform   exe-cnv-fil-000      thru exe-cnv-fil-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a fine programma              *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Test se fine esecuzione da operatore            *
      *              *-------------------------------------------------*
           if        w-cnt-qry-rou-pri    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-750
           else      go to main-250.
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

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
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
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
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *                  *---------------------------------------------*
      *                  * Tipo conversione                            *
      *                  *---------------------------------------------*
           perform   acc-aut-man-000      thru acc-aut-man-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           perform   acc-cod-azi-000      thru acc-cod-azi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
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
           move      "Conferma esecuzione del programma (S/E) ?"
                                          to   v-not                  .
           move      " "                  to   v-alf                  .
           move      "SE"                 to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
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
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo conversione                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conversione           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Codice azienda                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice azienda precedente  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione linea di separazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo conversione           *
      *    *-----------------------------------------------------------*
       acc-aut-man-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-aut-man-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aut-man-lun    to   v-car                  .
           move      w-exp-aut-man-num    to   v-ldt                  .
           move      "1234#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aut-man-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-aut-man           =    "A1"
                     move  01             to   v-num
           else if   rr-aut-man           =    "M1"
                     move  02             to   v-num
           else if   rr-aut-man           =    "A2"
                     move  03             to   v-num
           else if   rr-aut-man           =    "M2"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-aut-man-999.
       acc-aut-man-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "A1"           to   rr-aut-man
           else if   v-num                =    02
                     move  "M1"           to   rr-aut-man
           else if   v-num                =    03
                     move  "A2"           to   rr-aut-man
           else if   v-num                =    04
                     move  "M2"           to   rr-aut-man
           else      move  spaces         to   rr-aut-man             .
       acc-aut-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    spaces
                     go to acc-aut-man-100.
       acc-aut-man-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-aut-man-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-aut-man-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-aut-man-100.
       acc-aut-man-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice azienda                             *
      *    *-----------------------------------------------------------*
       acc-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-azi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-cod-azi           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-azi-999.
       acc-cod-azi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-azi             .
       acc-cod-azi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-cod-azi           =    spaces
                     go to acc-cod-azi-100.
       acc-cod-azi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-azi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-azi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-azi-100.
       acc-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice azienda                          *
      *    *-----------------------------------------------------------*
       vis-cod-azi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-azi           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-azi-999.
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
      *              * Controllo su tipo conversione                   *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "A1" or
                     rr-aut-man           =    "M1" or
                     rr-aut-man           =    "A2" or
                     rr-aut-man           =    "M2"
                     go to tdo-ric-sel-200.
           move      "ME"                 to   v-ope                  .
           move      "Tipo conversione errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su Codice azienda                     *
      *              *-------------------------------------------------*
           if        rr-cod-azi           not  = spaces
                     go to tdo-ric-sel-999.
           move      "ME"                 to   v-ope                  .
           move      "Manca il codice azienda !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-cod-azi             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di conversione                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fil-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-fil-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione tipo operazione da eseguire    *
      *              *-------------------------------------------------*
           if        rr-aut-man (2 : 1)   =    "1"
                     move  "PT"           to   f-xxx-ope
           else      move  "FP"           to   f-xxx-ope              .
       exe-cnv-fil-200.
      *              *-------------------------------------------------*
      *              * Tipo fase                                       *
      *              *-------------------------------------------------*
           move      "C"                  to   f-xxx-fas              .
      *              *-------------------------------------------------*
      *              * Copia archivi                                   *
      *              *-------------------------------------------------*
           perform   exe-cpp-fil-000      thru exe-cpp-fil-999        .
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-fil-300.
      *              *-------------------------------------------------*
      *              * Tipo fase                                       *
      *              *-------------------------------------------------*
           move      "X"                  to   f-xxx-fas              .
      *              *-------------------------------------------------*
      *              * Record letti                                    *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Record letti               :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Record scritti                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Record scritti             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Conversione [hla]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hla-000      thru exe-cnv-hla-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hls]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hls-000      thru exe-cnv-hls-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hmc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hmc-000      thru exe-cnv-hmc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hor]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hor-000      thru exe-cnv-hor-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hpt]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hpt-000      thru exe-cnv-hpt-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hrp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hrp-000      thru exe-cnv-hrp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hur]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hur-000      thru exe-cnv-hur-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hyf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hyf-000      thru exe-cnv-hyf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hzf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hzf-000      thru exe-cnv-hzf-999        .
       exe-cnv-fil-800.
      *              *-------------------------------------------------*
      *              * Visualizzazione rullino messaggi                *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-cnv-fil-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di copia                               *
      *    *-----------------------------------------------------------*
       exe-cpp-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori di comodo             *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
           move      zero                 to   w-wrk-ctr-002          .
       exe-cpp-fil-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-cpp-fil-200.
           add       1                    to   w-inx                  .
           if        w-inx                >    w-arc-ele-max
                     go to exe-cpp-fil-900.
       exe-cpp-fil-300.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      w-arc-ele-nam
                    (w-inx)               to   f-xxx-nam              .
           move      w-arc-ele-des
                    (w-inx)               to   f-xxx-des              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : a riciclo                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cpp-fil-200.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-cpp-fil-350.
      *              *-------------------------------------------------*
      *              * Accettazione del tasto per l'interruzione       *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
           move      "AA"                 to   v-ope                  .
           move      21                   to   v-lin                  .
           move      80                   to   v-pos                  .
           move      "[4] "               to   v-pfk (16)             .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se attesa disattivata : ad uscita           *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to exe-cpp-fil-370.
       exe-cpp-fil-360.
      *                  *---------------------------------------------*
      *                  * Se 'Pf4'                                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "[4] "
                     go to exe-cpp-fil-370.
      *                  *---------------------------------------------*
      *                  * Messaggio di interruzione                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "*** ESECUZIONE COPIA INTERROTTA ! ***"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cpp-fil-900.
       exe-cpp-fil-370.
      *              *-------------------------------------------------*
      *              * Aggiornamento contatore                         *
      *              *-------------------------------------------------*
           add       1                    to   w-wrk-ctr-001          .
      *              *-------------------------------------------------*
      *              * Determinazione percentuale avanzamento          *
      *              *-------------------------------------------------*
           divide    w-arc-ele-max        into w-wrk-ctr-001
                                        giving w-wrk-ctr-002          .
           multiply  100                  by   w-wrk-ctr-002          .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "%"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione %                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-wrk-ctr-002        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-cpp-fil-400.
      *              *-------------------------------------------------*
      *              * Preparazione File Copy by Pathname              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione                            *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di origine                         *
      *                  *---------------------------------------------*
           move      spaces               to   f-xxx-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-xxx-pat              .
      *
           move      "A5"                 to   o-ope                  .
           move      f-xxx-pat            to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di destinazione                    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      spaces               to   f-xxx-pat              .
           string    "/abd/azi/"
                                delimited by size
                     s-azi      delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-xxx-pat              .
      *
           move      "A5"                 to   o-ope                  .
           move      f-xxx-pat            to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       exe-cpp-fil-500.
      *              *-------------------------------------------------*
      *              * Esecuzione File Copy by Pathname                *
      *              *-------------------------------------------------*
           move      "CP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       exe-cpp-fil-600.
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cpp-fil-650.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cpp-fil-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cpp-fil-200.
       exe-cpp-fil-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cpp-fil-999.
       exe-cpp-fil-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hla]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hla-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hla "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhla"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hla-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hla-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hla                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hla-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hla-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hla                                       .
       exe-cnv-hla-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hla-k01                .
           start     hla    key not less
                            hla-k01
                            invalid key
                            go to exe-cnv-hla-800.
       exe-cnv-hla-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hla    next
                            with no lock
                            at end
                            go to exe-cnv-hla-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hla-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hla-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hla                 .
       exe-cnv-hla-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hla                 .
           move      hla-ide-dat          to   rf-hla-ide-dat         .
           move      hla-ide-ute          to   rf-hla-ide-ute         .
           move      hla-ide-fas          to   rf-hla-ide-fas         .
           move      hla-num-lav          to   rf-hla-num-lav         .
           move      hla-alf-lav          to   rf-hla-alf-lav         .
           move      hla-syn-lav          to   rf-hla-syn-lav         .
           move      hla-des-key          to   rf-hla-des-key         .
           move      hla-des-lav          to   rf-hla-des-lav         .
           move      hla-cla-lav          to   rf-hla-cla-lav         .
           move      hla-gru-lav          to   rf-hla-gru-lav         .
           move      hla-sgr-lav          to   rf-hla-sgr-lav         .
           move      hla-tip-lav          to   rf-hla-tip-lav         .
           move      hla-umi-prd          to   rf-hla-umi-prd         .
           move      hla-dec-qta          to   rf-hla-dec-qta         .
           move      hla-snx-2qt          to   rf-hla-snx-2qt         .
           move      hla-dec-2qt          to   rf-hla-dec-2qt         .
           move      hla-snx-3qt          to   rf-hla-snx-3qt         .
           move      hla-dec-3qt          to   rf-hla-dec-3qt         .
           move      hla-tip-vpr          to   rf-hla-tip-vpr         .
           move      hla-coe-mol          to   rf-hla-coe-mol         .
           move      hla-coe-div          to   rf-hla-coe-div         .
           move      hla-ann-rif          to   rf-hla-ann-rif         .
           move      hla-val-med          to   rf-hla-val-med         .
           move      hla-cod-s01          to   rf-hla-cod-s01         .
           move      hla-cod-s02          to   rf-hla-cod-s02         .
           move      hla-cod-s03          to   rf-hla-cod-s03         .
           move      hla-cla-bdg          to   rf-hla-cla-bdg         .
           move      hla-alx-exp          to   rf-hla-alx-exp         .
       exe-cnv-hla-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore medio nell'anno di riferimento       *
      *                  *---------------------------------------------*
           move      hla-val-med          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hla-val-med         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hla-msg-000
                                          thru exe-cnv-hla-msg-999    .
       exe-cnv-hla-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hla                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hla-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hla-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hla-250.
       exe-cnv-hla-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hla                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hla                                              .
       exe-cnv-hla-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hla-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hla-999.
       exe-cnv-hla-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hla]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hla-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing codice                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hla-num-lav          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-arc              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-arc  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hla-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hls]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hls-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hls "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhls"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hls-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hls-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hls                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hls-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hls-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hls                                       .
       exe-cnv-hls-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hls-k01                .
           start     hls    key not less
                            hls-k01
                            invalid key
                            go to exe-cnv-hls-800.
       exe-cnv-hls-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hls    next
                            with no lock
                            at end
                            go to exe-cnv-hls-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hls-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hls-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hls                 .
       exe-cnv-hls-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hls                 .
           move      hls-ide-dat          to   rf-hls-ide-dat         .
           move      hls-ide-ute          to   rf-hls-ide-ute         .
           move      hls-ide-fas          to   rf-hls-ide-fas         .
           move      hls-cod-lst          to   rf-hls-cod-lst         .
           move      hls-rif-cmq          to   rf-hls-rif-cmq         .
           move      hls-num-prg          to   rf-hls-num-prg         .
           move      hls-dec-prz          to   rf-hls-dec-prz         .
           move      hls-prz-00a          to   rf-hls-prz-00a         .
           move      hls-prz-00b          to   rf-hls-prz-00b         .
           move      hls-prz-00c          to   rf-hls-prz-00c         .
           move      hls-alx-exp          to   rf-hls-alx-exp         .
       exe-cnv-hls-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo (A)                                  *
      *                  *---------------------------------------------*
           move      hls-prz-00a          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hls-prz-00a         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hls-msg-000
                                          thru exe-cnv-hls-msg-999    .
      *                  *---------------------------------------------*
      *                  * Prezzo (B)                                  *
      *                  *---------------------------------------------*
           move      hls-prz-00b          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hls-prz-00b         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hls-msg-000
                                          thru exe-cnv-hls-msg-999    .
      *                  *---------------------------------------------*
      *                  * Prezzo (C)                                  *
      *                  *---------------------------------------------*
           move      hls-prz-00c          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hls-prz-00c         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hls-msg-000
                                          thru exe-cnv-hls-msg-999    .
       exe-cnv-hls-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hls                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hls-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hls-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hls-250.
       exe-cnv-hls-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hls                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hls                                              .
       exe-cnv-hls-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hls-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hls-999.
       exe-cnv-hls-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hls]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hls-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing riferimento                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hls-rif-cmq          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-arc              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-arc  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hls-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hmc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hmc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hmc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhmc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hmc-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hmc-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hmc                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hmc-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hmc-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hmc                                       .
       exe-cnv-hmc-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hmc-k01                .
           start     hmc    key not less
                            hmc-k01
                            invalid key
                            go to exe-cnv-hmc-800.
       exe-cnv-hmc-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hmc    next
                            with no lock
                            at end
                            go to exe-cnv-hmc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hmc-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hmc-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hmc                 .
       exe-cnv-hmc-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hmc                 .
           move      hmc-ide-dat          to   rf-hmc-ide-dat         .
           move      hmc-ide-ute          to   rf-hmc-ide-ute         .
           move      hmc-ide-fas          to   rf-hmc-ide-fas         .
           move      hmc-cod-dpz          to   rf-hmc-cod-dpz         .
           move      hmc-dat-reg          to   rf-hmc-dat-reg         .
           move      hmc-num-prt          to   rf-hmc-num-prt         .
           move      hmc-prt-cml          to   rf-hmc-prt-cml         .
           move      hmc-tip-cml          to   rf-hmc-tip-cml         .
           move      hmc-prt-mag          to   rf-hmc-prt-mag         .
           move      hmc-prt-orl          to   rf-hmc-prt-orl         .
           move      hmc-prt-orf          to   rf-hmc-prt-orf         .
           move      hmc-cod-cdc          to   rf-hmc-cod-cdc         .
           move      hmc-snx-mac          to   rf-hmc-snx-mac         .
           move      hmc-cod-mac          to   rf-hmc-cod-mac         .
           move      hmc-cod-adt          to   rf-hmc-cod-adt         .
           move      hmc-tip-cpm          to   rf-hmc-tip-cpm         .
           move      hmc-num-cpm          to   rf-hmc-num-cpm         .
           move      hmc-alf-cpm          to   rf-hmc-alf-cpm         .
           move      hmc-var-cpm          to   rf-hmc-var-cpm         .
           move      hmc-udm-cpm          to   rf-hmc-udm-cpm         .
           move      hmc-cod-fnt          to   rf-hmc-cod-fnt         .
           move      hmc-dpz-fnt          to   rf-hmc-dpz-fnt         .
           move      hmc-ddo-rif          to   rf-hmc-ddo-rif         .
           move      hmc-ndo-rif          to   rf-hmc-ndo-rif         .
           move      hmc-qta-prv          to   rf-hmc-qta-prv         .
           move      hmc-qta-eff          to   rf-hmc-qta-eff         .
           move      hmc-snx-ivc          to   rf-hmc-snx-ivc         .
           move      hmc-val-mag          to   rf-hmc-val-mag         .
           move      hmc-val-std          to   rf-hmc-val-std         .
           move      hmc-per-ric          to   rf-hmc-per-ric         .
           move      hmc-val-prv          to   rf-hmc-val-prv         .
           move      hmc-val-eff          to   rf-hmc-val-eff         .
           move      hmc-not-mov          to   rf-hmc-not-mov         .
           move      hmc-snx-agg          to   rf-hmc-snx-agg         .
           move      hmc-flg-ela          to   rf-hmc-flg-ela         .
           move      hmc-flg-pul          to   rf-hmc-flg-pul         .
           move      hmc-alx-exp          to   rf-hmc-alx-exp         .
       exe-cnv-hmc-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore unitario determinato dal magazzino   *
      *                  *---------------------------------------------*
           move      hmc-val-mag          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hmc-val-mag         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hmc-msg-000
                                          thru exe-cnv-hmc-msg-999    .
      *                  *---------------------------------------------*
      *                  * Costo unitario standard                     *
      *                  *---------------------------------------------*
           move      hmc-val-std          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hmc-val-std         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hmc-msg-000
                                          thru exe-cnv-hmc-msg-999    .
      *                  *---------------------------------------------*
      *                  * Valore unitario previsto                    *
      *                  *---------------------------------------------*
           move      hmc-val-prv          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hmc-val-prv         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hmc-msg-000
                                          thru exe-cnv-hmc-msg-999    .
      *                  *---------------------------------------------*
      *                  * Valore unitario effettivo                   *
      *                  *---------------------------------------------*
           move      hmc-val-eff          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hmc-val-eff         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hmc-msg-000
                                          thru exe-cnv-hmc-msg-999    .
       exe-cnv-hmc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hmc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hmc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hmc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hmc-250.
       exe-cnv-hmc-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hmc                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hmc                                              .
       exe-cnv-hmc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hmc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hmc-999.
       exe-cnv-hmc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hmc]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hmc-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hmc-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hmc-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hor]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hor-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hor "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhor"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hor-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hor-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hor-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hor-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hor                                       .
       exe-cnv-hor-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hor-k01                .
           start     hor    key not less
                            hor-k01
                            invalid key
                            go to exe-cnv-hor-800.
       exe-cnv-hor-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hor    next
                            with no lock
                            at end
                            go to exe-cnv-hor-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hor-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hor-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
       exe-cnv-hor-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hor                 .
           move      hor-num-prt          to   rf-hor-num-prt         .
           move      hor-num-prg          to   rf-hor-num-prg         .
           move      hor-tmo-orl          to   rf-hor-tmo-orl         .
           move      hor-cod-dpz          to   rf-hor-cod-dpz         .
           move      hor-prt-cml          to   rf-hor-prt-cml         .
           move      hor-tip-rig          to   rf-hor-tip-rig         .
           move      hor-cod-cdc          to   rf-hor-cod-cdc         .
           move      hor-tip-mag          to   rf-hor-tip-mag         .
           move      hor-num-mag          to   rf-hor-num-mag         .
           move      hor-alf-mag          to   rf-hor-alf-mag         .
           move      hor-umi-lav          to   rf-hor-umi-lav         .
           move      hor-dec-qta          to   rf-hor-dec-qta         .
           move      hor-des-ext          to   rf-hor-des-ext         .
           move      hor-des-rig          to   rf-hor-des-rig         .
           move      hor-qta-ord          to   rf-hor-qta-ord         .
           move      hor-prz-uni          to   rf-hor-prz-uni         .
           move      hor-dat-cns          to   rf-hor-dat-cns         .
           move      hor-prt-mcl          to   rf-hor-prt-mcl         .
           move      hor-flg-ela          to   rf-hor-flg-ela         .
           move      hor-flg-pul          to   rf-hor-flg-pul         .
           move      hor-alx-exp          to   rf-hor-alx-exp         .
       exe-cnv-hor-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario                             *
      *                  *---------------------------------------------*
           move      hor-prz-uni          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hor-prz-uni         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hor-msg-000
                                          thru exe-cnv-hor-msg-999    .
       exe-cnv-hor-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hor-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hor-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hor-250.
       exe-cnv-hor-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hor                                              .
       exe-cnv-hor-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hor-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hor-999.
       exe-cnv-hor-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hor]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hor-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hor-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hor-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-prg  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hor-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hpt]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hpt-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hpt "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhpt"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hpt-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hpt-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpt                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hpt-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hpt-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hpt                                       .
       exe-cnv-hpt-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hpt-k01                .
           start     hpt    key not less
                            hpt-k01
                            invalid key
                            go to exe-cnv-hpt-800.
       exe-cnv-hpt-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hpt    next
                            with no lock
                            at end
                            go to exe-cnv-hpt-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hpt-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hpt-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpt                 .
       exe-cnv-hpt-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hpt                 .
           move      hpt-ide-dat          to   rf-hpt-ide-dat         .
           move      hpt-ide-ute          to   rf-hpt-ide-ute         .
           move      hpt-ide-fas          to   rf-hpt-ide-fas         .
           move      hpt-num-prt          to   rf-hpt-num-prt         .
           move      hpt-prt-cml          to   rf-hpt-prt-cml         .
           move      hpt-dat-reg          to   rf-hpt-dat-reg         .
           move      hpt-cod-dpz          to   rf-hpt-cod-dpz         .
           move      hpt-cod-tpr          to   rf-hpt-cod-tpr         .
           move      hpt-tip-prv          to   rf-hpt-tip-prv         .
           move      hpt-dat-chi          to   rf-hpt-dat-chi         .
           move      hpt-dat-cop          to   rf-hpt-dat-cop         .
           move      hpt-dat-doc          to   rf-hpt-dat-doc         .
           move      hpt-num-doc          to   rf-hpt-num-doc         .
           move      hpt-cod-cli          to   rf-hpt-cod-cli         .
           move      hpt-dpz-cli          to   rf-hpt-dpz-cli         .
           move      hpt-cod-fop          to   rf-hpt-cod-fop         .
           move      hpt-cod-age          to   rf-hpt-cod-age         .
           move      hpt-cod-dst          to   rf-hpt-cod-dst         .
           move      hpt-dpz-dst          to   rf-hpt-dpz-dst         .
           move      hpt-prt-ftr          to   rf-hpt-prt-ftr         .
           move      hpt-cod-rdp          to   rf-hpt-cod-rdp         .
           move      hpt-dat-rcl          to   rf-hpt-dat-rcl         .
           move      hpt-num-rcl          to   rf-hpt-num-rcl         .
           move      hpt-num-cpp (1)      to   rf-hpt-num-cpp (1)     .
           move      hpt-num-cpp (2)      to   rf-hpt-num-cpp (2)     .
           move      hpt-num-cpp (3)      to   rf-hpt-num-cpp (3)     .
           move      hpt-num-cpp (4)      to   rf-hpt-num-cpp (4)     .
           move      hpt-num-cpp (5)      to   rf-hpt-num-cpp (5)     .
           move      hpt-num-cpp (6)      to   rf-hpt-num-cpp (6)     .
           move      hpt-vmn-cpp (1)      to   rf-hpt-vmn-cpp (1)     .
           move      hpt-vmn-cpp (2)      to   rf-hpt-vmn-cpp (2)     .
           move      hpt-vmn-cpp (3)      to   rf-hpt-vmn-cpp (3)     .
           move      hpt-vmn-cpp (4)      to   rf-hpt-vmn-cpp (4)     .
           move      hpt-vmn-cpp (5)      to   rf-hpt-vmn-cpp (5)     .
           move      hpt-vmn-cpp (6)      to   rf-hpt-vmn-cpp (6)     .
           move      hpt-des-prv          to   rf-hpt-des-prv         .
           move      hpt-ann-prv          to   rf-hpt-ann-prv         .
           move      hpt-flg-ela          to   rf-hpt-flg-ela         .
           move      hpt-flg-pul          to   rf-hpt-flg-pul         .
           move      hpt-nom-int          to   rf-hpt-nom-int         .
           move      hpt-num-tel          to   rf-hpt-num-tel         .
           move      hpt-num-cdr          to   rf-hpt-num-cdr         .
           move      hpt-cod-cns          to   rf-hpt-cod-cns         .
           move      hpt-des-cns          to   rf-hpt-des-cns         .
           move      hpt-cst-ccs          to   rf-hpt-cst-ccs         .
           move      hpt-cat-lav          to   rf-hpt-cat-lav         .
           move      hpt-mod-pag          to   rf-hpt-mod-pag         .
           move      hpt-num-fax          to   rf-hpt-num-fax         .
           move      hpt-iem-ail          to   rf-hpt-iem-ail         .
           move      hpt-num-pro          to   rf-hpt-num-pro         .
           move      hpt-vde-cod (01)     to   rf-hpt-vde-cod (01)    .
           move      hpt-vde-cod (02)     to   rf-hpt-vde-cod (02)    .
           move      hpt-vde-cod (03)     to   rf-hpt-vde-cod (03)    .
           move      hpt-vde-cod (04)     to   rf-hpt-vde-cod (04)    .
           move      hpt-vde-cod (05)     to   rf-hpt-vde-cod (05)    .
           move      hpt-vde-cod (06)     to   rf-hpt-vde-cod (06)    .
           move      hpt-vde-cod (07)     to   rf-hpt-vde-cod (07)    .
           move      hpt-vde-cod (08)     to   rf-hpt-vde-cod (08)    .
           move      hpt-vde-cod (09)     to   rf-hpt-vde-cod (09)    .
           move      hpt-vde-cod (10)     to   rf-hpt-vde-cod (10)    .
           move      hpt-vde-cod (11)     to   rf-hpt-vde-cod (11)    .
           move      hpt-vde-cod (12)     to   rf-hpt-vde-cod (12)    .
           move      hpt-ppv-age          to   rf-hpt-ppv-age         .
           move      hpt-per-scp          to   rf-hpt-per-scp         .
           move      hpt-fir-map          to   rf-hpt-fir-map         .
           move      hpt-cod-iva          to   rf-hpt-cod-iva         .
           move      hpt-alx-exp          to   rf-hpt-alx-exp         .
       exe-cnv-hpt-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Castelletto copie                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-cnv-hpt-520.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-cnv-hpt-530.
      *                  *---------------------------------------------*
      *                  * Costo copia manuale                         *
      *                  *---------------------------------------------*
           move      hpt-vmn-cpp (w-inx)  to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hpt-vmn-cpp (w-inx) .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hpt-msg-000
                                          thru exe-cnv-hpt-msg-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-cnv-hpt-520.
       exe-cnv-hpt-530.
      *                  *---------------------------------------------*
      *                  * Costo copia successive                      *
      *                  *---------------------------------------------*
           move      hpt-cst-ccs          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hpt-cst-ccs         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hpt-msg-000
                                          thru exe-cnv-hpt-msg-999    .
       exe-cnv-hpt-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpt                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hpt-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hpt-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hpt-250.
       exe-cnv-hpt-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpt                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hpt                                              .
       exe-cnv-hpt-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hpt-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hpt-999.
       exe-cnv-hpt-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hpt]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hpt-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hpt-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hpt-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hrp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hrp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hrp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhrp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hrp-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hrp-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrp                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hrp-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hrp-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hrp                                       .
       exe-cnv-hrp-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hrp-k01                .
           start     hrp    key not less
                            hrp-k01
                            invalid key
                            go to exe-cnv-hrp-800.
       exe-cnv-hrp-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hrp    next
                            with no lock
                            at end
                            go to exe-cnv-hrp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hrp-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hrp-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrp                 .
       exe-cnv-hrp-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hrp                 .
           move      hrp-ide-dat          to   rf-hrp-ide-dat         .
           move      hrp-ide-ute          to   rf-hrp-ide-ute         .
           move      hrp-ide-fas          to   rf-hrp-ide-fas         .
           move      hrp-prt-prv          to   rf-hrp-prt-prv         .
           move      hrp-num-prg          to   rf-hrp-num-prg         .
           move      hrp-cod-lav          to   rf-hrp-cod-lav         .
           move      hrp-tip-rig          to   rf-hrp-tip-rig         .
           move      hrp-ind-prz          to   rf-hrp-ind-prz         .
           move      hrp-qta-rig          to   rf-hrp-qta-rig         .
           move      hrp-prz-lav          to   rf-hrp-prz-lav         .
           move      hrp-stp-not          to   rf-hrp-stp-not         .
           move      hrp-dat-prv          to   rf-hrp-dat-prv         .
           move      hrp-dat-eff          to   rf-hrp-dat-eff         .
           move      hrp-tst-400          to   rf-hrp-tst-400         .
           move      hrp-alx-exp          to   rf-hrp-alx-exp         .
       exe-cnv-hrp-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo lavorazione                          *
      *                  *---------------------------------------------*
           move      hrp-prz-lav          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hrp-prz-lav         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hrp-msg-000
                                          thru exe-cnv-hrp-msg-999    .
       exe-cnv-hrp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hrp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hrp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hrp-250.
       exe-cnv-hrp-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrp                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hrp                                              .
       exe-cnv-hrp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hrp-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hrp-999.
       exe-cnv-hrp-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hrp]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hrp-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hrp-prt-prv          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hrp-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-prg  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hrp-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hur]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hur-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hur "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhur"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hur-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hur-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hur-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hur-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hur                                       .
       exe-cnv-hur-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hur-k01                .
           start     hur    key not less
                            hur-k01
                            invalid key
                            go to exe-cnv-hur-800.
       exe-cnv-hur-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hur    next
                            with no lock
                            at end
                            go to exe-cnv-hur-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hur-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hur-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
       exe-cnv-hur-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hur                 .
           move      hur-num-prt          to   rf-hur-num-prt         .
           move      hur-num-prg          to   rf-hur-num-prg         .
           move      hur-tmo-orl          to   rf-hur-tmo-orl         .
           move      hur-cod-dpz          to   rf-hur-cod-dpz         .
           move      hur-prt-prv          to   rf-hur-prt-prv         .
           move      hur-ver-prv          to   rf-hur-ver-prv         .
           move      hur-tip-rig          to   rf-hur-tip-rig         .
           move      hur-cod-cdc          to   rf-hur-cod-cdc         .
           move      hur-tip-mag          to   rf-hur-tip-mag         .
           move      hur-num-mag          to   rf-hur-num-mag         .
           move      hur-alf-mag          to   rf-hur-alf-mag         .
           move      hur-umi-lav          to   rf-hur-umi-lav         .
           move      hur-dec-qta          to   rf-hur-dec-qta         .
           move      hur-des-ext          to   rf-hur-des-ext         .
           move      hur-des-rig          to   rf-hur-des-rig         .
           move      hur-qta-ord          to   rf-hur-qta-ord         .
           move      hur-prz-uni          to   rf-hur-prz-uni         .
           move      hur-dat-cns          to   rf-hur-dat-cns         .
           move      hur-prt-mcl          to   rf-hur-prt-mcl         .
           move      hur-flg-ela          to   rf-hur-flg-ela         .
           move      hur-flg-pul          to   rf-hur-flg-pul         .
           move      hur-alx-exp          to   rf-hur-alx-exp         .
       exe-cnv-hur-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario                             *
      *                  *---------------------------------------------*
           move      hur-prz-uni          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hur-prz-uni         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hur-msg-000
                                          thru exe-cnv-hur-msg-999    .
       exe-cnv-hur-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hur-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hur-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hur-250.
       exe-cnv-hur-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hur                                              .
       exe-cnv-hur-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hur-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hur-999.
       exe-cnv-hur-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hur]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hur-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hur-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hur-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-prg  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hur-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hyf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hyf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hyf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhyf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hyf-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hyf-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hyf                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hyf-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hyf-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hyf                                       .
       exe-cnv-hyf-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hyf-k01                .
           start     hyf    key not less
                            hyf-k01
                            invalid key
                            go to exe-cnv-hyf-800.
       exe-cnv-hyf-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hyf    next
                            with no lock
                            at end
                            go to exe-cnv-hyf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hyf-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hyf-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hyf                 .
       exe-cnv-hyf-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hyf                 .
           move      hyf-prt-prv          to   rf-hyf-prt-prv         .
           move      hyf-num-prg          to   rf-hyf-num-prg         .
           move      hyf-tip-rig          to   rf-hyf-tip-rig         .
           move      hyf-fmt-bas          to   rf-hyf-fmt-bas         .
           move      hyf-fmt-alt          to   rf-hyf-fmt-alt         .
           move      hyf-fmt-cmq          to   rf-hyf-fmt-cmq         .
           move      hyf-per-scn          to   rf-hyf-per-scn         .
           move      hyf-cod-lst          to   rf-hyf-cod-lst         .
           move      hyf-dec-prz          to   rf-hyf-dec-prz         .
           move      hyf-prz-uni          to   rf-hyf-prz-uni         .
           move      hyf-num-fff          to   rf-hyf-num-fff         .
           move      hyf-tot-rig          to   rf-hyf-tot-rig         .
           move      hyf-alx-exp          to   rf-hyf-alx-exp         .
       exe-cnv-hyf-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario                             *
      *                  *---------------------------------------------*
           move      hyf-prz-uni          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hyf-prz-uni         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hyf-msg-000
                                          thru exe-cnv-hyf-msg-999    .
       exe-cnv-hyf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hyf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hyf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hyf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hyf-250.
       exe-cnv-hyf-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hyf                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hyf                                              .
       exe-cnv-hyf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hyf-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hyf-999.
       exe-cnv-hyf-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hyf]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hyf-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hyf-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prg  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hyf-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hzf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hzf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hzf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhzf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hzf-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Numero records letti e scritti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   f-xxx-nrl              .
           move      zero                 to   f-xxx-nrs              .
       exe-cnv-hzf-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hzf                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hzf-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hzf-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hzf                                       .
       exe-cnv-hzf-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hzf-k01                .
           start     hzf    key not less
                            hzf-k01
                            invalid key
                            go to exe-cnv-hzf-800.
       exe-cnv-hzf-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hzf    next
                            with no lock
                            at end
                            go to exe-cnv-hzf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hzf-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hzf-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hzf                 .
       exe-cnv-hzf-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hzf                 .
           move      hzf-prt-cml          to   rf-hzf-prt-cml         .
           move      hzf-num-prg          to   rf-hzf-num-prg         .
           move      hzf-tip-rig          to   rf-hzf-tip-rig         .
           move      hzf-fmt-bas          to   rf-hzf-fmt-bas         .
           move      hzf-fmt-alt          to   rf-hzf-fmt-alt         .
           move      hzf-fmt-cmq          to   rf-hzf-fmt-cmq         .
           move      hzf-per-scn          to   rf-hzf-per-scn         .
           move      hzf-cod-lst          to   rf-hzf-cod-lst         .
           move      hzf-dec-prz          to   rf-hzf-dec-prz         .
           move      hzf-prz-uni          to   rf-hzf-prz-uni         .
           move      hzf-num-fff          to   rf-hzf-num-fff         .
           move      hzf-tot-rig          to   rf-hzf-tot-rig         .
           move      hzf-alx-exp          to   rf-hzf-alx-exp         .
       exe-cnv-hzf-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario                             *
      *                  *---------------------------------------------*
           move      hzf-prz-uni          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hzf-prz-uni         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hzf-msg-000
                                          thru exe-cnv-hzf-msg-999    .
       exe-cnv-hzf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hzf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hzf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hzf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hzf-250.
       exe-cnv-hzf-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hzf                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hzf                                              .
       exe-cnv-hzf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hzf-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hzf-999.
       exe-cnv-hzf-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hzf]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hzf-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hzf-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prg  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hzf-msg-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di conferma, se manuale                         *
      *    *-----------------------------------------------------------*
       ric-cnf-man-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della fase               *
      *              *-------------------------------------------------*
           if        f-xxx-fas            =    "X"
                     go to ric-cnf-man-100.
       ric-cnf-man-050.
      *              *-------------------------------------------------*
      *              * Visualizzazione copia in corso                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Copia archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] "       delimited by   size
                     f-xxx-des  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     ric-cnf-man-120.
       ric-cnf-man-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione conversione in corso            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Conversione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     ric-cnf-man-120.
       ric-cnf-man-120.
      *              *-------------------------------------------------*
      *              * Visualizzazione a spaces area per conferma      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico : uscita con Ok     *
      *              *-------------------------------------------------*
           if        rr-aut-man (1 : 1)   not  = "M"
                     move  "S"            to   f-xxx-sts
                     go to ric-cnf-man-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt per conferma             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Conferma conversione       :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione risposta                        *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-sts              .
       ric-cnf-man-200.
      *              *-------------------------------------------------*
      *              * Accettazione risposta Si/No                     *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ris-snx-lun    to   v-car                  .
           move      w-exp-ris-snx-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ris-snx-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        f-xxx-sts             =    "S"
                     move  01             to   v-num
           else if   f-xxx-sts            =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       ric-cnf-man-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   f-xxx-sts
           else if   v-num                =    02
                     move  "N"            to   f-xxx-sts
           else      move  spaces         to   f-xxx-sts              .
       ric-cnf-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S" and
                     f-xxx-sts            not  = "N"
                     go to ric-cnf-man-200.
       ric-cnf-man-999.
           exit.

      *    *===========================================================*
      *    * Scrittura rullino messaggi                                *
      *    *-----------------------------------------------------------*
       wrt-rou-msg-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della fase               *
      *              *-------------------------------------------------*
           if        f-xxx-fas            =    "X"
                     go to wrt-rou-msg-200.
       wrt-rou-msg-100.
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Copia archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] "       delimited by   size
                     f-xxx-des  delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     wrt-rou-msg-999.
       wrt-rou-msg-200.
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Conversione archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"        delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records letti   : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records scritti : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       wrt-rou-msg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione presa visione                                *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della fase               *
      *              *-------------------------------------------------*
           if        f-xxx-fas            =    "X"
                     go to acc-pre-vis-200.
       acc-pre-vis-100.
      *              *-------------------------------------------------*
      *              * Messaggio di fine copia archivio                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Fine copia relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"                  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-pre-vis-300.
       acc-pre-vis-200.
      *              *-------------------------------------------------*
      *              * Messaggio di fine conversione archivio          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Fine conversione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "]"                  delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     acc-pre-vis-300.
       acc-pre-vis-300.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se funzionamento automa-   *
      *              * tico o manuale                                  *
      *              *-------------------------------------------------*
           if        rr-aut-man (1 : 1)   =    "M"
                     go to acc-pre-vis-600
           else      go to acc-pre-vis-400.
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attesa di 1 secondo                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Se funzionamento manuale                        *
      *              *-------------------------------------------------*
       acc-pre-vis-610.
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione presa visione               *
      *                  *---------------------------------------------*
           move      spaces               to   rr-pre-vis             .
      *                  *---------------------------------------------*
      *                  * Prompt per presa visione                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      " - Digitare OK : [  ]              "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vis-620.
      *                  *---------------------------------------------*
      *                  * Accettazione presa visione                  *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      rr-pre-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   rr-pre-vis             .
           if        rr-pre-vis           not  = "OK"
                     go to acc-pre-vis-620.
       acc-pre-vis-630.
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records letti                           *
      *    *-----------------------------------------------------------*
       inc-rec-let-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrl              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrl            >    100
                     go to inc-rec-let-100
           else if   f-xxx-nrl            >    10
                     go to inc-rec-let-200
           else      go to inc-rec-let-500.
       inc-rec-let-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
       inc-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records scritti                         *
      *    *-----------------------------------------------------------*
       inc-rec-scr-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrs              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrs            >    100
                     go to inc-rec-scr-100
           else if   f-xxx-nrs            >    10
                     go to inc-rec-scr-200
           else      go to inc-rec-scr-500.
       inc-rec-scr-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
       inc-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records letti                      *
      *    *-----------------------------------------------------------*
       vis-rec-let-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records scritti                    *
      *    *-----------------------------------------------------------*
       vis-rec-scr-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Routines per la conversione                               *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/prg/cpy/cnveur01.wks"                   .

