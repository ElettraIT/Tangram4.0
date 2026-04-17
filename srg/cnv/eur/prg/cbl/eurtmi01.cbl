       Identification Division.
       Program-Id.                                 eurtmi01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    eur                 *
      *                                   Fase:    eurtmi01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/00    *
      *                       Ultima revisione:    NdK del 17/05/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per EURO - TEMI                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Copia archivi                               *
      *                                                                *
      * {gsc}   [hco]      Archivio anagrafica comuni                  *
      * {gsc}   [hlc]      Legami tra clienti                          *
      * {gsc}   [hsx]      Schede clienti : estensioni                 *
      * {smp}   [hde]      Destinatario per il pacco postale           *
      * {smp}   [hop]      Tipi operazione                             *
      * {smp}   [hps]      Personalizzazioni area 'smp'                *
      * {tmi}   [hex]      File di confronto per l'esportazione        *
      * {tmi}   [hfp]      Estensione archivio anagrafica fornitori    *
      * {tmi}   [hgr]      Record circolari aziendali : righe          *
      * {tmi}   [hgt]      Record circolari aziendali : testate        *
      * {tmi}   [hnu]      Gestione numerazioni                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Conversione                                 *
      *                                                                *
      * {gsc} v [hsc]      Schede clienti - prodotti                   *
      * {smp} v [hpp]      Movimenti per spedizione postale            *
      * {smp} v [hds]      Distinte di spedizione pacchi postali       *
      * {tmi} v [hgp]      Gestione parcelle                           *
      * {tmi} v [hdd]      Distinte di spedizione pacchi postali       *
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
      *    * File Control [hsc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hsc   assign to disk           f-hsc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hsc-k01
                   alternate record key   is hsc-k02
                   alternate record key   is hsc-k03
                   alternate record key   is hsc-k04
                   alternate record key   is hsc-k05
                             file status  is                f-hsc-sts .

      *    *===========================================================*
      *    * File Control [hpp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hpp   assign to disk           f-hpp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hpp-k01
                   alternate record key   is hpp-k02
                   alternate record key   is hpp-k03
                   alternate record key   is hpp-k04
                   alternate record key   is hpp-k05
                   alternate record key   is hpp-k06
                             file status  is                f-hpp-sts .

      *    *===========================================================*
      *    * File Control [hds]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hds   assign to disk           f-hds-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hds-k01
                   alternate record key   is hds-k02
                             file status  is                f-hds-sts .

      *    *===========================================================*
      *    * File Control [hgp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hgp   assign to disk           f-hgp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hgp-k01
                   alternate record key   is hgp-k02
                   alternate record key   is hgp-k03
                   alternate record key   is hgp-k04
                             file status  is                f-hgp-sts .

      *    *===========================================================*
      *    * File Control [hdd]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hdd   assign to disk           f-hdd-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hdd-k01
                   alternate record key   is hdd-k02
                             file status  is                f-hdd-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [hsc]                                    *
      *    *-----------------------------------------------------------*
       fd  hsc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hsc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hsc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CLIPRO                         *
      *            *---------------------------------------------------*
               10  hsc-k01.
                   15  hsc-cod-cli        pic  9(07)       comp-3     .
                   15  hsc-dpz-cli        pic  x(04)                  .
                   15  hsc-num-prg        pic  9(05)       comp-3     .
                   15  hsc-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hsc-k02.
                   15  hsc-ide-dat        pic  9(07)       comp-3     .
                   15  hsc-cod-cli-2      pic  9(07)       comp-3     .
                   15  hsc-dpz-cli-2      pic  x(04)                  .
                   15  hsc-num-prg-2      pic  9(05)       comp-3     .
                   15  hsc-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROCLI                         *
      *            *---------------------------------------------------*
               10  hsc-k03.
                   15  hsc-num-pro-3      pic  9(07)       comp-3     .
                   15  hsc-num-prg-3      pic  9(05)       comp-3     .
                   15  hsc-cod-cli-3      pic  9(07)       comp-3     .
                   15  hsc-dpz-cli-3      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : FATCPR                         *
      *            *---------------------------------------------------*
               10  hsc-k04.
                   15  hsc-prt-fat        pic  9(09)       comp-3     .
                   15  hsc-cod-cli-4      pic  9(07)       comp-3     .
                   15  hsc-dpz-cli-4      pic  x(04)                  .
                   15  hsc-num-prg-4      pic  9(05)       comp-3     .
                   15  hsc-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DDSCPR                         *
      *            *---------------------------------------------------*
               10  hsc-k05.
                   15  hsc-prt-dds        pic  9(09)       comp-3     .
                   15  hsc-cod-cli-5      pic  9(07)       comp-3     .
                   15  hsc-dpz-cli-5      pic  x(04)                  .
                   15  hsc-num-prg-5      pic  9(05)       comp-3     .
                   15  hsc-num-pro-5      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hsc-dat.
               10  hsc-ide-ute            pic  x(08)                  .
               10  hsc-ide-fas            pic  x(06)                  .
               10  hsc-qta-pro            pic  9(05)       comp-3     .
               10  hsc-sgl-vlt            pic  x(03)                  .
               10  hsc-dec-vlt            pic  9(01)                  .
               10  hsc-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  hsc-tdc-vlt            pic  x(01)                  .
               10  hsc-prz-ven            pic  9(09)       comp-3     .
               10  hsc-dat-doc            pic  9(07)       comp-3     .
               10  hsc-num-doc            pic  x(10)                  .
               10  hsc-key-acc.
                   15  hsc-key-num occurs 03
                                          pic  x(12)                  .
               10  hsc-com-gsc            pic  x(20)                  .
               10  hsc-flg-pul            pic  x(01)                  .
               10  hsc-cod-old            pic  9(05)       comp-3     .
               10  hsc-flg-spd            pic  x(01)                  .
               10  hsc-flg-sna            pic  x(01)                  .
               10  hsc-flg-dds            pic  x(01)                  .
               10  hsc-alx-exp.
                   15  filler occurs  17  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hpp]                                    *
      *    *-----------------------------------------------------------*
       fd  hpp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hpp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hpp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMMSP                         *
      *            *---------------------------------------------------*
               10  hpp-k01.
                   15  hpp-num-msp        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hpp-k02.
                   15  hpp-ide-dat        pic  9(07)       comp-3     .
                   15  hpp-num-msp-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DSPMSP                         *
      *            *---------------------------------------------------*
               10  hpp-k03.
                   15  hpp-dsp-msp        pic  9(07)       comp-3     .
                   15  hpp-num-msp-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DSTDSP                         *
      *            *---------------------------------------------------*
               10  hpp-k04.
                   15  hpp-tip-dst        pic  9(02)                  .
                   15  hpp-cod-dst        pic  9(07)       comp-3     .
                   15  hpp-dsp-msp-4      pic  9(07)       comp-3     .
                   15  hpp-num-msp-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DDSMSP                         *
      *            *---------------------------------------------------*
               10  hpp-k05.
                   15  hpp-num-dds        pic  9(11)       comp-3     .
                   15  hpp-num-msp-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DDRMSP                         *
      *            *---------------------------------------------------*
               10  hpp-k06.
                   15  hpp-prt-ddr        pic  9(11)       comp-3     .
                   15  hpp-num-msp-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hpp-dat.
               10  hpp-ide-ute            pic  x(08)                  .
               10  hpp-ide-fas            pic  x(06)                  .
               10  hpp-dtr-emi            pic  9(07)       comp-3     .
               10  hpp-tip-msp            pic  9(02)                  .
               10  hpp-tac-msp            pic  9(02)                  .
               10  hpp-dpz-dst            pic  x(04)                  .
               10  hpp-inl-msp            pic  9(02)                  .
               10  hpp-int-dst            pic  x(40)                  .
               10  hpp-pes-msp            pic  9(06)v9(03) comp-3     .
               10  hpp-cta-msp            pic  9(02)                  .
               10  hpp-taf-msp            pic  9(09)       comp-3     .
               10  hpp-cco-msp            pic  9(05)       comp-3     .
               10  hpp-tco-msp            pic  x(40)                  .
               10  hpp-cmr-msp            pic  9(05)       comp-3     .
               10  hpp-tmr-msp            pic  x(40)                  .
               10  hpp-ann-msp.
                   15  hpp-rig-ann occurs 03
                                          pic  x(40)                  .
               10  hpp-tip-ddr            pic  9(02)                  .
               10  hpp-imp-ddr            pic s9(11)       comp-3     .
               10  hpp-tmo-ddr            pic  x(05)                  .
               10  hpp-dat-ddr            pic  9(07)       comp-3     .
               10  hpp-num-ddr            pic  x(10)                  .
               10  hpp-nra-ddr            pic  9(02)                  .
               10  hpp-flc-stp            pic  9(02)                  .
               10  hpp-flg-ela.
                   15  hpp-flg-blo.
                       20  hpp-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hpp-flg-nbl.
                       20  hpp-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hpp-flg-pul            pic  x(01)                  .
               10  hpp-cts-msp            pic  9(09)                  .
               10  hpp-num-pr1            pic  9(07)                  .
               10  hpp-num-pr2            pic  9(07)                  .
               10  hpp-num-pr3            pic  9(07)                  .
               10  hpp-alx-exp.
                   15  filler occurs 50   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hds]                                    *
      *    *-----------------------------------------------------------*
       fd  hds       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hds-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hds-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMDDS                         *
      *            *---------------------------------------------------*
               10  hds-k01.
                   15  hds-num-dds        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hds-k02.
                   15  hds-ide-dat        pic  9(07)       comp-3     .
                   15  hds-num-dds-2      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hds-dat.
               10  hds-ide-ute            pic  x(08)                  .
               10  hds-ide-fas            pic  x(06)                  .
               10  hds-dtr-com            pic  9(07)       comp-3     .
               10  hds-tip-dds            pic  9(02)                  .
               10  hds-tds-das            pic  9(04)                  .
               10  hds-dsp-min            pic  9(07)       comp-3     .
               10  hds-dsp-max            pic  9(07)       comp-3     .
               10  hds-ele-max            pic  9(05)       comp-3     .
               10  hds-ele-dds            pic  9(05)       comp-3     .
               10  hds-imp-max            pic  9(13)       comp-3     .
               10  hds-imp-dds            pic  9(13)       comp-3     .
               10  hds-dtr-pre            pic  9(07)       comp-3     .
               10  hds-tip-pre            pic  9(02)                  .
               10  hds-cod-cbp            pic  x(10)                  .
               10  hds-snx-stp            pic  x(01)                  .
               10  hds-flc-stp            pic  9(02)                  .
               10  hds-snx-ads            pic  x(01)                  .
               10  hds-flc-ads            pic  9(02)                  .
               10  hds-tip-ias            pic  9(02)                  .
               10  hds-flc-ias            pic  9(02)                  .
               10  hds-dtr-act            pic  9(07)       comp-3     .
               10  hds-spe-act            pic s9(11)       comp-3     .
               10  hds-flg-ela.
                   15  hds-flg-blo.
                       20  hds-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hds-flg-nbl.
                       20  hds-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hds-flg-pul            pic  x(01)                  .
               10  hds-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hgp]                                    *
      *    *-----------------------------------------------------------*
       fd  hgp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hgp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hgp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hgp-k01.
                   15  hgp-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hgp-k02.
                   15  hgp-ide-dat        pic  9(07)       comp-3     .
                   15  hgp-dat-reg        pic  9(07)       comp-3     .
                   15  hgp-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  hgp-k03.
                   15  hgp-dat-reg-3      pic  9(07)       comp-3     .
                   15  hgp-cod-tpr        pic  x(05)                  .
                   15  hgp-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : FNTDAT                         *
      *            *---------------------------------------------------*
               10  hgp-k04.
                   15  hgp-cod-fnt        pic  9(07)       comp-3     .
                   15  hgp-dat-reg-4      pic  9(07)       comp-3     .
                   15  hgp-cod-tpr-4      pic  x(05)                  .
                   15  hgp-num-prt-4      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hgp-dat.
               10  hgp-ide-ute            pic  x(08)                  .
               10  hgp-ide-fas            pic  x(06)                  .
               10  hgp-cod-dpz            pic  9(02)                  .
               10  hgp-dat-doc            pic  9(07)       comp-3     .
               10  hgp-num-doc            pic  x(10)                  .
               10  hgp-dpz-fnt            pic  x(04)                  .
               10  hgp-snx-prf            pic  9(02)                  .
               10  hgp-snx-tpv            pic  9(02)                  .
               10  hgp-cau-rit            pic  x(40)                  .
               10  hgp-cod-trb            pic  x(05)                  .
               10  hgp-per-rit            pic  9(03)v9(02) comp-3     .
               10  hgp-per-ctb            pic  9(03)v9(02) comp-3     .
               10  hgp-tot-ibl            pic s9(11)       comp-3     .
               10  hgp-tot-irt            pic s9(11)       comp-3     .
               10  hgp-tot-rmb            pic s9(11)       comp-3     .
               10  hgp-tot-pvn            pic s9(11)       comp-3     .
               10  hgp-tot-lbr occurs 09  pic s9(11)       comp-3     .
               10  hgp-dat-pag            pic  9(07)       comp-3     .
               10  hgp-tip-vrs            pic  9(02)                  .
               10  hgp-dat-vrs            pic  9(07)       comp-3     .
               10  hgp-num-vrs            pic  x(10)                  .
               10  hgp-ann-pcl.
                   15  hgp-rig-ann occurs 3
                                          pic  x(40)                  .
               10  hgp-flg-ela.
                   15  hgp-flg-blo.
                       20  hgp-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hgp-flg-nbl.
                       20  hgp-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hgp-flg-pul            pic  x(01)                  .
               10  hgp-alx-exp.
                   15  filler occurs 200  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hdd]                                    *
      *    *-----------------------------------------------------------*
       fd  hdd       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hdd-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hdd-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMDDS                         *
      *            *---------------------------------------------------*
               10  hdd-k01.
                   15  hdd-num-dds        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hdd-k02.
                   15  hdd-ide-dat        pic  9(07)       comp-3     .
                   15  hdd-num-dds-2      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hdd-dat.
               10  hdd-ide-ute            pic  x(08)                  .
               10  hdd-ide-fas            pic  x(06)                  .
               10  hdd-dtr-com            pic  9(07)       comp-3     .
               10  hdd-tip-dds            pic  9(02)                  .
               10  hdd-num-pro            pic  9(07)       comp-3     .
               10  hdd-ele-min            pic  9(07)       comp-3     .
               10  hdd-ele-max            pic  9(07)       comp-3     .
               10  hdd-num-pac            pic  9(09)       comp-3     .
               10  hdd-dat-spd            pic  9(07)       comp-3     .
               10  hdd-aff-spd            pic  9(07)       comp-3     .
               10  hdd-pes-spd            pic  9(06)v9(03) comp-3     .
               10  hdd-flg-ela.
                   15  hdd-flg-blo.
                       20  hdd-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hdd-flg-nbl.
                       20  hdd-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hdd-flg-pul            pic  x(01)                  .
               10  hdd-num-pr2            pic  9(07)                  .
               10  hdd-num-pr3            pic  9(07)                  .
               10  hdd-alx-exp.
                   15  filler occurs 06   pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [hsc]                                       *
      *    *-----------------------------------------------------------*
       01  f-hsc.
           05  f-hsc-nam                  pic  x(04)                  .
           05  f-hsc-pat                  pic  x(40)                  .
           05  f-hsc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hpp]                                       *
      *    *-----------------------------------------------------------*
       01  f-hpp.
           05  f-hpp-nam                  pic  x(04)                  .
           05  f-hpp-pat                  pic  x(40)                  .
           05  f-hpp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hds]                                       *
      *    *-----------------------------------------------------------*
       01  f-hds.
           05  f-hds-nam                  pic  x(04)                  .
           05  f-hds-pat                  pic  x(40)                  .
           05  f-hds-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hgp]                                       *
      *    *-----------------------------------------------------------*
       01  f-hgp.
           05  f-hgp-nam                  pic  x(04)                  .
           05  f-hgp-pat                  pic  x(40)                  .
           05  f-hgp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hdd]                                       *
      *    *-----------------------------------------------------------*
       01  f-hdd.
           05  f-hdd-nam                  pic  x(04)                  .
           05  f-hdd-pat                  pic  x(40)                  .
           05  f-hdd-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [hsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "tmi/gsc/fls/rec/rfhsc"                          .
      *        *-------------------------------------------------------*
      *        * [hpp]                                                 *
      *        *-------------------------------------------------------*
           copy      "tmi/smp/fls/rec/rfhpp"                          .
      *        *-------------------------------------------------------*
      *        * [hds]                                                 *
      *        *-------------------------------------------------------*
           copy      "tmi/smp/fls/rec/rfhds"                          .
      *        *-------------------------------------------------------*
      *        * [hgp]                                                 *
      *        *-------------------------------------------------------*
           copy      "tmi/tmi/fls/rec/rfhgp"                          .
      *        *-------------------------------------------------------*
      *        * [hdd]                                                 *
      *        *-------------------------------------------------------*
           copy      "tmi/tmi/fls/rec/rfhdd"                          .

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
                     "eurtmi"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "eurtmi01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONI PER EURO - TEMI       "       .

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
               10  w-arc-ele-max          pic  9(03) value 011        .
               10  w-arc-ele-tbl.
                   15  filler             pic  x(55) value
             "[hco]      Archivio anagrafica comuni                  ".
                   15  filler             pic  x(55) value
             "[hlc]      Legami tra clienti                          ".
                   15  filler             pic  x(55) value
             "[hsx]      Schede clienti : estensioni                 ".
                   15  filler             pic  x(55) value
             "[hde]      Destinatario per il pacco postale           ".
                   15  filler             pic  x(55) value
             "[hop]      Tipi operazione                             ".
                   15  filler             pic  x(55) value
             "[hps]      Personalizzazioni area 'smp'                ".
                   15  filler             pic  x(55) value
             "[hex]      File di confronto per l'esportazione        ".
                   15  filler             pic  x(55) value
             "[hfp]      Estensione archivio anagrafica fornitori    ".
                   15  filler             pic  x(55) value
             "[hgr]      Record circolari aziendali : righe          ".
                   15  filler             pic  x(55) value
             "[hgt]      Record circolari aziendali : testate        ".
                   15  filler             pic  x(55) value
             "[hnu]      Gestione numerazioni                        ".
               10  w-arc-ele-tbr redefines
                   w-arc-ele-tbl.
                   15  w-arc-ele-ele occurs 011.
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
      *              * Conversione [hsc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hsc-000      thru exe-cnv-hsc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hpp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hpp-000      thru exe-cnv-hpp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hds]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hds-000      thru exe-cnv-hds-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hgp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hgp-000      thru exe-cnv-hgp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hdd]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hdd-000      thru exe-cnv-hdd-999        .
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
      *    * Conversione [hsc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hsc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hsc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tmi/gsc/fls/ioc/obj/iofhsc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hsc-999.
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
       exe-cnv-hsc-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsc                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hsc-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hsc-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hsc                                       .
       exe-cnv-hsc-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hsc-k01                .
           start     hsc    key not less
                            hsc-k01
                            invalid key
                            go to exe-cnv-hsc-800.
       exe-cnv-hsc-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hsc    next
                            with no lock
                            at end
                            go to exe-cnv-hsc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hsc-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hsc-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsc                 .
       exe-cnv-hsc-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hsc                 .
           move      hsc-ide-dat          to   rf-hsc-ide-dat         .
           move      hsc-ide-ute          to   rf-hsc-ide-ute         .
           move      hsc-ide-fas          to   rf-hsc-ide-fas         .
           move      hsc-cod-cli          to   rf-hsc-cod-cli         .
           move      hsc-dpz-cli          to   rf-hsc-dpz-cli         .
           move      hsc-num-prg          to   rf-hsc-num-prg         .
           move      hsc-num-pro          to   rf-hsc-num-pro         .
           move      hsc-qta-pro          to   rf-hsc-qta-pro         .
           move      hsc-sgl-vlt          to   rf-hsc-sgl-vlt         .
           move      hsc-dec-vlt          to   rf-hsc-dec-vlt         .
           move      hsc-cdc-vlt          to   rf-hsc-cdc-vlt         .
           move      hsc-tdc-vlt          to   rf-hsc-tdc-vlt         .
           move      hsc-prz-ven          to   rf-hsc-prz-ven         .
           move      hsc-dat-doc          to   rf-hsc-dat-doc         .
           move      hsc-num-doc          to   rf-hsc-num-doc         .
           move      hsc-prt-fat          to   rf-hsc-prt-fat         .
           move      hsc-prt-dds          to   rf-hsc-prt-dds         .
           move      hsc-key-acc          to   rf-hsc-key-acc         .
           move      hsc-com-gsc          to   rf-hsc-com-gsc         .
           move      hsc-flg-pul          to   rf-hsc-flg-pul         .
           move      hsc-cod-old          to   rf-hsc-cod-old         .
           move      hsc-flg-spd          to   rf-hsc-flg-spd         .
           move      hsc-flg-sna          to   rf-hsc-flg-sna         .
           move      hsc-flg-dds          to   rf-hsc-flg-dds         .
           move      hsc-alx-exp          to   rf-hsc-alx-exp         .
       exe-cnv-hsc-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      hsc-sgl-vlt          to   w-exe-cnv-val-sgl      .
           move      hsc-dec-vlt          to   w-exe-cnv-val-dec      .
           move      hsc-tdc-vlt          to   w-exe-cnv-val-tdc      .
           move      hsc-cdc-vlt          to   w-exe-cnv-val-cdc      .
           move      hsc-dat-doc          to   w-exe-cnv-val-dat      .
      *
           perform   exe-cnv-val-000      thru exe-cnv-val-999        .
      *
           move      w-exe-cnv-val-sgl    to   rf-hsc-sgl-vlt         .
           move      w-exe-cnv-val-dec    to   rf-hsc-dec-vlt         .
           move      w-exe-cnv-val-tdc    to   rf-hsc-tdc-vlt         .
           move      w-exe-cnv-val-cdc    to   rf-hsc-cdc-vlt         .
      *                  *---------------------------------------------*
      *                  * Prezzo di vendita                           *
      *                  *---------------------------------------------*
           move      hsc-prz-ven          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hsc-prz-ven         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hsc-msg-000
                                          thru exe-cnv-hsc-msg-999    .
       exe-cnv-hsc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hsc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hsc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hsc-250.
       exe-cnv-hsc-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsc                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hsc                                              .
       exe-cnv-hsc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hsc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hsc-999.
       exe-cnv-hsc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hsc]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hsc-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing codice cliente                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hsc-cod-cli          to   v-num                  .
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
       exe-cnv-hsc-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hpp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hpp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hpp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tmi/smp/fls/ioc/obj/iofhpp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hpp-999.
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
       exe-cnv-hpp-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpp                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hpp-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hpp-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hpp                                       .
       exe-cnv-hpp-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hpp-k01                .
           start     hpp    key not less
                            hpp-k01
                            invalid key
                            go to exe-cnv-hpp-800.
       exe-cnv-hpp-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hpp    next
                            with no lock
                            at end
                            go to exe-cnv-hpp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hpp-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hpp-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpp                 .
       exe-cnv-hpp-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hpp                 .
           move      hpp-ide-dat          to   rf-hpp-ide-dat         .
           move      hpp-ide-ute          to   rf-hpp-ide-ute         .
           move      hpp-ide-fas          to   rf-hpp-ide-fas         .
           move      hpp-dtr-emi          to   rf-hpp-dtr-emi         .
           move      hpp-num-msp          to   rf-hpp-num-msp         .
           move      hpp-tip-msp          to   rf-hpp-tip-msp         .
           move      hpp-tac-msp          to   rf-hpp-tac-msp         .
           move      hpp-dsp-msp          to   rf-hpp-dsp-msp         .
           move      hpp-tip-dst          to   rf-hpp-tip-dst         .
           move      hpp-cod-dst          to   rf-hpp-cod-dst         .
           move      hpp-dpz-dst          to   rf-hpp-dpz-dst         .
           move      hpp-inl-msp          to   rf-hpp-inl-msp         .
           move      hpp-int-dst          to   rf-hpp-int-dst         .
           move      hpp-pes-msp          to   rf-hpp-pes-msp         .
           move      hpp-cta-msp          to   rf-hpp-cta-msp         .
           move      hpp-taf-msp          to   rf-hpp-taf-msp         .
           move      hpp-cco-msp          to   rf-hpp-cco-msp         .
           move      hpp-tco-msp          to   rf-hpp-tco-msp         .
           move      hpp-cmr-msp          to   rf-hpp-cmr-msp         .
           move      hpp-tmr-msp          to   rf-hpp-tmr-msp         .
           move      hpp-ann-msp          to   rf-hpp-ann-msp         .
           move      hpp-tip-ddr          to   rf-hpp-tip-ddr         .
           move      hpp-imp-ddr          to   rf-hpp-imp-ddr         .
           move      hpp-tmo-ddr          to   rf-hpp-tmo-ddr         .
           move      hpp-dat-ddr          to   rf-hpp-dat-ddr         .
           move      hpp-num-ddr          to   rf-hpp-num-ddr         .
           move      hpp-prt-ddr          to   rf-hpp-prt-ddr         .
           move      hpp-num-dds          to   rf-hpp-num-dds         .
           move      hpp-nra-ddr          to   rf-hpp-nra-ddr         .
           move      hpp-flc-stp          to   rf-hpp-flc-stp         .
           move      hpp-flg-ela          to   rf-hpp-flg-ela         .
           move      hpp-flg-pul          to   rf-hpp-flg-pul         .
           move      hpp-cts-msp          to   rf-hpp-cts-msp         .
           move      hpp-num-pr1          to   rf-hpp-num-pr1         .
           move      hpp-num-pr2          to   rf-hpp-num-pr2         .
           move      hpp-num-pr3          to   rf-hpp-num-pr3         .
           if        hpp-num-pr1          not  numeric
                     move  zero           to   rf-hpp-num-pr1         .
           if        hpp-num-pr2          not  numeric
                     move  zero           to   rf-hpp-num-pr2         .
           if        hpp-num-pr3          not  numeric
                     move  zero           to   rf-hpp-num-pr3         .
           move      hpp-alx-exp          to   rf-hpp-alx-exp         .
       exe-cnv-hpp-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Affrancatura                                *
      *                  *---------------------------------------------*
           move      hpp-taf-msp          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hpp-taf-msp         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hpp-msg-000
                                          thru exe-cnv-hpp-msg-999    .
      *                  *---------------------------------------------*
      *                  * Importo documento di riferimento            *
      *                  *---------------------------------------------*
           move      hpp-imp-ddr          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hpp-imp-ddr         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hpp-msg-000
                                          thru exe-cnv-hpp-msg-999    .
      *                  *---------------------------------------------*
      *                  * Importo per contrassegno                    *
      *                  *---------------------------------------------*
           move      hpp-cts-msp          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hpp-cts-msp         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hpp-msg-000
                                          thru exe-cnv-hpp-msg-999    .
       exe-cnv-hpp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hpp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hpp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hpp-250.
       exe-cnv-hpp-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpp                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hpp                                              .
       exe-cnv-hpp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hpp-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-hpp-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hpp]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hpp-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      hpp-dtr-emi          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-dat              .
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hpp-num-dds          to   v-num                  .
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
                     w-ide-dat  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hpp-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hds]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hds-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hds "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tmi/smp/fls/ioc/obj/iofhds"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hds-999.
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
       exe-cnv-hds-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hds                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hds-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hds-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hds                                       .
       exe-cnv-hds-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hds-k01                .
           start     hds    key not less
                            hds-k01
                            invalid key
                            go to exe-cnv-hds-800.
       exe-cnv-hds-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hds    next
                            with no lock
                            at end
                            go to exe-cnv-hds-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hds-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hds-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hds                 .
       exe-cnv-hds-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hds                 .
           move      hds-ide-dat          to   rf-hds-ide-dat         .
           move      hds-ide-ute          to   rf-hds-ide-ute         .
           move      hds-ide-fas          to   rf-hds-ide-fas         .
           move      hds-dtr-com          to   rf-hds-dtr-com         .
           move      hds-num-dds          to   rf-hds-num-dds         .
           move      hds-tip-dds          to   rf-hds-tip-dds         .
           move      hds-tds-das          to   rf-hds-tds-das         .
           move      hds-dsp-min          to   rf-hds-dsp-min         .
           move      hds-dsp-max          to   rf-hds-dsp-max         .
           move      hds-ele-max          to   rf-hds-ele-max         .
           move      hds-ele-dds          to   rf-hds-ele-dds         .
           move      hds-imp-max          to   rf-hds-imp-max         .
           move      hds-imp-dds          to   rf-hds-imp-dds         .
           move      hds-dtr-pre          to   rf-hds-dtr-pre         .
           move      hds-tip-pre          to   rf-hds-tip-pre         .
           move      hds-cod-cbp          to   rf-hds-cod-cbp         .
           move      hds-snx-stp          to   rf-hds-snx-stp         .
           move      hds-flc-stp          to   rf-hds-flc-stp         .
           move      hds-snx-ads          to   rf-hds-snx-ads         .
           move      hds-flc-ads          to   rf-hds-flc-ads         .
           move      hds-tip-ias          to   rf-hds-tip-ias         .
           move      hds-flc-ias          to   rf-hds-flc-ias         .
           move      hds-dtr-act          to   rf-hds-dtr-act         .
           move      hds-spe-act          to   rf-hds-spe-act         .
           move      hds-flg-ela          to   rf-hds-flg-ela         .
           move      hds-flg-pul          to   rf-hds-flg-pul         .
           move      hds-alx-exp          to   rf-hds-alx-exp         .
       exe-cnv-hds-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Importo massimo affrancatura distinta       *
      *                  *---------------------------------------------*
           move      hds-imp-max          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hds-imp-max         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hds-msg-000
                                          thru exe-cnv-hds-msg-999    .
      *                  *---------------------------------------------*
      *                  * Importo effettivo affrancatura distinta     *
      *                  *---------------------------------------------*
           move      hds-imp-dds          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hds-imp-dds         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hds-msg-000
                                          thru exe-cnv-hds-msg-999    .
      *                  *---------------------------------------------*
      *                  * Importo spese                               *
      *                  *---------------------------------------------*
           move      hds-spe-act          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hds-spe-act         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hds-msg-000
                                          thru exe-cnv-hds-msg-999    .
       exe-cnv-hds-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hds                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hds-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hds-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hds-250.
       exe-cnv-hds-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hds                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hds                                              .
       exe-cnv-hds-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hds-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-hds-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hds]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hds-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      hds-dtr-com          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-dat              .
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hds-num-dds          to   v-num                  .
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
                     w-ide-dat  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hds-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hgp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hgp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hgp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tmi/tmi/fls/ioc/obj/iofhgp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hgp-999.
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
       exe-cnv-hgp-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hgp                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hgp-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hgp-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hgp                                       .
       exe-cnv-hgp-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hgp-k01                .
           start     hgp    key not less
                            hgp-k01
                            invalid key
                            go to exe-cnv-hgp-800.
       exe-cnv-hgp-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hgp    next
                            with no lock
                            at end
                            go to exe-cnv-hgp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hgp-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hgp-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hgp                 .
       exe-cnv-hgp-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hgp                 .
           move      hgp-ide-dat          to   rf-hgp-ide-dat         .
           move      hgp-ide-ute          to   rf-hgp-ide-ute         .
           move      hgp-ide-fas          to   rf-hgp-ide-fas         .
           move      hgp-num-prt          to   rf-hgp-num-prt         .
           move      hgp-dat-reg          to   rf-hgp-dat-reg         .
           move      hgp-cod-dpz          to   rf-hgp-cod-dpz         .
           move      hgp-cod-tpr          to   rf-hgp-cod-tpr         .
           move      hgp-dat-doc          to   rf-hgp-dat-doc         .
           move      hgp-num-doc          to   rf-hgp-num-doc         .
           move      hgp-cod-fnt          to   rf-hgp-cod-fnt         .
           move      hgp-dpz-fnt          to   rf-hgp-dpz-fnt         .
           move      hgp-snx-prf          to   rf-hgp-snx-prf         .
           move      hgp-snx-tpv          to   rf-hgp-snx-tpv         .
           move      hgp-cau-rit          to   rf-hgp-cau-rit         .
           move      hgp-cod-trb          to   rf-hgp-cod-trb         .
           move      hgp-per-rit          to   rf-hgp-per-rit         .
           move      hgp-per-ctb          to   rf-hgp-per-ctb         .
           move      hgp-tot-ibl          to   rf-hgp-tot-ibl         .
           move      hgp-tot-irt          to   rf-hgp-tot-irt         .
           move      hgp-tot-rmb          to   rf-hgp-tot-rmb         .
           move      hgp-tot-pvn          to   rf-hgp-tot-pvn         .
           move      hgp-tot-lbr (1)      to   rf-hgp-tot-lbr (1)     .
           move      hgp-tot-lbr (2)      to   rf-hgp-tot-lbr (2)     .
           move      hgp-tot-lbr (3)      to   rf-hgp-tot-lbr (3)     .
           move      hgp-tot-lbr (4)      to   rf-hgp-tot-lbr (4)     .
           move      hgp-tot-lbr (5)      to   rf-hgp-tot-lbr (5)     .
           move      hgp-tot-lbr (6)      to   rf-hgp-tot-lbr (6)     .
           move      hgp-tot-lbr (7)      to   rf-hgp-tot-lbr (7)     .
           move      hgp-tot-lbr (8)      to   rf-hgp-tot-lbr (8)     .
           move      hgp-tot-lbr (9)      to   rf-hgp-tot-lbr (9)     .
           move      hgp-dat-pag          to   rf-hgp-dat-pag         .
           move      hgp-tip-vrs          to   rf-hgp-tip-vrs         .
           move      hgp-dat-vrs          to   rf-hgp-dat-vrs         .
           move      hgp-num-vrs          to   rf-hgp-num-vrs         .
           move      hgp-ann-pcl          to   rf-hgp-ann-pcl         .
           move      hgp-flg-ela          to   rf-hgp-flg-ela         .
           move      hgp-flg-pul          to   rf-hgp-flg-pul         .
           move      hgp-alx-exp          to   rf-hgp-alx-exp         .
       exe-cnv-hgp-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale imponibile IRPEF                     *
      *                  *---------------------------------------------*
           move      hgp-tot-ibl          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hgp-tot-ibl         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hgp-msg-000
                                          thru exe-cnv-hgp-msg-999    .
      *                  *---------------------------------------------*
      *                  * Totale importo ritenuta                     *
      *                  *---------------------------------------------*
           move      hgp-tot-irt          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hgp-tot-irt         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hgp-msg-000
                                          thru exe-cnv-hgp-msg-999    .
      *                  *---------------------------------------------*
      *                  * Totale rimborso                             *
      *                  *---------------------------------------------*
           move      hgp-tot-rmb          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hgp-tot-rmb         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hgp-msg-000
                                          thru exe-cnv-hgp-msg-999    .
      *                  *---------------------------------------------*
      *                  * Totale provvigioni non soggette             *
      *                  *---------------------------------------------*
           move      hgp-tot-pvn          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hgp-tot-pvn         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hgp-msg-000
                                          thru exe-cnv-hgp-msg-999    .
       exe-cnv-hgp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hgp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hgp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hgp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hgp-250.
       exe-cnv-hgp-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hgp                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hgp                                              .
       exe-cnv-hgp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hgp-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hgp-999.
       exe-cnv-hgp-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hdd]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hdd-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hdd "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tmi/tmi/fls/ioc/obj/iofhdd"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hdd-999.
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
       exe-cnv-hdd-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hdd                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hdd-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hdd-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hdd                                       .
       exe-cnv-hdd-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hdd-k01                .
           start     hdd    key not less
                            hdd-k01
                            invalid key
                            go to exe-cnv-hdd-800.
       exe-cnv-hdd-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hdd    next
                            with no lock
                            at end
                            go to exe-cnv-hdd-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hdd-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hdd-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hdd                 .
       exe-cnv-hdd-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hdd                 .
           move      hdd-ide-dat          to   rf-hdd-ide-dat         .
           move      hdd-ide-ute          to   rf-hdd-ide-ute         .
           move      hdd-ide-fas          to   rf-hdd-ide-fas         .
           move      hdd-dtr-com          to   rf-hdd-dtr-com         .
           move      hdd-num-dds          to   rf-hdd-num-dds         .
           move      hdd-tip-dds          to   rf-hdd-tip-dds         .
           move      hdd-num-pro          to   rf-hdd-num-pro         .
           move      hdd-ele-min          to   rf-hdd-ele-min         .
           move      hdd-ele-max          to   rf-hdd-ele-max         .
           move      hdd-num-pac          to   rf-hdd-num-pac         .
           move      hdd-dat-spd          to   rf-hdd-dat-spd         .
           move      hdd-aff-spd          to   rf-hdd-aff-spd         .
           move      hdd-pes-spd          to   rf-hdd-pes-spd         .
           move      hdd-flg-ela          to   rf-hdd-flg-ela         .
           move      hdd-flg-pul          to   rf-hdd-flg-pul         .
           move      hdd-num-pr2          to   rf-hdd-num-pr2         .
           move      hdd-num-pr3          to   rf-hdd-num-pr3         .
           move      hdd-alx-exp          to   rf-hdd-alx-exp         .
       exe-cnv-hdd-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Affrancatura                                *
      *                  *---------------------------------------------*
           move      hdd-aff-spd          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hdd-aff-spd         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hdd-msg-000
                                          thru exe-cnv-hdd-msg-999    .
       exe-cnv-hdd-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hdd                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hdd-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hdd-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hdd-250.
       exe-cnv-hdd-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hdd                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hdd                                              .
       exe-cnv-hdd-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hdd-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-hdd-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hdd]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hdd-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      hdd-dtr-com          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-dat              .
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hdd-num-dds          to   v-num                  .
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
                     w-ide-dat  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hdd-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hgp]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hgp-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing codice archivio                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hgp-cod-fnt          to   v-num                  .
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
       exe-cnv-hgp-msg-999.
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

