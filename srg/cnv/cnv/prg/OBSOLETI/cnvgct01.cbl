       Identification Division.
       Program-Id.                                 cnvgct01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    cnvgct01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/09/05    *
      *                       Ultima revisione:    NdK del 02/01/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per quantita' - Tip. RUMOR      *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * {gct} v [hst]      Scheda tecnica commessa   : testate         *
      *                    - mnm-qta                                   *
      *                    - mcl-qta                                   *
      *                                                                *
      * {gct} v [hsr]      Scheda tecnica commessa   : righe           *
      *                    - qta-buo                                   *
      *                    - qta-sca                                   *
      *                                                                *
      * {gct} v [hvt]      Scheda tecnica preventivo : testate         *
      *                    - mnm-qta                                   *
      *                    - mcl-qta                                   *
      *                                                                *
      * {gct} v [hvr]      Scheda tecnica preventivo : righe           *
      *                    - qta-buo                                   *
      *                    - qta-sca                                   *
      *                                                                *
      * {gct} v [hba]      Bancali per commessa                        *
      *                    - qta-buo                                   *
      *                    - qta-pos                                   *
      *                                                                *
      * {gct} v [hor]      Ordini di lavoro          : righe           *
      *                    - qta-ord                                   *
      *                                                                *
      * {gct} v [hur]      Ordini di lavoro prevent. : righe           *
      *                    - qta-ord                                   *
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

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

       Special-Names.       Decimal-Point is comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [hst]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hst   assign to disk           f-hst-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hst-k01
                   alternate record key   is hst-k02
                             file status  is                f-hst-sts .

      *    *===========================================================*
      *    * File Control [hsr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hsr   assign to disk           f-hsr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hsr-k01
                             file status  is                f-hsr-sts .

      *    *===========================================================*
      *    * File Control [hvt]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hvt   assign to disk           f-hvt-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hvt-k01
                   alternate record key   is hvt-k02
                             file status  is                f-hvt-sts .

      *    *===========================================================*
      *    * File Control [hvr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hvr   assign to disk           f-hvr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hvr-k01
                             file status  is                f-hvr-sts .

      *    *===========================================================*
      *    * File Control [hba]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hba   assign to disk           f-hba-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hba-k01
                             file status  is                f-hba-sts .

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
      *    * File Control [hur]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hur   assign to disk           f-hur-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hur-k01
                   alternate record key   is hur-k02
                   alternate record key   is hur-k03
                             file status  is                f-hur-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [hst]                                    *
      *    *-----------------------------------------------------------*
       fd  hst       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hst-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hst-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hst-k01.
                   15  hst-prt-cml        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hst-k02.
                   15  hst-ide-dat        pic  9(07)       comp-3     .
                   15  hst-prt-cml-2      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hst-dat.
               10  hst-ide-ute            pic  x(08)                  .
               10  hst-ide-fas            pic  x(06)                  .
               10  hst-tip-cml            pic  9(02)                  .
               10  hst-dlv-snx            pic  9(01)                  .
               10  hst-dlv-ext            pic  x(40)                  .
               10  hst-dla-snx            pic  9(01)                  .
               10  hst-dla-ext            pic  x(40)                  .
               10  hst-dat-fcm.
                   15  hst-fci-snx        pic  9(01)                  .
                   15  hst-fci-not.
                       20  hst-fci-rig occurs 3
                                          pic  x(40)                  .
                   15  hst-fci-qta        pic s9(06)v9(03) comp-3     .
                   15  hst-fce-snx        pic  9(01)                  .
                   15  hst-fcf-snx        pic  9(01)                  .
                   15  hst-fcf-not.
                       20  hst-fcf-rig occurs 3
                                          pic  x(40)                  .
               10  hst-dat-flt.
                   15  hst-fli-snx        pic  9(01)                  .
                   15  hst-fli-not.
                       20  hst-fli-rig occurs 3
                                          pic  x(40)                  .
                   15  hst-fli-qta        pic s9(06)v9(03) comp-3     .
                   15  hst-fle-snx        pic  9(01)                  .
                   15  hst-flf-snx        pic  9(01)                  .
                   15  hst-flf-not.
                       20  hst-flf-rig occurs 3
                                          pic  x(40)                  .
                   15  hst-flf-qta        pic s9(06)v9(03) comp-3     .
               10  hst-dat-mnt.
                   15  hst-mni-snx        pic  9(01)                  .
                   15  hst-mnx-snx        pic  9(01)                  .
                   15  hst-mnx-ext        pic  x(40)                  .
                   15  hst-mni-qta        pic s9(06)v9(03) comp-3     .
                   15  hst-mne-snx        pic  9(01)                  .
                   15  hst-mnf-snx        pic  9(01)                  .
                   15  hst-mnf-not.
                       20  hst-mnf-rig occurs 3
                                          pic  x(40)                  .
                   15  hst-mni-esi        pic  9(01)                  .
                   15  hst-mni-arc        pic  9(01)                  .
                   15  hst-mni-fmc        pic  x(20)                  .
                   15  hst-mni-fpr        pic  x(20)                  .
                   15  hst-mnv-snx        pic  9(01)                  .
                   15  hst-mnv-ext        pic  x(40)                  .
               10  hst-dat-fin.
                   15  hst-fii-snx        pic  9(01)                  .
                   15  hst-fie-snx        pic  9(01)                  .
                   15  hst-fii-esi        pic  9(01)                  .
                   15  hst-fii-new        pic  9(01)                  .
                   15  hst-fii-arc        pic  9(01)                  .
                   15  hst-fii-eli        pic  9(01)                  .
                   15  hst-fii-qth        pic s9(06)v9(03) comp-3     .
                   15  hst-fii-cst  occurs 06.
                       20  hst-fii-mac    pic  9(05)       comp-3     .
                       20  hst-fii-lst    pic  9(07)       comp-3     .
                       20  hst-fii-not.
                           25  hst-fii-rig occurs 3
                                          pic  x(40)                  .
                       20  hst-fii-qta    pic s9(06)v9(03) comp-3     .
                   15  hst-fiv-snx        pic  9(01)                  .
                   15  hst-fiv-ext        pic  x(40)                  .
               10  hst-dat-mag.
                   15  hst-mag-snn        pic  9(01)                  .
                   15  hst-mag-snc        pic  9(01)                  .
                   15  hst-mnm-cst  occurs 06.
                       20  hst-mnm-num    pic  9(07)       comp-3     .
                       20  hst-mnm-not.
                           25  hst-mnm-rig occurs 2
                                          pic  x(40)                  .
                       20  hst-mnm-qta    pic s9(06)v9(03) comp-3     .
                   15  hst-mcl-cst  occurs 06.
                       20  hst-mcl-num    pic  9(07)       comp-3     .
                       20  hst-mcl-not.
                           25  hst-mcl-rig occurs 2
                                          pic  x(40)                  .
                       20  hst-mcl-qta    pic s9(06)v9(03) comp-3     .
                   15  hst-mcv-snx        pic  9(01)                  .
                   15  hst-mcv-ext        pic  x(40)                  .
               10  hst-dat-lgt.
                   15  hst-lgi-snx        pic  9(01)                  .
                   15  hst-lge-snx        pic  9(01)                  .
                   15  hst-lgi-cst  occurs 06.
                       20  hst-lgi-tip    pic  9(02)                  .
                       20  hst-lgi-fdt    pic  9(06)v9(03) comp-3     .
                       20  hst-lgi-fin    pic  x(20)                  .
                       20  hst-lgi-ffi    pic  x(20)                  .
                   15  hst-lgn-snx        pic  9(01)                  .
                   15  hst-lgn-ext        pic  x(40)                  .
                   15  hst-lgi-qth        pic s9(06)v9(03) comp-3     .
                   15  hst-lgi-snp        pic  9(01)                  .
                   15  hst-lgi-p04        pic  9(05)       comp-3     .
                   15  hst-lgi-p06        pic  9(05)       comp-3     .
                   15  hst-lgi-p08        pic  9(05)       comp-3     .
                   15  hst-lgi-p12        pic  9(05)       comp-3     .
                   15  hst-lgi-p16        pic  9(05)       comp-3     .
                   15  hst-lgi-p24        pic  9(05)       comp-3     .
                   15  hst-lgi-p32        pic  9(05)       comp-3     .
                   15  hst-lgt-cst  occurs 12.
                       20  hst-lgt-til    pic  9(05)       comp-3     .
                   15  hst-lgt-snx        pic  9(01)                  .
                   15  hst-lgt-ext        pic  x(40)                  .
               10  hst-dat-stp.
                   15  hst-sti-snx        pic  9(01)                  .
                   15  hst-stp-snx        pic  9(01)                  .
                   15  hst-stp-ext        pic  x(40)                  .
               10  hst-flg-ela.
                   15  hst-flg-blo.
                       20  hst-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hst-flg-nbl.
                       20  hst-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hst-flg-pul            pic  x(01)                  .
      *
               10  hst-seg-fcm.
                   15  hst-fci-cst  occurs 06.
                       20  hst-fci-cod    pic  9(07)       comp-3     .
                       20  hst-fci-tla    pic  9(06)v9(03) comp-3     .
               10  hst-seg-flt.
                   15  hst-fci-cst  occurs 06.
                       20  hst-fli-cod    pic  9(07)       comp-3     .
                       20  hst-fli-tla    pic  9(06)v9(03) comp-3     .
               10  hst-seg-mnt.
                   15  hst-mni-cst  occurs 06.
                       20  hst-mni-cod    pic  9(07)       comp-3     .
                       20  hst-mni-tla    pic  9(06)v9(03) comp-3     .
               10  hst-seg-fin.
                   15  hst-fii-cst  occurs 06.
                       20  hst-fii-cod    pic  9(07)       comp-3     .
                       20  hst-fii-tla    pic  9(06)v9(03) comp-3     .
               10  hst-seg-lgt.
                   15  hst-fii-lgi  occurs 06.
                       20  hst-lgi-cod    pic  9(07)       comp-3     .
                       20  hst-lgi-tla    pic  9(06)v9(03) comp-3     .
      *
               10  hst-alx-exp.
                   15  filler  occurs 200 pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hsr]                                    *
      *    *-----------------------------------------------------------*
       fd  hsr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hsr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hsr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRTCML                         *
      *            *---------------------------------------------------*
               10  hsr-k01.
                   15  hsr-prt-cml        pic  9(09)       comp-3     .
                   15  hsr-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hsr-dat.
               10  hsr-cod-tip            pic  9(03)       comp-3     .
               10  hsr-tip-sgn            pic  x(30)                  .
               10  hsr-num-bia            pic  9(03)       comp-3     .
               10  hsr-num-vlt            pic  9(03)       comp-3     .
               10  hsr-bia-vlt            pic  9(03)       comp-3     .
               10  hsr-qta-buo            pic s9(06)v9(03) comp-3     .
               10  hsr-qta-sca            pic s9(06)v9(03) comp-3     .
               10  hsr-des-fmt            pic  x(30)                  .
               10  hsr-cod-fmt            pic  9(07)       comp-3     .
               10  hsr-alf-fmt            pic  x(14)                  .
               10  hsr-res-fmt            pic  9(02)v9(01) comp-3     .
               10  hsr-cod-mac            pic  9(05)       comp-3     .
               10  hsr-qta-lav            pic s9(06)v9(03) comp-3     .
               10  hsr-cod-cdc            pic  9(05)       comp-3     .
               10  hsr-dat-prv            pic  9(07)       comp-3     .
               10  hsr-nst-snx            pic  9(01)                  .
               10  hsr-nst-ext            pic  x(40)                  .
               10  hsr-flg-ela.
                   15  hsr-flg-blo.
                       20  hsr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hsr-flg-nbl.
                       20  hsr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hsr-flg-pul            pic  x(01)                  .
               10  hsr-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hvt]                                    *
      *    *-----------------------------------------------------------*
       fd  hvt       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hvt-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hvt-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hvt-k01.
                   15  hvt-prt-prv        pic  9(09)       comp-3     .
                   15  hvt-ver-prv        pic  9(03)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hvt-k02.
                   15  hvt-ide-dat        pic  9(07)       comp-3     .
                   15  hvt-prt-prv-2      pic  9(09)       comp-3     .
                   15  hvt-ver-prv-2      pic  9(03)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hvt-dat.
               10  hvt-ide-ute            pic  x(08)                  .
               10  hvt-ide-fas            pic  x(06)                  .
               10  hvt-tip-prv            pic  9(02)                  .
               10  hvt-dlv-snx            pic  9(01)                  .
               10  hvt-dlv-ext            pic  x(40)                  .
               10  hvt-dla-snx            pic  9(01)                  .
               10  hvt-dla-ext            pic  x(40)                  .
               10  hvt-dat-fcm.
                   15  hvt-fci-snx        pic  9(01)                  .
                   15  hvt-fci-not.
                       20  hvt-fci-rig occurs 3
                                          pic  x(40)                  .
                   15  hvt-fci-qta        pic s9(06)v9(03) comp-3     .
                   15  hvt-fce-snx        pic  9(01)                  .
                   15  hvt-fcf-snx        pic  9(01)                  .
                   15  hvt-fcf-not.
                       20  hvt-fcf-rig occurs 3
                                          pic  x(40)                  .
               10  hvt-dat-flt.
                   15  hvt-fli-snx        pic  9(01)                  .
                   15  hvt-fli-not.
                       20  hvt-fli-rig occurs 3
                                          pic  x(40)                  .
                   15  hvt-fli-qta        pic s9(06)v9(03) comp-3     .
                   15  hvt-fle-snx        pic  9(01)                  .
                   15  hvt-flf-snx        pic  9(01)                  .
                   15  hvt-flf-not.
                       20  hvt-flf-rig occurs 3
                                          pic  x(40)                  .
                   15  hvt-flf-qta        pic s9(06)v9(03) comp-3     .
               10  hvt-dat-mnt.
                   15  hvt-mni-snx        pic  9(01)                  .
                   15  hvt-mnx-snx        pic  9(01)                  .
                   15  hvt-mnx-ext        pic  x(40)                  .
                   15  hvt-mni-qta        pic s9(06)v9(03) comp-3     .
                   15  hvt-mne-snx        pic  9(01)                  .
                   15  hvt-mnf-snx        pic  9(01)                  .
                   15  hvt-mnf-not.
                       20  hvt-mnf-rig occurs 3
                                          pic  x(40)                  .
                   15  hvt-mni-esi        pic  9(01)                  .
                   15  hvt-mni-arc        pic  9(01)                  .
                   15  hvt-mni-fmc        pic  x(20)                  .
                   15  hvt-mni-fpr        pic  x(20)                  .
                   15  hvt-mnv-snx        pic  9(01)                  .
                   15  hvt-mnv-ext        pic  x(40)                  .
               10  hvt-dat-fin.
                   15  hvt-fii-snx        pic  9(01)                  .
                   15  hvt-fie-snx        pic  9(01)                  .
                   15  hvt-fii-esi        pic  9(01)                  .
                   15  hvt-fii-new        pic  9(01)                  .
                   15  hvt-fii-arc        pic  9(01)                  .
                   15  hvt-fii-eli        pic  9(01)                  .
                   15  hvt-fii-qth        pic s9(06)v9(03) comp-3     .
                   15  hvt-fii-cst  occurs 06.
                       20  hvt-fii-mac    pic  9(05)       comp-3     .
                       20  hvt-fii-lst    pic  9(07)       comp-3     .
                       20  hvt-fii-not.
                           25  hvt-fii-rig occurs 3
                                          pic  x(40)                  .
                       20  hvt-fii-qta    pic s9(06)v9(03) comp-3     .
                   15  hvt-fiv-snx        pic  9(01)                  .
                   15  hvt-fiv-ext        pic  x(40)                  .
               10  hvt-dat-mag.
                   15  hvt-mag-snn        pic  9(01)                  .
                   15  hvt-mag-snc        pic  9(01)                  .
                   15  hvt-mnm-cst  occurs 06.
                       20  hvt-mnm-num    pic  9(07)       comp-3     .
                       20  hvt-mnm-not.
                           25  hvt-mnm-rig occurs 2
                                          pic  x(40)                  .
                       20  hvt-mnm-qta    pic s9(06)v9(03) comp-3     .
                   15  hvt-mcl-cst  occurs 06.
                       20  hvt-mcl-num    pic  9(07)       comp-3     .
                       20  hvt-mcl-not.
                           25  hvt-mcl-rig occurs 2
                                          pic  x(40)                  .
                       20  hvt-mcl-qta    pic s9(06)v9(03) comp-3     .
                   15  hvt-mcv-snx        pic  9(01)                  .
                   15  hvt-mcv-ext        pic  x(40)                  .
               10  hvt-dat-lgt.
                   15  hvt-lgi-snx        pic  9(01)                  .
                   15  hvt-lge-snx        pic  9(01)                  .
                   15  hvt-lgi-cst  occurs 06.
                       20  hvt-lgi-tip    pic  9(02)                  .
                       20  hvt-lgi-fdt    pic  9(06)v9(03) comp-3     .
                       20  hvt-lgi-fin    pic  x(20)                  .
                       20  hvt-lgi-ffi    pic  x(20)                  .
                   15  hvt-lgn-snx        pic  9(01)                  .
                   15  hvt-lgn-ext        pic  x(40)                  .
                   15  hvt-lgi-qth        pic s9(06)v9(03) comp-3     .
                   15  hvt-lgi-snp        pic  9(01)                  .
                   15  hvt-lgi-p04        pic  9(05)       comp-3     .
                   15  hvt-lgi-p06        pic  9(05)       comp-3     .
                   15  hvt-lgi-p08        pic  9(05)       comp-3     .
                   15  hvt-lgi-p12        pic  9(05)       comp-3     .
                   15  hvt-lgi-p16        pic  9(05)       comp-3     .
                   15  hvt-lgi-p24        pic  9(05)       comp-3     .
                   15  hvt-lgi-p32        pic  9(05)       comp-3     .
                   15  hvt-lgt-cst  occurs 12.
                       20  hvt-lgt-til    pic  9(05)       comp-3     .
                   15  hvt-lgt-snx        pic  9(01)                  .
                   15  hvt-lgt-ext        pic  x(40)                  .
               10  hvt-dat-stp.
                   15  hvt-sti-snx        pic  9(01)                  .
                   15  hvt-stp-snx        pic  9(01)                  .
                   15  hvt-stp-ext        pic  x(40)                  .
               10  hvt-flg-ela.
                   15  hvt-flg-blo.
                       20  hvt-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hvt-flg-nbl.
                       20  hvt-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hvt-flg-pul            pic  x(01)                  .
      *
               10  hvt-seg-fcm.
                   15  hvt-fci-cst  occurs 06.
                       20  hvt-fci-cod    pic  9(07)       comp-3     .
                       20  hvt-fci-tla    pic  9(06)v9(03) comp-3     .
               10  hvt-seg-flt.
                   15  hvt-fci-cst  occurs 06.
                       20  hvt-fli-cod    pic  9(07)       comp-3     .
                       20  hvt-fli-tla    pic  9(06)v9(03) comp-3     .
               10  hvt-seg-mnt.
                   15  hvt-mni-cst  occurs 06.
                       20  hvt-mni-cod    pic  9(07)       comp-3     .
                       20  hvt-mni-tla    pic  9(06)v9(03) comp-3     .
               10  hvt-seg-fin.
                   15  hvt-fii-cst  occurs 06.
                       20  hvt-fii-cod    pic  9(07)       comp-3     .
                       20  hvt-fii-tla    pic  9(06)v9(03) comp-3     .
               10  hvt-seg-lgt.
                   15  hvt-fii-lgi  occurs 06.
                       20  hvt-lgi-cod    pic  9(07)       comp-3     .
                       20  hvt-lgi-tla    pic  9(06)v9(03) comp-3     .
      *
               10  hvt-alx-exp.
                   15  filler  occurs 200 pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hvr]                                    *
      *    *-----------------------------------------------------------*
       fd  hvr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hvr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hvr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRVVER                         *
      *            *---------------------------------------------------*
               10  hvr-k01.
                   15  hvr-prt-prv        pic  9(09)       comp-3     .
                   15  hvr-ver-prv        pic  9(03)       comp-3     .
                   15  hvr-num-prg        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hvr-dat.
               10  hvr-cod-tip            pic  9(03)       comp-3     .
               10  hvr-tip-sgn            pic  x(30)                  .
               10  hvr-num-bia            pic  9(03)       comp-3     .
               10  hvr-num-vlt            pic  9(03)       comp-3     .
               10  hvr-bia-vlt            pic  9(03)       comp-3     .
               10  hvr-qta-buo            pic s9(06)v9(03) comp-3     .
               10  hvr-qta-sca            pic s9(06)v9(03) comp-3     .
               10  hvr-des-fmt            pic  x(30)                  .
               10  hvr-cod-fmt            pic  9(07)       comp-3     .
               10  hvr-alf-fmt            pic  x(14)                  .
               10  hvr-res-fmt            pic  9(02)v9(01) comp-3     .
               10  hvr-cod-mac            pic  9(05)       comp-3     .
               10  hvr-qta-lav            pic s9(06)v9(03) comp-3     .
               10  hvr-cod-cdc            pic  9(05)       comp-3     .
               10  hvr-dat-prv            pic  9(07)       comp-3     .
               10  hvr-nst-snx            pic  9(01)                  .
               10  hvr-nst-ext            pic  x(40)                  .
               10  hvr-flg-ela.
                   15  hvr-flg-blo.
                       20  hvr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hvr-flg-nbl.
                       20  hvr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hvr-flg-pul            pic  x(01)                  .
               10  hvr-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hba]                                    *
      *    *-----------------------------------------------------------*
       fd  hba       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hba-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hba-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRTCML                         *
      *            *---------------------------------------------------*
               10  hba-k01.
                   15  hba-prt-cml        pic  9(09)       comp-3     .
                   15  hba-num-prg        pic  9(05)       comp-3     .
                   15  hba-prg-imp        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hba-dat.
               10  hba-cod-adt            pic  9(05)       comp-3     .
               10  hba-dat-stp            pic  9(07)       comp-3     .
               10  hba-tip-sgn            pic  x(30)                  .
               10  hba-qta-buo            pic s9(06)v9(03) comp-3     .
               10  hba-qta-pos            pic s9(06)v9(03) comp-3     .
               10  hba-nst-ex1            pic  x(40)                  .
               10  hba-nst-ex2            pic  x(40)                  .
               10  hba-alx-exp.
                   15  filler  occurs 200 pic  x(01)                  .

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

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [hst]                                       *
      *    *-----------------------------------------------------------*
       01  f-hst.
           05  f-hst-nam                  pic  x(04)                  .
           05  f-hst-pat                  pic  x(40)                  .
           05  f-hst-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hsr]                                       *
      *    *-----------------------------------------------------------*
       01  f-hsr.
           05  f-hsr-nam                  pic  x(04)                  .
           05  f-hsr-pat                  pic  x(40)                  .
           05  f-hsr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hvt]                                       *
      *    *-----------------------------------------------------------*
       01  f-hvt.
           05  f-hvt-nam                  pic  x(04)                  .
           05  f-hvt-pat                  pic  x(40)                  .
           05  f-hvt-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hvr]                                       *
      *    *-----------------------------------------------------------*
       01  f-hvr.
           05  f-hvr-nam                  pic  x(04)                  .
           05  f-hvr-pat                  pic  x(40)                  .
           05  f-hvr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hba]                                       *
      *    *-----------------------------------------------------------*
       01  f-hba.
           05  f-hba-nam                  pic  x(04)                  .
           05  f-hba-pat                  pic  x(40)                  .
           05  f-hba-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hor]                                       *
      *    *-----------------------------------------------------------*
       01  f-hor.
           05  f-hor-nam                  pic  x(04)                  .
           05  f-hor-pat                  pic  x(40)                  .
           05  f-hor-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hur]                                       *
      *    *-----------------------------------------------------------*
       01  f-hur.
           05  f-hur-nam                  pic  x(04)                  .
           05  f-hur-pat                  pic  x(40)                  .
           05  f-hur-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [hst]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhst"                          .
      *        *-------------------------------------------------------*
      *        * [hsr]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhsr"                          .
      *        *-------------------------------------------------------*
      *        * [hvt]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhvt"                          .
      *        *-------------------------------------------------------*
      *        * [hvr]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhvr"                          .
      *        *-------------------------------------------------------*
      *        * [hba]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhba"                          .
      *        *-------------------------------------------------------*
      *        * [hor]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhor"                          .
      *        *-------------------------------------------------------*
      *        * [hur]                                                 *
      *        *-------------------------------------------------------*
           copy      "tpg/gct/fls/rec/rfhur"                          .

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
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cnvgct"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnvgct01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "CONVERSIONI QUANTITA' - Tipografia RUMOR"       .

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
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-nam                  pic  x(04)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-sts                  pic  x(02)                  .
           05  f-xxx-obj                  pic  x(40)                  .
           05  f-xxx-nrl                  pic  9(09)                  .
           05  f-xxx-nrs                  pic  9(09)                  .
           05  f-xxx-nrc                  pic  9(02)                  .
           05  f-xxx-nrd                  pic  9(01)                  .

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
      *        *  - A : Automatica                                     *
      *        *  - M : Manuale                                        *
      *        *-------------------------------------------------------*
           05  rr-aut-man                 pic  x(01)                  .
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
               10  w-exp-aut-man-num      pic  9(02)       value 02   .
               10  w-exp-aut-man-lun      pic  9(02)       value 40   .
               10  w-exp-aut-man-tbl.
                   15  filler             pic  x(40) value
                          "Automatica, per tutti gli archivi       "  .
                   15  filler             pic  x(40) value
                          "Manuale, un archivio alla volta         "  .
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
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  I                          pic  9(05)                  .
           05  w-c01                      pic  9(02)                  .
           05  w-c02                      pic  9(02)                  .

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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conversione           :"
                                          to   v-alf                  .
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
           move      spaces               to   v-edm                  .
           move      w-exp-aut-man-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-aut-man           =    "A"
                     move  01             to   v-num
           else if   rr-aut-man           =    "M"
                     move  02             to   v-num
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
                     move  "A"            to   rr-aut-man
           else if   v-num                =    02
                     move  "M"            to   rr-aut-man
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
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo conversione                   *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "A" or
                     rr-aut-man           =    "M"
                     go to tdo-ric-sel-999.
           move      "ME"                 to   v-ope                  .
           move      "Tipo conversione errato !"
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
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di conversione                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fil-000.
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
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-fil-200.
      *              *-------------------------------------------------*
      *              * Conversione [hst]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hst-000      thru exe-cnv-hst-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hsr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hsr-000      thru exe-cnv-hsr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hvt]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hvt-000      thru exe-cnv-hvt-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hvr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hvr-000      thru exe-cnv-hvr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hba]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hba-000      thru exe-cnv-hba-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hor]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hor-000      thru exe-cnv-hor-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hur]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hur-000      thru exe-cnv-hur-999        .
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
      *    * Conversione [hst]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hst-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hst "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhst"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hst-999.
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
       exe-cnv-hst-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hst-pat              .
       exe-cnv-hst-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hst]                         *
      *                  *---------------------------------------------*
           open      i-o    hst                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hst]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hst                 .
       exe-cnv-hst-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hst]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hst-k01                .
           start     hst    key not less
                            hst-k01
                            invalid key
                            go to exe-cnv-hst-800.
       exe-cnv-hst-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hst]                               *
      *              *-------------------------------------------------*
           read      hst    next
                            with no lock
                            at end
                            go to exe-cnv-hst-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hst-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-hst]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hst                 .
       exe-cnv-hst-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hst]                          *
      *              *-------------------------------------------------*
       exe-cnv-hst-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hst                 .
           move      hst-ide-dat          to   rf-hst-ide-dat         .
           move      hst-ide-ute          to   rf-hst-ide-ute         .
           move      hst-ide-fas          to   rf-hst-ide-fas         .
           move      hst-prt-cml          to   rf-hst-prt-cml         .
           move      hst-tip-cml          to   rf-hst-tip-cml         .
           move      hst-dlv-snx          to   rf-hst-dlv-snx         .
           move      hst-dlv-ext          to   rf-hst-dlv-ext         .
           move      hst-dla-snx          to   rf-hst-dla-snx         .
           move      hst-dla-ext          to   rf-hst-dla-ext         .
           move      hst-fci-snx          to   rf-hst-fci-snx         .
           move      hst-fci-not          to   rf-hst-fci-not         .
           move      hst-fci-qta          to   rf-hst-fci-qta         .
           move      hst-fce-snx          to   rf-hst-fce-snx         .
           move      hst-fcf-snx          to   rf-hst-fcf-snx         .
           move      hst-fcf-not          to   rf-hst-fcf-not         .
           move      hst-fli-snx          to   rf-hst-fli-snx         .
           move      hst-fli-not          to   rf-hst-fli-not         .
           move      hst-fli-qta          to   rf-hst-fli-qta         .
           move      hst-fle-snx          to   rf-hst-fle-snx         .
           move      hst-flf-snx          to   rf-hst-flf-snx         .
           move      hst-flf-not          to   rf-hst-flf-not         .
           move      hst-flf-qta          to   rf-hst-flf-qta         .
           move      hst-mni-snx          to   rf-hst-mni-snx         .
           move      hst-mnx-snx          to   rf-hst-mnx-snx         .
           move      hst-mnx-ext          to   rf-hst-mnx-ext         .
           move      hst-mni-qta          to   rf-hst-mni-qta         .
           move      hst-mne-snx          to   rf-hst-mne-snx         .
           move      hst-mnf-snx          to   rf-hst-mnf-snx         .
           move      hst-mnf-not          to   rf-hst-mnf-not         .
           move      hst-mni-esi          to   rf-hst-mni-esi         .
           move      hst-mni-arc          to   rf-hst-mni-arc         .
           move      hst-mni-fmc          to   rf-hst-mni-fmc         .
           move      hst-mni-fpr          to   rf-hst-mni-fpr         .
           move      hst-mnv-snx          to   rf-hst-mnv-snx         .
           move      hst-mnv-ext          to   rf-hst-mnv-ext         .
           move      hst-fii-snx          to   rf-hst-fii-snx         .
           move      hst-fie-snx          to   rf-hst-fie-snx         .
           move      hst-fii-esi          to   rf-hst-fii-esi         .
           move      hst-fii-new          to   rf-hst-fii-new         .
           move      hst-fii-arc          to   rf-hst-fii-arc         .
           move      hst-fii-eli          to   rf-hst-fii-eli         .
           move      hst-fii-qth          to   rf-hst-fii-qth         .
           move      zero                 to   I                      .
       dec-fis-hst-100.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hst-120.
           move      hst-fii-mac (I)      to   rf-hst-fii-mac (I)     .
           move      hst-fii-lst (I)      to   rf-hst-fii-lst (I)     .
           move      hst-fii-not (I)      to   rf-hst-fii-not (I)     .
           move      hst-fii-qta (I)      to   rf-hst-fii-qta (I)     .
           go to     dec-fis-hst-100.
       dec-fis-hst-120.
           move      hst-fiv-snx          to   rf-hst-fiv-snx         .
           move      hst-fiv-ext          to   rf-hst-fiv-ext         .
           move      hst-mag-snn          to   rf-hst-mag-snn         .
           move      hst-mag-snc          to   rf-hst-mag-snc         .
           move      zero                 to   I                      .
       dec-fis-hst-200.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hst-220.
           move      hst-mnm-num (I)      to   rf-hst-mnm-num (I)     .
           move      hst-mnm-not (I)      to   rf-hst-mnm-not (I)     .
           move      hst-mnm-qta (I)      to   rf-hst-mnm-qta (I)     .
           go to     dec-fis-hst-200.
       dec-fis-hst-220.
           move      zero                 to   I                      .
       dec-fis-hst-300.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hst-320.
           move      hst-mcl-num (I)      to   rf-hst-mcl-num (I)     .
           move      hst-mcl-not (I)      to   rf-hst-mcl-not (I)     .
           move      hst-mcl-qta (I)      to   rf-hst-mcl-qta (I)     .
           go to     dec-fis-hst-300.
       dec-fis-hst-320.
           move      hst-mcv-snx          to   rf-hst-mcv-snx         .
           move      hst-mcv-ext          to   rf-hst-mcv-ext         .
           move      hst-lgi-snx          to   rf-hst-lgi-snx         .
           move      hst-lge-snx          to   rf-hst-lge-snx         .
           move      zero                 to   I                      .
       dec-fis-hst-400.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hst-420.
           move      hst-lgi-tip (I)      to   rf-hst-lgi-tip (I)     .
           move      hst-lgi-fdt (I)      to   rf-hst-lgi-fdt (I)     .
           move      hst-lgi-fin (I)      to   rf-hst-lgi-fin (I)     .
           move      hst-lgi-ffi (I)      to   rf-hst-lgi-ffi (I)     .
           go to     dec-fis-hst-400.
       dec-fis-hst-420.
           move      hst-lgn-snx          to   rf-hst-lgn-snx         .
           move      hst-lgn-ext          to   rf-hst-lgn-ext         .
           move      hst-lgi-qth          to   rf-hst-lgi-qth         .
           move      hst-lgi-snp          to   rf-hst-lgi-snp         .
           move      hst-lgi-p04          to   rf-hst-lgi-p04         .
           move      hst-lgi-p06          to   rf-hst-lgi-p06         .
           move      hst-lgi-p08          to   rf-hst-lgi-p08         .
           move      hst-lgi-p12          to   rf-hst-lgi-p12         .
           move      hst-lgi-p16          to   rf-hst-lgi-p16         .
           move      hst-lgi-p24          to   rf-hst-lgi-p24         .
           move      hst-lgi-p32          to   rf-hst-lgi-p32         .
           move      zero                 to   I                      .
       dec-fis-hst-500.
           add       1                    to   I                      .
           if        I                    >    12
                     go to dec-fis-hst-520.
           move      hst-lgt-til (I)      to   rf-hst-lgt-til (I)     .
           go to     dec-fis-hst-500.
       dec-fis-hst-520.
           move      hst-lgt-snx          to   rf-hst-lgt-snx         .
           move      hst-lgt-ext          to   rf-hst-lgt-ext         .
           move      hst-sti-snx          to   rf-hst-sti-snx         .
           move      hst-stp-snx          to   rf-hst-stp-snx         .
           move      hst-stp-ext          to   rf-hst-stp-ext         .
           move      hst-flg-blo          to   rf-hst-flg-blo         .
           move      hst-flg-nbl          to   rf-hst-flg-nbl         .
           move      hst-flg-pul          to   rf-hst-flg-pul         .
      *
           move      zero                 to   I                      .
       dec-fis-hst-600.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hst-610.
      *
           move      hst-fci-cod (I)      to   rf-hst-fci-cod (I)     .
           move      hst-fci-tla (I)      to   rf-hst-fci-tla (I)     .
      *
           move      hst-fli-cod (I)      to   rf-hst-fli-cod (I)     .
           move      hst-fli-tla (I)      to   rf-hst-fli-tla (I)     .
      *
           move      hst-mni-cod (I)      to   rf-hst-mni-cod (I)     .
           move      hst-mni-tla (I)      to   rf-hst-mni-tla (I)     .
      *
           move      hst-fii-cod (I)      to   rf-hst-fii-cod (I)     .
           move      hst-fii-tla (I)      to   rf-hst-fii-tla (I)     .
      *
           move      hst-lgi-cod (I)      to   rf-hst-lgi-cod (I)     .
           move      hst-lgi-tla (I)      to   rf-hst-lgi-tla (I)     .
      *
           go to     dec-fis-hst-600.
       dec-fis-hst-610.
           move      hst-alx-exp          to   rf-hst-alx-exp         .
       exe-cnv-hst-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hst]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hst                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hst-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hst-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hst-250.
       exe-cnv-hst-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hst]                        *
      *                  *---------------------------------------------*
           close     hst                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hst]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hst                 .
       exe-cnv-hst-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hst-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-hst] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hst-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hsr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hsr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hsr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhsr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hsr-999.
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
       exe-cnv-hsr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hsr-pat              .
       exe-cnv-hsr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hsr]                         *
      *                  *---------------------------------------------*
           open      i-o    hsr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hsr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsr                 .
       exe-cnv-hsr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hsr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hsr-k01                .
           start     hsr    key not less
                            hsr-k01
                            invalid key
                            go to exe-cnv-hsr-800.
       exe-cnv-hsr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hsr]                               *
      *              *-------------------------------------------------*
           read      hsr    next
                            with no lock
                            at end
                            go to exe-cnv-hsr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hsr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-hsr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsr                 .
       exe-cnv-hsr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hsr]                          *
      *              *-------------------------------------------------*
       exe-cnv-hsr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hsr                 .
           move      hsr-prt-cml          to   rf-hsr-prt-cml         .
           move      hsr-num-prg          to   rf-hsr-num-prg         .
           move      hsr-cod-tip          to   rf-hsr-cod-tip         .
           move      hsr-tip-sgn          to   rf-hsr-tip-sgn         .
           move      hsr-num-bia          to   rf-hsr-num-bia         .
           move      hsr-num-vlt          to   rf-hsr-num-vlt         .
           move      hsr-bia-vlt          to   rf-hsr-bia-vlt         .
           move      hsr-qta-buo          to   rf-hsr-qta-buo         .
           move      hsr-qta-sca          to   rf-hsr-qta-sca         .
           move      hsr-des-fmt          to   rf-hsr-des-fmt         .
           move      hsr-cod-fmt          to   rf-hsr-cod-fmt         .
           move      hsr-alf-fmt          to   rf-hsr-alf-fmt         .
           move      hsr-res-fmt          to   rf-hsr-res-fmt         .
           move      hsr-cod-mac          to   rf-hsr-cod-mac         .
           move      hsr-qta-lav          to   rf-hsr-qta-lav         .
           move      hsr-cod-cdc          to   rf-hsr-cod-cdc         .
           move      hsr-dat-prv          to   rf-hsr-dat-prv         .
           move      hsr-nst-snx          to   rf-hsr-nst-snx         .
           move      hsr-nst-ext          to   rf-hsr-nst-ext         .
           move      hsr-flg-blo          to   rf-hsr-flg-blo         .
           move      hsr-flg-nbl          to   rf-hsr-flg-nbl         .
           move      hsr-flg-pul          to   rf-hsr-flg-pul         .
           move      hsr-alx-exp          to   rf-hsr-alx-exp         .
       exe-cnv-hsr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hsr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hsr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hsr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hsr-250.
       exe-cnv-hsr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hsr]                        *
      *                  *---------------------------------------------*
           close     hsr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hsr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsr                 .
       exe-cnv-hsr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hsr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-hsr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hsr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hvt]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hvt-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hvt "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhvt"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hvt-999.
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
       exe-cnv-hvt-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hvt-pat              .
       exe-cnv-hvt-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hvt]                         *
      *                  *---------------------------------------------*
           open      i-o    hvt                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hvt]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvt                 .
       exe-cnv-hvt-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hvt]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hvt-k01                .
           start     hvt    key not less
                            hvt-k01
                            invalid key
                            go to exe-cnv-hvt-800.
       exe-cnv-hvt-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hvt]                               *
      *              *-------------------------------------------------*
           read      hvt    next
                            with no lock
                            at end
                            go to exe-cnv-hvt-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hvt-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-hvt]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvt                 .
       exe-cnv-hvt-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hvt]                          *
      *              *-------------------------------------------------*
       exe-cnv-hvt-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hvt                 .
           move      hvt-ide-dat          to   rf-hvt-ide-dat         .
           move      hvt-ide-ute          to   rf-hvt-ide-ute         .
           move      hvt-ide-fas          to   rf-hvt-ide-fas         .
           move      hvt-prt-prv          to   rf-hvt-prt-prv         .
           move      hvt-ver-prv          to   rf-hvt-ver-prv         .
           move      hvt-tip-prv          to   rf-hvt-tip-prv         .
           move      hvt-dlv-snx          to   rf-hvt-dlv-snx         .
           move      hvt-dlv-ext          to   rf-hvt-dlv-ext         .
           move      hvt-dla-snx          to   rf-hvt-dla-snx         .
           move      hvt-dla-ext          to   rf-hvt-dla-ext         .
           move      hvt-fci-snx          to   rf-hvt-fci-snx         .
           move      hvt-fci-not          to   rf-hvt-fci-not         .
           move      hvt-fci-qta          to   rf-hvt-fci-qta         .
           move      hvt-fce-snx          to   rf-hvt-fce-snx         .
           move      hvt-fcf-snx          to   rf-hvt-fcf-snx         .
           move      hvt-fcf-not          to   rf-hvt-fcf-not         .
           move      hvt-fli-snx          to   rf-hvt-fli-snx         .
           move      hvt-fli-not          to   rf-hvt-fli-not         .
           move      hvt-fli-qta          to   rf-hvt-fli-qta         .
           move      hvt-fle-snx          to   rf-hvt-fle-snx         .
           move      hvt-flf-snx          to   rf-hvt-flf-snx         .
           move      hvt-flf-not          to   rf-hvt-flf-not         .
           move      hvt-flf-qta          to   rf-hvt-flf-qta         .
           move      hvt-mni-snx          to   rf-hvt-mni-snx         .
           move      hvt-mnx-snx          to   rf-hvt-mnx-snx         .
           move      hvt-mnx-ext          to   rf-hvt-mnx-ext         .
           move      hvt-mni-qta          to   rf-hvt-mni-qta         .
           move      hvt-mne-snx          to   rf-hvt-mne-snx         .
           move      hvt-mnf-snx          to   rf-hvt-mnf-snx         .
           move      hvt-mnf-not          to   rf-hvt-mnf-not         .
           move      hvt-mni-esi          to   rf-hvt-mni-esi         .
           move      hvt-mni-arc          to   rf-hvt-mni-arc         .
           move      hvt-mni-fmc          to   rf-hvt-mni-fmc         .
           move      hvt-mni-fpr          to   rf-hvt-mni-fpr         .
           move      hvt-mnv-snx          to   rf-hvt-mnv-snx         .
           move      hvt-mnv-ext          to   rf-hvt-mnv-ext         .
           move      hvt-fii-snx          to   rf-hvt-fii-snx         .
           move      hvt-fie-snx          to   rf-hvt-fie-snx         .
           move      hvt-fii-esi          to   rf-hvt-fii-esi         .
           move      hvt-fii-new          to   rf-hvt-fii-new         .
           move      hvt-fii-arc          to   rf-hvt-fii-arc         .
           move      hvt-fii-eli          to   rf-hvt-fii-eli         .
           move      hvt-fii-qth          to   rf-hvt-fii-qth         .
           move      zero                 to   I                      .
       dec-fis-hvt-100.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hvt-120.
           move      hvt-fii-mac (I)      to   rf-hvt-fii-mac (I)     .
           move      hvt-fii-lst (I)      to   rf-hvt-fii-lst (I)     .
           move      hvt-fii-not (I)      to   rf-hvt-fii-not (I)     .
           move      hvt-fii-qta (I)      to   rf-hvt-fii-qta (I)     .
           go to     dec-fis-hvt-100.
       dec-fis-hvt-120.
           move      hvt-fiv-snx          to   rf-hvt-fiv-snx         .
           move      hvt-fiv-ext          to   rf-hvt-fiv-ext         .
           move      hvt-mag-snn          to   rf-hvt-mag-snn         .
           move      hvt-mag-snc          to   rf-hvt-mag-snc         .
           move      zero                 to   I                      .
       dec-fis-hvt-200.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hvt-220.
           move      hvt-mnm-num (I)      to   rf-hvt-mnm-num (I)     .
           move      hvt-mnm-not (I)      to   rf-hvt-mnm-not (I)     .
           move      hvt-mnm-qta (I)      to   rf-hvt-mnm-qta (I)     .
           go to     dec-fis-hvt-200.
       dec-fis-hvt-220.
           move      zero                 to   I                      .
       dec-fis-hvt-300.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hvt-320.
           move      hvt-mcl-num (I)      to   rf-hvt-mcl-num (I)     .
           move      hvt-mcl-not (I)      to   rf-hvt-mcl-not (I)     .
           move      hvt-mcl-qta (I)      to   rf-hvt-mcl-qta (I)     .
           go to     dec-fis-hvt-300.
       dec-fis-hvt-320.
           move      hvt-mcv-snx          to   rf-hvt-mcv-snx         .
           move      hvt-mcv-ext          to   rf-hvt-mcv-ext         .
           move      hvt-lgi-snx          to   rf-hvt-lgi-snx         .
           move      hvt-lge-snx          to   rf-hvt-lge-snx         .
           move      zero                 to   I                      .
       dec-fis-hvt-400.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hvt-420.
           move      hvt-lgi-tip (I)      to   rf-hvt-lgi-tip (I)     .
           move      hvt-lgi-fdt (I)      to   rf-hvt-lgi-fdt (I)     .
           move      hvt-lgi-fin (I)      to   rf-hvt-lgi-fin (I)     .
           move      hvt-lgi-ffi (I)      to   rf-hvt-lgi-ffi (I)     .
           go to     dec-fis-hvt-400.
       dec-fis-hvt-420.
           move      hvt-lgn-snx          to   rf-hvt-lgn-snx         .
           move      hvt-lgn-ext          to   rf-hvt-lgn-ext         .
           move      hvt-lgi-qth          to   rf-hvt-lgi-qth         .
           move      hvt-lgi-snp          to   rf-hvt-lgi-snp         .
           move      hvt-lgi-p04          to   rf-hvt-lgi-p04         .
           move      hvt-lgi-p06          to   rf-hvt-lgi-p06         .
           move      hvt-lgi-p08          to   rf-hvt-lgi-p08         .
           move      hvt-lgi-p12          to   rf-hvt-lgi-p12         .
           move      hvt-lgi-p16          to   rf-hvt-lgi-p16         .
           move      hvt-lgi-p24          to   rf-hvt-lgi-p24         .
           move      hvt-lgi-p32          to   rf-hvt-lgi-p32         .
           move      zero                 to   I                      .
       dec-fis-hvt-500.
           add       1                    to   I                      .
           if        I                    >    12
                     go to dec-fis-hvt-520.
           move      hvt-lgt-til (I)      to   rf-hvt-lgt-til (I)     .
           go to     dec-fis-hvt-500.
       dec-fis-hvt-520.
           move      hvt-lgt-snx          to   rf-hvt-lgt-snx         .
           move      hvt-lgt-ext          to   rf-hvt-lgt-ext         .
           move      hvt-sti-snx          to   rf-hvt-sti-snx         .
           move      hvt-stp-snx          to   rf-hvt-stp-snx         .
           move      hvt-stp-ext          to   rf-hvt-stp-ext         .
           move      hvt-flg-blo          to   rf-hvt-flg-blo         .
           move      hvt-flg-nbl          to   rf-hvt-flg-nbl         .
           move      hvt-flg-pul          to   rf-hvt-flg-pul         .
      *
           move      zero                 to   I                      .
       dec-fis-hvt-600.
           add       1                    to   I                      .
           if        I                    >    6
                     go to dec-fis-hvt-610.
      *
           move      hvt-fci-cod (I)      to   rf-hvt-fci-cod (I)     .
           move      hvt-fci-tla (I)      to   rf-hvt-fci-tla (I)     .
      *
           move      hvt-fli-cod (I)      to   rf-hvt-fli-cod (I)     .
           move      hvt-fli-tla (I)      to   rf-hvt-fli-tla (I)     .
      *
           move      hvt-mni-cod (I)      to   rf-hvt-mni-cod (I)     .
           move      hvt-mni-tla (I)      to   rf-hvt-mni-tla (I)     .
      *
           move      hvt-fii-cod (I)      to   rf-hvt-fii-cod (I)     .
           move      hvt-fii-tla (I)      to   rf-hvt-fii-tla (I)     .
      *
           move      hvt-lgi-cod (I)      to   rf-hvt-lgi-cod (I)     .
           move      hvt-lgi-tla (I)      to   rf-hvt-lgi-tla (I)     .
      *
           go to     dec-fis-hvt-600.
       dec-fis-hvt-610.
           move      hvt-alx-exp          to   rf-hvt-alx-exp         .
       exe-cnv-hvt-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hvt]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvt                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hvt-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hvt-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hvt-250.
       exe-cnv-hvt-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hvt]                        *
      *                  *---------------------------------------------*
           close     hvt                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hvt]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvt                 .
       exe-cnv-hvt-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hvt-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-hvt] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hvt-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hvr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hvr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hvr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhvr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hvr-999.
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
       exe-cnv-hvr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hvr-pat              .
       exe-cnv-hvr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hvr]                         *
      *                  *---------------------------------------------*
           open      i-o    hvr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hvr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvr                 .
       exe-cnv-hvr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hvr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hvr-k01                .
           start     hvr    key not less
                            hvr-k01
                            invalid key
                            go to exe-cnv-hvr-800.
       exe-cnv-hvr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hvr]                               *
      *              *-------------------------------------------------*
           read      hvr    next
                            with no lock
                            at end
                            go to exe-cnv-hvr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hvr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-hvr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvr                 .
       exe-cnv-hvr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hvr]                          *
      *              *-------------------------------------------------*
       exe-cnv-hvr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hvr                 .
           move      hvr-prt-prv          to   rf-hvr-prt-prv         .
           move      hvr-ver-prv          to   rf-hvr-ver-prv         .
           move      hvr-num-prg          to   rf-hvr-num-prg         .
           move      hvr-cod-tip          to   rf-hvr-cod-tip         .
           move      hvr-tip-sgn          to   rf-hvr-tip-sgn         .
           move      hvr-num-bia          to   rf-hvr-num-bia         .
           move      hvr-num-vlt          to   rf-hvr-num-vlt         .
           move      hvr-bia-vlt          to   rf-hvr-bia-vlt         .
           move      hvr-qta-buo          to   rf-hvr-qta-buo         .
           move      hvr-qta-sca          to   rf-hvr-qta-sca         .
           move      hvr-des-fmt          to   rf-hvr-des-fmt         .
           move      hvr-cod-fmt          to   rf-hvr-cod-fmt         .
           move      hvr-alf-fmt          to   rf-hvr-alf-fmt         .
           move      hvr-res-fmt          to   rf-hvr-res-fmt         .
           move      hvr-cod-mac          to   rf-hvr-cod-mac         .
           move      hvr-qta-lav          to   rf-hvr-qta-lav         .
           move      hvr-cod-cdc          to   rf-hvr-cod-cdc         .
           move      hvr-dat-prv          to   rf-hvr-dat-prv         .
           move      hvr-nst-snx          to   rf-hvr-nst-snx         .
           move      hvr-nst-ext          to   rf-hvr-nst-ext         .
           move      hvr-flg-blo          to   rf-hvr-flg-blo         .
           move      hvr-flg-nbl          to   rf-hvr-flg-nbl         .
           move      hvr-flg-pul          to   rf-hvr-flg-pul         .
           move      hvr-alx-exp          to   rf-hvr-alx-exp         .
       exe-cnv-hvr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hvr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hvr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hvr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hvr-250.
       exe-cnv-hvr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hvr]                        *
      *                  *---------------------------------------------*
           close     hvr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hvr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hvr                 .
       exe-cnv-hvr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hvr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-hvr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hvr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hba]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hba-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hba "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "tpg/gct/fls/ioc/obj/iofhba"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hba-999.
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
       exe-cnv-hba-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hba-pat              .
       exe-cnv-hba-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hba]                         *
      *                  *---------------------------------------------*
           open      i-o    hba                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hba]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hba                 .
       exe-cnv-hba-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hba]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hba-k01                .
           start     hba    key not less
                            hba-k01
                            invalid key
                            go to exe-cnv-hba-800.
       exe-cnv-hba-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hba]                               *
      *              *-------------------------------------------------*
           read      hba    next
                            with no lock
                            at end
                            go to exe-cnv-hba-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hba-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-hba]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hba                 .
       exe-cnv-hba-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hba]                          *
      *              *-------------------------------------------------*
       exe-cnv-hba-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hba                 .
           move      hba-prt-cml          to   rf-hba-prt-cml         .
           move      hba-num-prg          to   rf-hba-num-prg         .
           move      hba-prg-imp          to   rf-hba-prg-imp         .
           move      hba-cod-adt          to   rf-hba-cod-adt         .
           move      hba-dat-stp          to   rf-hba-dat-stp         .
           move      hba-tip-sgn          to   rf-hba-tip-sgn         .
           move      hba-qta-buo          to   rf-hba-qta-buo         .
           move      hba-qta-pos          to   rf-hba-qta-pos         .
           move      hba-nst-ex1          to   rf-hba-nst-ex1         .
           move      hba-nst-ex2          to   rf-hba-nst-ex2         .
           move      hba-alx-exp          to   rf-hba-alx-exp         .
       exe-cnv-hba-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hba]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hba                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hba-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hba-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hba-250.
       exe-cnv-hba-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hba]                        *
      *                  *---------------------------------------------*
           close     hba                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hba]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hba                 .
       exe-cnv-hba-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hba-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-hba] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hba-999.
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
       exe-cnv-hor-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hor-pat              .
       exe-cnv-hor-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hor]                         *
      *                  *---------------------------------------------*
           open      i-o    hor                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hor]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
       exe-cnv-hor-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hor]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hor-k01                .
           start     hor    key not less
                            hor-k01
                            invalid key
                            go to exe-cnv-hor-800.
       exe-cnv-hor-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hor]                               *
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
      *              * Normalizzazione preliminare [new-hor]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
       exe-cnv-hor-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hor]                          *
      *              *-------------------------------------------------*
       exe-cnv-hor-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
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
       exe-cnv-hor-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hor]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
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
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hor]                        *
      *                  *---------------------------------------------*
           close     hor                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hor]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
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
      *              * Delete file [old-hor] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hor-999.
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
       exe-cnv-hur-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-hur-pat              .
       exe-cnv-hur-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-hur]                         *
      *                  *---------------------------------------------*
           open      i-o    hur                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-hur]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
       exe-cnv-hur-200.
      *              *-------------------------------------------------*
      *              * Start su [old-hur]                              *
      *              *-------------------------------------------------*
           move      low-values           to   hur-k01                .
           start     hur    key not less
                            hur-k01
                            invalid key
                            go to exe-cnv-hur-800.
       exe-cnv-hur-250.
      *              *-------------------------------------------------*
      *              * Next su [old-hur]                               *
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
      *              * Normalizzazione preliminare [new-hur]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
       exe-cnv-hur-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-hur]                          *
      *              *-------------------------------------------------*
       exe-cnv-hur-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
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
       exe-cnv-hur-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-hur]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
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
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-hur]                        *
      *                  *---------------------------------------------*
           close     hur                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-hur]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hur                 .
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
      *              * Delete file [old-hur] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-hur-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di conferma, se manuale                         *
      *    *-----------------------------------------------------------*
       ric-cnf-man-000.
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
           if        rr-aut-man           not  = "M"
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
      *    * Rename file iniziale ed estrazione pathname file origina- *
      *    * le                                                        *
      *    *-----------------------------------------------------------*
       fil-ren-ini-000.
      *              *-------------------------------------------------*
      *              * Rename file originale [xxx] in [xxx2]           *
      *              *-------------------------------------------------*
           move      "FR"                 to   s-ope                  .
           move      f-xxx-nam            to   s-nam                  .
           move      spaces               to   s-alf                  .
           string    f-xxx-nam  delimited by   spaces
                     "2"        delimited by   size
                                          into s-alf                  .
           move      "K"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Estrazione pathname file originale              *
      *              *-------------------------------------------------*
           move      "PG"                 to   s-ope                  .
           move      spaces               to   s-nam                  .
           string    f-xxx-nam  delimited by   spaces
                     "2"        delimited by   size
                                          into s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-xxx-pat              .
      *              *-------------------------------------------------*
      *              * Prompt numero records letti                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero records letti       : 0"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt numero records scritti                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero records scritti     : 0"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fil-ren-ini-999.
           exit.

      *    *===========================================================*
      *    * Delete file finale e scrittura rullino messaggi           *
      *    *-----------------------------------------------------------*
       fil-del-fin-000.
      *              *-------------------------------------------------*
      *              * Delete                                          *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-xxx-pat            to   s-pat                  .
           move      "K"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
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
       fil-del-fin-999.
           exit.

      *    *===========================================================*
      *    * Accettazione presa visione                                *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
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
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se funzionamento automa-   *
      *              * tico o manuale                                  *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "M"
                     go to acc-pre-vis-600
           else      go to acc-pre-vis-400.
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attesa di 5  secondi                        *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
           call      "swd/mod/prg/obj/mwait0"                         .
           call      "swd/mod/prg/obj/mwait0"                         .
           call      "swd/mod/prg/obj/mwait0"                         .
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

