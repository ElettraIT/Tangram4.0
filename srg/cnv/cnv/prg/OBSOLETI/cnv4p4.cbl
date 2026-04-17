       Identification Division.
       Program-Id.                                 cnv4p4             .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    cnv                 *
      *                                   Fase:    cnv4p4              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/05/03    *
      *                       Ultima revisione:    NdK del 22/05/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per versione 4.4                *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Aggiornamento file [bef] : 06/05/03         *
      *                    Aggiornamento file [ocr] : 06/05/03         *
      *                    Aggiornamento file [ofr] : 06/05/03         *
      *                    Aggiornamento file [aaq] : 06/05/03         *
      *                    Creazione     file [adc] : 16/05/03         *
      *                    Aggiornamento file [lst] : 22/05/03         *
      *                    Aggiornamento file [lsd] : 22/05/03         *
      *                    Aggiornamento file [zvf] : 29/05/03         *
      *                    Aggiornamento file [yvf] : 29/05/03         *
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
      *    * File Control [bef]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bef   assign to disk           f-bef-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bef-k01
                   alternate record key   is bef-k02
                   alternate record key   is bef-k03
                             file status  is                f-bef-sts .

      *    *===========================================================*
      *    * File Control [ocr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ocr   assign to disk           f-ocr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ocr-k01
                   alternate record key   is ocr-k02
                   alternate record key   is ocr-k03
                   alternate record key   is ocr-k04
                             file status  is                f-ocr-sts .

      *    *===========================================================*
      *    * File Control [ofr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ofr   assign to disk           f-ofr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ofr-k01
                   alternate record key   is ofr-k02
                   alternate record key   is ofr-k03
                   alternate record key   is ofr-k04
                             file status  is                f-ofr-sts .

      *    *===========================================================*
      *    * File Control [aaq]                                        *
      *    *-----------------------------------------------------------*
           select  optional  aaq   assign to disk           f-aaq-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is aaq-k01
                   alternate record key   is aaq-k02
                   alternate record key   is aaq-k03
                   alternate record key   is aaq-k04
                             file status  is                f-aaq-sts .

      *    *===========================================================*
      *    * File Control [lst]                                        *
      *    *-----------------------------------------------------------*
           select  optional  lst   assign to disk           f-lst-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is lst-k01
                   alternate record key   is lst-k02
                             file status  is                f-lst-sts .

      *    *===========================================================*
      *    * File Control [lsd]                                        *
      *    *-----------------------------------------------------------*
           select  optional  lsd   assign to disk           f-lsd-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is lsd-k01
                   alternate record key   is lsd-k02
                             file status  is                f-lsd-sts .

      *    *===========================================================*
      *    * File Control [zvf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  zvf   assign to disk           f-zvf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is zvf-k01
                             file status  is                f-zvf-sts .

      *    *===========================================================*
      *    * File Control [yvf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  yvf   assign to disk           f-yvf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is yvf-k01
                             file status  is                f-yvf-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [bef]                                    *
      *    *-----------------------------------------------------------*
       fd  bef       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bef-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bef-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODBEF                         *
      *            *---------------------------------------------------*
               10  bef-k01.
                   15  bef-cod-dcf        pic  9(07)       comp-3     .
                   15  bef-dpz-dcf        pic  x(04)                  .
                   15  bef-cod-bef        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  bef-k02.
                   15  bef-cod-dcf-2      pic  9(07)       comp-3     .
                   15  bef-dpz-dcf-2      pic  x(04)                  .
                   15  bef-des-key        pic  x(20)                  .
                   15  bef-cod-bef-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CODMNE                         *
      *            *---------------------------------------------------*
               10  bef-k03.
                   15  bef-cod-dcf-3      pic  9(07)       comp-3     .
                   15  bef-dpz-dcf-3      pic  x(04)                  .
                   15  bef-mne-bef        pic  x(05)                  .
                   15  bef-cod-bef-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bef-dat.
               10  bef-ide-dat            pic  9(07)       comp-3     .
               10  bef-ide-ute            pic  x(08)                  .
               10  bef-ide-fas            pic  x(06)                  .
               10  bef-des-bef            pic  x(40)                  .
               10  bef-fil-bef            pic  x(40)                  .
               10  bef-ccc-bef            pic  x(20)                  .
               10  bef-cod-swf            pic  x(20)                  .
               10  bef-alx-exp            pic  x(60)                  .

      *    *===========================================================*
      *    * File Description [ocr]                                    *
      *    *-----------------------------------------------------------*
       fd  ocr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ocr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ocr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  ocr-k01.
                   15  ocr-num-prt        pic  9(11)       comp-3     .
                   15  ocr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  ocr-k02.
                   15  ocr-cod-dpz        pic  9(02)                  .
                   15  ocr-tip-mag        pic  9(02)                  .
                   15  ocr-num-pro        pic  9(07)       comp-3     .
                   15  ocr-dat-doc        pic  9(07)       comp-3     .
                   15  ocr-num-doc        pic  9(11)       comp-3     .
                   15  ocr-num-prt-2      pic  9(11)       comp-3     .
                   15  ocr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  ocr-k03.
                   15  ocr-cod-dpz-3      pic  9(02)                  .
                   15  ocr-flg-rch        pic  x(01)                  .
                   15  ocr-tip-mag-3      pic  9(02)                  .
                   15  ocr-num-pro-3      pic  9(07)       comp-3     .
                   15  ocr-num-prt-3      pic  9(11)       comp-3     .
                   15  ocr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  ocr-k04.
                   15  ocr-cod-dpz-4      pic  9(02)                  .
                   15  ocr-flg-rch-4      pic  x(01)                  .
                   15  ocr-tip-arc        pic  x(01)                  .
                   15  ocr-cod-arc        pic  9(07)       comp-3     .
                   15  ocr-num-prt-4      pic  9(11)       comp-3     .
                   15  ocr-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ocr-dat.
               10  ocr-tmo-orc            pic  x(05)                  .
               10  ocr-dpz-arc            pic  x(04)                  .
               10  ocr-cod-lng            pic  x(03)                  .
               10  ocr-ocl-dat            pic  9(07)       comp-3     .
               10  ocr-ocl-num            pic  x(10)                  .
               10  ocr-pri-eva            pic  x(02)                  .
               10  ocr-vpf.
                   15  ocr-sgl-vpf        pic  x(03)                  .
                   15  ocr-dec-vpf        pic  9(01)                  .
                   15  ocr-tdc-vpf        pic  x(01)                  .
                   15  ocr-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  ocr-bld-flb            pic  9(01)                  .
               10  ocr-bld-tpb            pic  9(01)                  .
               10  ocr-bld-rgb            pic  9(01)                  .
               10  ocr-tip-rig            pic  x(05)                  .
               10  ocr-alf-pro            pic  x(14)                  .
               10  ocr-sgl-vrn            pic  x(14)                  .
               10  ocr-cop-scl            pic  x(14)                  .
               10  ocr-des-ext            pic  9(01)                  .
               10  ocr-des-rig            pic  x(40)                  .
               10  ocr-tip-pro            pic  9(02)                  .
               10  ocr-cod-iva            pic  9(05)       comp-3     .
               10  ocr-ctp-ven            pic  9(07)       comp-3     .
               10  ocr-umi-ven            pic  x(03)                  .
               10  ocr-dec-qta            pic  9(01)                  .
               10  ocr-qta-ord            pic s9(06)v9(03) comp-3     .
               10  ocr-sdr-ccs            pic  x(01)                  .
               10  ocr-cod-dsl            pic  x(07)                  .
               10  ocr-snx-2qt            pic  9(01)                  .
               10  ocr-dec-2qt            pic  9(01)                  .
               10  ocr-qta-a02            pic s9(06)v9(03) comp-3     .
               10  ocr-snx-3qt            pic  9(01)                  .
               10  ocr-dec-3qt            pic  9(01)                  .
               10  ocr-qta-a03            pic s9(06)v9(03) comp-3     .
               10  ocr-dec-prz            pic  9(01)                  .
               10  ocr-vps.
                   15  ocr-sgl-vps        pic  x(03)                  .
                   15  ocr-dec-vps        pic  9(01)                  .
                   15  ocr-tdc-vps        pic  x(01)                  .
                   15  ocr-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  ocr-prz-lrs            pic  9(09)       comp-3     .
               10  ocr-prz-nts            pic  9(09)       comp-3     .
               10  ocr-vpp.
                   15  ocr-sgl-vpp        pic  x(03)                  .
                   15  ocr-dec-vpp        pic  9(01)                  .
                   15  ocr-tdc-vpp        pic  x(01)                  .
                   15  ocr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  ocr-prz-ven            pic  9(09)       comp-3     .
               10  ocr-snx-2pz            pic  9(01)                  .
               10  ocr-prz-a02            pic  9(09)       comp-3     .
               10  ocr-vpl.
                   15  ocr-sgl-vpl        pic  x(03)                  .
                   15  ocr-dec-vpl        pic  9(01)                  .
                   15  ocr-tdc-vpl        pic  x(01)                  .
                   15  ocr-prz-vpl        pic  9(09)       comp-3     .
                   15  ocr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  ocr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  ocr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  ocr-tlm-vpl        pic  x(01)                  .
                   15  ocr-map-vpl        pic  x(01)                  .
               10  ocr-epz-rgf            pic  9(01)                  .
               10  ocr-csr-aap            pic  9(05)       comp-3     .
               10  ocr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  ocr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  ocr-prz-net            pic  9(09)       comp-3     .
               10  ocr-epz-pes            pic  9(02)                  .
               10  ocr-vpc.
                   15  ocr-sgl-vpc        pic  x(03)                  .
                   15  ocr-dec-vpc        pic  9(01)                  .
                   15  ocr-tdc-vpc        pic  x(01)                  .
                   15  ocr-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  ocr-dec-cos            pic  9(01)                  .
               10  ocr-cos-rif            pic  9(09)       comp-3     .
               10  ocr-imp-rig            pic s9(11)       comp-3     .
               10  ocr-iau-rig            pic s9(11)       comp-3     .
               10  ocr-cpv-aap            pic  9(05)       comp-3     .
               10  ocr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  ocr-fsp-rig            pic  9(02)                  .
               10  ocr-cpv-rig            pic  9(05)       comp-3     .
               10  ocr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  ocr-pvf-rig            pic s9(11)       comp-3     .
               10  ocr-dcn-ric            pic  9(07)       comp-3     .
               10  ocr-dcn-prv            pic  9(07)       comp-3     .
               10  ocr-flg-cnf            pic  x(01)                  .
               10  ocr-cmc-tip            pic  x(05)                  .
               10  ocr-cmc-dat            pic  9(07)       comp-3     .
               10  ocr-cmc-num            pic  9(11)       comp-3     .
               10  ocr-flg-ela.
                   15  ocr-flg-blo.
                       20  ocr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  ocr-flg-nbl.
                       20  ocr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  ocr-flg-pul            pic  x(01)                  .
               10  ocr-flg-puq            pic  x(01)                  .
               10  ocr-tip-ord            pic  x(01)                  .
               10  ocr-alx-exp.
                   15  filler  occurs 18  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ofr]                                    *
      *    *-----------------------------------------------------------*
       fd  ofr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ofr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ofr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  ofr-k01.
                   15  ofr-num-prt        pic  9(11)       comp-3     .
                   15  ofr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  ofr-k02.
                   15  ofr-cod-dpz        pic  9(02)                  .
                   15  ofr-tip-mag        pic  9(02)                  .
                   15  ofr-num-mag        pic  9(07)       comp-3     .
                   15  ofr-dat-doc        pic  9(07)       comp-3     .
                   15  ofr-num-doc        pic  9(11)       comp-3     .
                   15  ofr-num-prt-2      pic  9(11)       comp-3     .
                   15  ofr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  ofr-k03.
                   15  ofr-cod-dpz-3      pic  9(02)                  .
                   15  ofr-flg-rch        pic  x(01)                  .
                   15  ofr-tip-mag-3      pic  9(02)                  .
                   15  ofr-num-mag-3      pic  9(07)       comp-3     .
                   15  ofr-num-prt-3      pic  9(11)       comp-3     .
                   15  ofr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  ofr-k04.
                   15  ofr-cod-dpz-4      pic  9(02)                  .
                   15  ofr-flg-rch-4      pic  x(01)                  .
                   15  ofr-tip-arc        pic  x(01)                  .
                   15  ofr-cod-arc        pic  9(07)       comp-3     .
                   15  ofr-num-prt-4      pic  9(11)       comp-3     .
                   15  ofr-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ofr-dat.
               10  ofr-tmo-orf            pic  x(05)                  .
               10  ofr-dpz-arc            pic  x(04)                  .
               10  ofr-cod-lng            pic  x(03)                  .
               10  ofr-cof-dat            pic  9(07)       comp-3     .
               10  ofr-cof-num            pic  x(10)                  .
               10  ofr-pri-eva            pic  x(02)                  .
               10  ofr-vpf.
                   15  ofr-sgl-vpf        pic  x(03)                  .
                   15  ofr-dec-vpf        pic  9(01)                  .
                   15  ofr-tdc-vpf        pic  x(01)                  .
                   15  ofr-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  ofr-bld-flb            pic  9(01)                  .
               10  ofr-bld-tpb            pic  9(01)                  .
               10  ofr-bld-rgb            pic  9(01)                  .
               10  ofr-tip-rig            pic  x(05)                  .
               10  ofr-alf-mag            pic  x(14)                  .
               10  ofr-sgl-vrn            pic  x(14)                  .
               10  ofr-fda-pif            pic  x(14)                  .
               10  ofr-cop-sfn            pic  x(14)                  .
               10  ofr-snx-tum            pic  x(01)                  .
               10  ofr-umf-tum            pic  x(03)                  .
               10  ofr-nde-tum            pic  9(01)                  .
               10  ofr-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  ofr-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  ofr-des-ext            pic  9(01)                  .
               10  ofr-des-rig            pic  x(40)                  .
               10  ofr-tip-pro            pic  9(02)                  .
               10  ofr-cod-iva            pic  9(05)       comp-3     .
               10  ofr-ctp-acq            pic  9(07)       comp-3     .
               10  ofr-umi-acq            pic  x(03)                  .
               10  ofr-dec-qta            pic  9(01)                  .
               10  ofr-qta-fda            pic s9(06)v9(03) comp-3     .
               10  ofr-qta-ord            pic s9(06)v9(03) comp-3     .
               10  ofr-sdr-ccs            pic  x(01)                  .
               10  ofr-cod-dsl            pic  x(07)                  .
               10  ofr-snx-2qt            pic  9(01)                  .
               10  ofr-dec-2qt            pic  9(01)                  .
               10  ofr-qta-a02            pic s9(06)v9(03) comp-3     .
               10  ofr-snx-3qt            pic  9(01)                  .
               10  ofr-dec-3qt            pic  9(01)                  .
               10  ofr-qta-a03            pic s9(06)v9(03) comp-3     .
               10  ofr-dec-prz            pic  9(01)                  .
               10  ofr-vpp.
                   15  ofr-sgl-vpp        pic  x(03)                  .
                   15  ofr-dec-vpp        pic  9(01)                  .
                   15  ofr-tdc-vpp        pic  x(01)                  .
                   15  ofr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  ofr-prz-acq            pic  9(09)       comp-3     .
               10  ofr-snx-2pz            pic  9(01)                  .
               10  ofr-dec-2pz            pic  9(01)                  .
               10  ofr-prz-a02            pic  9(09)       comp-3     .
               10  ofr-vpl.
                   15  ofr-sgl-vpl        pic  x(03)                  .
                   15  ofr-dec-vpl        pic  9(01)                  .
                   15  ofr-tdc-vpl        pic  x(01)                  .
                   15  ofr-prz-vpl        pic  9(09)       comp-3     .
                   15  ofr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  ofr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  ofr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  ofr-tlm-vpl        pic  x(01)                  .
                   15  ofr-map-vpl        pic  x(01)                  .
               10  ofr-epz-rgo            pic  9(01)                  .
               10  ofr-csr-aap            pic  9(05)                  .
               10  ofr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  ofr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  ofr-prz-net            pic  9(09)       comp-3     .
               10  ofr-imp-rig            pic s9(11)       comp-3     .
               10  ofr-iau-rig            pic s9(11)       comp-3     .
               10  ofr-cpv-aap            pic  9(05)       comp-3     .
               10  ofr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  ofr-fsp-rig            pic  9(02)                  .
               10  ofr-cpv-rig            pic  9(05)       comp-3     .
               10  ofr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  ofr-pvf-rig            pic s9(11)       comp-3     .
               10  ofr-dcn-ric            pic  9(07)                  .
               10  ofr-fds-dcr            pic  x(01)                  .
               10  ofr-dcn-prv            pic  9(07)                  .
               10  ofr-flg-cnf            pic  x(01)                  .
               10  ofr-oda-tip            pic  x(05)                  .
               10  ofr-oda-dat            pic  9(07)       comp-3     .
               10  ofr-oda-num            pic  9(11)       comp-3     .
               10  ofr-flg-ela.
                   15  ofr-flg-blo.
                       20  ofr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  ofr-flg-nbl.
                       20  ofr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  ofr-flg-pul            pic  x(01)                  .
               10  ofr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [aaq]                                    *
      *    *-----------------------------------------------------------*
       fd  aaq       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  aaq-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  aaq-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  aaq-k01.
                   15  aaq-tip-mag        pic  9(02)                  .
                   15  aaq-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  aaq-k02.
                   15  aaq-ide-dat        pic  9(07)       comp-3     .
                   15  aaq-tip-mag-2      pic  9(02)                  .
                   15  aaq-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CDPPDT                         *
      *            *---------------------------------------------------*
               10  aaq-k03.
                   15  aaq-cdp-pdt        pic  x(14)                  .
                   15  aaq-tip-mag-3      pic  9(02)                  .
                   15  aaq-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PDTCDP                         *
      *            *---------------------------------------------------*
               10  aaq-k04.
                   15  aaq-cod-pdt        pic  9(07)       comp-3     .
                   15  aaq-cdp-pdt-4      pic  x(14)                  .
                   15  aaq-tip-mag-4      pic  9(02)                  .
                   15  aaq-num-pro-4      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  aaq-dat.
               10  aaq-ide-ute            pic  x(08)                  .
               10  aaq-ide-fas            pic  x(06)                  .
               10  aaq-cod-iva            pic  9(05)       comp-3     .
               10  aaq-ctp-acq            pic  9(07)       comp-3     .
               10  aaq-dcf-pfz            pic  9(07)       comp-3     .
               10  aaq-dpz-pfz            pic  x(04)                  .
               10  aaq-sgl-vlt            pic  x(03)                  .
               10  aaq-dec-vlt            pic  9(01)                  .
               10  aaq-dec-prz            pic  9(01)                  .
               10  aaq-prz-acr            pic  9(09)       comp-3     .
               10  aaq-uda-par            pic  9(07)       comp-3     .
               10  aaq-tmp-apv            pic  9(03)       comp-3     .
               10  aaq-epz-rgf            pic  9(01)                  .
               10  aaq-snx-2qt            pic  9(01)                  .
               10  aaq-dec-2qt            pic  9(01)                  .
               10  aaq-snx-3qt            pic  9(01)                  .
               10  aaq-dec-3qt            pic  9(01)                  .
               10  aaq-snx-2pz            pic  9(01)                  .
               10  aaq-dec-2pz            pic  9(01)                  .
               10  aaq-aut-lst            pic  x(03)                  .
               10  aaq-tip-vac            pic  x(03)                  .
               10  aaq-cdp-aqt            pic  9(05)       comp-3     .
               10  aaq-pdp-aqt  occurs 03
                                          pic  9(02)v9(01)            .
               10  aaq-lot-acq            pic  9(06)v9(03) comp-3     .
               10  aaq-cla-bdg            pic  9(05)       comp-3     .
               10  aaq-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [lst]                                    *
      *    *-----------------------------------------------------------*
       fd  lst       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  lst-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  lst-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTPRO                         *
      *            *---------------------------------------------------*
               10  lst-k01.
                   15  lst-tip-rec        pic  9(02)                  .
                   15  lst-cod-lst        pic  x(03)                  .
                   15  lst-cod-cli        pic  9(07)       comp-3     .
                   15  lst-sgl-vlt        pic  x(03)                  .
                   15  lst-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PROLST                         *
      *            *---------------------------------------------------*
               10  lst-k02.
                   15  lst-tip-rec-2      pic  9(02)                  .
                   15  lst-num-pro-2      pic  9(07)       comp-3     .
                   15  lst-sgl-vlt-2      pic  x(03)                  .
                   15  lst-cod-lst-2      pic  x(03)                  .
                   15  lst-cod-cli-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  lst-dat.
               10  lst-dec-vlt            pic  9(01)                  .
               10  lst-prz-lst            pic  9(09)       comp-3     .
               10  lst-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  lst-per-sco occurs 05  pic  9(02)v9(01) comp-3     .
               10  lst-snx-prz            pic  x(01)                  .
               10  lst-snx-sco            pic  x(01)                  .
               10  lst-snx-pvg            pic  x(01)                  .
               10  lst-dva-ini            pic  9(07)                  .
               10  lst-dva-fin            pic  9(07)                  .
               10  lst-alx-exp.
                   15  filler occurs 06   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [lsd]                                    *
      *    *-----------------------------------------------------------*
       fd  lsd       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  lsd-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  lsd-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : LSTPRODVF                      *
      *            *---------------------------------------------------*
               10  lsd-k01.
                   15  lsd-tip-rec        pic  9(02)                  .
                   15  lsd-cod-lst        pic  x(03)                  .
                   15  lsd-cod-cli        pic  9(07)       comp-3     .
                   15  lsd-sgl-vlt        pic  x(03)                  .
                   15  lsd-num-pro        pic  9(07)       comp-3     .
                   15  lsd-dva-fin        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : LSTPRODFR                      *
      *            *---------------------------------------------------*
               10  lsd-k02.
                   15  lsd-tip-rec-2      pic  9(02)                  .
                   15  lsd-cod-lst-2      pic  x(03)                  .
                   15  lsd-cod-cli-2      pic  9(07)       comp-3     .
                   15  lsd-sgl-vlt-2      pic  x(03)                  .
                   15  lsd-num-pro-2      pic  9(07)       comp-3     .
                   15  lsd-dvf-rev        pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  lsd-dat.
               10  lsd-dec-vlt            pic  9(01)                  .
               10  lsd-prz-lst            pic  9(09)       comp-3     .
               10  lsd-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  lsd-per-sco occurs 05  pic  9(02)v9(01) comp-3     .
               10  lsd-snx-prz            pic  x(01)                  .
               10  lsd-snx-sco            pic  x(01)                  .
               10  lsd-snx-pvg            pic  x(01)                  .
               10  lsd-dva-ini            pic  9(07)                  .
               10  lsd-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [zvf]                                    *
      *    *-----------------------------------------------------------*
       fd  zvf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  zvf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  zvf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODVDF                         *
      *            *---------------------------------------------------*
               10  zvf-k01.
                   15  zvf-num-def        pic  9(03)       comp-3     .
                   15  zvf-cod-def        pic  x(03)                  .
                   15  zvf-cod-lng        pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  zvf-dat.
               10  zvf-des-stp            pic  x(25)                  .
               10  zvf-alx-exp            pic  x(20)                  .

      *    *===========================================================*
      *    * File Description [yvf]                                    *
      *    *-----------------------------------------------------------*
       fd  yvf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  yvf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  yvf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODVDF                         *
      *            *---------------------------------------------------*
               10  yvf-k01.
                   15  yvf-num-def        pic  9(03)       comp-3     .
                   15  yvf-cod-def        pic  x(03)                  .
                   15  yvf-cod-lng        pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  yvf-dat.
               10  yvf-des-stp            pic  x(40)                  .
               10  yvf-alx-exp            pic  x(05)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [bef]                                       *
      *    *-----------------------------------------------------------*
       01  f-bef.
           05  f-bef-nam                  pic  x(04)                  .
           05  f-bef-pat                  pic  x(40)                  .
           05  f-bef-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ocr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ocr.
           05  f-ocr-nam                  pic  x(04)                  .
           05  f-ocr-pat                  pic  x(40)                  .
           05  f-ocr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ofr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ofr.
           05  f-ofr-nam                  pic  x(04)                  .
           05  f-ofr-pat                  pic  x(40)                  .
           05  f-ofr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [aaq]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaq.
           05  f-aaq-nam                  pic  x(04)                  .
           05  f-aaq-pat                  pic  x(40)                  .
           05  f-aaq-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [lst]                                       *
      *    *-----------------------------------------------------------*
       01  f-lst.
           05  f-lst-nam                  pic  x(04)                  .
           05  f-lst-pat                  pic  x(40)                  .
           05  f-lst-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [lsd]                                       *
      *    *-----------------------------------------------------------*
       01  f-lsd.
           05  f-lsd-nam                  pic  x(04)                  .
           05  f-lsd-pat                  pic  x(40)                  .
           05  f-lsd-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zvf]                                       *
      *    *-----------------------------------------------------------*
       01  f-zvf.
           05  f-zvf-nam                  pic  x(04)                  .
           05  f-zvf-pat                  pic  x(40)                  .
           05  f-zvf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [yvf]                                       *
      *    *-----------------------------------------------------------*
       01  f-yvf.
           05  f-yvf-nam                  pic  x(04)                  .
           05  f-yvf-pat                  pic  x(40)                  .
           05  f-yvf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bef]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfbef"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcm"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [yvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyvf"                          .

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
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cnv4p4"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnv4p4  "                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONE PER RELEASE 4.4       "       .

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
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mopsys" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-nam                  pic  x(04)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-sts                  pic  x(02)                  .
           05  f-xxx-obj                  pic  x(40)                  .
           05  f-xxx-nrf                  pic  9(13)                  .
           05  f-xxx-nrl                  pic  9(11)                  .
           05  f-xxx-nrs                  pic  9(11)                  .
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
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det numero records                           *
      *        *-------------------------------------------------------*
           05  w-det-rec-fil.
               10  w-det-rec-fil-flg      pic  x(01)                  .
               10  w-det-rec-fil-alf.
                   15  w-det-rec-fil-tst  pic  x(13)                  .
                   15  w-det-rec-fil-ler  pic  x(05)                  .
                   15  w-det-rec-fil-dat  pic  x(13)                  .
               10  w-det-rec-fil-rec      pic  9(11)                  .
               10  w-det-rec-fil-s15      pic s9(15)                  .
               10  w-det-rec-fil-v02      pic s9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Cal                               *
      *    *-----------------------------------------------------------*
       01  w-cal.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  w-c01                      pic  9(02)                  .
           05  w-c02                      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Exe                               *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Comodo per esportazione contatti                      *
      *        *-------------------------------------------------------*
           05  w-exe-con.
               10  w-exe-con-arc          pic  9(02)                  .
               10  w-exe-con-cod          pic  9(07)                  .
               10  w-exe-con-dpz          pic  x(04)                  .
               10  w-exe-con-des          pic  x(40)                  .
               10  w-exe-con-tip          pic  x(03)                  .
               10  w-exe-con-lun          pic  9(02)                  .
               10  w-exe-con-num          pic  x(80)                  .
               10  w-exe-con-nue          pic  x(20)                  .
               10  w-exe-con-pre          pic  x(20)                  .
               10  w-exe-con-int          pic  x(30)                  .
               10  w-exe-con-idd          pic  9(07)                  .
               10  w-exe-con-idu          pic  x(08)                  .
               10  w-exe-con-idf          pic  x(06)                  .
               10  w-exe-con-prg          pic  9(05)                  .

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
           move      "UP  "               to   v-pfk (01)             .
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
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
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
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
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
           move      "AM#"                to   v-msk                  .
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
      *              * Conversione [bef]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bef-000      thru exe-cnv-bef-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ocr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ocr-000      thru exe-cnv-ocr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ofr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ofr-000      thru exe-cnv-ofr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [aaq]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaq-000      thru exe-cnv-aaq-999        .
      *              *-------------------------------------------------*
      *              * Conversione [adc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-000      thru exe-cnv-adc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [lst]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-lst-000      thru exe-cnv-lst-999        .
      *              *-------------------------------------------------*
      *              * Conversione [lsd]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-lsd-000      thru exe-cnv-lsd-999        .
      *              *-------------------------------------------------*
      *              * Conversione [zvf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-zvf-000      thru exe-cnv-zvf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [yvf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-yvf-000      thru exe-cnv-yvf-999        .
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
      *    * Conversione [bef]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bef-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bef "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/scf/fls/ioc/obj/iofbef"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bef-999.
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
       exe-cnv-bef-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-bef-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-bef-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-bef]                         *
      *                  *---------------------------------------------*
           open      i-o    bef                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-bef]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
       exe-cnv-bef-200.
      *              *-------------------------------------------------*
      *              * Start su [old-bef]                              *
      *              *-------------------------------------------------*
           move      low-values           to   bef-k01                .
           start     bef    key not less
                            bef-k01
                            invalid key
                            go to exe-cnv-bef-800.
       exe-cnv-bef-250.
      *              *-------------------------------------------------*
      *              * Next su [old-bef]                               *
      *              *-------------------------------------------------*
           read      bef    next
                            with no lock
                            at end
                            go to exe-cnv-bef-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bef-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-bef]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
       exe-cnv-bef-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-bef]                          *
      *              *-------------------------------------------------*
       exe-cnv-bef-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bef                 .
           move      bef-ide-dat          to   rf-bef-ide-dat         .
           move      bef-ide-ute          to   rf-bef-ide-ute         .
           move      bef-ide-fas          to   rf-bef-ide-fas         .
           move      bef-cod-dcf          to   rf-bef-cod-dcf         .
           move      bef-dpz-dcf          to   rf-bef-dpz-dcf         .
           move      bef-cod-bef          to   rf-bef-cod-bef         .
           move      bef-mne-bef          to   rf-bef-mne-bef         .
           move      bef-des-bef          to   rf-bef-des-bef         .
           move      bef-des-key          to   rf-bef-des-key         .
           move      bef-fil-bef          to   rf-bef-fil-bef         .
           move      bef-ccc-bef          to   rf-bef-ccc-bef         .
           move      bef-cod-swf          to   rf-bef-cod-swf         .
           move      bef-alx-exp          to   rf-bef-alx-exp         .
       exe-cnv-bef-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-bef]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bef-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bef-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bef-250.
       exe-cnv-bef-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-bef]                        *
      *                  *---------------------------------------------*
           close     bef                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-bef]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bef                 .
       exe-cnv-bef-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bef-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-bef] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-bef-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ocr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ocr-000.
       exe-cnv-ocr-010.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ocr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ocr-999.
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
       exe-cnv-ocr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-ocr-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-ocr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-ocr]                         *
      *                  *---------------------------------------------*
           open      i-o    ocr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-ocr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-cnv-ocr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-ocr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   ocr-k01                .
           start     ocr    key not less
                            ocr-k01
                            invalid key
                            go to exe-cnv-ocr-800.
       exe-cnv-ocr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-ocr]                               *
      *              *-------------------------------------------------*
           read      ocr    next
                            with no lock
                            at end
                            go to exe-cnv-ocr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ocr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-ocr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-cnv-ocr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-ocr]                          *
      *              *-------------------------------------------------*
       exe-cnv-ocr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ocr                 .
           move      ocr-num-prt          to   rf-ocr-num-prt         .
           move      ocr-num-prg          to   rf-ocr-num-prg         .
           move      ocr-tmo-orc          to   rf-ocr-tmo-orc         .
           move      ocr-cod-dpz          to   rf-ocr-cod-dpz         .
           move      ocr-dat-doc          to   rf-ocr-dat-doc         .
           move      ocr-num-doc          to   rf-ocr-num-doc         .
           move      ocr-tip-arc          to   rf-ocr-tip-arc         .
           move      ocr-cod-arc          to   rf-ocr-cod-arc         .
           move      ocr-dpz-arc          to   rf-ocr-dpz-arc         .
           move      ocr-cod-lng          to   rf-ocr-cod-lng         .
           move      ocr-ocl-dat          to   rf-ocr-ocl-dat         .
           move      ocr-ocl-num          to   rf-ocr-ocl-num         .
           move      ocr-pri-eva          to   rf-ocr-pri-eva         .
           move      ocr-sgl-vpf          to   rf-ocr-sgl-vpf         .
           move      ocr-dec-vpf          to   rf-ocr-dec-vpf         .
           move      ocr-tdc-vpf          to   rf-ocr-tdc-vpf         .
           move      ocr-cdc-vpf          to   rf-ocr-cdc-vpf         .
           move      ocr-bld-flb          to   rf-ocr-bld-flb         .
           move      ocr-bld-tpb          to   rf-ocr-bld-tpb         .
           move      ocr-bld-rgb          to   rf-ocr-bld-rgb         .
           move      ocr-tip-rig          to   rf-ocr-tip-rig         .
           move      ocr-tip-mag          to   rf-ocr-tip-mag         .
           move      ocr-num-pro          to   rf-ocr-num-pro         .
           move      ocr-alf-pro          to   rf-ocr-alf-pro         .
           move      ocr-sgl-vrn          to   rf-ocr-sgl-vrn         .
           move      ocr-cop-scl          to   rf-ocr-cop-scl         .
           move      ocr-des-ext          to   rf-ocr-des-ext         .
           move      ocr-des-rig          to   rf-ocr-des-rig         .
           move      ocr-tip-pro          to   rf-ocr-tip-pro         .
           move      ocr-cod-iva          to   rf-ocr-cod-iva         .
           move      ocr-ctp-ven          to   rf-ocr-ctp-ven         .
           move      ocr-umi-ven          to   rf-ocr-umi-ven         .
           move      ocr-dec-qta          to   rf-ocr-dec-qta         .
           move      ocr-qta-ord          to   rf-ocr-qta-ord         .
           move      ocr-sdr-ccs          to   rf-ocr-sdr-ccs         .
           move      ocr-cod-dsl          to   rf-ocr-cod-dsl         .
           move      ocr-snx-2qt          to   rf-ocr-snx-2qt         .
           move      ocr-dec-2qt          to   rf-ocr-dec-2qt         .
           move      ocr-qta-a02          to   rf-ocr-qta-a02         .
           move      ocr-snx-3qt          to   rf-ocr-snx-3qt         .
           move      ocr-dec-3qt          to   rf-ocr-dec-3qt         .
           move      ocr-qta-a03          to   rf-ocr-qta-a03         .
           move      ocr-dec-prz          to   rf-ocr-dec-prz         .
           move      ocr-sgl-vps          to   rf-ocr-sgl-vps         .
           move      ocr-dec-vps          to   rf-ocr-dec-vps         .
           move      ocr-tdc-vps          to   rf-ocr-tdc-vps         .
           move      ocr-cdc-vps          to   rf-ocr-cdc-vps         .
           move      ocr-prz-lrs          to   rf-ocr-prz-lrs         .
           move      ocr-prz-nts          to   rf-ocr-prz-nts         .
           move      ocr-sgl-vpp          to   rf-ocr-sgl-vpp         .
           move      ocr-dec-vpp          to   rf-ocr-dec-vpp         .
           move      ocr-tdc-vpp          to   rf-ocr-tdc-vpp         .
           move      ocr-cdc-vpp          to   rf-ocr-cdc-vpp         .
           move      ocr-prz-ven          to   rf-ocr-prz-ven         .
           move      ocr-snx-2pz          to   rf-ocr-snx-2pz         .
           move      ocr-prz-a02          to   rf-ocr-prz-a02         .
           move      ocr-sgl-vpl          to   rf-ocr-sgl-vpl         .
           move      ocr-dec-vpl          to   rf-ocr-dec-vpl         .
           move      ocr-tdc-vpl          to   rf-ocr-tdc-vpl         .
           move      ocr-prz-vpl          to   rf-ocr-prz-vpl         .
           move      ocr-cdc-vpl          to   rf-ocr-cdc-vpl         .
           move      ocr-ccr-vpl          to   rf-ocr-ccr-vpl         .
           move      ocr-plm-vpl          to   rf-ocr-plm-vpl         .
           move      ocr-tlm-vpl          to   rf-ocr-tlm-vpl         .
           move      ocr-map-vpl          to   rf-ocr-map-vpl         .
           move      ocr-epz-rgf          to   rf-ocr-epz-rgf         .
           move      ocr-csr-aap          to   rf-ocr-csr-aap         .
           move      ocr-psr-aap (1)      to   rf-ocr-psr-aap (1)     .
           move      ocr-psr-aap (2)      to   rf-ocr-psr-aap (2)     .
           move      ocr-psr-aap (3)      to   rf-ocr-psr-aap (3)     .
           move      ocr-psr-aap (4)      to   rf-ocr-psr-aap (4)     .
           move      ocr-psr-aap (5)      to   rf-ocr-psr-aap (5)     .
           move      ocr-per-scr (1)      to   rf-ocr-per-scr (1)     .
           move      ocr-per-scr (2)      to   rf-ocr-per-scr (2)     .
           move      ocr-per-scr (3)      to   rf-ocr-per-scr (3)     .
           move      ocr-per-scr (4)      to   rf-ocr-per-scr (4)     .
           move      ocr-per-scr (5)      to   rf-ocr-per-scr (5)     .
           move      ocr-prz-net          to   rf-ocr-prz-net         .
           move      ocr-epz-pes          to   rf-ocr-epz-pes         .
           move      ocr-sgl-vpc          to   rf-ocr-sgl-vpc         .
           move      ocr-dec-vpc          to   rf-ocr-dec-vpc         .
           move      ocr-tdc-vpc          to   rf-ocr-tdc-vpc         .
           move      ocr-cdc-vpc          to   rf-ocr-cdc-vpc         .
           move      ocr-dec-cos          to   rf-ocr-dec-cos         .
           move      ocr-cos-rif          to   rf-ocr-cos-rif         .
           move      ocr-imp-rig          to   rf-ocr-imp-rig         .
           move      ocr-iau-rig          to   rf-ocr-iau-rig         .
           move      ocr-cpv-aap          to   rf-ocr-cpv-aap         .
           move      ocr-ppv-aap (1)      to   rf-ocr-ppv-aap (1)     .
           move      ocr-ppv-aap (2)      to   rf-ocr-ppv-aap (2)     .
           move      ocr-ppv-aap (3)      to   rf-ocr-ppv-aap (3)     .
           move      ocr-fsp-rig          to   rf-ocr-fsp-rig         .
           move      ocr-cpv-rig          to   rf-ocr-cpv-rig         .
           move      ocr-ppv-rig (1)      to   rf-ocr-ppv-rig (1)     .
           move      ocr-ppv-rig (2)      to   rf-ocr-ppv-rig (2)     .
           move      ocr-ppv-rig (3)      to   rf-ocr-ppv-rig (3)     .
           move      ocr-pvf-rig          to   rf-ocr-pvf-rig         .
           move      ocr-dcn-ric          to   rf-ocr-dcn-ric         .
           move      ocr-dcn-prv          to   rf-ocr-dcn-prv         .
           move      ocr-flg-cnf          to   rf-ocr-flg-cnf         .
           move      ocr-cmc-tip          to   rf-ocr-cmc-tip         .
           move      ocr-cmc-dat          to   rf-ocr-cmc-dat         .
           move      ocr-cmc-num          to   rf-ocr-cmc-num         .
           move      ocr-flg-rch          to   rf-ocr-flg-rch         .
           move      ocr-flg-blx (1)      to   rf-ocr-flg-blx (1)     .
           move      ocr-flg-blx (2)      to   rf-ocr-flg-blx (2)     .
           move      ocr-flg-blx (3)      to   rf-ocr-flg-blx (3)     .
           move      ocr-flg-blx (4)      to   rf-ocr-flg-blx (4)     .
           move      ocr-flg-blx (5)      to   rf-ocr-flg-blx (5)     .
           move      ocr-flg-blx (6)      to   rf-ocr-flg-blx (6)     .
           move      ocr-flg-blx (7)      to   rf-ocr-flg-blx (7)     .
           move      ocr-flg-nbx (1)      to   rf-ocr-flg-nbx (1)     .
           move      ocr-flg-nbx (2)      to   rf-ocr-flg-nbx (2)     .
           move      ocr-flg-nbx (3)      to   rf-ocr-flg-nbx (3)     .
           move      ocr-flg-pul          to   rf-ocr-flg-pul         .
           move      ocr-flg-puq          to   rf-ocr-flg-puq         .
           move      ocr-tip-ord          to   rf-ocr-tip-ord         .
           move      ocr-alx-exp          to   rf-ocr-alx-exp         .
       exe-cnv-ocr-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * dcn-cnf = data consegna confermata          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-ocr-dcn-cnf         .
       exe-cnv-ocr-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * dec-2pz = decimali secondo prezzo           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-ocr-dec-2pz         .
       exe-cnv-ocr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-ocr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ocr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ocr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ocr-250.
       exe-cnv-ocr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-ocr]                        *
      *                  *---------------------------------------------*
           close     ocr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-ocr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-cnv-ocr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ocr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-ocr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-ocr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ofr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ofr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ofr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ofr-999.
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
       exe-cnv-ofr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-ofr-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-ofr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-ofr]                         *
      *                  *---------------------------------------------*
           open      i-o    ofr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-ofr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       exe-cnv-ofr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-ofr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   ofr-k01                .
           start     ofr    key not less
                            ofr-k01
                            invalid key
                            go to exe-cnv-ofr-800.
       exe-cnv-ofr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-ofr]                               *
      *              *-------------------------------------------------*
           read      ofr    next
                            with no lock
                            at end
                            go to exe-cnv-ofr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ofr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-ofr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       exe-cnv-ofr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-ofr]                          *
      *              *-------------------------------------------------*
       exe-cnv-ofr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ofr                 .
           move      ofr-num-prt          to   rf-ofr-num-prt         .
           move      ofr-num-prg          to   rf-ofr-num-prg         .
           move      ofr-tmo-orf          to   rf-ofr-tmo-orf         .
           move      ofr-cod-dpz          to   rf-ofr-cod-dpz         .
           move      ofr-dat-doc          to   rf-ofr-dat-doc         .
           move      ofr-num-doc          to   rf-ofr-num-doc         .
           move      ofr-tip-arc          to   rf-ofr-tip-arc         .
           move      ofr-cod-arc          to   rf-ofr-cod-arc         .
           move      ofr-dpz-arc          to   rf-ofr-dpz-arc         .
           move      ofr-cod-lng          to   rf-ofr-cod-lng         .
           move      ofr-cof-dat          to   rf-ofr-cof-dat         .
           move      ofr-cof-num          to   rf-ofr-cof-num         .
           move      ofr-pri-eva          to   rf-ofr-pri-eva         .
           move      ofr-sgl-vpf          to   rf-ofr-sgl-vpf         .
           move      ofr-dec-vpf          to   rf-ofr-dec-vpf         .
           move      ofr-tdc-vpf          to   rf-ofr-tdc-vpf         .
           move      ofr-cdc-vpf          to   rf-ofr-cdc-vpf         .
           move      ofr-bld-flb          to   rf-ofr-bld-flb         .
           move      ofr-bld-tpb          to   rf-ofr-bld-tpb         .
           move      ofr-bld-rgb          to   rf-ofr-bld-rgb         .
           move      ofr-tip-rig          to   rf-ofr-tip-rig         .
           move      ofr-tip-mag          to   rf-ofr-tip-mag         .
           move      ofr-num-mag          to   rf-ofr-num-mag         .
           move      ofr-alf-mag          to   rf-ofr-alf-mag         .
           move      ofr-sgl-vrn          to   rf-ofr-sgl-vrn         .
           move      ofr-fda-pif          to   rf-ofr-fda-pif         .
           move      ofr-cop-sfn          to   rf-ofr-cop-sfn         .
           move      ofr-snx-tum          to   rf-ofr-snx-tum         .
           move      ofr-umf-tum          to   rf-ofr-umf-tum         .
           move      ofr-nde-tum          to   rf-ofr-nde-tum         .
           move      ofr-cmo-tum          to   rf-ofr-cmo-tum         .
           move      ofr-cdi-tum          to   rf-ofr-cdi-tum         .
           move      ofr-des-ext          to   rf-ofr-des-ext         .
           move      ofr-des-rig          to   rf-ofr-des-rig         .
           move      ofr-tip-pro          to   rf-ofr-tip-pro         .
           move      ofr-cod-iva          to   rf-ofr-cod-iva         .
           move      ofr-ctp-acq          to   rf-ofr-ctp-acq         .
           move      ofr-umi-acq          to   rf-ofr-umi-acq         .
           move      ofr-dec-qta          to   rf-ofr-dec-qta         .
           move      ofr-qta-fda          to   rf-ofr-qta-fda         .
           move      ofr-qta-ord          to   rf-ofr-qta-ord         .
           move      ofr-sdr-ccs          to   rf-ofr-sdr-ccs         .
           move      ofr-cod-dsl          to   rf-ofr-cod-dsl         .
           move      ofr-snx-2qt          to   rf-ofr-snx-2qt         .
           move      ofr-dec-2qt          to   rf-ofr-dec-2qt         .
           move      ofr-qta-a02          to   rf-ofr-qta-a02         .
           move      ofr-snx-3qt          to   rf-ofr-snx-3qt         .
           move      ofr-dec-3qt          to   rf-ofr-dec-3qt         .
           move      ofr-qta-a03          to   rf-ofr-qta-a03         .
           move      ofr-dec-prz          to   rf-ofr-dec-prz         .
           move      ofr-sgl-vpp          to   rf-ofr-sgl-vpp         .
           move      ofr-dec-vpp          to   rf-ofr-dec-vpp         .
           move      ofr-tdc-vpp          to   rf-ofr-tdc-vpp         .
           move      ofr-cdc-vpp          to   rf-ofr-cdc-vpp         .
           move      ofr-prz-acq          to   rf-ofr-prz-acq         .
           move      ofr-snx-2pz          to   rf-ofr-snx-2pz         .
           move      ofr-dec-2pz          to   rf-ofr-dec-2pz         .
           move      ofr-prz-a02          to   rf-ofr-prz-a02         .
           move      ofr-sgl-vpl          to   rf-ofr-sgl-vpl         .
           move      ofr-dec-vpl          to   rf-ofr-dec-vpl         .
           move      ofr-tdc-vpl          to   rf-ofr-tdc-vpl         .
           move      ofr-prz-vpl          to   rf-ofr-prz-vpl         .
           move      ofr-cdc-vpl          to   rf-ofr-cdc-vpl         .
           move      ofr-ccr-vpl          to   rf-ofr-ccr-vpl         .
           move      ofr-plm-vpl          to   rf-ofr-plm-vpl         .
           move      ofr-tlm-vpl          to   rf-ofr-tlm-vpl         .
           move      ofr-map-vpl          to   rf-ofr-map-vpl         .
           move      ofr-epz-rgo          to   rf-ofr-epz-rgo         .
           move      ofr-csr-aap          to   rf-ofr-csr-aap         .
           move      ofr-psr-aap (1)      to   rf-ofr-psr-aap (1)     .
           move      ofr-psr-aap (2)      to   rf-ofr-psr-aap (2)     .
           move      ofr-psr-aap (3)      to   rf-ofr-psr-aap (3)     .
           move      ofr-psr-aap (4)      to   rf-ofr-psr-aap (4)     .
           move      ofr-psr-aap (5)      to   rf-ofr-psr-aap (5)     .
           move      ofr-per-scr (1)      to   rf-ofr-per-scr (1)     .
           move      ofr-per-scr (2)      to   rf-ofr-per-scr (2)     .
           move      ofr-per-scr (3)      to   rf-ofr-per-scr (3)     .
           move      ofr-per-scr (4)      to   rf-ofr-per-scr (4)     .
           move      ofr-per-scr (5)      to   rf-ofr-per-scr (5)     .
           move      ofr-prz-net          to   rf-ofr-prz-net         .
           move      ofr-imp-rig          to   rf-ofr-imp-rig         .
           move      ofr-iau-rig          to   rf-ofr-iau-rig         .
           move      ofr-cpv-aap          to   rf-ofr-cpv-aap         .
           move      ofr-ppv-aap (1)      to   rf-ofr-ppv-aap (1)     .
           move      ofr-ppv-aap (2)      to   rf-ofr-ppv-aap (2)     .
           move      ofr-ppv-aap (3)      to   rf-ofr-ppv-aap (3)     .
           move      ofr-fsp-rig          to   rf-ofr-fsp-rig         .
           move      ofr-cpv-rig          to   rf-ofr-cpv-rig         .
           move      ofr-ppv-rig (1)      to   rf-ofr-ppv-rig (1)     .
           move      ofr-ppv-rig (2)      to   rf-ofr-ppv-rig (2)     .
           move      ofr-ppv-rig (3)      to   rf-ofr-ppv-rig (3)     .
           move      ofr-pvf-rig          to   rf-ofr-pvf-rig         .
           move      ofr-dcn-ric          to   rf-ofr-dcn-ric         .
           move      ofr-fds-dcr          to   rf-ofr-fds-dcr         .
           move      ofr-dcn-prv          to   rf-ofr-dcn-prv         .
           move      ofr-flg-cnf          to   rf-ofr-flg-cnf         .
           move      ofr-oda-tip          to   rf-ofr-oda-tip         .
           move      ofr-oda-dat          to   rf-ofr-oda-dat         .
           move      ofr-oda-num          to   rf-ofr-oda-num         .
           move      ofr-flg-rch          to   rf-ofr-flg-rch         .
           move      ofr-flg-blx (1)      to   rf-ofr-flg-blx (1)     .
           move      ofr-flg-blx (2)      to   rf-ofr-flg-blx (2)     .
           move      ofr-flg-blx (3)      to   rf-ofr-flg-blx (3)     .
           move      ofr-flg-blx (4)      to   rf-ofr-flg-blx (4)     .
           move      ofr-flg-blx (5)      to   rf-ofr-flg-blx (5)     .
           move      ofr-flg-blx (6)      to   rf-ofr-flg-blx (6)     .
           move      ofr-flg-blx (7)      to   rf-ofr-flg-blx (7)     .
           move      ofr-flg-nbx (1)      to   rf-ofr-flg-nbx (1)     .
           move      ofr-flg-nbx (2)      to   rf-ofr-flg-nbx (2)     .
           move      ofr-flg-nbx (3)      to   rf-ofr-flg-nbx (3)     .
           move      ofr-flg-pul          to   rf-ofr-flg-pul         .
           move      ofr-alx-exp          to   rf-ofr-alx-exp         .
       exe-cnv-ofr-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * dcn-cnf = data consegna confermata          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-ofr-dcn-cnf         .
       exe-cnv-ofr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-ofr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ofr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ofr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ofr-250.
       exe-cnv-ofr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-ofr]                        *
      *                  *---------------------------------------------*
           close     ofr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-ofr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
       exe-cnv-ofr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ofr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-ofr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-ofr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [aaq]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-aaq-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "aaq "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-aaq-999.
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
       exe-cnv-aaq-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-aaq-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-aaq-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-aaq]                         *
      *                  *---------------------------------------------*
           open      i-o    aaq                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-aaq]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       exe-cnv-aaq-200.
      *              *-------------------------------------------------*
      *              * Start su [old-aaq]                              *
      *              *-------------------------------------------------*
           move      low-values           to   aaq-k01                .
           start     aaq    key not less
                            aaq-k01
                            invalid key
                            go to exe-cnv-aaq-800.
       exe-cnv-aaq-250.
      *              *-------------------------------------------------*
      *              * Next su [old-aaq]                               *
      *              *-------------------------------------------------*
           read      aaq    next
                            with no lock
                            at end
                            go to exe-cnv-aaq-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-aaq-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-aaq]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       exe-cnv-aaq-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-aaq]                          *
      *              *-------------------------------------------------*
       exe-cnv-aaq-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-aaq                 .
           move      aaq-ide-dat          to   rf-aaq-ide-dat         .
           move      aaq-ide-ute          to   rf-aaq-ide-ute         .
           move      aaq-ide-fas          to   rf-aaq-ide-fas         .
           move      aaq-tip-mag          to   rf-aaq-tip-mag         .
           move      aaq-num-pro          to   rf-aaq-num-pro         .
           move      aaq-cod-iva          to   rf-aaq-cod-iva         .
           move      aaq-ctp-acq          to   rf-aaq-ctp-acq         .
           move      aaq-dcf-pfz          to   rf-aaq-dcf-pfz         .
           move      aaq-dpz-pfz          to   rf-aaq-dpz-pfz         .
           move      aaq-sgl-vlt          to   rf-aaq-sgl-vlt         .
           move      aaq-dec-vlt          to   rf-aaq-dec-vlt         .
           move      aaq-dec-prz          to   rf-aaq-dec-prz         .
           move      aaq-prz-acr          to   rf-aaq-prz-acr         .
           move      aaq-uda-par          to   rf-aaq-uda-par         .
           move      aaq-cod-pdt          to   rf-aaq-cod-pdt         .
           move      aaq-cdp-pdt          to   rf-aaq-cdp-pdt         .
           move      aaq-tmp-apv          to   rf-aaq-tmp-apv         .
           move      aaq-epz-rgf          to   rf-aaq-epz-rgf         .
           move      aaq-snx-2qt          to   rf-aaq-snx-2qt         .
           move      aaq-dec-2qt          to   rf-aaq-dec-2qt         .
           move      aaq-snx-3qt          to   rf-aaq-snx-3qt         .
           move      aaq-dec-3qt          to   rf-aaq-dec-3qt         .
           move      aaq-snx-2pz          to   rf-aaq-snx-2pz         .
           move      aaq-dec-2pz          to   rf-aaq-dec-2pz         .
           move      aaq-aut-lst          to   rf-aaq-aut-lst         .
           move      aaq-tip-vac          to   rf-aaq-tip-vac         .
           move      aaq-cdp-aqt          to   rf-aaq-cdp-aqt         .
           move      aaq-pdp-aqt (1)      to   rf-aaq-pdp-aqt (1)     .
           move      aaq-pdp-aqt (2)      to   rf-aaq-pdp-aqt (2)     .
           move      aaq-pdp-aqt (3)      to   rf-aaq-pdp-aqt (3)     .
           move      aaq-lot-acq          to   rf-aaq-lot-acq         .
           move      aaq-cla-bdg          to   rf-aaq-cla-bdg         .
           move      aaq-alx-exp          to   rf-aaq-alx-exp         .
       exe-cnv-aaq-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-aaq]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-aaq-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-aaq-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-aaq-250.
       exe-cnv-aaq-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-aaq]                        *
      *                  *---------------------------------------------*
           close     aaq                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-aaq]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       exe-cnv-aaq-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-aaq-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-aaq] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-aaq-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [adc] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "adc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-adc-999.
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
       exe-cnv-adc-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [adc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       exe-cnv-adc-100.
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-cli-000  thru exe-cnv-adc-cli-999    .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-dcc-000  thru exe-cnv-adc-dcc-999    .
      *              *-------------------------------------------------*
      *              * [dcm]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-dcm-000  thru exe-cnv-adc-dcm-999    .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-fnt-000  thru exe-cnv-adc-fnt-999    .
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-dcf-000  thru exe-cnv-adc-dcf-999    .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-vet-000  thru exe-cnv-adc-vet-999    .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           perform   exe-cnv-adc-age-000  thru exe-cnv-adc-age-999    .
       exe-cnv-adc-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [adc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       exe-cnv-adc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-adc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       exe-cnv-adc-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [cli] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-cli-000.
       exe-cnv-adc-cli-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cli]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       exe-cnv-adc-cli-100.
      *              *-------------------------------------------------*
      *              * Start su file [cli]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-cli-800.
       exe-cnv-adc-cli-200.
      *              *-------------------------------------------------*
      *              * Next su [cli]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-cli-800.
       exe-cnv-adc-cli-300.
      *              *-------------------------------------------------*
      *              * Max su [cli]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-cli-400.
      *              *-------------------------------------------------*
      *              * Sel su [cli]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-cli-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-cli-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-cli-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      01                   to   w-exe-con-arc          .
           move      rf-cli-cod-cli       to   w-exe-con-cod          .
           move      spaces               to   w-exe-con-dpz          .
           move      rf-cli-rag-key       to   w-exe-con-des          .
           move      rf-cli-ide-dat       to   w-exe-con-idd          .
           move      rf-cli-ide-ute       to   w-exe-con-idu          .
           move      rf-cli-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-cli-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-cli-num-tel       to   w-exe-con-num          .
           move      rf-cli-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-cli-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-cli-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-cli-660.
      *              *-------------------------------------------------*
      *              * Cellulare 1                                     *
      *              *-------------------------------------------------*
           move      "CEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-cli-num-tlx       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-cli-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [cli]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-cli-200.
       exe-cnv-adc-cli-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cli]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       exe-cnv-adc-cli-850.
       exe-cnv-adc-cli-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [dcc] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-dcc-000.
       exe-cnv-adc-dcc-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
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
       exe-cnv-adc-dcc-100.
      *              *-------------------------------------------------*
      *              * Start su file [dcc]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      zero                 to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-dcc-800.
       exe-cnv-adc-dcc-200.
      *              *-------------------------------------------------*
      *              * Next su [dcc]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-dcc-800.
       exe-cnv-adc-dcc-300.
      *              *-------------------------------------------------*
      *              * Max su [dcc]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcc-400.
      *              *-------------------------------------------------*
      *              * Sel su [dcc]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcc-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-dcc-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcc-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      02                   to   w-exe-con-arc          .
           move      rf-dcc-cod-cli       to   w-exe-con-cod          .
           move      rf-dcc-dpz-cli       to   w-exe-con-dpz          .
           move      rf-dcc-rag-key       to   w-exe-con-des          .
           move      rf-dcc-ide-dat       to   w-exe-con-idd          .
           move      rf-dcc-ide-ute       to   w-exe-con-idu          .
           move      rf-dcc-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-dcc-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcc-num-tel       to   w-exe-con-num          .
           move      rf-dcc-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcc-640.
      *              *-------------------------------------------------*
      *              * Telefono alternativo                            *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcc-tel-alt       to   w-exe-con-num          .
           move      rf-dcc-int-alt       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcc-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcc-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcc-650.
      *              *-------------------------------------------------*
      *              * E-mail                                          *
      *              *-------------------------------------------------*
           move      "EML"                to   w-exe-con-tip          .
           move      80                   to   w-exe-con-lun          .
      *
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      rf-dcc-idn-ema       to   w-all-str-cat (1)      .
           move      rf-dcc-idn-em2       to   w-all-str-cat (2)      .
           move      rf-dcc-num-tlx       to   w-all-str-cat (3)      .
           move      rf-dcc-tel-mdm       to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           move      w-all-str-alf        to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcc-660.
      *              *-------------------------------------------------*
      *              * Cellulare 1                                     *
      *              *-------------------------------------------------*
           move      "CEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcc-num-ptp       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcc-670.
      *              *-------------------------------------------------*
      *              * Cellulare 2                                     *
      *              *-------------------------------------------------*
           move      "CEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcc-num-ptf       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [dcc]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-dcc-200.
       exe-cnv-adc-dcc-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
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
       exe-cnv-adc-dcc-850.
       exe-cnv-adc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [dcm] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-dcm-000.
       exe-cnv-adc-dcm-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [dcm]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcm                 .
       exe-cnv-adc-dcm-100.
      *              *-------------------------------------------------*
      *              * Start su file [dcm]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      zero                 to   rf-dcm-cod-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcm                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-dcm-800.
       exe-cnv-adc-dcm-200.
      *              *-------------------------------------------------*
      *              * Next su [dcm]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcm                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-dcm-800.
       exe-cnv-adc-dcm-300.
      *              *-------------------------------------------------*
      *              * Max su [dcm]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcm-400.
      *              *-------------------------------------------------*
      *              * Sel su [dcm]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcm-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-dcm-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcm-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      03                   to   w-exe-con-arc          .
           move      rf-dcm-cod-cli       to   w-exe-con-cod          .
           move      spaces               to   w-exe-con-dpz          .
           move      rf-dcm-rag-key       to   w-exe-con-des          .
           move      rf-dcm-ide-dat       to   w-exe-con-idd          .
           move      rf-dcm-ide-ute       to   w-exe-con-idu          .
           move      rf-dcm-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-dcm-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcm-num-tel       to   w-exe-con-num          .
           move      rf-dcm-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcm-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcm-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcm-650.
      *              *-------------------------------------------------*
      *              * E-mail                                          *
      *              *-------------------------------------------------*
           move      "EML"                to   w-exe-con-tip          .
           move      80                   to   w-exe-con-lun          .
           move      rf-dcm-iem-ail       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcm-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [dcm]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-dcm-200.
       exe-cnv-adc-dcm-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [dcm]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcm                 .
       exe-cnv-adc-dcm-850.
       exe-cnv-adc-dcm-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [fnt] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-fnt-000.
       exe-cnv-adc-fnt-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [fnt]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       exe-cnv-adc-fnt-100.
      *              *-------------------------------------------------*
      *              * Start su file [fnt]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFNT    "         to   f-key                  .
           move      zero                 to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-fnt-800.
       exe-cnv-adc-fnt-200.
      *              *-------------------------------------------------*
      *              * Next su [fnt]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-fnt-800.
       exe-cnv-adc-fnt-300.
      *              *-------------------------------------------------*
      *              * Max su [fnt]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-fnt-400.
      *              *-------------------------------------------------*
      *              * Sel su [fnt]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-fnt-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-fnt-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-fnt-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      11                   to   w-exe-con-arc          .
           move      rf-fnt-cod-fnt       to   w-exe-con-cod          .
           move      spaces               to   w-exe-con-dpz          .
           move      rf-fnt-rag-key       to   w-exe-con-des          .
           move      rf-fnt-ide-dat       to   w-exe-con-idd          .
           move      rf-fnt-ide-ute       to   w-exe-con-idu          .
           move      rf-fnt-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-fnt-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-fnt-num-tel       to   w-exe-con-num          .
           move      rf-fnt-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-fnt-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-fnt-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-fnt-660.
      *              *-------------------------------------------------*
      *              * Cellulare 1                                     *
      *              *-------------------------------------------------*
           move      "CEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-fnt-num-tlx       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-fnt-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [fnt]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-fnt-200.
       exe-cnv-adc-fnt-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [fnt]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       exe-cnv-adc-fnt-850.
       exe-cnv-adc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [dcf] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-dcf-000.
       exe-cnv-adc-dcf-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
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
       exe-cnv-adc-dcf-100.
      *              *-------------------------------------------------*
      *              * Start su file [dcf]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFNT    "         to   f-key                  .
           move      zero                 to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-dcf-800.
       exe-cnv-adc-dcf-200.
      *              *-------------------------------------------------*
      *              * Next su [dcf]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-dcf-800.
       exe-cnv-adc-dcf-300.
      *              *-------------------------------------------------*
      *              * Max su [dcf]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcf-400.
      *              *-------------------------------------------------*
      *              * Sel su [dcf]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcf-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-dcf-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-dcf-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      12                   to   w-exe-con-arc          .
           move      rf-dcf-cod-fnt       to   w-exe-con-cod          .
           move      rf-dcf-dpz-fnt       to   w-exe-con-dpz          .
           move      rf-dcf-rag-key       to   w-exe-con-des          .
           move      rf-dcf-ide-dat       to   w-exe-con-idd          .
           move      rf-dcf-ide-ute       to   w-exe-con-idu          .
           move      rf-dcf-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-dcf-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcf-num-tel       to   w-exe-con-num          .
           move      rf-dcf-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcf-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcf-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcf-650.
      *              *-------------------------------------------------*
      *              * E-mail                                          *
      *              *-------------------------------------------------*
           move      "EML"                to   w-exe-con-tip          .
           move      40                   to   w-exe-con-lun          .
           move      rf-dcf-eml-int       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcf-660.
      *              *-------------------------------------------------*
      *              * Cellulare 1                                     *
      *              *-------------------------------------------------*
           move      "CEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-dcf-num-tlx       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-dcf-670.
       exe-cnv-adc-dcf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [dcf]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-dcf-200.
       exe-cnv-adc-dcf-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
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
       exe-cnv-adc-dcf-850.
       exe-cnv-adc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [vet] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-vet-000.
       exe-cnv-adc-vet-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [vet]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       exe-cnv-adc-vet-100.
      *              *-------------------------------------------------*
      *              * Start su file [vet]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODVET    "         to   f-key                  .
           move      zero                 to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-vet-800.
       exe-cnv-adc-vet-200.
      *              *-------------------------------------------------*
      *              * Next su [vet]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-vet-800.
       exe-cnv-adc-vet-300.
      *              *-------------------------------------------------*
      *              * Max su [vet]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-vet-400.
      *              *-------------------------------------------------*
      *              * Sel su [vet]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-vet-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-vet-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-vet-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      41                   to   w-exe-con-arc          .
           move      rf-vet-cod-vet       to   w-exe-con-cod          .
           move      spaces               to   w-exe-con-dpz          .
           move      rf-vet-rag-key       to   w-exe-con-des          .
           move      rf-vet-ide-dat       to   w-exe-con-idd          .
           move      rf-vet-ide-ute       to   w-exe-con-idu          .
           move      rf-vet-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-vet-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-vet-num-tel       to   w-exe-con-num          .
           move      rf-vet-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-vet-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-vet-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-vet-660.
      *              *-------------------------------------------------*
      *              * Cellulare 1                                     *
      *              *-------------------------------------------------*
           move      "CEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-vet-num-tlx       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-vet-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [vet]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-vet-200.
       exe-cnv-adc-vet-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [vet]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       exe-cnv-adc-vet-850.
       exe-cnv-adc-vet-999.
           exit.

      *    *===========================================================*
      *    * Esportazione [age] - contatti                             *
      *    *-----------------------------------------------------------*
       exe-cnv-adc-age-000.
       exe-cnv-adc-age-020.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
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
       exe-cnv-adc-age-100.
      *              *-------------------------------------------------*
      *              * Start su file [age]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODAGE    "         to   f-key                  .
           move      zero                 to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a close file              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-age-800.
       exe-cnv-adc-age-200.
      *              *-------------------------------------------------*
      *              * Next su [age]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a close file                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-adc-age-800.
       exe-cnv-adc-age-300.
      *              *-------------------------------------------------*
      *              * Max su [age]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-age-400.
      *              *-------------------------------------------------*
      *              * Sel su [age]                                    *
      *              *-------------------------------------------------*
       exe-cnv-adc-age-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-adc-age-600.
      *              *-------------------------------------------------*
      *              * Composizione record in output                   *
      *              *-------------------------------------------------*
       exe-cnv-adc-age-620.
      *              *-------------------------------------------------*
      *              * Valori generici                                 *
      *              *-------------------------------------------------*
           move      31                   to   w-exe-con-arc          .
           move      rf-age-cod-age       to   w-exe-con-cod          .
           move      spaces               to   w-exe-con-dpz          .
           move      rf-age-rag-key       to   w-exe-con-des          .
           move      rf-age-ide-dat       to   w-exe-con-idd          .
           move      rf-age-ide-ute       to   w-exe-con-idu          .
           move      rf-age-ide-fas       to   w-exe-con-idf          .
           move      zero                 to   w-exe-con-prg          .
       exe-cnv-adc-age-630.
      *              *-------------------------------------------------*
      *              * Telefono                                        *
      *              *-------------------------------------------------*
           move      "TEL"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-age-num-tel       to   w-exe-con-num          .
           move      rf-age-nom-int       to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-age-640.
      *              *-------------------------------------------------*
      *              * Fax                                             *
      *              *-------------------------------------------------*
           move      "FAX"                to   w-exe-con-tip          .
           move      20                   to   w-exe-con-lun          .
           move      rf-age-num-fax       to   w-exe-con-num          .
           move      spaces               to   w-exe-con-int          .
           perform   exe-cnv-con-000      thru exe-cnv-con-999        .
       exe-cnv-adc-age-660.
       exe-cnv-adc-age-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [age]                         *
      *              *-------------------------------------------------*
           go to     exe-cnv-adc-age-200.
       exe-cnv-adc-age-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
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
       exe-cnv-adc-age-850.
       exe-cnv-adc-age-999.
           exit.

      *    *===========================================================*
      *    * Esportazione contatti generico                            *
      *    *-----------------------------------------------------------*
       exe-cnv-con-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-exe-con-num        =    spaces and
                     w-exe-con-int        =    spaces
                     go to exe-cnv-con-999.
      *              *-------------------------------------------------*
      *              * Incremento progressivo                          *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-con-prg          .
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       exe-cnv-con-100.
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      w-exe-con-idd        to   rf-adc-ide-dat         .
           move      w-exe-con-idu        to   rf-adc-ide-ute         .
           move      w-exe-con-idf        to   rf-adc-ide-fas         .
           move      w-exe-con-arc        to   rf-adc-tip-arc         .
           move      w-exe-con-cod        to   rf-adc-cod-arc         .
           move      w-exe-con-dpz        to   rf-adc-dpz-arc         .
           move      w-exe-con-des        to   rf-adc-des-key         .
           move      w-exe-con-tip        to   rf-adc-tip-con         .
           move      w-exe-con-prg        to   rf-adc-num-prg         .
           move      w-exe-con-int        to   rf-adc-int-con         .
           move      spaces               to   rf-adc-rep-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      w-exe-con-num        to   rf-adc-num-con         .
           move      rf-adc-ide-dat       to   rf-adc-dat-agg         .
           move      spaces               to   rf-adc-alx-exp         .
       exe-cnv-con-200.
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       exe-cnv-con-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-con-999.
       exe-cnv-con-999.
           exit.

      *    *===========================================================*
      *    * Conversione [lst]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-lst-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "lst "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-lst-999.
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
       exe-cnv-lst-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-lst-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-lst-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-lst]                         *
      *                  *---------------------------------------------*
           open      i-o    lst                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-lst]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       exe-cnv-lst-200.
      *              *-------------------------------------------------*
      *              * Start su [old-lst]                              *
      *              *-------------------------------------------------*
           move      low-values           to   lst-k01                .
           start     lst    key not less
                            lst-k01
                            invalid key
                            go to exe-cnv-lst-800.
       exe-cnv-lst-250.
      *              *-------------------------------------------------*
      *              * Next su [old-lst]                               *
      *              *-------------------------------------------------*
           read      lst    next
                            with no lock
                            at end
                            go to exe-cnv-lst-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-lst-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-lst]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       exe-cnv-lst-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-lst]                          *
      *              *-------------------------------------------------*
       exe-cnv-lst-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-lst                 .
           move      lst-tip-rec          to   rf-lst-tip-rec         .
           move      lst-cod-lst          to   rf-lst-cod-lst         .
           move      lst-cod-cli          to   rf-lst-cod-cli         .
           move      lst-sgl-vlt          to   rf-lst-sgl-vlt         .
           move      lst-dec-vlt          to   rf-lst-dec-vlt         .
           move      lst-num-pro          to   rf-lst-num-pro         .
           move      lst-prz-lst          to   rf-lst-prz-lst         .
           move      lst-per-pvg (1)      to   rf-lst-per-pvg (1)     .
           move      lst-per-pvg (2)      to   rf-lst-per-pvg (2)     .
           move      lst-per-pvg (3)      to   rf-lst-per-pvg (3)     .
           move      lst-per-sco (1)      to   rf-lst-per-sco (1)     .
           move      lst-per-sco (2)      to   rf-lst-per-sco (2)     .
           move      lst-per-sco (3)      to   rf-lst-per-sco (3)     .
           move      lst-per-sco (4)      to   rf-lst-per-sco (4)     .
           move      lst-per-sco (5)      to   rf-lst-per-sco (5)     .
           move      lst-snx-prz          to   rf-lst-snx-prz         .
           move      lst-snx-pvg          to   rf-lst-snx-pvg         .
           move      lst-snx-sco          to   rf-lst-snx-sco         .
           move      lst-dva-ini          to   rf-lst-dva-ini         .
           move      lst-dva-fin          to   rf-lst-dva-fin         .
           move      lst-alx-exp          to   rf-lst-alx-exp         .
       exe-cnv-lst-500.
      *                  *---------------------------------------------*
      *                  * Integrazioni                                *
      *                  *---------------------------------------------*
           move      zero                 to   rf-lst-cat-pvg         .
           move      zero                 to   rf-lst-cat-sco         .
           move      zero                 to   rf-lst-qta-rif         .
       exe-cnv-lst-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-lst]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-lst-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-lst-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-lst-250.
       exe-cnv-lst-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-lst]                        *
      *                  *---------------------------------------------*
           close     lst                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-lst]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       exe-cnv-lst-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-lst-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-lst] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-lst-999.
           exit.

      *    *===========================================================*
      *    * Conversione [lsd]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-lsd-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "lsd "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-lsd-999.
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
       exe-cnv-lsd-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-lsd-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-lsd-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-lsd]                         *
      *                  *---------------------------------------------*
           open      i-o    lsd                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-lsd]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       exe-cnv-lsd-200.
      *              *-------------------------------------------------*
      *              * Start su [old-lsd]                              *
      *              *-------------------------------------------------*
           move      low-values           to   lsd-k01                .
           start     lsd    key not less
                            lsd-k01
                            invalid key
                            go to exe-cnv-lsd-800.
       exe-cnv-lsd-250.
      *              *-------------------------------------------------*
      *              * Next su [old-lsd]                               *
      *              *-------------------------------------------------*
           read      lsd    next
                            with no lock
                            at end
                            go to exe-cnv-lsd-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-lsd-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-lsd]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       exe-cnv-lsd-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-lsd]                          *
      *              *-------------------------------------------------*
       exe-cnv-lsd-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-lsd                 .
           move      lsd-tip-rec          to   rf-lsd-tip-rec         .
           move      lsd-cod-lst          to   rf-lsd-cod-lst         .
           move      lsd-cod-cli          to   rf-lsd-cod-cli         .
           move      lsd-sgl-vlt          to   rf-lsd-sgl-vlt         .
           move      lsd-dec-vlt          to   rf-lsd-dec-vlt         .
           move      lsd-num-pro          to   rf-lsd-num-pro         .
           move      lsd-prz-lst          to   rf-lsd-prz-lst         .
           move      lsd-per-pvg (1)      to   rf-lsd-per-pvg (1)     .
           move      lsd-per-pvg (2)      to   rf-lsd-per-pvg (2)     .
           move      lsd-per-pvg (3)      to   rf-lsd-per-pvg (3)     .
           move      lsd-per-sco (1)      to   rf-lsd-per-sco (1)     .
           move      lsd-per-sco (2)      to   rf-lsd-per-sco (2)     .
           move      lsd-per-sco (3)      to   rf-lsd-per-sco (3)     .
           move      lsd-per-sco (4)      to   rf-lsd-per-sco (4)     .
           move      lsd-per-sco (5)      to   rf-lsd-per-sco (5)     .
           move      lsd-snx-prz          to   rf-lsd-snx-prz         .
           move      lsd-snx-pvg          to   rf-lsd-snx-pvg         .
           move      lsd-snx-sco          to   rf-lsd-snx-sco         .
           move      lsd-dva-ini          to   rf-lsd-dva-ini         .
           move      lsd-dva-fin          to   rf-lsd-dva-fin         .
           move      lsd-dvf-rev          to   rf-lsd-dvf-rev         .
           move      lsd-alx-exp          to   rf-lsd-alx-exp         .
       exe-cnv-lsd-500.
      *                  *---------------------------------------------*
      *                  * Integrazioni                                *
      *                  *---------------------------------------------*
           move      zero                 to   rf-lsd-cat-pvg         .
           move      zero                 to   rf-lsd-cat-sco         .
           move      zero                 to   rf-lsd-qta-rif         .
       exe-cnv-lsd-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-lsd]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-lsd-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-lsd-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-lsd-250.
       exe-cnv-lsd-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-lsd]                        *
      *                  *---------------------------------------------*
           close     lsd                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-lsd]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       exe-cnv-lsd-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-lsd-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-lsd] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-lsd-999.
           exit.

      *    *===========================================================*
      *    * Conversione [zvf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-zvf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "zvf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-zvf-999.
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
       exe-cnv-zvf-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-zvf-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-zvf-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-zvf]                         *
      *                  *---------------------------------------------*
           open      i-o    zvf                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-zvf]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
       exe-cnv-zvf-200.
      *              *-------------------------------------------------*
      *              * Start su [old-zvf]                              *
      *              *-------------------------------------------------*
           move      low-values           to   zvf-k01                .
           start     zvf    key not less
                            zvf-k01
                            invalid key
                            go to exe-cnv-zvf-800.
       exe-cnv-zvf-250.
      *              *-------------------------------------------------*
      *              * Next su [old-zvf]                               *
      *              *-------------------------------------------------*
           read      zvf    next
                            with no lock
                            at end
                            go to exe-cnv-zvf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-zvf-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-zvf]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
       exe-cnv-zvf-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-zvf]                          *
      *              *-------------------------------------------------*
       exe-cnv-zvf-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-zvf                 .
           move      zvf-num-def          to   rf-zvf-num-def         .
           move      zvf-cod-def          to   rf-zvf-cod-def         .
           move      zvf-cod-lng          to   rf-zvf-cod-lng         .
           move      zvf-des-stp          to   rf-zvf-des-stp         .
           move      zvf-alx-exp          to   rf-zvf-alx-exp         .
       exe-cnv-zvf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-zvf]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-zvf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-zvf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-zvf-250.
       exe-cnv-zvf-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-zvf]                        *
      *                  *---------------------------------------------*
           close     zvf                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-zvf]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
       exe-cnv-zvf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-zvf-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-zvf] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-zvf-999.
           exit.

      *    *===========================================================*
      *    * Conversione [yvf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-yvf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "yvf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/fls/ioc/obj/iofyvf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-yvf-999.
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
       exe-cnv-yvf-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-yvf-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-yvf-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-yvf]                         *
      *                  *---------------------------------------------*
           open      i-o    yvf                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-yvf]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvf                 .
       exe-cnv-yvf-200.
      *              *-------------------------------------------------*
      *              * Start su [old-yvf]                              *
      *              *-------------------------------------------------*
           move      low-values           to   yvf-k01                .
           start     yvf    key not less
                            yvf-k01
                            invalid key
                            go to exe-cnv-yvf-800.
       exe-cnv-yvf-250.
      *              *-------------------------------------------------*
      *              * Next su [old-yvf]                               *
      *              *-------------------------------------------------*
           read      yvf    next
                            with no lock
                            at end
                            go to exe-cnv-yvf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-yvf-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-yvf]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvf                 .
       exe-cnv-yvf-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-yvf]                          *
      *              *-------------------------------------------------*
       exe-cnv-yvf-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-yvf                 .
           move      yvf-num-def          to   rf-yvf-num-def         .
           move      yvf-cod-def          to   rf-yvf-cod-def         .
           move      yvf-cod-lng          to   rf-yvf-cod-lng         .
           move      yvf-des-stp          to   rf-yvf-des-stp         .
           move      yvf-alx-exp          to   rf-yvf-alx-exp         .
       exe-cnv-yvf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-yvf]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-yvf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-yvf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-yvf-250.
       exe-cnv-yvf-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-yvf]                        *
      *                  *---------------------------------------------*
           close     yvf                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-yvf]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yvf                 .
       exe-cnv-yvf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-yvf-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-yvf] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-yvf-999.
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
           perform   wrt-rum-msg-000      thru wrt-rum-msg-999        .
       fil-del-fin-999.
           exit.

      *    *===========================================================*
      *    * Scrittura rullino messaggi                                *
      *    *-----------------------------------------------------------*
       wrt-rum-msg-000.
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
       wrt-rum-msg-999.
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
      *                  * Attesa di 2  secondi                        *
      *                  *---------------------------------------------*
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
       inc-rec-let-600.
      *              *-------------------------------------------------*
      *              * Determinazione %                                *
      *              *-------------------------------------------------*
           multiply  100                  by   f-xxx-nrl
                                        giving w-det-rec-fil-s15      .
           divide    w-det-rec-fil-rec    into w-det-rec-fil-s15
                                        giving w-det-rec-fil-v02
                                               rounded                .
           if        w-det-rec-fil-v02    >    100
                     move  100            to   w-det-rec-fil-v02      .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "% :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione %                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      16                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-det-rec-fil-v02    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
      *    * Determinazione numero records                             *
      *    *-----------------------------------------------------------*
       det-rec-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-rec-fil-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record determinati              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rec-fil-rec      .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Comando di determinazione                       *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "vutil -info"        to   w-all-str-cat (1)      .
           move      f-xxx-pat            to   w-all-str-cat (2)      .
           move      "| cat > /abd/asc/tmp/fil_size.txt"
                                          to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   o-shs                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       det-rec-fil-300.
      *              *-------------------------------------------------*
      *              * Open file sequenziale                           *
      *              *-------------------------------------------------*
           perform   opn-fil-seq-000      thru opn-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Se errore : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to det-rec-fil-400.
      *              *-------------------------------------------------*
      *              * Close file sequenziale                          *
      *              *-------------------------------------------------*
           perform   cls-fil-seq-000      thru cls-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Flag di uscita                                  *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-rec-fil-flg      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     det-rec-fil-900.
       det-rec-fil-400.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 1                  *
      *              *-------------------------------------------------*
           perform   rea-fil-seq-000      thru rea-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 2                  *
      *              *-------------------------------------------------*
           perform   rea-fil-seq-000      thru rea-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 3                  *
      *              *-------------------------------------------------*
           perform   rea-fil-seq-000      thru rea-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
       det-rec-fil-500.
      *              *-------------------------------------------------*
      *              * Comodo per lettura file                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di comodo                          *
      *                  *---------------------------------------------*
           move      g-rec                to   w-det-rec-fil-alf      .
      *                  *---------------------------------------------*
      *                  * Test su contenuto                           *
      *                  *---------------------------------------------*
           if        w-det-rec-fil-tst    not  = "# of records:"
                     go to det-rec-fil-800.
      *                  *---------------------------------------------*
      *                  * Conversione numero letto                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      w-det-rec-fil-dat    to   v-alf                  .
           move      13                   to   v-car                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-rec-fil-rec      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-rec-fil-flg      .
      *                  *---------------------------------------------*
      *                  * A close                                     *
      *                  *---------------------------------------------*
           go to     det-rec-fil-800.
       det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Close file sequenziale                          *
      *              *-------------------------------------------------*
           perform   cls-fil-seq-000      thru cls-fil-seq-999        .
       det-rec-fil-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-rec-fil-999.
       det-rec-fil-999.
           exit.

      *    *===========================================================*
      *    * Open file [seq]                                           *
      *    *-----------------------------------------------------------*
       opn-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Apertura del file in input                      *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "siz "               to   g-nam                  .
           move      "/abd/asc/tmp/fil_size.txt"
                                          to   g-pat                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
       opn-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Close file [seq]                                          *
      *    *-----------------------------------------------------------*
       cls-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvinp"                         .
       cls-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Read file [seq]                                           *
      *    *-----------------------------------------------------------*
       rea-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Read effettiva                                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
       rea-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

