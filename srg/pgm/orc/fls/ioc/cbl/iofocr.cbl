       Identification Division.
       Program-Id.                                 iofocr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ocr                         *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [fil]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fil   assign to disk           f-fil-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fil-k01
                   alternate record key   is fil-k02
                   alternate record key   is fil-k03
                   alternate record key   is fil-k04
                   alternate record key   is fil-k05
                   alternate record key   is fil-k06
                   alternate record key   is fil-k07
                   alternate record key   is fil-k08
                             file status  is                f-fil-sts .

      *    *===========================================================*
      *    * File Control [pul]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pul   assign to disk           f-pul-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pul-k01
                   alternate record key   is pul-k02
                   alternate record key   is pul-k03
                   alternate record key   is pul-k04
                   alternate record key   is pul-k05
                   alternate record key   is pul-k06
                   alternate record key   is pul-k07
                   alternate record key   is pul-k08
                             file status  is                f-pul-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [fil]                                    *
      *    *-----------------------------------------------------------*
       fd  fil       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fil-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fil-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-prt        pic  9(11)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-pro        pic  9(07)       comp-3     .
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-doc        pic  9(11)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dpz-3      pic  9(02)                  .
                   15  fil-flg-rch        pic  x(01)                  .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-flg-rch-4      pic  x(01)                  .
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : ARCNDO                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-tip-arc-5      pic  x(01)                  .
                   15  fil-cod-arc-5      pic  9(07)       comp-3     .
                   15  fil-num-doc-5      pic  9(11)       comp-3     .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
                   15  fil-num-prg-5      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : ARCDCR                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-dpz-6      pic  9(02)                  .
                   15  fil-tip-arc-6      pic  x(01)                  .
                   15  fil-cod-arc-6      pic  9(07)       comp-3     .
                   15  fil-dcn-ric-6      pic  9(07)       comp-3     .
                   15  fil-num-prt-6      pic  9(11)       comp-3     .
                   15  fil-num-prg-6      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : ARCDEP                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cod-dpz-7      pic  9(02)                  .
                   15  fil-tip-arc-7      pic  x(01)                  .
                   15  fil-cod-arc-7      pic  9(07)       comp-3     .
                   15  fil-des-rig-7      pic  x(40)                  .
                   15  fil-dcn-ric-7      pic  9(07)       comp-3     .
                   15  fil-num-prt-7      pic  9(11)       comp-3     .
                   15  fil-num-prg-7      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : ARCPRO                         *
      *            *---------------------------------------------------*
               10  fil-k08.
                   15  fil-tip-arc-8      pic  x(01)                  .
                   15  fil-cod-arc-8      pic  9(07)       comp-3     .
                   15  fil-tip-mag-8      pic  9(02)                  .
                   15  fil-num-pro-8      pic  9(07)       comp-3     .
                   15  fil-dat-doc-8      pic  9(07)       comp-3     .
                   15  fil-cod-dpz-8      pic  9(02)                  .
                   15  fil-num-prt-8      pic  9(11)       comp-3     .
                   15  fil-num-prg-8      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tmo-orc            pic  x(05)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-ocl-dat            pic  9(07)       comp-3     .
               10  fil-ocl-num            pic  x(10)                  .
               10  fil-pri-eva            pic  x(02)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-bld-flb            pic  9(01)                  .
               10  fil-bld-tpb            pic  9(01)                  .
               10  fil-bld-rgb            pic  9(01)                  .
               10  fil-tip-rig            pic  x(05)                  .
               10  fil-alf-pro            pic  x(14)                  .
               10  fil-sgl-vrn            pic  x(14)                  .
               10  fil-cop-scl            pic  x(14)                  .
               10  fil-des-ext            pic  9(01)                  .
               10  fil-des-rig            pic  x(40)                  .
               10  fil-tip-pro            pic  9(02)                  .
               10  fil-cod-iva            pic  9(05)       comp-3     .
               10  fil-ctp-ven            pic  9(07)       comp-3     .
               10  fil-umi-ven            pic  x(03)                  .
               10  fil-dec-qta            pic  9(01)                  .
               10  fil-qta-ord            pic s9(10)v9(03) comp-3     .
               10  fil-sdr-ccs            pic  x(01)                  .
               10  fil-cod-dsl            pic  x(07)                  .
               10  fil-snx-2qt            pic  9(01)                  .
               10  fil-dec-2qt            pic  9(01)                  .
               10  fil-qta-a02            pic s9(10)v9(03) comp-3     .
               10  fil-snx-3qt            pic  9(01)                  .
               10  fil-dec-3qt            pic  9(01)                  .
               10  fil-qta-a03            pic s9(10)v9(03) comp-3     .
               10  fil-dec-prz            pic  9(01)                  .
               10  fil-vps.
                   15  fil-sgl-vps        pic  x(03)                  .
                   15  fil-dec-vps        pic  9(01)                  .
                   15  fil-tdc-vps        pic  x(01)                  .
                   15  fil-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  fil-prz-lrs            pic  9(09)       comp-3     .
               10  fil-prz-nts            pic  9(09)       comp-3     .
               10  fil-vpp.
                   15  fil-sgl-vpp        pic  x(03)                  .
                   15  fil-dec-vpp        pic  9(01)                  .
                   15  fil-tdc-vpp        pic  x(01)                  .
                   15  fil-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  fil-prz-ven            pic  9(09)       comp-3     .
               10  fil-snx-2pz            pic  9(01)                  .
               10  fil-dec-2pz            pic  9(01)                  .
               10  fil-prz-a02            pic  9(09)       comp-3     .
               10  fil-vpl.
                   15  fil-sgl-vpl        pic  x(03)                  .
                   15  fil-dec-vpl        pic  9(01)                  .
                   15  fil-tdc-vpl        pic  x(01)                  .
                   15  fil-prz-vpl        pic  9(09)       comp-3     .
                   15  fil-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  fil-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  fil-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  fil-tlm-vpl        pic  x(01)                  .
                   15  fil-map-vpl        pic  x(01)                  .
               10  fil-epz-rgf            pic  9(01)                  .
               10  fil-csr-aap            pic  9(05)       comp-3     .
               10  fil-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-prz-net            pic  9(09)       comp-3     .
               10  fil-epz-pes            pic  9(02)                  .
               10  fil-vpc.
                   15  fil-sgl-vpc        pic  x(03)                  .
                   15  fil-dec-vpc        pic  9(01)                  .
                   15  fil-tdc-vpc        pic  x(01)                  .
                   15  fil-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  fil-dec-cos            pic  9(01)                  .
               10  fil-cos-rif            pic  9(09)       comp-3     .
               10  fil-imp-rig            pic s9(11)       comp-3     .
               10  fil-iau-rig            pic s9(11)       comp-3     .
               10  fil-cpv-aap            pic  9(05)       comp-3     .
               10  fil-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-fsp-rig            pic  9(02)                  .
               10  fil-cpv-rig            pic  9(05)       comp-3     .
               10  fil-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-pvf-rig            pic s9(11)       comp-3     .
               10  fil-dcn-ric            pic  9(07)       comp-3     .
               10  fil-dcn-prv            pic  9(07)       comp-3     .
               10  fil-dcn-cnf            pic  9(07)       comp-3     .
               10  fil-flg-cnf            pic  x(01)                  .
               10  fil-cmc-tip            pic  x(05)                  .
               10  fil-cmc-dat            pic  9(07)       comp-3     .
               10  fil-cmc-num            pic  9(11)       comp-3     .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-flg-puq            pic  x(01)                  .
               10  fil-tip-ord            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [pul]                                    *
      *    *-----------------------------------------------------------*
       fd  pul       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  pul-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  pul-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-prt        pic  9(11)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-pro        pic  9(07)       comp-3     .
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-doc        pic  9(11)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dpz-3      pic  9(02)                  .
                   15  pul-flg-rch        pic  x(01)                  .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-pro-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-flg-rch-4      pic  x(01)                  .
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : ARCNDO                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-tip-arc-5      pic  x(01)                  .
                   15  pul-cod-arc-5      pic  9(07)       comp-3     .
                   15  pul-num-doc-5      pic  9(11)       comp-3     .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
                   15  pul-num-prg-5      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : ARCDCR                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-dpz-6      pic  9(02)                  .
                   15  pul-tip-arc-6      pic  x(01)                  .
                   15  pul-cod-arc-6      pic  9(07)       comp-3     .
                   15  pul-dcn-ric-6      pic  9(07)       comp-3     .
                   15  pul-num-prt-6      pic  9(11)       comp-3     .
                   15  pul-num-prg-6      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : ARCDEP                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-cod-dpz-7      pic  9(02)                  .
                   15  pul-tip-arc-7      pic  x(01)                  .
                   15  pul-cod-arc-7      pic  9(07)       comp-3     .
                   15  pul-des-rig-7      pic  x(40)                  .
                   15  pul-dcn-ric-7      pic  9(07)       comp-3     .
                   15  pul-num-prt-7      pic  9(11)       comp-3     .
                   15  pul-num-prg-7      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : ARCPRO                         *
      *            *---------------------------------------------------*
               10  pul-k08.
                   15  pul-tip-arc-8      pic  x(01)                  .
                   15  pul-cod-arc-8      pic  9(07)       comp-3     .
                   15  pul-tip-mag-8      pic  9(02)                  .
                   15  pul-num-pro-8      pic  9(07)       comp-3     .
                   15  pul-dat-doc-8      pic  9(07)       comp-3     .
                   15  pul-cod-dpz-8      pic  9(02)                  .
                   15  pul-num-prt-8      pic  9(11)       comp-3     .
                   15  pul-num-prg-8      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tmo-orc            pic  x(05)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-ocl-dat            pic  9(07)       comp-3     .
               10  pul-ocl-num            pic  x(10)                  .
               10  pul-pri-eva            pic  x(02)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-bld-flb            pic  9(01)                  .
               10  pul-bld-tpb            pic  9(01)                  .
               10  pul-bld-rgb            pic  9(01)                  .
               10  pul-tip-rig            pic  x(05)                  .
               10  pul-alf-pro            pic  x(14)                  .
               10  pul-sgl-vrn            pic  x(14)                  .
               10  pul-cop-scl            pic  x(14)                  .
               10  pul-des-ext            pic  9(01)                  .
               10  pul-des-rig            pic  x(40)                  .
               10  pul-tip-pro            pic  9(02)                  .
               10  pul-cod-iva            pic  9(05)       comp-3     .
               10  pul-ctp-ven            pic  9(07)       comp-3     .
               10  pul-umi-ven            pic  x(03)                  .
               10  pul-dec-qta            pic  9(01)                  .
               10  pul-qta-ord            pic s9(10)v9(03) comp-3     .
               10  pul-sdr-ccs            pic  x(01)                  .
               10  pul-cod-dsl            pic  x(07)                  .
               10  pul-snx-2qt            pic  9(01)                  .
               10  pul-dec-2qt            pic  9(01)                  .
               10  pul-qta-a02            pic s9(10)v9(03) comp-3     .
               10  pul-snx-3qt            pic  9(01)                  .
               10  pul-dec-3qt            pic  9(01)                  .
               10  pul-qta-a03            pic s9(10)v9(03) comp-3     .
               10  pul-dec-prz            pic  9(01)                  .
               10  pul-vps.
                   15  pul-sgl-vps        pic  x(03)                  .
                   15  pul-dec-vps        pic  9(01)                  .
                   15  pul-tdc-vps        pic  x(01)                  .
                   15  pul-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  pul-prz-lrs            pic  9(09)       comp-3     .
               10  pul-prz-nts            pic  9(09)       comp-3     .
               10  pul-vpp.
                   15  pul-sgl-vpp        pic  x(03)                  .
                   15  pul-dec-vpp        pic  9(01)                  .
                   15  pul-tdc-vpp        pic  x(01)                  .
                   15  pul-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  pul-prz-ven            pic  9(09)       comp-3     .
               10  pul-snx-2pz            pic  9(01)                  .
               10  pul-dec-2pz            pic  9(01)                  .
               10  pul-prz-a02            pic  9(09)       comp-3     .
               10  pul-vpl.
                   15  pul-sgl-vpl        pic  x(03)                  .
                   15  pul-dec-vpl        pic  9(01)                  .
                   15  pul-tdc-vpl        pic  x(01)                  .
                   15  pul-prz-vpl        pic  9(09)       comp-3     .
                   15  pul-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  pul-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  pul-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  pul-tlm-vpl        pic  x(01)                  .
                   15  pul-map-vpl        pic  x(01)                  .
               10  pul-epz-rgf            pic  9(01)                  .
               10  pul-csr-aap            pic  9(05)       comp-3     .
               10  pul-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-prz-net            pic  9(09)       comp-3     .
               10  pul-epz-pes            pic  9(02)                  .
               10  pul-vpc.
                   15  pul-sgl-vpc        pic  x(03)                  .
                   15  pul-dec-vpc        pic  9(01)                  .
                   15  pul-tdc-vpc        pic  x(01)                  .
                   15  pul-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  pul-dec-cos            pic  9(01)                  .
               10  pul-cos-rif            pic  9(09)       comp-3     .
               10  pul-imp-rig            pic s9(11)       comp-3     .
               10  pul-iau-rig            pic s9(11)       comp-3     .
               10  pul-cpv-aap            pic  9(05)       comp-3     .
               10  pul-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-fsp-rig            pic  9(02)                  .
               10  pul-cpv-rig            pic  9(05)       comp-3     .
               10  pul-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-pvf-rig            pic s9(11)       comp-3     .
               10  pul-dcn-ric            pic  9(07)       comp-3     .
               10  pul-dcn-prv            pic  9(07)       comp-3     .
               10  pul-dcn-cnf            pic  9(07)       comp-3     .
               10  pul-flg-cnf            pic  x(01)                  .
               10  pul-cmc-tip            pic  x(05)                  .
               10  pul-cmc-dat            pic  9(07)       comp-3     .
               10  pul-cmc-num            pic  9(11)       comp-3     .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-flg-puq            pic  x(01)                  .
               10  pul-tip-ord            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sigla del file                                        *
      *        *-------------------------------------------------------*
           02  i-ide-sdf                  pic  x(04) value
                     "ocr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/orc/fls/ioc/obj/iofocr              "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work-area fissa per tutti i moduli di gestione i-o        *
      *    *-----------------------------------------------------------*
           copy      "swd/ske/iof/iofcp30"                            .

      *    *===========================================================*
      *    * Area per elenco chiavi previste                           *
      *    *-----------------------------------------------------------*
       01  k.
      *        *-------------------------------------------------------*
      *        * Numero chiavi di accesso                              *
      *        *-------------------------------------------------------*
           05  k-ctr                      pic  9(02) value 8          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMPRT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "MAGDAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RCHMAG    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RCHARC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCNDO    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCDCR    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCDEP    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 8                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCPRO    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    8      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [ocr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .

      ******************************************************************
       Procedure Division                using f rf-ocr               .
      ******************************************************************

      *    *===========================================================*
      *    * Procedure division fissa per tutti i moduli di i-o        *
      *    *-----------------------------------------------------------*
           copy      "swd/ske/iof/iofcp50"                            .

      *    *===========================================================*
      *    * Start su chiave                                           *
      *    *-----------------------------------------------------------*
       str-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status                          *
      *              *-------------------------------------------------*
           move      "00"                 to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Selezione indice sequenza di accesso            *
      *              *-------------------------------------------------*
           if        f-cfr                =    "NG"
                     move   3             to   z-tco
           else if   f-cfr                =    "GT"
                     move   2             to   z-tco
           else      move   1             to   z-tco                  .
      *              *-------------------------------------------------*
      *              * Composizione chiave fisica                      *
      *              *-------------------------------------------------*
           perform   cmp-key-fis-000      thru cmp-key-fis-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'indice z-key        *
      *              *-------------------------------------------------*
           go to     str-100
                     str-200
                     str-300
                     str-400
                     str-500
                     str-600
                     str-700
                     str-800
                     depending            on   z-key                  .
       str-100.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 1                       *
      *              *-------------------------------------------------*
           go to     str-110
                     str-120
                     str-130
                     depending            on   z-tco                  .
       str-110.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 1           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k01
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-120.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 1           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k01
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-130.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 1           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k01
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-200.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 2                       *
      *              *-------------------------------------------------*
           go to     str-210
                     str-220
                     str-230
                     depending            on   z-tco                  .
       str-210.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 2           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k02
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-220.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 2           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k02
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-230.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 2           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k02
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-300.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 3                       *
      *              *-------------------------------------------------*
           go to     str-310
                     str-320
                     str-330
                     depending            on   z-tco                  .
       str-310.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 3           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k03
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-320.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 3           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k03
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-330.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 3           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k03
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-400.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 4                       *
      *              *-------------------------------------------------*
           go to     str-410
                     str-420
                     str-430
                     depending            on   z-tco                  .
       str-410.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-420.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-430.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 4           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k04
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-500.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 5                       *
      *              *-------------------------------------------------*
           go to     str-510
                     str-520
                     str-530
                     depending            on   z-tco                  .
       str-510.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-520.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-530.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 5           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k05
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-600.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 6                       *
      *              *-------------------------------------------------*
           go to     str-610
                     str-620
                     str-630
                     depending            on   z-tco                  .
       str-610.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-620.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-630.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 6           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k06
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-700.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 7                       *
      *              *-------------------------------------------------*
           go to     str-710
                     str-720
                     str-730
                     depending            on   z-tco                  .
       str-710.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 7           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k07
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-720.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 7           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k07
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-730.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 7           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k07
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-800.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 8                       *
      *              *-------------------------------------------------*
           go to     str-810
                     str-820
                     str-830
                     depending            on   z-tco                  .
       str-810.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-820.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-830.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-980.
      *              *-------------------------------------------------*
      *              * Non invalid key                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
      *                  *---------------------------------------------*
      *                  * Ogni i-o error e' considerato fatal error   *
      *                  *---------------------------------------------*
           if        e-sts                =    "00"
                     go to str-999
           else      perform fte-000      thru fte-999                .
       str-990.
      *              *-------------------------------------------------*
      *              * Invalid key                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-end-fil            to   f-sts                  .
       str-999.
           exit.

      *    *===========================================================*
      *    * Read record generica su chiave                            *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave fisica                      *
      *              *-------------------------------------------------*
           perform   cmp-key-fis-000      thru cmp-key-fis-999        .
       rea-010.
      *              *-------------------------------------------------*
      *              * Normalizzazione status                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'indice z-key        *
      *              *-------------------------------------------------*
           go to     rea-100
                     rea-200
                     rea-300
                     rea-400
                     rea-500
                     rea-600
                     rea-700
                     rea-800
                     depending            on   z-key                  .
       rea-100.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 1                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-110.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k01
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-110.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k01
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-200.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 2                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-210.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k02
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-210.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k02
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-300.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 3                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-310.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k03
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-310.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k03
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-400.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 4                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-410.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k04
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-410.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k04
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-500.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 5                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-510.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k05
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-510.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k05
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-600.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 6                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-610.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k06
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-610.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k06
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-700.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 7                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-710.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k07
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-710.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k07
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-800.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 8                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-810.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k08
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-810.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k08
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-980.
      *              *-------------------------------------------------*
      *              * Non invalid key                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
      *                  *---------------------------------------------*
      *                  * Se record locked si esegue una pausa di un  *
      *                  * secondo e poi si ritorna a rileggere        *
      *                  *---------------------------------------------*
           if        e-sts                =    e-use-err
                     perform wai-000      thru wai-999
                     go to   rea-010.
      *                  *---------------------------------------------*
      *                  * Ogni altro i-o error viene considerato un   *
      *                  * fatal error                                 *
      *                  *---------------------------------------------*
           if        e-sts                not  = "00"
                     perform fte-000      thru fte-999                .
      *                  *---------------------------------------------*
      *                  * Se richiesta la decomposizione da record    *
      *                  * fisico a record logico la si esegue         *
      *                  *---------------------------------------------*
           if        z-dec                =    1
                     perform dec-fis-log-000
                        thru dec-fis-log-999.
           go to     rea-999.
       rea-990.
      *              *-------------------------------------------------*
      *              * Invalid key                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status in uscita                            *
      *                  *---------------------------------------------*
           move      e-not-fnd            to   f-sts                  .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-ocr                 .
           move      zero                 to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      spaces               to   rf-ocr-tmo-orc         .
           move      zero                 to   rf-ocr-cod-dpz         .
           move      zero                 to   rf-ocr-dat-doc         .
           move      zero                 to   rf-ocr-num-doc         .
           move      spaces               to   rf-ocr-tip-arc         .
           move      zero                 to   rf-ocr-cod-arc         .
           move      spaces               to   rf-ocr-dpz-arc         .
           move      spaces               to   rf-ocr-cod-lng         .
           move      zero                 to   rf-ocr-ocl-dat         .
           move      zero                 to   rf-ocr-ocl-num         .
           move      spaces               to   rf-ocr-pri-eva         .
           move      spaces               to   rf-ocr-sgl-vpf         .
           move      zero                 to   rf-ocr-dec-vpf         .
           move      spaces               to   rf-ocr-tdc-vpf         .
           move      zero                 to   rf-ocr-cdc-vpf         .
           move      zero                 to   rf-ocr-bld-flb         .
           move      zero                 to   rf-ocr-bld-tpb         .
           move      zero                 to   rf-ocr-bld-rgb         .
           move      spaces               to   rf-ocr-tip-rig         .
           move      zero                 to   rf-ocr-tip-mag         .
           move      zero                 to   rf-ocr-num-pro         .
           move      spaces               to   rf-ocr-alf-pro         .
           move      spaces               to   rf-ocr-sgl-vrn         .
           move      spaces               to   rf-ocr-cop-scl         .
           move      zero                 to   rf-ocr-des-ext         .
           move      spaces               to   rf-ocr-des-rig         .
           move      zero                 to   rf-ocr-tip-pro         .
           move      zero                 to   rf-ocr-cod-iva         .
           move      zero                 to   rf-ocr-ctp-ven         .
           move      spaces               to   rf-ocr-umi-ven         .
           move      zero                 to   rf-ocr-dec-qta         .
           move      zero                 to   rf-ocr-qta-ord         .
           move      spaces               to   rf-ocr-sdr-ccs         .
           move      spaces               to   rf-ocr-cod-dsl         .
           move      zero                 to   rf-ocr-snx-2qt         .
           move      zero                 to   rf-ocr-dec-2qt         .
           move      zero                 to   rf-ocr-qta-a02         .
           move      zero                 to   rf-ocr-snx-3qt         .
           move      zero                 to   rf-ocr-dec-3qt         .
           move      zero                 to   rf-ocr-qta-a03         .
           move      zero                 to   rf-ocr-dec-prz         .
           move      spaces               to   rf-ocr-sgl-vps         .
           move      zero                 to   rf-ocr-dec-vps         .
           move      spaces               to   rf-ocr-tdc-vps         .
           move      zero                 to   rf-ocr-cdc-vps         .
           move      zero                 to   rf-ocr-prz-lrs         .
           move      zero                 to   rf-ocr-prz-nts         .
           move      spaces               to   rf-ocr-sgl-vpp         .
           move      zero                 to   rf-ocr-dec-vpp         .
           move      spaces               to   rf-ocr-tdc-vpp         .
           move      zero                 to   rf-ocr-cdc-vpp         .
           move      zero                 to   rf-ocr-prz-ven         .
           move      zero                 to   rf-ocr-snx-2pz         .
           move      zero                 to   rf-ocr-dec-2pz         .
           move      zero                 to   rf-ocr-prz-a02         .
           move      spaces               to   rf-ocr-sgl-vpl         .
           move      zero                 to   rf-ocr-dec-vpl         .
           move      spaces               to   rf-ocr-tdc-vpl         .
           move      zero                 to   rf-ocr-prz-vpl         .
           move      zero                 to   rf-ocr-cdc-vpl         .
           move      zero                 to   rf-ocr-ccr-vpl         .
           move      zero                 to   rf-ocr-plm-vpl         .
           move      spaces               to   rf-ocr-tlm-vpl         .
           move      spaces               to   rf-ocr-map-vpl         .
           move      zero                 to   rf-ocr-epz-rgf         .
           move      zero                 to   rf-ocr-csr-aap         .
           move      zero                 to   rf-ocr-psr-aap (1)     .
           move      zero                 to   rf-ocr-psr-aap (2)     .
           move      zero                 to   rf-ocr-psr-aap (3)     .
           move      zero                 to   rf-ocr-psr-aap (4)     .
           move      zero                 to   rf-ocr-psr-aap (5)     .
           move      zero                 to   rf-ocr-per-scr (1)     .
           move      zero                 to   rf-ocr-per-scr (2)     .
           move      zero                 to   rf-ocr-per-scr (3)     .
           move      zero                 to   rf-ocr-per-scr (4)     .
           move      zero                 to   rf-ocr-per-scr (5)     .
           move      zero                 to   rf-ocr-prz-net         .
           move      zero                 to   rf-ocr-epz-pes         .
           move      spaces               to   rf-ocr-sgl-vpc         .
           move      zero                 to   rf-ocr-dec-vpc         .
           move      spaces               to   rf-ocr-tdc-vpc         .
           move      zero                 to   rf-ocr-cdc-vpc         .
           move      zero                 to   rf-ocr-dec-cos         .
           move      zero                 to   rf-ocr-cos-rif         .
           move      zero                 to   rf-ocr-imp-rig         .
           move      zero                 to   rf-ocr-iau-rig         .
           move      zero                 to   rf-ocr-cpv-aap         .
           move      zero                 to   rf-ocr-ppv-aap (1)     .
           move      zero                 to   rf-ocr-ppv-aap (2)     .
           move      zero                 to   rf-ocr-ppv-aap (3)     .
           move      zero                 to   rf-ocr-fsp-rig         .
           move      zero                 to   rf-ocr-cpv-rig         .
           move      zero                 to   rf-ocr-ppv-rig (1)     .
           move      zero                 to   rf-ocr-ppv-rig (2)     .
           move      zero                 to   rf-ocr-ppv-rig (3)     .
           move      zero                 to   rf-ocr-pvf-rig         .
           move      zero                 to   rf-ocr-dcn-ric         .
           move      zero                 to   rf-ocr-dcn-prv         .
           move      zero                 to   rf-ocr-dcn-cnf         .
           move      spaces               to   rf-ocr-flg-cnf         .
           move      spaces               to   rf-ocr-cmc-tip         .
           move      zero                 to   rf-ocr-cmc-dat         .
           move      zero                 to   rf-ocr-cmc-num         .
           move      spaces               to   rf-ocr-flg-rch         .
           move      spaces               to   rf-ocr-flg-blx (1)     .
           move      spaces               to   rf-ocr-flg-blx (2)     .
           move      spaces               to   rf-ocr-flg-blx (3)     .
           move      spaces               to   rf-ocr-flg-blx (4)     .
           move      spaces               to   rf-ocr-flg-blx (5)     .
           move      spaces               to   rf-ocr-flg-blx (6)     .
           move      spaces               to   rf-ocr-flg-blx (7)     .
           move      spaces               to   rf-ocr-flg-nbx (1)     .
           move      spaces               to   rf-ocr-flg-nbx (2)     .
           move      spaces               to   rf-ocr-flg-nbx (3)     .
           move      spaces               to   rf-ocr-flg-pul         .
           move      spaces               to   rf-ocr-flg-puq         .
           move      spaces               to   rf-ocr-tip-ord         .
           move      spaces               to   rf-ocr-alx-exp         .
       nor-rec-log-999.
           exit.

      *    *===========================================================*
      *    * Composizione record da logico a fisico                    *
      *    *-----------------------------------------------------------*
       cmp-log-fis-000.
      *              *-------------------------------------------------*
      *              * Spaces in tutto il record fisico                *
      *              *-------------------------------------------------*
           move      spaces               to   fil-rec                .
      *              *-------------------------------------------------*
      *              * Composizione area chiavi                        *
      *              *-------------------------------------------------*
           move      zero                 to   z-key                  .
       cmp-log-fis-100.
           if        z-key                <    k-ctr
                     add     1            to   z-key
                     perform cmp-key-fis-000
                        thru cmp-key-fis-999
                     go to   cmp-log-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione area dati                          *
      *              *-------------------------------------------------*
           move      rf-ocr-tmo-orc       to   fil-tmo-orc            .
           move      rf-ocr-dpz-arc       to   fil-dpz-arc            .
           move      rf-ocr-cod-lng       to   fil-cod-lng            .
           move      rf-ocr-ocl-dat       to   fil-ocl-dat            .
           move      rf-ocr-ocl-num       to   fil-ocl-num            .
           move      rf-ocr-pri-eva       to   fil-pri-eva            .
           move      rf-ocr-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-ocr-dec-vpf       to   fil-dec-vpf            .
           move      rf-ocr-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-ocr-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-ocr-bld-flb       to   fil-bld-flb            .
           move      rf-ocr-bld-tpb       to   fil-bld-tpb            .
           move      rf-ocr-bld-rgb       to   fil-bld-rgb            .
           move      rf-ocr-tip-rig       to   fil-tip-rig            .
           move      rf-ocr-alf-pro       to   fil-alf-pro            .
           move      rf-ocr-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-ocr-cop-scl       to   fil-cop-scl            .
           move      rf-ocr-des-ext       to   fil-des-ext            .
           move      rf-ocr-des-rig       to   fil-des-rig            .
           move      rf-ocr-tip-pro       to   fil-tip-pro            .
           move      rf-ocr-cod-iva       to   fil-cod-iva            .
           move      rf-ocr-ctp-ven       to   fil-ctp-ven            .
           move      rf-ocr-umi-ven       to   fil-umi-ven            .
           move      rf-ocr-dec-qta       to   fil-dec-qta            .
           move      rf-ocr-qta-ord       to   fil-qta-ord            .
           move      rf-ocr-sdr-ccs       to   fil-sdr-ccs            .
           move      rf-ocr-snx-2qt       to   fil-snx-2qt            .
           move      rf-ocr-dec-2qt       to   fil-dec-2qt            .
           move      rf-ocr-qta-a02       to   fil-qta-a02            .
           move      rf-ocr-snx-3qt       to   fil-snx-3qt            .
           move      rf-ocr-dec-3qt       to   fil-dec-3qt            .
           move      rf-ocr-qta-a03       to   fil-qta-a03            .
           move      rf-ocr-dec-prz       to   fil-dec-prz            .
           move      rf-ocr-sgl-vps       to   fil-sgl-vps            .
           move      rf-ocr-dec-vps       to   fil-dec-vps            .
           move      rf-ocr-tdc-vps       to   fil-tdc-vps            .
           move      rf-ocr-cdc-vps       to   fil-cdc-vps            .
           move      rf-ocr-prz-lrs       to   fil-prz-lrs            .
           move      rf-ocr-prz-nts       to   fil-prz-nts            .
           move      rf-ocr-sgl-vpp       to   fil-sgl-vpp            .
           move      rf-ocr-dec-vpp       to   fil-dec-vpp            .
           move      rf-ocr-tdc-vpp       to   fil-tdc-vpp            .
           move      rf-ocr-cdc-vpp       to   fil-cdc-vpp            .
           move      rf-ocr-prz-ven       to   fil-prz-ven            .
           move      rf-ocr-snx-2pz       to   fil-snx-2pz            .
           move      rf-ocr-dec-2pz       to   fil-dec-2pz            .
           move      rf-ocr-prz-a02       to   fil-prz-a02            .
           move      rf-ocr-sgl-vpl       to   fil-sgl-vpl            .
           move      rf-ocr-dec-vpl       to   fil-dec-vpl            .
           move      rf-ocr-tdc-vpl       to   fil-tdc-vpl            .
           move      rf-ocr-prz-vpl       to   fil-prz-vpl            .
           move      rf-ocr-cdc-vpl       to   fil-cdc-vpl            .
           move      rf-ocr-ccr-vpl       to   fil-ccr-vpl            .
           move      rf-ocr-plm-vpl       to   fil-plm-vpl            .
           move      rf-ocr-tlm-vpl       to   fil-tlm-vpl            .
           move      rf-ocr-map-vpl       to   fil-map-vpl            .
           move      rf-ocr-epz-rgf       to   fil-epz-rgf            .
           move      rf-ocr-csr-aap       to   fil-csr-aap            .
           move      rf-ocr-psr-aap (1)   to   fil-psr-aap (1)        .
           move      rf-ocr-psr-aap (2)   to   fil-psr-aap (2)        .
           move      rf-ocr-psr-aap (3)   to   fil-psr-aap (3)        .
           move      rf-ocr-psr-aap (4)   to   fil-psr-aap (4)        .
           move      rf-ocr-psr-aap (5)   to   fil-psr-aap (5)        .
           move      rf-ocr-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-ocr-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-ocr-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-ocr-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-ocr-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-ocr-prz-net       to   fil-prz-net            .
           move      rf-ocr-epz-pes       to   fil-epz-pes            .
           move      rf-ocr-sgl-vpc       to   fil-sgl-vpc            .
           move      rf-ocr-dec-vpc       to   fil-dec-vpc            .
           move      rf-ocr-tdc-vpc       to   fil-tdc-vpc            .
           move      rf-ocr-cdc-vpc       to   fil-cdc-vpc            .
           move      rf-ocr-dec-cos       to   fil-dec-cos            .
           move      rf-ocr-cos-rif       to   fil-cos-rif            .
           move      rf-ocr-imp-rig       to   fil-imp-rig            .
           move      rf-ocr-iau-rig       to   fil-iau-rig            .
           move      rf-ocr-cpv-aap       to   fil-cpv-aap            .
           move      rf-ocr-ppv-aap (1)   to   fil-ppv-aap (1)        .
           move      rf-ocr-ppv-aap (2)   to   fil-ppv-aap (2)        .
           move      rf-ocr-ppv-aap (3)   to   fil-ppv-aap (3)        .
           move      rf-ocr-fsp-rig       to   fil-fsp-rig            .
           move      rf-ocr-cpv-rig       to   fil-cpv-rig            .
           move      rf-ocr-ppv-rig (1)   to   fil-ppv-rig (1)        .
           move      rf-ocr-ppv-rig (2)   to   fil-ppv-rig (2)        .
           move      rf-ocr-ppv-rig (3)   to   fil-ppv-rig (3)        .
           move      rf-ocr-pvf-rig       to   fil-pvf-rig            .
           move      rf-ocr-dcn-ric       to   fil-dcn-ric            .
           move      rf-ocr-dcn-prv       to   fil-dcn-prv            .
           move      rf-ocr-dcn-cnf       to   fil-dcn-cnf            .
           move      rf-ocr-flg-cnf       to   fil-flg-cnf            .
           move      rf-ocr-cmc-tip       to   fil-cmc-tip            .
           move      rf-ocr-cmc-dat       to   fil-cmc-dat            .
           move      rf-ocr-cmc-num       to   fil-cmc-num            .
           move      rf-ocr-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-ocr-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-ocr-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-ocr-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-ocr-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-ocr-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-ocr-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-ocr-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-ocr-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-ocr-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-ocr-flg-pul       to   fil-flg-pul            .
           move      rf-ocr-flg-puq       to   fil-flg-puq            .
           move      rf-ocr-tip-ord       to   fil-tip-ord            .
           move      rf-ocr-alx-exp       to   fil-alx-exp            .
       cmp-log-fis-999.
           exit.
           
      *    *===========================================================*
      *    * Composizione chiave da logica a fisica secondo z-key      *
      *    *-----------------------------------------------------------*
       cmp-key-fis-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'indice z-key        *
      *              *-------------------------------------------------*
           go to     cmp-key-fis-100
                     cmp-key-fis-200
                     cmp-key-fis-300
                     cmp-key-fis-400
                     cmp-key-fis-500
                     cmp-key-fis-600
                     cmp-key-fis-700
                     cmp-key-fis-800
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-ocr-num-prt       to   fil-num-prt            .
           move      rf-ocr-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz            .
           move      rf-ocr-tip-mag       to   fil-tip-mag            .
           move      rf-ocr-num-pro       to   fil-num-pro            .
           move      rf-ocr-dat-doc       to   fil-dat-doc            .
           move      rf-ocr-num-doc       to   fil-num-doc            .
           move      rf-ocr-num-prt       to   fil-num-prt-2          .
           move      rf-ocr-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz-3          .
           move      rf-ocr-flg-rch       to   fil-flg-rch            .
           move      rf-ocr-tip-mag       to   fil-tip-mag-3          .
           move      rf-ocr-num-pro       to   fil-num-pro-3          .
           move      rf-ocr-num-prt       to   fil-num-prt-3          .
           move      rf-ocr-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-ocr-flg-rch       to   fil-flg-rch-4          .
           move      rf-ocr-tip-arc       to   fil-tip-arc            .
           move      rf-ocr-cod-arc       to   fil-cod-arc            .
           move      rf-ocr-num-prt       to   fil-num-prt-4          .
           move      rf-ocr-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-ocr-tip-arc       to   fil-tip-arc-5          .
           move      rf-ocr-cod-arc       to   fil-cod-arc-5          .
           move      rf-ocr-num-doc       to   fil-num-doc-5          .
           move      rf-ocr-num-prt       to   fil-num-prt-5          .
           move      rf-ocr-num-prg       to   fil-num-prg-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz-6          .
           move      rf-ocr-tip-arc       to   fil-tip-arc-6          .
           move      rf-ocr-cod-arc       to   fil-cod-arc-6          .
           move      rf-ocr-dcn-ric       to   fil-dcn-ric-6          .
           move      rf-ocr-num-prt       to   fil-num-prt-6          .
           move      rf-ocr-num-prg       to   fil-num-prg-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz-7          .
           move      rf-ocr-tip-arc       to   fil-tip-arc-7          .
           move      rf-ocr-cod-arc       to   fil-cod-arc-7          .
           move      rf-ocr-des-rig       to   fil-des-rig-7          .
           move      rf-ocr-dcn-ric       to   fil-dcn-ric-7          .
           move      rf-ocr-num-prt       to   fil-num-prt-7          .
           move      rf-ocr-num-prg       to   fil-num-prg-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-800.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 8                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k08                .
           move      rf-ocr-tip-arc       to   fil-tip-arc-8          .
           move      rf-ocr-cod-arc       to   fil-cod-arc-8          .
           move      rf-ocr-tip-mag       to   fil-tip-mag-8          .
           move      rf-ocr-num-pro       to   fil-num-pro-8          .
           move      rf-ocr-dat-doc       to   fil-dat-doc-8          .
           move      rf-ocr-cod-dpz       to   fil-cod-dpz-8          .
           move      rf-ocr-num-prt       to   fil-num-prt-8          .
           move      rf-ocr-num-prg       to   fil-num-prg-8          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ocr                 .
           move      fil-num-prt          to   rf-ocr-num-prt         .
           move      fil-num-prg          to   rf-ocr-num-prg         .
           move      fil-tmo-orc          to   rf-ocr-tmo-orc         .
           move      fil-cod-dpz          to   rf-ocr-cod-dpz         .
           move      fil-dat-doc          to   rf-ocr-dat-doc         .
           move      fil-num-doc          to   rf-ocr-num-doc         .
           move      fil-tip-arc          to   rf-ocr-tip-arc         .
           move      fil-cod-arc          to   rf-ocr-cod-arc         .
           move      fil-dpz-arc          to   rf-ocr-dpz-arc         .
           move      fil-cod-lng          to   rf-ocr-cod-lng         .
           move      fil-ocl-dat          to   rf-ocr-ocl-dat         .
           move      fil-ocl-num          to   rf-ocr-ocl-num         .
           move      fil-pri-eva          to   rf-ocr-pri-eva         .
           move      fil-sgl-vpf          to   rf-ocr-sgl-vpf         .
           move      fil-dec-vpf          to   rf-ocr-dec-vpf         .
           move      fil-tdc-vpf          to   rf-ocr-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-ocr-cdc-vpf         .
           move      fil-bld-flb          to   rf-ocr-bld-flb         .
           move      fil-bld-tpb          to   rf-ocr-bld-tpb         .
           move      fil-bld-rgb          to   rf-ocr-bld-rgb         .
           move      fil-tip-rig          to   rf-ocr-tip-rig         .
           move      fil-tip-mag          to   rf-ocr-tip-mag         .
           move      fil-num-pro          to   rf-ocr-num-pro         .
           move      fil-alf-pro          to   rf-ocr-alf-pro         .
           move      fil-sgl-vrn          to   rf-ocr-sgl-vrn         .
           move      fil-cop-scl          to   rf-ocr-cop-scl         .
           move      fil-des-ext          to   rf-ocr-des-ext         .
           move      fil-des-rig          to   rf-ocr-des-rig         .
           move      fil-tip-pro          to   rf-ocr-tip-pro         .
           move      fil-cod-iva          to   rf-ocr-cod-iva         .
           move      fil-ctp-ven          to   rf-ocr-ctp-ven         .
           move      fil-umi-ven          to   rf-ocr-umi-ven         .
           move      fil-dec-qta          to   rf-ocr-dec-qta         .
           move      fil-qta-ord          to   rf-ocr-qta-ord         .
           move      fil-sdr-ccs          to   rf-ocr-sdr-ccs         .
           move      fil-cod-dsl          to   rf-ocr-cod-dsl         .
           move      fil-snx-2qt          to   rf-ocr-snx-2qt         .
           move      fil-dec-2qt          to   rf-ocr-dec-2qt         .
           move      fil-qta-a02          to   rf-ocr-qta-a02         .
           move      fil-snx-3qt          to   rf-ocr-snx-3qt         .
           move      fil-dec-3qt          to   rf-ocr-dec-3qt         .
           move      fil-qta-a03          to   rf-ocr-qta-a03         .
           move      fil-dec-prz          to   rf-ocr-dec-prz         .
           move      fil-sgl-vps          to   rf-ocr-sgl-vps         .
           move      fil-dec-vps          to   rf-ocr-dec-vps         .
           move      fil-tdc-vps          to   rf-ocr-tdc-vps         .
           move      fil-cdc-vps          to   rf-ocr-cdc-vps         .
           move      fil-prz-lrs          to   rf-ocr-prz-lrs         .
           move      fil-prz-nts          to   rf-ocr-prz-nts         .
           move      fil-sgl-vpp          to   rf-ocr-sgl-vpp         .
           move      fil-dec-vpp          to   rf-ocr-dec-vpp         .
           move      fil-tdc-vpp          to   rf-ocr-tdc-vpp         .
           move      fil-cdc-vpp          to   rf-ocr-cdc-vpp         .
           move      fil-prz-ven          to   rf-ocr-prz-ven         .
           move      fil-snx-2pz          to   rf-ocr-snx-2pz         .
           move      fil-dec-2pz          to   rf-ocr-dec-2pz         .
           move      fil-prz-a02          to   rf-ocr-prz-a02         .
           move      fil-sgl-vpl          to   rf-ocr-sgl-vpl         .
           move      fil-dec-vpl          to   rf-ocr-dec-vpl         .
           move      fil-tdc-vpl          to   rf-ocr-tdc-vpl         .
           move      fil-prz-vpl          to   rf-ocr-prz-vpl         .
           move      fil-cdc-vpl          to   rf-ocr-cdc-vpl         .
           move      fil-ccr-vpl          to   rf-ocr-ccr-vpl         .
           move      fil-plm-vpl          to   rf-ocr-plm-vpl         .
           move      fil-tlm-vpl          to   rf-ocr-tlm-vpl         .
           move      fil-map-vpl          to   rf-ocr-map-vpl         .
           move      fil-epz-rgf          to   rf-ocr-epz-rgf         .
           move      fil-csr-aap          to   rf-ocr-csr-aap         .
           move      fil-psr-aap (1)      to   rf-ocr-psr-aap (1)     .
           move      fil-psr-aap (2)      to   rf-ocr-psr-aap (2)     .
           move      fil-psr-aap (3)      to   rf-ocr-psr-aap (3)     .
           move      fil-psr-aap (4)      to   rf-ocr-psr-aap (4)     .
           move      fil-psr-aap (5)      to   rf-ocr-psr-aap (5)     .
           move      fil-per-scr (1)      to   rf-ocr-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-ocr-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-ocr-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-ocr-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-ocr-per-scr (5)     .
           move      fil-prz-net          to   rf-ocr-prz-net         .
           move      fil-epz-pes          to   rf-ocr-epz-pes         .
           move      fil-sgl-vpc          to   rf-ocr-sgl-vpc         .
           move      fil-dec-vpc          to   rf-ocr-dec-vpc         .
           move      fil-tdc-vpc          to   rf-ocr-tdc-vpc         .
           move      fil-cdc-vpc          to   rf-ocr-cdc-vpc         .
           move      fil-dec-cos          to   rf-ocr-dec-cos         .
           move      fil-cos-rif          to   rf-ocr-cos-rif         .
           move      fil-imp-rig          to   rf-ocr-imp-rig         .
           move      fil-iau-rig          to   rf-ocr-iau-rig         .
           move      fil-cpv-aap          to   rf-ocr-cpv-aap         .
           move      fil-ppv-aap (1)      to   rf-ocr-ppv-aap (1)     .
           move      fil-ppv-aap (2)      to   rf-ocr-ppv-aap (2)     .
           move      fil-ppv-aap (3)      to   rf-ocr-ppv-aap (3)     .
           move      fil-fsp-rig          to   rf-ocr-fsp-rig         .
           move      fil-cpv-rig          to   rf-ocr-cpv-rig         .
           move      fil-ppv-rig (1)      to   rf-ocr-ppv-rig (1)     .
           move      fil-ppv-rig (2)      to   rf-ocr-ppv-rig (2)     .
           move      fil-ppv-rig (3)      to   rf-ocr-ppv-rig (3)     .
           move      fil-pvf-rig          to   rf-ocr-pvf-rig         .
           move      fil-dcn-ric          to   rf-ocr-dcn-ric         .
           move      fil-dcn-prv          to   rf-ocr-dcn-prv         .
           move      fil-dcn-cnf          to   rf-ocr-dcn-cnf         .
           move      fil-flg-cnf          to   rf-ocr-flg-cnf         .
           move      fil-cmc-tip          to   rf-ocr-cmc-tip         .
           move      fil-cmc-dat          to   rf-ocr-cmc-dat         .
           move      fil-cmc-num          to   rf-ocr-cmc-num         .
           move      fil-flg-rch          to   rf-ocr-flg-rch         .
           move      fil-flg-blx (1)      to   rf-ocr-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-ocr-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-ocr-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-ocr-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-ocr-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-ocr-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-ocr-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-ocr-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-ocr-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-ocr-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-ocr-flg-pul         .
           move      fil-flg-puq          to   rf-ocr-flg-puq         .
           move      fil-tip-ord          to   rf-ocr-tip-ord         .
           move      fil-alx-exp          to   rf-ocr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ocr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ocr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

