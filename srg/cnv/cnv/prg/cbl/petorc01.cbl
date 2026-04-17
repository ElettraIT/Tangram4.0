       Identification Division.
       Program-Id.                                 petorc01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    petorc              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/05/20    *
      *                       Ultima revisione:    NdK del 11/05/20    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Recupero ordini cancellati erroneamente     *
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
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      *    *===========================================================*
      *    * File Control [wct]                                        *
      *    *-----------------------------------------------------------*
           select  optional  wct   assign to disk           f-wct-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is wct-k01
                   alternate record key   is wct-k02
                   alternate record key   is wct-k03
                   alternate record key   is wct-k04
                   alternate record key   is wct-k05
                   alternate record key   is wct-k06
                   alternate record key   is wct-k07
                             file status  is                f-wct-sts .

      *    *===========================================================*
      *    * File Control [wcr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  wcr   assign to disk           f-wcr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is wcr-k01
                   alternate record key   is wcr-k02
                   alternate record key   is wcr-k03
                   alternate record key   is wcr-k04
                   alternate record key   is wcr-k05
                   alternate record key   is wcr-k06
                   alternate record key   is wcr-k07
                   alternate record key   is wcr-k08
                             file status  is                f-wcr-sts .

      *    *===========================================================*
      *    * File Control [wcx]                                        *
      *    *-----------------------------------------------------------*
           select  optional  wcx   assign to disk           f-wcx-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is wcx-k01
                             file status  is                f-wcx-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Tipo record                                       *
      *            *---------------------------------------------------*
               10  srt-tip-rec            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero protocollo                                 *
      *            *---------------------------------------------------*
               10  srt-num-prt            pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Progressivo riga                                  *
      *            *---------------------------------------------------*
               10  srt-num-prg            pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo estensione                                   *
      *            *---------------------------------------------------*
               10  srt-tip-ext            pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Chiavi integrative testata                        *
      *            *---------------------------------------------------*
               10  srt-ide-dat            pic  9(07)       comp-3     .
               10  srt-dat-doc            pic  9(07)       comp-3     .
               10  srt-cod-dpz            pic  9(02)                  .
               10  srt-num-doc            pic  9(11)       comp-3     .
               10  srt-tmo-orc            pic  x(05)                  .
               10  srt-scl-ann            pic  9(03)       comp-3     .
               10  srt-sgl-num            pic  x(03)                  .
               10  srt-tip-arc            pic  x(01)                  .
               10  srt-cod-arc            pic  9(07)       comp-3     .
               10  srt-flg-och            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Chiavi integrative righe                          *
      *            *---------------------------------------------------*
               10  srt-tip-mag            pic  9(02)                  .
               10  srt-num-pro            pic  9(07)       comp-3     .
               10  srt-flg-rch            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Intero record                                     *
      *            *---------------------------------------------------*
               10  srt-dat-rec.
                   15  filler  occurs  2048
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [wct]                                    *
      *    *-----------------------------------------------------------*
       fd  wct       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  wct-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  wct-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  wct-k01.
                   15  wct-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  wct-k02.
                   15  wct-ide-dat        pic  9(07)       comp-3     .
                   15  wct-dat-doc        pic  9(07)       comp-3     .
                   15  wct-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  wct-k03.
                   15  wct-dat-doc-3      pic  9(07)       comp-3     .
                   15  wct-cod-dpz        pic  9(02)                  .
                   15  wct-num-doc        pic  9(11)       comp-3     .
                   15  wct-tmo-orc        pic  x(05)                  .
                   15  wct-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  wct-k04.
                   15  wct-scl-ann        pic  9(03)       comp-3     .
                   15  wct-cod-dpz-4      pic  9(02)                  .
                   15  wct-sgl-num        pic  x(03)                  .
                   15  wct-num-doc-4      pic  9(11)       comp-3     .
                   15  wct-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  wct-k05.
                   15  wct-cod-dpz-5      pic  9(02)                  .
                   15  wct-tip-arc        pic  x(01)                  .
                   15  wct-cod-arc        pic  9(07)       comp-3     .
                   15  wct-dat-doc-5      pic  9(07)       comp-3     .
                   15  wct-num-doc-5      pic  9(11)       comp-3     .
                   15  wct-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMODAT                      *
      *            *---------------------------------------------------*
               10  wct-k06.
                   15  wct-cod-dpz-6      pic  9(02)                  .
                   15  wct-tmo-orc-6      pic  x(05)                  .
                   15  wct-dat-doc-6      pic  9(07)       comp-3     .
                   15  wct-num-doc-6      pic  9(11)       comp-3     .
                   15  wct-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZOCH                         *
      *            *---------------------------------------------------*
               10  wct-k07.
                   15  wct-cod-dpz-7      pic  9(02)                  .
                   15  wct-flg-och        pic  x(01)                  .
                   15  wct-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  wct-dat.
               10  wct-ide-ute            pic  x(08)                  .
               10  wct-ide-fas            pic  x(06)                  .
               10  wct-dpz-arc            pic  x(04)                  .
               10  wct-tip-frn            pic  9(02)                  .
               10  wct-arc-plf            pic  9(07)       comp-3     .
               10  wct-dpz-plf            pic  x(04)                  .
               10  wct-tip-ftz            pic  9(02)                  .
               10  wct-tip-ids            pic  9(02)                  .
               10  wct-ocl-dat            pic  9(07)       comp-3     .
               10  wct-ocl-num            pic  x(10)                  .
               10  wct-ocl-rif            pic  x(40)                  .
               10  wct-cod-rsp            pic  9(05)       comp-3     .
               10  wct-dat-cns            pic  9(07)       comp-3     .
               10  wct-fds-dtc            pic  9(02)                  .
               10  wct-tip-eva            pic  9(02)                  .
               10  wct-pri-eva            pic  x(02)                  .
               10  wct-cod-cdv            pic  9(03)       comp-3     .
               10  wct-com-int.
                   15  wct-com-rig occurs 03
                                          pic  x(40)                  .
               10  wct-cod-lng            pic  x(03)                  .
               10  wct-vpf.
                   15  wct-sgl-vpf        pic  x(03)                  .
                   15  wct-dec-vpf        pic  9(01)                  .
                   15  wct-tdc-vpf        pic  x(01)                  .
                   15  wct-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  wct-ass-iva            pic  9(05)       comp-3     .
               10  wct-ctp-ven            pic  9(07)       comp-3     .
               10  wct-fat-sep            pic  x(01)                  .
               10  wct-inl-dcm            pic  9(02)                  .
               10  wct-inl-pgt            pic  9(02)                  .
               10  wct-cod-lst            pic  x(03)                  .
               10  wct-csr-aac            pic  9(05)       comp-3     .
               10  wct-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  wct-csc-aac            pic  9(05)       comp-3     .
               10  wct-psc-aac            pic  9(02)v9(01) comp-3     .
               10  wct-cpv-aac            pic  9(05)       comp-3     .
               10  wct-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  wct-voc-des occurs 06  pic  x(03)                  .
               10  wct-cod-fop            pic  9(07)       comp-3     .
               10  wct-scp-aap            pic  9(02)v9(01) comp-3     .
               10  wct-cod-abi            pic  9(05)       comp-3     .
               10  wct-cod-cab            pic  9(05)       comp-3     .
               10  wct-ccc-app            pic  x(12)                  .
               10  wct-nos-ban            pic  x(10)                  .
               10  wct-nos-ccp            pic  x(10)                  .
               10  wct-add-spi            pic  x(03)                  .
               10  wct-add-spb            pic  x(03)                  .
               10  wct-ipr-iel            pic  9(02)                  .
               10  wct-pag-dsm            pic  9(07)       comp-3     .
               10  wct-pag-qaf            pic  9(09)       comp-3     .
               10  wct-pag-act            pic  9(09)       comp-3     .
               10  wct-cod-age            pic  9(07)       comp-3     .
               10  wct-fsp-doc            pic  9(02)                  .
               10  wct-pvf-age            pic  9(11)       comp-3     .
               10  wct-tip-vpa            pic  9(02)                  .
               10  wct-cpv-aaa            pic  9(05)       comp-3     .
               10  wct-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  wct-cod-ime            pic  9(07)       comp-3     .
               10  wct-pvf-ime            pic  9(11)       comp-3     .
               10  wct-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  wct-tot-scc            pic s9(11)       comp-3     .
               10  wct-per-scc            pic  9(02)v9(01) comp-3     .
               10  wct-tot-scp            pic s9(11)       comp-3     .
               10  wct-per-scp            pic  9(02)v9(01) comp-3     .
               10  wct-spe-add occurs 06.
                   15  wct-spe-snx        pic  9(01)                  .
                   15  wct-spe-mad        pic  9(01)                  .
                   15  wct-spe-per        pic  9(02)v9(01) comp-3     .
                   15  wct-spe-ibl        pic  9(02)                  .
                   15  wct-ibt-spe.
                       20  wct-ibx-spe  occurs 09
                                          pic  x(01)                  .
                   15  wct-spe-imp        pic s9(09)       comp-3     .
               10  wct-tot-doc            pic s9(11)       comp-3     .
               10  wct-ctr-stp            pic  9(02)                  .
               10  wct-flg-ela.
                   15  wct-flg-blo.
                       20  wct-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  wct-flg-nbl.
                       20  wct-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  wct-flg-pul            pic  x(01)                  .
               10  wct-tip-ord            pic  x(01)                  .
               10  wct-cod-vet            pic  9(07)                  .
               10  wct-flg-rfp            pic  x(01)                  .
               10  wct-alx-exp.
                   15  filler  occurs 31  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [wcr]                                    *
      *    *-----------------------------------------------------------*
       fd  wcr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  wcr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  wcr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  wcr-k01.
                   15  wcr-num-prt        pic  9(11)       comp-3     .
                   15  wcr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  wcr-k02.
                   15  wcr-cod-dpz        pic  9(02)                  .
                   15  wcr-tip-mag        pic  9(02)                  .
                   15  wcr-num-pro        pic  9(07)       comp-3     .
                   15  wcr-dat-doc        pic  9(07)       comp-3     .
                   15  wcr-num-doc        pic  9(11)       comp-3     .
                   15  wcr-num-prt-2      pic  9(11)       comp-3     .
                   15  wcr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  wcr-k03.
                   15  wcr-cod-dpz-3      pic  9(02)                  .
                   15  wcr-flg-rch        pic  x(01)                  .
                   15  wcr-tip-mag-3      pic  9(02)                  .
                   15  wcr-num-pro-3      pic  9(07)       comp-3     .
                   15  wcr-num-prt-3      pic  9(11)       comp-3     .
                   15  wcr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHARC                         *
      *            *---------------------------------------------------*
               10  wcr-k04.
                   15  wcr-cod-dpz-4      pic  9(02)                  .
                   15  wcr-flg-rch-4      pic  x(01)                  .
                   15  wcr-tip-arc        pic  x(01)                  .
                   15  wcr-cod-arc        pic  9(07)       comp-3     .
                   15  wcr-num-prt-4      pic  9(11)       comp-3     .
                   15  wcr-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : ARCNDO                         *
      *            *---------------------------------------------------*
               10  wcr-k05.
                   15  wcr-cod-dpz-5      pic  9(02)                  .
                   15  wcr-tip-arc-5      pic  x(01)                  .
                   15  wcr-cod-arc-5      pic  9(07)       comp-3     .
                   15  wcr-num-doc-5      pic  9(11)       comp-3     .
                   15  wcr-num-prt-5      pic  9(11)       comp-3     .
                   15  wcr-num-prg-5      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : ARCDCR                         *
      *            *---------------------------------------------------*
               10  wcr-k06.
                   15  wcr-cod-dpz-6      pic  9(02)                  .
                   15  wcr-tip-arc-6      pic  x(01)                  .
                   15  wcr-cod-arc-6      pic  9(07)       comp-3     .
                   15  wcr-dcn-ric-6      pic  9(07)       comp-3     .
                   15  wcr-num-prt-6      pic  9(11)       comp-3     .
                   15  wcr-num-prg-6      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : ARCDEP                         *
      *            *---------------------------------------------------*
               10  wcr-k07.
                   15  wcr-cod-dpz-7      pic  9(02)                  .
                   15  wcr-tip-arc-7      pic  x(01)                  .
                   15  wcr-cod-arc-7      pic  9(07)       comp-3     .
                   15  wcr-des-rig-7      pic  x(40)                  .
                   15  wcr-dcn-ric-7      pic  9(07)       comp-3     .
                   15  wcr-num-prt-7      pic  9(11)       comp-3     .
                   15  wcr-num-prg-7      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : ARCPRO                         *
      *            *---------------------------------------------------*
               10  wcr-k08.
                   15  wcr-tip-arc-8      pic  x(01)                  .
                   15  wcr-cod-arc-8      pic  9(07)       comp-3     .
                   15  wcr-tip-mag-8      pic  9(02)                  .
                   15  wcr-num-pro-8      pic  9(07)       comp-3     .
                   15  wcr-dat-doc-8      pic  9(07)       comp-3     .
                   15  wcr-cod-dpz-8      pic  9(02)                  .
                   15  wcr-num-prt-8      pic  9(11)       comp-3     .
                   15  wcr-num-prg-8      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  wcr-dat.
               10  wcr-tmo-orc            pic  x(05)                  .
               10  wcr-dpz-arc            pic  x(04)                  .
               10  wcr-cod-lng            pic  x(03)                  .
               10  wcr-ocl-dat            pic  9(07)       comp-3     .
               10  wcr-ocl-num            pic  x(10)                  .
               10  wcr-pri-eva            pic  x(02)                  .
               10  wcr-vpf.
                   15  wcr-sgl-vpf        pic  x(03)                  .
                   15  wcr-dec-vpf        pic  9(01)                  .
                   15  wcr-tdc-vpf        pic  x(01)                  .
                   15  wcr-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  wcr-bld-flb            pic  9(01)                  .
               10  wcr-bld-tpb            pic  9(01)                  .
               10  wcr-bld-rgb            pic  9(01)                  .
               10  wcr-tip-rig            pic  x(05)                  .
               10  wcr-alf-pro            pic  x(14)                  .
               10  wcr-sgl-vrn            pic  x(14)                  .
               10  wcr-cop-scl            pic  x(14)                  .
               10  wcr-des-ext            pic  9(01)                  .
               10  wcr-des-rig            pic  x(40)                  .
               10  wcr-tip-pro            pic  9(02)                  .
               10  wcr-cod-iva            pic  9(05)       comp-3     .
               10  wcr-ctp-ven            pic  9(07)       comp-3     .
               10  wcr-umi-ven            pic  x(03)                  .
               10  wcr-dec-qta            pic  9(01)                  .
               10  wcr-qta-ord            pic s9(10)v9(03) comp-3     .
               10  wcr-sdr-ccs            pic  x(01)                  .
               10  wcr-cod-dsl            pic  x(07)                  .
               10  wcr-snx-2qt            pic  9(01)                  .
               10  wcr-dec-2qt            pic  9(01)                  .
               10  wcr-qta-a02            pic s9(10)v9(03) comp-3     .
               10  wcr-snx-3qt            pic  9(01)                  .
               10  wcr-dec-3qt            pic  9(01)                  .
               10  wcr-qta-a03            pic s9(10)v9(03) comp-3     .
               10  wcr-dec-prz            pic  9(01)                  .
               10  wcr-vps.
                   15  wcr-sgl-vps        pic  x(03)                  .
                   15  wcr-dec-vps        pic  9(01)                  .
                   15  wcr-tdc-vps        pic  x(01)                  .
                   15  wcr-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  wcr-prz-lrs            pic  9(09)       comp-3     .
               10  wcr-prz-nts            pic  9(09)       comp-3     .
               10  wcr-vpp.
                   15  wcr-sgl-vpp        pic  x(03)                  .
                   15  wcr-dec-vpp        pic  9(01)                  .
                   15  wcr-tdc-vpp        pic  x(01)                  .
                   15  wcr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  wcr-prz-ven            pic  9(09)       comp-3     .
               10  wcr-snx-2pz            pic  9(01)                  .
               10  wcr-dec-2pz            pic  9(01)                  .
               10  wcr-prz-a02            pic  9(09)       comp-3     .
               10  wcr-vpl.
                   15  wcr-sgl-vpl        pic  x(03)                  .
                   15  wcr-dec-vpl        pic  9(01)                  .
                   15  wcr-tdc-vpl        pic  x(01)                  .
                   15  wcr-prz-vpl        pic  9(09)       comp-3     .
                   15  wcr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  wcr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  wcr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  wcr-tlm-vpl        pic  x(01)                  .
                   15  wcr-map-vpl        pic  x(01)                  .
               10  wcr-epz-rgf            pic  9(01)                  .
               10  wcr-csr-aap            pic  9(05)       comp-3     .
               10  wcr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  wcr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  wcr-prz-net            pic  9(09)       comp-3     .
               10  wcr-epz-pes            pic  9(02)                  .
               10  wcr-vpc.
                   15  wcr-sgl-vpc        pic  x(03)                  .
                   15  wcr-dec-vpc        pic  9(01)                  .
                   15  wcr-tdc-vpc        pic  x(01)                  .
                   15  wcr-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  wcr-dec-cos            pic  9(01)                  .
               10  wcr-cos-rif            pic  9(09)       comp-3     .
               10  wcr-imp-rig            pic s9(11)       comp-3     .
               10  wcr-iau-rig            pic s9(11)       comp-3     .
               10  wcr-cpv-aap            pic  9(05)       comp-3     .
               10  wcr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  wcr-fsp-rig            pic  9(02)                  .
               10  wcr-cpv-rig            pic  9(05)       comp-3     .
               10  wcr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  wcr-pvf-rig            pic s9(11)       comp-3     .
               10  wcr-dcn-ric            pic  9(07)       comp-3     .
               10  wcr-dcn-prv            pic  9(07)       comp-3     .
               10  wcr-dcn-cnf            pic  9(07)       comp-3     .
               10  wcr-flg-cnf            pic  x(01)                  .
               10  wcr-cmc-tip            pic  x(05)                  .
               10  wcr-cmc-dat            pic  9(07)       comp-3     .
               10  wcr-cmc-num            pic  9(11)       comp-3     .
               10  wcr-flg-ela.
                   15  wcr-flg-blo.
                       20  wcr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  wcr-flg-nbl.
                       20  wcr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  wcr-flg-pul            pic  x(01)                  .
               10  wcr-flg-puq            pic  x(01)                  .
               10  wcr-tip-ord            pic  x(01)                  .
               10  wcr-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [wcx]                                    *
      *    *-----------------------------------------------------------*
       fd  wcx       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  wcx-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  wcx-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  wcx-k01.
                   15  wcx-num-prt        pic  9(11)       comp-3     .
                   15  wcx-num-prg        pic  9(05)       comp-3     .
                   15  wcx-tip-rec        pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  wcx-dat.
               10  wcx-val-rec.
                   15  wcx-rec-001.
                       20  wcx-dti-ids.
                           25  wcx-rag-ids
                                          pic  x(40)                  .
                           25  wcx-via-ids
                                          pic  x(40)                  .
                           25  wcx-loc-ids
                                          pic  x(40)                  .
                       20  wcx-dti-cdv.
                           25  wcx-des-cdv.
                               30  wcx-rig-cdv occurs 03
                                          pic  x(40)                  .
                       20  wcx-rs2-ids    pic  x(40)                  .
                       20  filler occurs 120
                                          pic  x(01)                  .
                   15  wcx-rec-011        redefines
                       wcx-rec-001.
                       20  wcx-des-400.
                           25  wcx-rig-400 occurs 10
                                          pic  x(40)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [wct]                                       *
      *    *-----------------------------------------------------------*
       01  f-wct.
           05  f-wct-nam                  pic  x(04)                  .
           05  f-wct-pat                  pic  x(40)                  .
           05  f-wct-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [wcr]                                       *
      *    *-----------------------------------------------------------*
       01  f-wcr.
           05  f-wcr-nam                  pic  x(04)                  .
           05  f-wcr-pat                  pic  x(40)                  .
           05  f-wcr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [wcx]                                       *
      *    *-----------------------------------------------------------*
       01  f-wcx.
           05  f-wcx-nam                  pic  x(04)                  .
           05  f-wcx-pat                  pic  x(40)                  .
           05  f-wcx-sts                  pic  x(02)                  .

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
                     "petorc"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "petorc01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   Recupero ordini clienti cancellati   "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
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
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico                       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ocx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocx"                          .

      *    *===========================================================*
      *    * Work per contatori ed indici                              *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Contatore generico 'I'                                *
      *        *-------------------------------------------------------*
           05  I                          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore generico 'J'                                *
      *        *-------------------------------------------------------*
           05  J                          pic  9(05)                  .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

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
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per 'N'                           *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "N"
                     go to main-250.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-800.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-800
           else      go to main-250.
       main-800.
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
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
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
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
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
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
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
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-pgm-frg-300.
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura parametri di selezione stampa       *
      *                  *---------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione eventuale sort preliminare       *
      *                  *---------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se sort eseguito       *
      *                  *---------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-pgm-frg-400
           else      go to exe-pgm-frg-500.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Se sort non eseguito                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di report-program                 *
      *                      *-----------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-500.
      *                  *---------------------------------------------*
      *                  * Se sort eseguito                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-600.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-pgm-frg-900.
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Se errori                           *
      *                          *-------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
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
           move      "N"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico stampa              *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-stp      .
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
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Apertura files area 'orc'                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
       rou-opn-fls-300.
      *              *-------------------------------------------------*
      *              * Apertura old-files area 'orc'                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [wct]                                       *
      *                  *---------------------------------------------*
           move      "/abd/azi/ptu/wct"   to   f-wct-pat              .
           open      i-o    wct                                       .
      *                  *---------------------------------------------*
      *                  * [wcr]                                       *
      *                  *---------------------------------------------*
           move      "/abd/azi/ptu/wcr"   to   f-wcr-pat              .
           open      i-o    wcr                                       .
      *                  *---------------------------------------------*
      *                  * [wcx]                                       *
      *                  *---------------------------------------------*
           move      "/abd/azi/ptu/wcx"   to   f-wcx-pat              .
           open      i-o    wcx                                       .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Chiusura files area 'orc'                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
       rou-cls-fls-300.
      *              *-------------------------------------------------*
      *              * Chiusura old-files area 'orc'                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [wct]                                       *
      *                  *---------------------------------------------*
           close     wct                                              .
      *                  *---------------------------------------------*
      *                  * [wcr]                                       *
      *                  *---------------------------------------------*
           close     wcr                                              .
      *                  *---------------------------------------------*
      *                  * [wcx]                                       *
      *                  *---------------------------------------------*
           close     wcx                                              .
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
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
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
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : Si'                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-exe-rou-srt      .
      *              *-------------------------------------------------*
      *              * Esecuzione sort                                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   stp-srt-inp-000
                                          thru stp-srt-inp-999
                     output procedure     is   prn-rou-pri-000
                                          thru prn-rou-pri-999        .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       stp-srt-inp-000.
      *              *-------------------------------------------------*
      *              * Ciclo [oct]                                     *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-oct-000  thru   stp-srt-inp-oct-999  .
      *              *-------------------------------------------------*
      *              * Ciclo [ocr]                                     *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-ocr-000  thru   stp-srt-inp-ocr-999  .
      *              *-------------------------------------------------*
      *              * Ciclo [ocx]                                     *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-ocx-000  thru   stp-srt-inp-ocx-999  .
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine [oct]                                          *
      *    *-----------------------------------------------------------*
       stp-srt-inp-oct-000.
      *              *-------------------------------------------------*
      *              * Start su [wct]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   wct-k01                .
           start     wct    key not less
                            wct-k01
                            invalid key
                            go to stp-srt-inp-oct-900.
       stp-srt-inp-oct-200.
      *              *-------------------------------------------------*
      *              * Next su [wct]                                   *
      *              *-------------------------------------------------*
           read      wct    next
                            with no lock
                            at end
                            go to stp-srt-inp-oct-900.
       stp-srt-inp-oct-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare totale del record   *
      *              *-------------------------------------------------*
           move      spaces               to   srt-rec                .
      *              *-------------------------------------------------*
      *              * Composizione record di sort                     *
      *              *-------------------------------------------------*
           move      "T"                  to   srt-tip-rec            .
           move      wct-num-prt          to   srt-num-prt            .
           move      zero                 to   srt-num-prg            .
           move      zero                 to   srt-tip-ext            .
      *
           move      wct-ide-dat          to   srt-ide-dat            .
           move      wct-dat-doc          to   srt-dat-doc            .
           move      wct-cod-dpz          to   srt-cod-dpz            .
           move      wct-num-doc          to   srt-num-doc            .
           move      wct-tmo-orc          to   srt-tmo-orc            .
           move      wct-scl-ann          to   srt-scl-ann            .
           move      wct-sgl-num          to   srt-sgl-num            .
           move      wct-tip-arc          to   srt-tip-arc            .
           move      wct-cod-arc          to   srt-cod-arc            .
           move      wct-flg-och          to   srt-flg-och            .
      *
           move      wct-dat              to   srt-dat-rec            .
       stp-srt-inp-oct-600.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-oct-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-oct-200.
       stp-srt-inp-oct-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-oct-999.
       stp-srt-inp-oct-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine [ocr]                                          *
      *    *-----------------------------------------------------------*
       stp-srt-inp-ocr-000.
      *              *-------------------------------------------------*
      *              * Start su [wcr]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   wcr-k01                .
           start     wcr    key not less
                            wcr-k01
                            invalid key
                            go to stp-srt-inp-ocr-900.
       stp-srt-inp-ocr-200.
      *              *-------------------------------------------------*
      *              * Next su [wcr]                                   *
      *              *-------------------------------------------------*
           read      wcr    next
                            with no lock
                            at end
                            go to stp-srt-inp-ocr-900.
       stp-srt-inp-ocr-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare totale del record   *
      *              *-------------------------------------------------*
           move      spaces               to   srt-rec                .
      *              *-------------------------------------------------*
      *              * Composizione record di sort                     *
      *              *-------------------------------------------------*
           move      "R"                  to   srt-tip-rec            .
           move      wcr-num-prt          to   srt-num-prt            .
           move      wcr-num-prg          to   srt-num-prg            .
           move      zero                 to   srt-tip-ext            .
      *
           move      wcr-cod-dpz          to   srt-cod-dpz            .
           move      wcr-tip-mag          to   srt-tip-mag            .
           move      wcr-num-pro          to   srt-num-pro            .
           move      wcr-dat-doc          to   srt-dat-doc            .
           move      wcr-num-doc          to   srt-num-doc            .
           move      wcr-flg-rch          to   srt-flg-rch            .
           move      wcr-tip-arc          to   srt-tip-arc            .
           move      wcr-cod-arc          to   srt-cod-arc            .
      *
           move      wcr-dat              to   srt-dat-rec            .
       stp-srt-inp-ocr-600.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-ocr-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-ocr-200.
       stp-srt-inp-ocr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-ocr-999.
       stp-srt-inp-ocr-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine [ocx]                                          *
      *    *-----------------------------------------------------------*
       stp-srt-inp-ocx-000.
      *              *-------------------------------------------------*
      *              * Start su [wcx]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   wcx-k01                .
           start     wcx    key not less
                            wcx-k01
                            invalid key
                            go to stp-srt-inp-ocx-900.
       stp-srt-inp-ocx-200.
      *              *-------------------------------------------------*
      *              * Next su [wcx]                                   *
      *              *-------------------------------------------------*
           read      wcx    next
                            with no lock
                            at end
                            go to stp-srt-inp-ocx-900.
       stp-srt-inp-ocx-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare totale del record   *
      *              *-------------------------------------------------*
           move      spaces               to   srt-rec                .
      *              *-------------------------------------------------*
      *              * Composizione record di sort                     *
      *              *-------------------------------------------------*
           move      "X"                  to   srt-tip-rec            .
           move      wcx-num-prt          to   srt-num-prt            .
           move      wcx-num-prg          to   srt-num-prg            .
           move      wcx-tip-rec          to   srt-tip-ext            .
      *
           move      wcx-dat              to   srt-dat-rec            .
       stp-srt-inp-ocx-600.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-ocx-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-ocx-200.
       stp-srt-inp-ocx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-ocx-999.
       stp-srt-inp-ocx-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun elemento entro i limiti assegnati !"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
           return    srt    at end
                            move  "#"     to   w-cnt-prn-flg-sub
                            go to prn-let-seq-999.
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo record          *
      *              *-------------------------------------------------*
           if        srt-tip-rec          =    "T"
                     perform   prn-liv-det-oct-000
                                          thru   prn-liv-det-oct-999
           else if   srt-tip-rec          =    "R"
                     perform   prn-liv-det-ocr-000
                                          thru   prn-liv-det-ocr-999
           else if   srt-tip-rec          =    "X"
                     perform   prn-liv-det-ocx-000
                                          thru   prn-liv-det-ocx-999  .
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Trattamento [oct]                                         *
      *    *-----------------------------------------------------------*
       prn-liv-det-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [oct]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       prn-liv-det-oct-100.
      *              *-------------------------------------------------*
      *              * Lettura record [oct]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      srt-num-prt          to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se record trovato: ad uscita                *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prn-liv-det-oct-900.
       prn-liv-det-oct-400.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-oct-000      thru cmp-rec-oct-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       prn-liv-det-oct-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-oct-999.
       prn-liv-det-oct-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Trattamento [ocr]                                         *
      *    *-----------------------------------------------------------*
       prn-liv-det-ocr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [ocr]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       prn-liv-det-ocr-100.
      *              *-------------------------------------------------*
      *              * Lettura record [ocr]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      srt-num-prt          to   rf-ocr-num-prt         .
           move      srt-num-prg          to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se record trovato: ad uscita                *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prn-liv-det-ocr-900.
       prn-liv-det-ocr-400.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ocr-000      thru cmp-rec-ocr-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       prn-liv-det-ocr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-ocr-999.
       prn-liv-det-ocr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Trattamento [ocx]                                         *
      *    *-----------------------------------------------------------*
       prn-liv-det-ocx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [ocx]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
       prn-liv-det-ocx-100.
      *              *-------------------------------------------------*
      *              * Lettura record [ocx]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      srt-num-prt          to   rf-ocx-num-prt         .
           move      srt-num-prg          to   rf-ocx-num-prg         .
           move      srt-tip-ext          to   rf-ocx-tip-rec         .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * Se record trovato: ad uscita                *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prn-liv-det-ocx-900.
       prn-liv-det-ocx-400.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-ocx-000      thru cmp-rec-ocx-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
       prn-liv-det-ocx-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-ocx-999.
       prn-liv-det-ocx-999.
           exit.

      *    *===========================================================*
      *    * Composizione record testata [oct]                         *
      *    *-----------------------------------------------------------*
       cmp-rec-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Chiavi integrative                              *
      *              *-------------------------------------------------*
           move      srt-num-prt          to   rf-oct-num-prt         .
           move      srt-ide-dat          to   rf-oct-ide-dat         .
           move      srt-dat-doc          to   rf-oct-dat-doc         .
           move      srt-cod-dpz          to   rf-oct-cod-dpz         .
           move      srt-num-doc          to   rf-oct-num-doc         .
           move      srt-tmo-orc          to   rf-oct-tmo-orc         .
           move      srt-scl-ann          to   rf-oct-scl-ann         .
           move      srt-sgl-num          to   rf-oct-sgl-num         .
           move      srt-tip-arc          to   rf-oct-tip-arc         .
           move      srt-cod-arc          to   rf-oct-cod-arc         .
           move      srt-flg-och          to   rf-oct-flg-och         .      
      *              *-------------------------------------------------*
      *              * Dati                                            *
      *              *-------------------------------------------------*
           move      srt-dat-rec          to   wct-dat                .
      *
           move      wct-ide-ute          to   rf-oct-ide-ute         .
           move      wct-ide-fas          to   rf-oct-ide-fas         .
           move      wct-dpz-arc          to   rf-oct-dpz-arc         .
           move      wct-tip-frn          to   rf-oct-tip-frn         .
           move      wct-arc-plf          to   rf-oct-arc-plf         .
           move      wct-dpz-plf          to   rf-oct-dpz-plf         .
           move      wct-tip-ftz          to   rf-oct-tip-ftz         .
           move      wct-tip-ids          to   rf-oct-tip-ids         .
           move      wct-ocl-dat          to   rf-oct-ocl-dat         .
           move      wct-ocl-num          to   rf-oct-ocl-num         .
           move      wct-ocl-rif          to   rf-oct-ocl-rif         .
           move      wct-cod-rsp          to   rf-oct-cod-rsp         .
           move      wct-dat-cns          to   rf-oct-dat-cns         .
           move      wct-fds-dtc          to   rf-oct-fds-dtc         .
           move      wct-tip-eva          to   rf-oct-tip-eva         .
           move      wct-pri-eva          to   rf-oct-pri-eva         .
           move      wct-cod-cdv          to   rf-oct-cod-cdv         .
           move      wct-com-int          to   rf-oct-com-int         .
           move      wct-cod-lng          to   rf-oct-cod-lng         .
           move      wct-sgl-vpf          to   rf-oct-sgl-vpf         .
           move      wct-dec-vpf          to   rf-oct-dec-vpf         .
           move      wct-tdc-vpf          to   rf-oct-tdc-vpf         .
           move      wct-cdc-vpf          to   rf-oct-cdc-vpf         .
           move      wct-ass-iva          to   rf-oct-ass-iva         .
           move      wct-ctp-ven          to   rf-oct-ctp-ven         .
           move      wct-fat-sep          to   rf-oct-fat-sep         .
           move      wct-inl-dcm          to   rf-oct-inl-dcm         .
           move      wct-inl-pgt          to   rf-oct-inl-pgt         .
           move      wct-cod-lst          to   rf-oct-cod-lst         .
           move      wct-csr-aac          to   rf-oct-csr-aac         .
           move      wct-psr-aac (1)      to   rf-oct-psr-aac (1)     .
           move      wct-psr-aac (2)      to   rf-oct-psr-aac (2)     .
           move      wct-psr-aac (3)      to   rf-oct-psr-aac (3)     .
           move      wct-psr-aac (4)      to   rf-oct-psr-aac (4)     .
           move      wct-psr-aac (5)      to   rf-oct-psr-aac (5)     .
           move      wct-csc-aac          to   rf-oct-csc-aac         .
           move      wct-psc-aac          to   rf-oct-psc-aac         .
           move      wct-cpv-aac          to   rf-oct-cpv-aac         .
           move      wct-ppv-aac (1)      to   rf-oct-ppv-aac (1)     .
           move      wct-ppv-aac (2)      to   rf-oct-ppv-aac (2)     .
           move      wct-ppv-aac (3)      to   rf-oct-ppv-aac (3)     .
           move      wct-voc-des (1)      to   rf-oct-voc-des (1)     .
           move      wct-voc-des (2)      to   rf-oct-voc-des (2)     .
           move      wct-voc-des (3)      to   rf-oct-voc-des (3)     .
           move      wct-voc-des (4)      to   rf-oct-voc-des (4)     .
           move      wct-voc-des (5)      to   rf-oct-voc-des (5)     .
           move      wct-voc-des (6)      to   rf-oct-voc-des (6)     .
           move      wct-cod-fop          to   rf-oct-cod-fop         .
           move      wct-scp-aap          to   rf-oct-scp-aap         .
           move      wct-cod-abi          to   rf-oct-cod-abi         .
           move      wct-cod-cab          to   rf-oct-cod-cab         .
           move      wct-ccc-app          to   rf-oct-ccc-app         .
           move      wct-nos-ban          to   rf-oct-nos-ban         .
           move      wct-nos-ccp          to   rf-oct-nos-ccp         .
           move      wct-add-spi          to   rf-oct-add-spi         .
           move      wct-add-spb          to   rf-oct-add-spb         .
           move      wct-ipr-iel          to   rf-oct-ipr-iel         .
           move      wct-pag-dsm          to   rf-oct-pag-dsm         .
           move      wct-pag-qaf          to   rf-oct-pag-qaf         .
           move      wct-pag-act          to   rf-oct-pag-act         .
           move      wct-cod-age          to   rf-oct-cod-age         .
           move      wct-fsp-doc          to   rf-oct-fsp-doc         .
           move      wct-pvf-age          to   rf-oct-pvf-age         .
           move      wct-tip-vpa          to   rf-oct-tip-vpa         .
           move      wct-cpv-aaa          to   rf-oct-cpv-aaa         .
           move      wct-ppv-aaa (1)      to   rf-oct-ppv-aaa (1)     .
           move      wct-ppv-aaa (2)      to   rf-oct-ppv-aaa (2)     .
           move      wct-ppv-aaa (3)      to   rf-oct-ppv-aaa (3)     .
           move      wct-cod-ime          to   rf-oct-cod-ime         .
           move      wct-pvf-ime          to   rf-oct-pvf-ime         .
           move      wct-tot-rig (1)      to   rf-oct-tot-rig (1)     .
           move      wct-tot-rig (2)      to   rf-oct-tot-rig (2)     .
           move      wct-tot-rig (3)      to   rf-oct-tot-rig (3)     .
           move      wct-tot-rig (4)      to   rf-oct-tot-rig (4)     .
           move      wct-tot-rig (5)      to   rf-oct-tot-rig (5)     .
           move      wct-tot-rig (6)      to   rf-oct-tot-rig (6)     .
           move      wct-tot-rig (7)      to   rf-oct-tot-rig (7)     .
           move      wct-tot-rig (8)      to   rf-oct-tot-rig (8)     .
           move      wct-tot-rig (9)      to   rf-oct-tot-rig (9)     .
           move      wct-tot-scc          to   rf-oct-tot-scc         .
           move      wct-per-scc          to   rf-oct-per-scc         .
           move      wct-tot-scp          to   rf-oct-tot-scp         .
           move      wct-per-scp          to   rf-oct-per-scp         .
           move      zero                 to   I                      .
       cmp-rec-oct-100.
           add       1                    to   I                      .
           if        I                    >    6
                     go to cmp-rec-oct-120.
           move      wct-spe-snx (I)      to   rf-oct-spe-snx (I)     .
           move      wct-spe-mad (I)      to   rf-oct-spe-mad (I)     .
           move      wct-spe-per (I)      to   rf-oct-spe-per (I)     .
           move      wct-spe-ibl (I)      to   rf-oct-spe-ibl (I)     .
           move      wct-ibt-spe (I)      to   rf-oct-ibt-spe (I)     .
           move      wct-spe-imp (I)      to   rf-oct-spe-imp (I)     .
           go to     cmp-rec-oct-100.
       cmp-rec-oct-120.
           move      wct-tot-doc          to   rf-oct-tot-doc         .
           move      wct-ctr-stp          to   rf-oct-ctr-stp         .
           move      wct-flg-och          to   rf-oct-flg-och         .
           move      wct-flg-ela          to   rf-oct-flg-ela         .
           move      wct-flg-pul          to   rf-oct-flg-pul         .
           move      wct-tip-ord          to   rf-oct-tip-ord         .
           move      wct-cod-vet          to   rf-oct-cod-vet         .
           move      wct-flg-rfp          to   rf-oct-flg-rfp         .
           move      wct-alx-exp          to   rf-oct-alx-exp         .
       cmp-rec-oct-999.
           exit.

      *    *===========================================================*
      *    * Composizione record righe [ocr]                           *
      *    *-----------------------------------------------------------*
       cmp-rec-ocr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Chiavi integrative                              *
      *              *-------------------------------------------------*
           move      srt-num-prt          to   rf-ocr-num-prt         .
           move      srt-num-prg          to   rf-ocr-num-prg         .
           move      srt-cod-dpz          to   rf-ocr-cod-dpz         .
           move      srt-tip-mag          to   rf-ocr-tip-mag         .
           move      srt-num-pro          to   rf-ocr-num-pro         .
           move      srt-dat-doc          to   rf-ocr-dat-doc         .
           move      srt-num-doc          to   rf-ocr-num-doc         .
           move      srt-flg-rch          to   rf-ocr-flg-rch         .      
           move      srt-tip-arc          to   rf-ocr-tip-arc         .
           move      srt-cod-arc          to   rf-ocr-cod-arc         .
      *              *-------------------------------------------------*
      *              * Dati                                            *
      *              *-------------------------------------------------*
           move      srt-dat-rec          to   wcr-dat                .
      *
           move      wcr-tmo-orc          to   rf-ocr-tmo-orc         .
           move      wcr-dpz-arc          to   rf-ocr-dpz-arc         .
           move      wcr-cod-lng          to   rf-ocr-cod-lng         .
           move      wcr-ocl-dat          to   rf-ocr-ocl-dat         .
           move      wcr-ocl-num          to   rf-ocr-ocl-num         .
           move      wcr-pri-eva          to   rf-ocr-pri-eva         .
           move      wcr-sgl-vpf          to   rf-ocr-sgl-vpf         .
           move      wcr-dec-vpf          to   rf-ocr-dec-vpf         .
           move      wcr-tdc-vpf          to   rf-ocr-tdc-vpf         .
           move      wcr-cdc-vpf          to   rf-ocr-cdc-vpf         .
           move      wcr-bld-flb          to   rf-ocr-bld-flb         .
           move      wcr-bld-tpb          to   rf-ocr-bld-tpb         .
           move      wcr-bld-rgb          to   rf-ocr-bld-rgb         .
           move      wcr-tip-rig          to   rf-ocr-tip-rig         .
           move      wcr-alf-pro          to   rf-ocr-alf-pro         .
           move      wcr-sgl-vrn          to   rf-ocr-sgl-vrn         .
           move      wcr-cop-scl          to   rf-ocr-cop-scl         .
           move      wcr-des-ext          to   rf-ocr-des-ext         .
           move      wcr-des-rig          to   rf-ocr-des-rig         .
           move      wcr-tip-pro          to   rf-ocr-tip-pro         .
           move      wcr-cod-iva          to   rf-ocr-cod-iva         .
           move      wcr-ctp-ven          to   rf-ocr-ctp-ven         .
           move      wcr-umi-ven          to   rf-ocr-umi-ven         .
           move      wcr-dec-qta          to   rf-ocr-dec-qta         .
           move      wcr-qta-ord          to   rf-ocr-qta-ord         .
           move      wcr-sdr-ccs          to   rf-ocr-sdr-ccs         .
           move      wcr-snx-2qt          to   rf-ocr-snx-2qt         .
           move      wcr-dec-2qt          to   rf-ocr-dec-2qt         .
           move      wcr-qta-a02          to   rf-ocr-qta-a02         .
           move      wcr-snx-3qt          to   rf-ocr-snx-3qt         .
           move      wcr-dec-3qt          to   rf-ocr-dec-3qt         .
           move      wcr-qta-a03          to   rf-ocr-qta-a03         .
           move      wcr-dec-prz          to   rf-ocr-dec-prz         .
           move      wcr-sgl-vps          to   rf-ocr-sgl-vps         .
           move      wcr-dec-vps          to   rf-ocr-dec-vps         .
           move      wcr-tdc-vps          to   rf-ocr-tdc-vps         .
           move      wcr-cdc-vps          to   rf-ocr-cdc-vps         .
           move      wcr-prz-lrs          to   rf-ocr-prz-lrs         .
           move      wcr-prz-nts          to   rf-ocr-prz-nts         .
           move      wcr-sgl-vpp          to   rf-ocr-sgl-vpp         .
           move      wcr-dec-vpp          to   rf-ocr-dec-vpp         .
           move      wcr-tdc-vpp          to   rf-ocr-tdc-vpp         .
           move      wcr-cdc-vpp          to   rf-ocr-cdc-vpp         .
           move      wcr-prz-ven          to   rf-ocr-prz-ven         .
           move      wcr-snx-2pz          to   rf-ocr-snx-2pz         .
           move      wcr-prz-a02          to   rf-ocr-prz-a02         .
           move      wcr-sgl-vpl          to   rf-ocr-sgl-vpl         .
           move      wcr-dec-vpl          to   rf-ocr-dec-vpl         .
           move      wcr-tdc-vpl          to   rf-ocr-tdc-vpl         .
           move      wcr-prz-vpl          to   rf-ocr-prz-vpl         .
           move      wcr-cdc-vpl          to   rf-ocr-cdc-vpl         .
           move      wcr-ccr-vpl          to   rf-ocr-ccr-vpl         .
           move      wcr-plm-vpl          to   rf-ocr-plm-vpl         .
           move      wcr-tlm-vpl          to   rf-ocr-tlm-vpl         .
           move      wcr-map-vpl          to   rf-ocr-map-vpl         .
           move      wcr-epz-rgf          to   rf-ocr-epz-rgf         .
           move      wcr-csr-aap          to   rf-ocr-csr-aap         .
           move      wcr-psr-aap (1)      to   rf-ocr-psr-aap (1)     .
           move      wcr-psr-aap (2)      to   rf-ocr-psr-aap (2)     .
           move      wcr-psr-aap (3)      to   rf-ocr-psr-aap (3)     .
           move      wcr-psr-aap (4)      to   rf-ocr-psr-aap (4)     .
           move      wcr-psr-aap (5)      to   rf-ocr-psr-aap (5)     .
           move      wcr-per-scr (1)      to   rf-ocr-per-scr (1)     .
           move      wcr-per-scr (2)      to   rf-ocr-per-scr (2)     .
           move      wcr-per-scr (3)      to   rf-ocr-per-scr (3)     .
           move      wcr-per-scr (4)      to   rf-ocr-per-scr (4)     .
           move      wcr-per-scr (5)      to   rf-ocr-per-scr (5)     .
           move      wcr-prz-net          to   rf-ocr-prz-net         .
           move      wcr-epz-pes          to   rf-ocr-epz-pes         .
           move      wcr-sgl-vpc          to   rf-ocr-sgl-vpc         .
           move      wcr-dec-vpc          to   rf-ocr-dec-vpc         .
           move      wcr-tdc-vpc          to   rf-ocr-tdc-vpc         .
           move      wcr-cdc-vpc          to   rf-ocr-cdc-vpc         .
           move      wcr-dec-cos          to   rf-ocr-dec-cos         .
           move      wcr-cos-rif          to   rf-ocr-cos-rif         .
           move      wcr-imp-rig          to   rf-ocr-imp-rig         .
           move      wcr-iau-rig          to   rf-ocr-iau-rig         .
           move      wcr-cpv-aap          to   rf-ocr-cpv-aap         .
           move      wcr-ppv-aap (1)      to   rf-ocr-ppv-aap (1)     .
           move      wcr-ppv-aap (2)      to   rf-ocr-ppv-aap (2)     .
           move      wcr-ppv-aap (3)      to   rf-ocr-ppv-aap (3)     .
           move      wcr-fsp-rig          to   rf-ocr-fsp-rig         .
           move      wcr-cpv-rig          to   rf-ocr-cpv-rig         .
           move      wcr-ppv-rig (1)      to   rf-ocr-ppv-rig (1)     .
           move      wcr-ppv-rig (2)      to   rf-ocr-ppv-rig (2)     .
           move      wcr-ppv-rig (3)      to   rf-ocr-ppv-rig (3)     .
           move      wcr-pvf-rig          to   rf-ocr-pvf-rig         .
           move      wcr-dcn-ric          to   rf-ocr-dcn-ric         .
           move      wcr-dcn-prv          to   rf-ocr-dcn-prv         .
           move      wcr-dcn-cnf          to   rf-ocr-dcn-cnf         .
           move      wcr-flg-cnf          to   rf-ocr-flg-cnf         .
           move      wcr-cmc-tip          to   rf-ocr-cmc-tip         .
           move      wcr-cmc-dat          to   rf-ocr-cmc-dat         .
           move      wcr-cmc-num          to   rf-ocr-cmc-num         .
           move      wcr-flg-rch          to   rf-ocr-flg-rch         .
           move      wcr-flg-ela          to   rf-ocr-flg-ela         .
           move      wcr-flg-pul          to   rf-ocr-flg-pul         .
           move      wcr-flg-puq          to   rf-ocr-flg-puq         .
           move      wcr-tip-ord          to   rf-ocr-tip-ord         .
           move      wcr-flg-ela          to   rf-ocr-flg-ela         .
           move      wcr-alx-exp          to   rf-ocr-alx-exp         .
       cmp-rec-ocr-999.
           exit.

      *    *===========================================================*
      *    * Composizione record righe [ocx]                           *
      *    *-----------------------------------------------------------*
       cmp-rec-ocx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      srt-num-prt          to   rf-ocx-num-prt         .
           move      srt-num-prg          to   rf-ocx-num-prg         .
           move      srt-tip-ext          to   rf-ocx-tip-rec         .
           move      srt-dat-rec          to   wcx-dat                .
      *
           move      wcx-val-rec          to   rf-ocx-val-rec         .
       cmp-rec-ocx-999.
           exit.

