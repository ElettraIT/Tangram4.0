       Identification Division.
       Program-Id.                                 iofbit             .
      *================================================================*
      *                                                                *
      *                  Input-Output File bit                         *
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
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-doc-3      pic  9(07)       comp-3     .
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-num-doc        pic  9(11)       comp-3     .
                   15  fil-cod-tmb        pic  x(05)                  .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-snx-fac        pic  x(01)                  .
                   15  fil-scl-ann        pic  9(03)       comp-3     .
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-sgl-num        pic  x(03)                  .
                   15  fil-num-doc-4      pic  9(11)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-dat-doc-5      pic  9(07)       comp-3     .
                   15  fil-num-doc-5      pic  9(11)       comp-3     .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-dpz-6      pic  9(02)                  .
                   15  fil-cod-tmb-6      pic  x(05)                  .
                   15  fil-dat-doc-6      pic  9(07)       comp-3     .
                   15  fil-num-doc-6      pic  9(11)       comp-3     .
                   15  fil-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : FATARC                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-fat-snx        pic  x(01)                  .
                   15  fil-fat-dat        pic  9(07)       comp-3     .
                   15  fil-fat-num        pic  9(11)       comp-3     .
                   15  fil-fat-npb        pic  9(03)       comp-3     .
                   15  fil-tip-arc-7      pic  x(01)                  .
                   15  fil-arc-plf-7      pic  9(07)       comp-3     .
                   15  fil-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-prt-fti            pic  9(11)       comp-3     .
               10  fil-prt-mag            pic  9(11)       comp-3     .
               10  fil-pr2-mag            pic  9(11)       comp-3     .
               10  fil-int-ftr            pic  9(02)                  .
               10  fil-tmo-ftr            pic  x(05)                  .
               10  fil-snx-acm            pic  x(01)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-tip-frn            pic  9(02)                  .
               10  fil-arc-plf            pic  9(07)       comp-3     .
               10  fil-dpz-plf            pic  x(04)                  .
               10  fil-tip-ftz            pic  9(02)                  .
               10  fil-tip-ids            pic  9(02)                  .
               10  fil-cod-lng            pic  x(03)                  .
               10  fil-vpf.
                   15  fil-sgl-vpf        pic  x(03)                  .
                   15  fil-dec-vpf        pic  9(01)                  .
                   15  fil-tdc-vpf        pic  x(01)                  .
                   15  fil-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fil-ass-iva            pic  9(05)       comp-3     .
               10  fil-ctp-ven            pic  9(07)       comp-3     .
               10  fil-fat-sep            pic  x(01)                  .
               10  fil-inl-dcm            pic  9(02)                  .
               10  fil-inl-pgt            pic  9(02)                  .
               10  fil-cod-lst            pic  x(03)                  .
               10  fil-csr-aac            pic  9(05)       comp-3     .
               10  fil-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  fil-csc-aac            pic  9(05)       comp-3     .
               10  fil-psc-aac            pic  9(02)v9(01) comp-3     .
               10  fil-cpv-aac            pic  9(05)       comp-3     .
               10  fil-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-voc-des occurs 06  pic  x(03)                  .
               10  fil-cod-fop            pic  9(07)       comp-3     .
               10  fil-scp-aap            pic  9(02)v9(01) comp-3     .
               10  fil-cod-abi            pic  9(05)       comp-3     .
               10  fil-cod-cab            pic  9(05)       comp-3     .
               10  fil-ccc-app            pic  x(12)                  .
               10  fil-nos-ban            pic  x(10)                  .
               10  fil-nos-ccp            pic  x(10)                  .
               10  fil-add-spi            pic  x(03)                  .
               10  fil-add-spb            pic  x(03)                  .
               10  fil-ipr-iel            pic  9(02)                  .
               10  fil-pag-dsm            pic  9(07)       comp-3     .
               10  fil-pag-qaf            pic  9(09)       comp-3     .
               10  fil-pag-act            pic  9(09)       comp-3     .
               10  fil-cod-age            pic  9(07)       comp-3     .
               10  fil-fsp-doc            pic  9(02)                  .
               10  fil-pvf-age            pic  9(11)       comp-3     .
               10  fil-tip-vpa            pic  9(02)                  .
               10  fil-cpv-aaa            pic  9(05)       comp-3     .
               10  fil-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  fil-cod-ime            pic  9(07)       comp-3     .
               10  fil-pvf-ime            pic  9(11)       comp-3     .
               10  fil-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  fil-tot-scc            pic s9(11)       comp-3     .
               10  fil-per-scc            pic  9(02)v9(01) comp-3     .
               10  fil-civ-scc            pic  9(05)       comp-3     .
               10  fil-ccp-scc            pic  9(07)       comp-3     .
               10  fil-tot-scp            pic s9(11)       comp-3     .
               10  fil-per-scp            pic  9(02)v9(01) comp-3     .
               10  fil-civ-scp            pic  9(05)       comp-3     .
               10  fil-ccp-scp            pic  9(07)       comp-3     .
               10  fil-spe-add occurs 06.
                   15  fil-spe-snx        pic  9(01)                  .
                   15  fil-spe-mad        pic  9(01)                  .
                   15  fil-spe-per        pic  9(02)v9(01) comp-3     .
                   15  fil-spe-ibl        pic  9(02)                  .
                   15  fil-ibt-spe.
                       20  fil-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  fil-spe-imp        pic s9(09)       comp-3     .
                   15  fil-spe-civ        pic  9(05)       comp-3     .
                   15  fil-spe-ccp        pic  9(07)       comp-3     .
               10  fil-civ-spi            pic  9(05)       comp-3     .
               10  fil-ccp-spi            pic  9(07)       comp-3     .
               10  fil-tot-sic            pic s9(09)       comp-3     .
               10  fil-tot-sia            pic s9(09)       comp-3     .
               10  fil-tot-spb            pic s9(09)       comp-3     .
               10  fil-civ-spb            pic  9(05)       comp-3     .
               10  fil-ccp-spb            pic  9(07)       comp-3     .
               10  fil-dtr-tra.
                   15  fil-tra-cur        pic  9(02)                  .
                   15  fil-cau-tra        pic  x(03)                  .
                   15  fil-asp-ben        pic  x(03)                  .
                   15  fil-num-col        pic  9(05)       comp-3     .
                   15  fil-pes-tot        pic  9(06)v9(03) comp-3     .
                   15  fil-dat-itr        pic  9(07)       comp-3     .
                   15  fil-ora-itr        pic  9(04)                  .
                   15  fil-cod-vet        pic  9(07)       comp-3     .
                   15  fil-cod-vt2        pic  9(07)       comp-3     .
                   15  fil-cod-vt3        pic  9(07)       comp-3     .
                   15  fil-cod-ant        pic  9(03)       comp-3     .
               10  fil-prt-mgd            pic  9(07)       comp-3     .
               10  fil-nrg-mgd            pic  9(02)                  .
               10  fil-dri-mgd            pic  9(07)       comp-3     .
               10  fil-nri-mgd            pic  x(10)                  .
               10  fil-nps-sdb            pic  9(11)                  .
               10  fil-ctr-sdb            pic  9(02)                  .
               10  fil-iva-cst.
                   15  fil-iva-rig occurs 06.
                       20  fil-iva-cod    pic  9(05)       comp-3     .
                       20  fil-iva-ibl    pic s9(11)       comp-3     .
                       20  fil-iva-imp    pic s9(11)       comp-3     .
                   15  fil-iva-tdo        pic s9(11)       comp-3     .
               10  fil-ctp-cst.
                   15  fil-ctp-rig occurs 10.
                       20  fil-ctp-cod    pic  9(07)       comp-3     .
                       20  fil-ctp-imp    pic s9(11)       comp-3     .
               10  fil-ctr-stp            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-flg-rfp            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 39   pic  x(01)                  .

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
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-doc-3      pic  9(07)       comp-3     .
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-num-doc        pic  9(11)       comp-3     .
                   15  pul-cod-tmb        pic  x(05)                  .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-snx-fac        pic  x(01)                  .
                   15  pul-scl-ann        pic  9(03)       comp-3     .
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-sgl-num        pic  x(03)                  .
                   15  pul-num-doc-4      pic  9(11)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-dat-doc-5      pic  9(07)       comp-3     .
                   15  pul-num-doc-5      pic  9(11)       comp-3     .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-dpz-6      pic  9(02)                  .
                   15  pul-cod-tmb-6      pic  x(05)                  .
                   15  pul-dat-doc-6      pic  9(07)       comp-3     .
                   15  pul-num-doc-6      pic  9(11)       comp-3     .
                   15  pul-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : FATARC                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-fat-snx        pic  x(01)                  .
                   15  pul-fat-dat        pic  9(07)       comp-3     .
                   15  pul-fat-num        pic  9(11)       comp-3     .
                   15  pul-fat-npb        pic  9(03)       comp-3     .
                   15  pul-tip-arc-7      pic  x(01)                  .
                   15  pul-arc-plf-7      pic  9(07)       comp-3     .
                   15  pul-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-prt-fti            pic  9(11)       comp-3     .
               10  pul-prt-mag            pic  9(11)       comp-3     .
               10  pul-pr2-mag            pic  9(11)       comp-3     .
               10  pul-int-ftr            pic  9(02)                  .
               10  pul-tmo-ftr            pic  x(05)                  .
               10  pul-snx-acm            pic  x(01)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-tip-frn            pic  9(02)                  .
               10  pul-arc-plf            pic  9(07)       comp-3     .
               10  pul-dpz-plf            pic  x(04)                  .
               10  pul-tip-ftz            pic  9(02)                  .
               10  pul-tip-ids            pic  9(02)                  .
               10  pul-cod-lng            pic  x(03)                  .
               10  pul-vpf.
                   15  pul-sgl-vpf        pic  x(03)                  .
                   15  pul-dec-vpf        pic  9(01)                  .
                   15  pul-tdc-vpf        pic  x(01)                  .
                   15  pul-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  pul-ass-iva            pic  9(05)       comp-3     .
               10  pul-ctp-ven            pic  9(07)       comp-3     .
               10  pul-fat-sep            pic  x(01)                  .
               10  pul-inl-dcm            pic  9(02)                  .
               10  pul-inl-pgt            pic  9(02)                  .
               10  pul-cod-lst            pic  x(03)                  .
               10  pul-csr-aac            pic  9(05)       comp-3     .
               10  pul-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  pul-csc-aac            pic  9(05)       comp-3     .
               10  pul-psc-aac            pic  9(02)v9(01) comp-3     .
               10  pul-cpv-aac            pic  9(05)       comp-3     .
               10  pul-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-voc-des occurs 06  pic  x(03)                  .
               10  pul-cod-fop            pic  9(07)       comp-3     .
               10  pul-scp-aap            pic  9(02)v9(01) comp-3     .
               10  pul-cod-abi            pic  9(05)       comp-3     .
               10  pul-cod-cab            pic  9(05)       comp-3     .
               10  pul-ccc-app            pic  x(12)                  .
               10  pul-nos-ban            pic  x(10)                  .
               10  pul-nos-ccp            pic  x(10)                  .
               10  pul-add-spi            pic  x(03)                  .
               10  pul-add-spb            pic  x(03)                  .
               10  pul-ipr-iel            pic  9(02)                  .
               10  pul-pag-dsm            pic  9(07)       comp-3     .
               10  pul-pag-qaf            pic  9(09)       comp-3     .
               10  pul-pag-act            pic  9(09)       comp-3     .
               10  pul-cod-age            pic  9(07)       comp-3     .
               10  pul-fsp-doc            pic  9(02)                  .
               10  pul-pvf-age            pic  9(11)       comp-3     .
               10  pul-tip-vpa            pic  9(02)                  .
               10  pul-cpv-aaa            pic  9(05)       comp-3     .
               10  pul-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  pul-cod-ime            pic  9(07)       comp-3     .
               10  pul-pvf-ime            pic  9(11)       comp-3     .
               10  pul-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  pul-tot-scc            pic s9(11)       comp-3     .
               10  pul-per-scc            pic  9(02)v9(01) comp-3     .
               10  pul-civ-scc            pic  9(05)       comp-3     .
               10  pul-ccp-scc            pic  9(07)       comp-3     .
               10  pul-tot-scp            pic s9(11)       comp-3     .
               10  pul-per-scp            pic  9(02)v9(01) comp-3     .
               10  pul-civ-scp            pic  9(05)       comp-3     .
               10  pul-ccp-scp            pic  9(07)       comp-3     .
               10  pul-spe-add occurs 06.
                   15  pul-spe-snx        pic  9(01)                  .
                   15  pul-spe-mad        pic  9(01)                  .
                   15  pul-spe-per        pic  9(02)v9(01) comp-3     .
                   15  pul-spe-ibl        pic  9(02)                  .
                   15  pul-ibt-spe.
                       20  pul-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  pul-spe-imp        pic s9(09)       comp-3     .
                   15  pul-spe-civ        pic  9(05)       comp-3     .
                   15  pul-spe-ccp        pic  9(07)       comp-3     .
               10  pul-civ-spi            pic  9(05)       comp-3     .
               10  pul-ccp-spi            pic  9(07)       comp-3     .
               10  pul-tot-sic            pic s9(09)       comp-3     .
               10  pul-tot-sia            pic s9(09)       comp-3     .
               10  pul-tot-spb            pic s9(09)       comp-3     .
               10  pul-civ-spb            pic  9(05)       comp-3     .
               10  pul-ccp-spb            pic  9(07)       comp-3     .
               10  pul-dtr-tra.
                   15  pul-tra-cur        pic  9(02)                  .
                   15  pul-cau-tra        pic  x(03)                  .
                   15  pul-asp-ben        pic  x(03)                  .
                   15  pul-num-col        pic  9(05)       comp-3     .
                   15  pul-pes-tot        pic  9(06)v9(03) comp-3     .
                   15  pul-dat-itr        pic  9(07)       comp-3     .
                   15  pul-ora-itr        pic  9(04)                  .
                   15  pul-cod-vet        pic  9(07)       comp-3     .
                   15  pul-cod-vt2        pic  9(07)       comp-3     .
                   15  pul-cod-vt3        pic  9(07)       comp-3     .
                   15  pul-cod-ant        pic  9(03)       comp-3     .
               10  pul-prt-mgd            pic  9(07)       comp-3     .
               10  pul-nrg-mgd            pic  9(02)                  .
               10  pul-dri-mgd            pic  9(07)       comp-3     .
               10  pul-nri-mgd            pic  x(10)                  .
               10  pul-nps-sdb            pic  9(11)                  .
               10  pul-ctr-sdb            pic  9(02)                  .
               10  pul-iva-cst.
                   15  pul-iva-rig occurs 06.
                       20  pul-iva-cod    pic  9(05)       comp-3     .
                       20  pul-iva-ibl    pic s9(11)       comp-3     .
                       20  pul-iva-imp    pic s9(11)       comp-3     .
                   15  pul-iva-tdo        pic s9(11)       comp-3     .
               10  pul-ctp-cst.
                   15  pul-ctp-rig occurs 10.
                       20  pul-ctp-cod    pic  9(07)       comp-3     .
                       20  pul-ctp-imp    pic s9(11)       comp-3     .
               10  pul-ctr-stp            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-flg-rfp            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 39   pic  x(01)                  .

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
                     "bit "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bol/fls/ioc/obj/iofbit              "       .

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
           05  k-ctr                      pic  9(02) value 7          .
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
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "IDEDOC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CNTDEN    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZARCDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZTMBDAT "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "FATARC    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    7      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  w-c01                      pic  9(02)                  .
           05  w-c02                      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [bit]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .

      ******************************************************************
       Procedure Division                using f rf-bit               .
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
           move      spaces               to   rf-bit                 .
           move      zero                 to   rf-bit-ide-dat         .
           move      spaces               to   rf-bit-ide-ute         .
           move      spaces               to   rf-bit-ide-fas         .
           move      zero                 to   rf-bit-num-prt         .
           move      spaces               to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-prt-fti         .
           move      zero                 to   rf-bit-prt-mag         .
           move      zero                 to   rf-bit-pr2-mag         .
           move      zero                 to   rf-bit-int-ftr         .
           move      spaces               to   rf-bit-tmo-ftr         .
           move      spaces               to   rf-bit-fat-snx         .
           move      zero                 to   rf-bit-fat-dat         .
           move      zero                 to   rf-bit-fat-num         .
           move      zero                 to   rf-bit-fat-npb         .
           move      spaces               to   rf-bit-snx-acm         .
           move      zero                 to   rf-bit-cod-dpz         .
           move      zero                 to   rf-bit-dat-doc         .
           move      zero                 to   rf-bit-num-doc         .
           move      spaces               to   rf-bit-snx-fac         .
           move      zero                 to   rf-bit-scl-ann         .
           move      spaces               to   rf-bit-sgl-num         .
           move      spaces               to   rf-bit-tip-arc         .
           move      zero                 to   rf-bit-cod-arc         .
           move      spaces               to   rf-bit-dpz-arc         .
           move      zero                 to   rf-bit-tip-frn         .
           move      zero                 to   rf-bit-arc-plf         .
           move      spaces               to   rf-bit-dpz-plf         .
           move      zero                 to   rf-bit-tip-ftz         .
           move      zero                 to   rf-bit-tip-ids         .
           move      spaces               to   rf-bit-cod-lng         .
           move      spaces               to   rf-bit-sgl-vpf         .
           move      zero                 to   rf-bit-dec-vpf         .
           move      spaces               to   rf-bit-tdc-vpf         .
           move      zero                 to   rf-bit-cdc-vpf         .
           move      zero                 to   rf-bit-ass-iva         .
           move      zero                 to   rf-bit-ctp-ven         .
           move      spaces               to   rf-bit-fat-sep         .
           move      zero                 to   rf-bit-inl-dcm         .
           move      zero                 to   rf-bit-inl-pgt         .
           move      spaces               to   rf-bit-cod-lst         .
           move      zero                 to   rf-bit-csr-aac         .
           move      zero                 to   rf-bit-psr-aac (1)     .
           move      zero                 to   rf-bit-psr-aac (2)     .
           move      zero                 to   rf-bit-psr-aac (3)     .
           move      zero                 to   rf-bit-psr-aac (4)     .
           move      zero                 to   rf-bit-psr-aac (5)     .
           move      zero                 to   rf-bit-csc-aac         .
           move      zero                 to   rf-bit-psc-aac         .
           move      zero                 to   rf-bit-cpv-aac         .
           move      zero                 to   rf-bit-ppv-aac (1)     .
           move      zero                 to   rf-bit-ppv-aac (2)     .
           move      zero                 to   rf-bit-ppv-aac (3)     .
           move      spaces               to   rf-bit-voc-des (1)     .
           move      spaces               to   rf-bit-voc-des (2)     .
           move      spaces               to   rf-bit-voc-des (3)     .
           move      spaces               to   rf-bit-voc-des (4)     .
           move      spaces               to   rf-bit-voc-des (5)     .
           move      spaces               to   rf-bit-voc-des (6)     .
           move      zero                 to   rf-bit-cod-fop         .
           move      zero                 to   rf-bit-scp-aap         .
           move      zero                 to   rf-bit-cod-abi         .
           move      zero                 to   rf-bit-cod-cab         .
           move      spaces               to   rf-bit-ccc-app         .
           move      spaces               to   rf-bit-nos-ban         .
           move      spaces               to   rf-bit-nos-ccp         .
           move      spaces               to   rf-bit-add-spi         .
           move      spaces               to   rf-bit-add-spb         .
           move      zero                 to   rf-bit-ipr-iel         .
           move      zero                 to   rf-bit-pag-dsm         .
           move      zero                 to   rf-bit-pag-qaf         .
           move      zero                 to   rf-bit-pag-act         .
           move      zero                 to   rf-bit-cod-age         .
           move      zero                 to   rf-bit-fsp-doc         .
           move      zero                 to   rf-bit-pvf-age         .
           move      zero                 to   rf-bit-tip-vpa         .
           move      zero                 to   rf-bit-cpv-aaa         .
           move      zero                 to   rf-bit-ppv-aaa (1)     .
           move      zero                 to   rf-bit-ppv-aaa (2)     .
           move      zero                 to   rf-bit-ppv-aaa (3)     .
           move      zero                 to   rf-bit-cod-ime         .
           move      zero                 to   rf-bit-pvf-ime         .
           move      zero                 to   rf-bit-tot-rig (1)     .
           move      zero                 to   rf-bit-tot-rig (2)     .
           move      zero                 to   rf-bit-tot-rig (3)     .
           move      zero                 to   rf-bit-tot-rig (4)     .
           move      zero                 to   rf-bit-tot-rig (5)     .
           move      zero                 to   rf-bit-tot-rig (6)     .
           move      zero                 to   rf-bit-tot-rig (7)     .
           move      zero                 to   rf-bit-tot-rig (8)     .
           move      zero                 to   rf-bit-tot-rig (9)     .
           move      zero                 to   rf-bit-tot-scc         .
           move      zero                 to   rf-bit-per-scc         .
           move      zero                 to   rf-bit-civ-scc         .
           move      zero                 to   rf-bit-ccp-scc         .
           move      zero                 to   rf-bit-tot-scp         .
           move      zero                 to   rf-bit-per-scp         .
           move      zero                 to   rf-bit-civ-scp         .
           move      zero                 to   rf-bit-ccp-scp         .
           move      zero                 to   w-c01                  .
       nor-rec-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-240.
           move      zero                 to   rf-bit-spe-snx (w-c01) .
           move      zero                 to   rf-bit-spe-mad (w-c01) .
           move      zero                 to   rf-bit-spe-per (w-c01) .
           move      zero                 to   rf-bit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       nor-rec-log-200.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to nor-rec-log-220.
           move      spaces               to   rf-bit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     nor-rec-log-200.
       nor-rec-log-220.
           move      zero                 to   rf-bit-spe-imp (w-c01) .
           move      zero                 to   rf-bit-spe-civ (w-c01) .
           move      zero                 to   rf-bit-spe-ccp (w-c01) .
           go to     nor-rec-log-100.
       nor-rec-log-240.
           move      zero                 to   rf-bit-civ-spi         .
           move      zero                 to   rf-bit-ccp-spi         .
           move      zero                 to   rf-bit-tot-sic         .
           move      zero                 to   rf-bit-tot-sia         .
           move      zero                 to   rf-bit-tot-spb         .
           move      zero                 to   rf-bit-civ-spb         .
           move      zero                 to   rf-bit-ccp-spb         .
           move      zero                 to   rf-bit-tra-cur         .
           move      spaces               to   rf-bit-cau-tra         .
           move      spaces               to   rf-bit-asp-ben         .
           move      zero                 to   rf-bit-num-col         .
           move      zero                 to   rf-bit-pes-tot         .
           move      zero                 to   rf-bit-dat-itr         .
           move      zero                 to   rf-bit-ora-itr         .
           move      zero                 to   rf-bit-cod-vet         .
           move      zero                 to   rf-bit-cod-vt2         .
           move      zero                 to   rf-bit-cod-vt3         .
           move      zero                 to   rf-bit-cod-ant         .
           move      zero                 to   rf-bit-prt-mgd         .
           move      zero                 to   rf-bit-nrg-mgd         .
           move      zero                 to   rf-bit-dri-mgd         .
           move      spaces               to   rf-bit-nri-mgd         .
           move      zero                 to   rf-bit-nps-sdb         .
           move      zero                 to   rf-bit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       nor-rec-log-300.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-320.
           move      zero                 to   rf-bit-iva-cod (w-c01) .
           move      zero                 to   rf-bit-iva-ibl (w-c01) .
           move      zero                 to   rf-bit-iva-imp (w-c01) .
           go to     nor-rec-log-300.
       nor-rec-log-320.
           move      zero                 to   rf-bit-iva-tdo         .
           move      zero                 to   w-c01                  .
       nor-rec-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to nor-rec-log-420.
           move      zero                 to   rf-bit-ctp-cod (w-c01) .
           move      zero                 to   rf-bit-ctp-imp (w-c01) .
           go to     nor-rec-log-400.
       nor-rec-log-420.
           move      zero                 to   rf-bit-ctr-stp         .
           move      spaces               to   rf-bit-flg-blx (1)     .
           move      spaces               to   rf-bit-flg-blx (2)     .
           move      spaces               to   rf-bit-flg-blx (3)     .
           move      spaces               to   rf-bit-flg-blx (4)     .
           move      spaces               to   rf-bit-flg-blx (5)     .
           move      spaces               to   rf-bit-flg-blx (6)     .
           move      spaces               to   rf-bit-flg-blx (7)     .
           move      spaces               to   rf-bit-flg-nbx (1)     .
           move      spaces               to   rf-bit-flg-nbx (2)     .
           move      spaces               to   rf-bit-flg-nbx (3)     .
           move      spaces               to   rf-bit-flg-pul         .
           move      spaces               to   rf-bit-flg-rfp         .
           move      spaces               to   rf-bit-alx-exp         .
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
           move      rf-bit-ide-ute       to   fil-ide-ute            .
           move      rf-bit-ide-fas       to   fil-ide-fas            .
           move      rf-bit-prt-fti       to   fil-prt-fti            .
           move      rf-bit-prt-mag       to   fil-prt-mag            .
           move      rf-bit-pr2-mag       to   fil-pr2-mag            .
           move      rf-bit-int-ftr       to   fil-int-ftr            .
           move      rf-bit-tmo-ftr       to   fil-tmo-ftr            .
           move      rf-bit-snx-acm       to   fil-snx-acm            .
           move      rf-bit-dpz-arc       to   fil-dpz-arc            .
           move      rf-bit-tip-frn       to   fil-tip-frn            .
           move      rf-bit-arc-plf       to   fil-arc-plf            .
           move      rf-bit-dpz-plf       to   fil-dpz-plf            .
           move      rf-bit-tip-ftz       to   fil-tip-ftz            .
           move      rf-bit-tip-ids       to   fil-tip-ids            .
           move      rf-bit-cod-lng       to   fil-cod-lng            .
           move      rf-bit-sgl-vpf       to   fil-sgl-vpf            .
           move      rf-bit-dec-vpf       to   fil-dec-vpf            .
           move      rf-bit-tdc-vpf       to   fil-tdc-vpf            .
           move      rf-bit-cdc-vpf       to   fil-cdc-vpf            .
           move      rf-bit-ass-iva       to   fil-ass-iva            .
           move      rf-bit-ctp-ven       to   fil-ctp-ven            .
           move      rf-bit-fat-sep       to   fil-fat-sep            .
           move      rf-bit-inl-dcm       to   fil-inl-dcm            .
           move      rf-bit-inl-pgt       to   fil-inl-pgt            .
           move      rf-bit-cod-lst       to   fil-cod-lst            .
           move      rf-bit-csr-aac       to   fil-csr-aac            .
           move      rf-bit-psr-aac (1)   to   fil-psr-aac (1)        .
           move      rf-bit-psr-aac (2)   to   fil-psr-aac (2)        .
           move      rf-bit-psr-aac (3)   to   fil-psr-aac (3)        .
           move      rf-bit-psr-aac (4)   to   fil-psr-aac (4)        .
           move      rf-bit-psr-aac (5)   to   fil-psr-aac (5)        .
           move      rf-bit-csc-aac       to   fil-csc-aac            .
           move      rf-bit-psc-aac       to   fil-psc-aac            .
           move      rf-bit-cpv-aac       to   fil-cpv-aac            .
           move      rf-bit-ppv-aac (1)   to   fil-ppv-aac (1)        .
           move      rf-bit-ppv-aac (2)   to   fil-ppv-aac (2)        .
           move      rf-bit-ppv-aac (3)   to   fil-ppv-aac (3)        .
           move      rf-bit-voc-des (1)   to   fil-voc-des (1)        .
           move      rf-bit-voc-des (2)   to   fil-voc-des (2)        .
           move      rf-bit-voc-des (3)   to   fil-voc-des (3)        .
           move      rf-bit-voc-des (4)   to   fil-voc-des (4)        .
           move      rf-bit-voc-des (5)   to   fil-voc-des (5)        .
           move      rf-bit-voc-des (6)   to   fil-voc-des (6)        .
           move      rf-bit-cod-fop       to   fil-cod-fop            .
           move      rf-bit-scp-aap       to   fil-scp-aap            .
           move      rf-bit-cod-abi       to   fil-cod-abi            .
           move      rf-bit-cod-cab       to   fil-cod-cab            .
           move      rf-bit-ccc-app       to   fil-ccc-app            .
           move      rf-bit-nos-ban       to   fil-nos-ban            .
           move      rf-bit-nos-ccp       to   fil-nos-ccp            .
           move      rf-bit-add-spi       to   fil-add-spi            .
           move      rf-bit-add-spb       to   fil-add-spb            .
           move      rf-bit-ipr-iel       to   fil-ipr-iel            .
           move      rf-bit-pag-dsm       to   fil-pag-dsm            .
           move      rf-bit-pag-qaf       to   fil-pag-qaf            .
           move      rf-bit-pag-act       to   fil-pag-act            .
           move      rf-bit-cod-age       to   fil-cod-age            .
           move      rf-bit-fsp-doc       to   fil-fsp-doc            .
           move      rf-bit-pvf-age       to   fil-pvf-age            .
           move      rf-bit-tip-vpa       to   fil-tip-vpa            .
           move      rf-bit-cpv-aaa       to   fil-cpv-aaa            .
           move      rf-bit-ppv-aaa (1)   to   fil-ppv-aaa (1)        .
           move      rf-bit-ppv-aaa (2)   to   fil-ppv-aaa (2)        .
           move      rf-bit-ppv-aaa (3)   to   fil-ppv-aaa (3)        .
           move      rf-bit-cod-ime       to   fil-cod-ime            .
           move      rf-bit-pvf-ime       to   fil-pvf-ime            .
           move      rf-bit-tot-rig (1)   to   fil-tot-rig (1)        .
           move      rf-bit-tot-rig (2)   to   fil-tot-rig (2)        .
           move      rf-bit-tot-rig (3)   to   fil-tot-rig (3)        .
           move      rf-bit-tot-rig (4)   to   fil-tot-rig (4)        .
           move      rf-bit-tot-rig (5)   to   fil-tot-rig (5)        .
           move      rf-bit-tot-rig (6)   to   fil-tot-rig (6)        .
           move      rf-bit-tot-rig (7)   to   fil-tot-rig (7)        .
           move      rf-bit-tot-rig (8)   to   fil-tot-rig (8)        .
           move      rf-bit-tot-rig (9)   to   fil-tot-rig (9)        .
           move      rf-bit-tot-scc       to   fil-tot-scc            .
           move      rf-bit-per-scc       to   fil-per-scc            .
           move      rf-bit-civ-scc       to   fil-civ-scc            .
           move      rf-bit-ccp-scc       to   fil-ccp-scc            .
           move      rf-bit-tot-scp       to   fil-tot-scp            .
           move      rf-bit-per-scp       to   fil-per-scp            .
           move      rf-bit-civ-scp       to   fil-civ-scp            .
           move      rf-bit-ccp-scp       to   fil-ccp-scp            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-340.
           move      rf-bit-spe-snx (w-c01)
                                          to   fil-spe-snx (w-c01)    .
           move      rf-bit-spe-mad (w-c01)
                                          to   fil-spe-mad (w-c01)    .
           move      rf-bit-spe-per (w-c01)
                                          to   fil-spe-per (w-c01)    .
           move      rf-bit-spe-ibl (w-c01)
                                          to   fil-spe-ibl (w-c01)    .
           move      zero                 to   w-c02                  .
       cmp-log-fis-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to cmp-log-fis-320.
           move      rf-bit-ibx-spe
                     (w-c01, w-c02)       to   fil-ibx-spe
                                              (w-c01, w-c02)          .
           go to     cmp-log-fis-300.
       cmp-log-fis-320.
           move      rf-bit-spe-imp (w-c01)
                                          to   fil-spe-imp (w-c01)    .
           move      rf-bit-spe-civ (w-c01)
                                          to   fil-spe-civ (w-c01)    .
           move      rf-bit-spe-ccp (w-c01)
                                          to   fil-spe-ccp (w-c01)    .
           go to     cmp-log-fis-200.
       cmp-log-fis-340.
           move      rf-bit-civ-spi       to   fil-civ-spi            .
           move      rf-bit-ccp-spi       to   fil-ccp-spi            .
           move      rf-bit-tot-sic       to   fil-tot-sic            .
           move      rf-bit-tot-sia       to   fil-tot-sia            .
           move      rf-bit-tot-spb       to   fil-tot-spb            .
           move      rf-bit-civ-spb       to   fil-civ-spb            .
           move      rf-bit-ccp-spb       to   fil-ccp-spb            .
           move      rf-bit-tra-cur       to   fil-tra-cur            .
           move      rf-bit-cau-tra       to   fil-cau-tra            .
           move      rf-bit-asp-ben       to   fil-asp-ben            .
           move      rf-bit-num-col       to   fil-num-col            .
           move      rf-bit-pes-tot       to   fil-pes-tot            .
           move      rf-bit-dat-itr       to   fil-dat-itr            .
           move      rf-bit-ora-itr       to   fil-ora-itr            .
           move      rf-bit-cod-vet       to   fil-cod-vet            .
           move      rf-bit-cod-vt2       to   fil-cod-vt2            .
           move      rf-bit-cod-vt3       to   fil-cod-vt3            .
           move      rf-bit-cod-ant       to   fil-cod-ant            .
           move      rf-bit-prt-mgd       to   fil-prt-mgd            .
           move      rf-bit-nrg-mgd       to   fil-nrg-mgd            .
           move      rf-bit-dri-mgd       to   fil-dri-mgd            .
           move      rf-bit-nri-mgd       to   fil-nri-mgd            .
           move      rf-bit-nps-sdb       to   fil-nps-sdb            .
           move      rf-bit-ctr-sdb       to   fil-ctr-sdb            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-420.
           move      rf-bit-iva-cod (w-c01)
                                          to   fil-iva-cod (w-c01)    .
           move      rf-bit-iva-ibl (w-c01)
                                          to   fil-iva-ibl (w-c01)    .
           move      rf-bit-iva-imp (w-c01)
                                          to   fil-iva-imp (w-c01)    .
           go to     cmp-log-fis-400.
       cmp-log-fis-420.
           move      rf-bit-iva-tdo       to   fil-iva-tdo            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to cmp-log-fis-520.
           move      rf-bit-ctp-cod (w-c01)
                                          to   fil-ctp-cod (w-c01)    .
           move      rf-bit-ctp-imp (w-c01)
                                          to   fil-ctp-imp (w-c01)    .
           go to     cmp-log-fis-500.
       cmp-log-fis-520.
           move      rf-bit-ctr-stp       to   fil-ctr-stp            .
           move      rf-bit-flg-blx (1)   to   fil-flg-blx (1)        .
           move      rf-bit-flg-blx (2)   to   fil-flg-blx (2)        .
           move      rf-bit-flg-blx (3)   to   fil-flg-blx (3)        .
           move      rf-bit-flg-blx (4)   to   fil-flg-blx (4)        .
           move      rf-bit-flg-blx (5)   to   fil-flg-blx (5)        .
           move      rf-bit-flg-blx (6)   to   fil-flg-blx (6)        .
           move      rf-bit-flg-blx (7)   to   fil-flg-blx (7)        .
           move      rf-bit-flg-nbx (1)   to   fil-flg-nbx (1)        .
           move      rf-bit-flg-nbx (2)   to   fil-flg-nbx (2)        .
           move      rf-bit-flg-nbx (3)   to   fil-flg-nbx (3)        .
           move      rf-bit-flg-pul       to   fil-flg-pul            .
           move      rf-bit-flg-rfp       to   fil-flg-rfp            .
           move      rf-bit-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-bit-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-bit-ide-dat       to   fil-ide-dat            .
           move      rf-bit-dat-doc       to   fil-dat-doc            .
           move      rf-bit-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-bit-cod-dpz       to   fil-cod-dpz            .
           move      rf-bit-cod-tmb       to   fil-cod-tmb            .
           move      rf-bit-dat-doc       to   fil-dat-doc-3          .
           move      rf-bit-num-doc       to   fil-num-doc            .
           move      rf-bit-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-bit-snx-fac       to   fil-snx-fac            .
           move      rf-bit-scl-ann       to   fil-scl-ann            .
           move      rf-bit-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-bit-sgl-num       to   fil-sgl-num            .
           move      rf-bit-num-doc       to   fil-num-doc-4          .
           move      rf-bit-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-bit-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-bit-tip-arc       to   fil-tip-arc            .
           move      rf-bit-cod-arc       to   fil-cod-arc            .
           move      rf-bit-dat-doc       to   fil-dat-doc-5          .
           move      rf-bit-num-doc       to   fil-num-doc-5          .
           move      rf-bit-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-bit-cod-dpz       to   fil-cod-dpz-6          .
           move      rf-bit-cod-tmb       to   fil-cod-tmb-6          .
           move      rf-bit-dat-doc       to   fil-dat-doc-6          .
           move      rf-bit-num-doc       to   fil-num-doc-6          .
           move      rf-bit-num-prt       to   fil-num-prt-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-bit-fat-snx       to   fil-fat-snx            .
           move      rf-bit-fat-dat       to   fil-fat-dat            .
           move      rf-bit-fat-num       to   fil-fat-num            .
           move      rf-bit-fat-npb       to   fil-fat-npb            .
           move      rf-bit-tip-arc       to   fil-tip-arc-7          .
           move      rf-bit-arc-plf       to   fil-arc-plf-7          .
           move      rf-bit-num-prt       to   fil-num-prt-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-bit                 .
           move      fil-ide-dat          to   rf-bit-ide-dat         .
           move      fil-ide-ute          to   rf-bit-ide-ute         .
           move      fil-ide-fas          to   rf-bit-ide-fas         .
           move      fil-num-prt          to   rf-bit-num-prt         .
           move      fil-cod-tmb          to   rf-bit-cod-tmb         .
           move      fil-prt-fti          to   rf-bit-prt-fti         .
           move      fil-prt-mag          to   rf-bit-prt-mag         .
           move      fil-pr2-mag          to   rf-bit-pr2-mag         .
           move      fil-int-ftr          to   rf-bit-int-ftr         .
           move      fil-tmo-ftr          to   rf-bit-tmo-ftr         .
           move      fil-fat-snx          to   rf-bit-fat-snx         .
           move      fil-fat-dat          to   rf-bit-fat-dat         .
           move      fil-fat-num          to   rf-bit-fat-num         .
           move      fil-fat-npb          to   rf-bit-fat-npb         .
           move      fil-snx-acm          to   rf-bit-snx-acm         .
           move      fil-cod-dpz          to   rf-bit-cod-dpz         .
           move      fil-dat-doc          to   rf-bit-dat-doc         .
           move      fil-num-doc          to   rf-bit-num-doc         .
           move      fil-snx-fac          to   rf-bit-snx-fac         .
           move      fil-scl-ann          to   rf-bit-scl-ann         .
           move      fil-sgl-num          to   rf-bit-sgl-num         .
           move      fil-tip-arc          to   rf-bit-tip-arc         .
           move      fil-cod-arc          to   rf-bit-cod-arc         .
           move      fil-dpz-arc          to   rf-bit-dpz-arc         .
           move      fil-tip-frn          to   rf-bit-tip-frn         .
           move      fil-arc-plf          to   rf-bit-arc-plf         .
           move      fil-dpz-plf          to   rf-bit-dpz-plf         .
           move      fil-tip-ftz          to   rf-bit-tip-ftz         .
           move      fil-tip-ids          to   rf-bit-tip-ids         .
           move      fil-cod-lng          to   rf-bit-cod-lng         .
           move      fil-sgl-vpf          to   rf-bit-sgl-vpf         .
           move      fil-dec-vpf          to   rf-bit-dec-vpf         .
           move      fil-tdc-vpf          to   rf-bit-tdc-vpf         .
           move      fil-cdc-vpf          to   rf-bit-cdc-vpf         .
           move      fil-ass-iva          to   rf-bit-ass-iva         .
           move      fil-ctp-ven          to   rf-bit-ctp-ven         .
           move      fil-fat-sep          to   rf-bit-fat-sep         .
           move      fil-inl-dcm          to   rf-bit-inl-dcm         .
           move      fil-inl-pgt          to   rf-bit-inl-pgt         .
           move      fil-cod-lst          to   rf-bit-cod-lst         .
           move      fil-csr-aac          to   rf-bit-csr-aac         .
           move      fil-psr-aac (1)      to   rf-bit-psr-aac (1)     .
           move      fil-psr-aac (2)      to   rf-bit-psr-aac (2)     .
           move      fil-psr-aac (3)      to   rf-bit-psr-aac (3)     .
           move      fil-psr-aac (4)      to   rf-bit-psr-aac (4)     .
           move      fil-psr-aac (5)      to   rf-bit-psr-aac (5)     .
           move      fil-csc-aac          to   rf-bit-csc-aac         .
           move      fil-psc-aac          to   rf-bit-psc-aac         .
           move      fil-cpv-aac          to   rf-bit-cpv-aac         .
           move      fil-ppv-aac (1)      to   rf-bit-ppv-aac (1)     .
           move      fil-ppv-aac (2)      to   rf-bit-ppv-aac (2)     .
           move      fil-ppv-aac (3)      to   rf-bit-ppv-aac (3)     .
           move      fil-voc-des (1)      to   rf-bit-voc-des (1)     .
           move      fil-voc-des (2)      to   rf-bit-voc-des (2)     .
           move      fil-voc-des (3)      to   rf-bit-voc-des (3)     .
           move      fil-voc-des (4)      to   rf-bit-voc-des (4)     .
           move      fil-voc-des (5)      to   rf-bit-voc-des (5)     .
           move      fil-voc-des (6)      to   rf-bit-voc-des (6)     .
           move      fil-cod-fop          to   rf-bit-cod-fop         .
           move      fil-scp-aap          to   rf-bit-scp-aap         .
           move      fil-cod-abi          to   rf-bit-cod-abi         .
           move      fil-cod-cab          to   rf-bit-cod-cab         .
           move      fil-ccc-app          to   rf-bit-ccc-app         .
           move      fil-nos-ban          to   rf-bit-nos-ban         .
           move      fil-nos-ccp          to   rf-bit-nos-ccp         .
           move      fil-add-spi          to   rf-bit-add-spi         .
           move      fil-add-spb          to   rf-bit-add-spb         .
           move      fil-ipr-iel          to   rf-bit-ipr-iel         .
           move      fil-pag-dsm          to   rf-bit-pag-dsm         .
           move      fil-pag-qaf          to   rf-bit-pag-qaf         .
           move      fil-pag-act          to   rf-bit-pag-act         .
           move      fil-cod-age          to   rf-bit-cod-age         .
           move      fil-fsp-doc          to   rf-bit-fsp-doc         .
           move      fil-pvf-age          to   rf-bit-pvf-age         .
           move      fil-tip-vpa          to   rf-bit-tip-vpa         .
           move      fil-cpv-aaa          to   rf-bit-cpv-aaa         .
           move      fil-ppv-aaa (1)      to   rf-bit-ppv-aaa (1)     .
           move      fil-ppv-aaa (2)      to   rf-bit-ppv-aaa (2)     .
           move      fil-ppv-aaa (3)      to   rf-bit-ppv-aaa (3)     .
           move      fil-cod-ime          to   rf-bit-cod-ime         .
           move      fil-pvf-ime          to   rf-bit-pvf-ime         .
           move      fil-tot-rig (1)      to   rf-bit-tot-rig (1)     .
           move      fil-tot-rig (2)      to   rf-bit-tot-rig (2)     .
           move      fil-tot-rig (3)      to   rf-bit-tot-rig (3)     .
           move      fil-tot-rig (4)      to   rf-bit-tot-rig (4)     .
           move      fil-tot-rig (5)      to   rf-bit-tot-rig (5)     .
           move      fil-tot-rig (6)      to   rf-bit-tot-rig (6)     .
           move      fil-tot-rig (7)      to   rf-bit-tot-rig (7)     .
           move      fil-tot-rig (8)      to   rf-bit-tot-rig (8)     .
           move      fil-tot-rig (9)      to   rf-bit-tot-rig (9)     .
           move      fil-tot-scc          to   rf-bit-tot-scc         .
           move      fil-per-scc          to   rf-bit-per-scc         .
           move      fil-civ-scc          to   rf-bit-civ-scc         .
           move      fil-ccp-scc          to   rf-bit-ccp-scc         .
           move      fil-tot-scp          to   rf-bit-tot-scp         .
           move      fil-per-scp          to   rf-bit-per-scp         .
           move      fil-civ-scp          to   rf-bit-civ-scp         .
           move      fil-ccp-scp          to   rf-bit-ccp-scp         .
           move      zero                 to   w-c01                  .
       dec-fis-log-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-340.
           move      fil-spe-snx (w-c01)  to   rf-bit-spe-snx (w-c01) .
           move      fil-spe-mad (w-c01)  to   rf-bit-spe-mad (w-c01) .
           move      fil-spe-per (w-c01)  to   rf-bit-spe-per (w-c01) .
           move      fil-spe-ibl (w-c01)  to   rf-bit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       dec-fis-log-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to dec-fis-log-320.
           move      fil-ibx-spe
                    (w-c01, w-c02)        to   rf-bit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      fil-spe-imp (w-c01)  to   rf-bit-spe-imp (w-c01) .
           move      fil-spe-civ (w-c01)  to   rf-bit-spe-civ (w-c01) .
           move      fil-spe-ccp (w-c01)  to   rf-bit-spe-ccp (w-c01) .
           go to     dec-fis-log-200.
       dec-fis-log-340.
           move      fil-civ-spi          to   rf-bit-civ-spi         .
           move      fil-ccp-spi          to   rf-bit-ccp-spi         .
           move      fil-tot-sic          to   rf-bit-tot-sic         .
           move      fil-tot-sia          to   rf-bit-tot-sia         .
           move      fil-tot-spb          to   rf-bit-tot-spb         .
           move      fil-civ-spb          to   rf-bit-civ-spb         .
           move      fil-ccp-spb          to   rf-bit-ccp-spb         .
           move      fil-tra-cur          to   rf-bit-tra-cur         .
           move      fil-cau-tra          to   rf-bit-cau-tra         .
           move      fil-asp-ben          to   rf-bit-asp-ben         .
           move      fil-num-col          to   rf-bit-num-col         .
           move      fil-pes-tot          to   rf-bit-pes-tot         .
           move      fil-dat-itr          to   rf-bit-dat-itr         .
           move      fil-ora-itr          to   rf-bit-ora-itr         .
           move      fil-cod-vet          to   rf-bit-cod-vet         .
           move      fil-cod-vt2          to   rf-bit-cod-vt2         .
           move      fil-cod-vt3          to   rf-bit-cod-vt3         .
           move      fil-cod-ant          to   rf-bit-cod-ant         .
           move      fil-prt-mgd          to   rf-bit-prt-mgd         .
           move      fil-nrg-mgd          to   rf-bit-nrg-mgd         .
           move      fil-dri-mgd          to   rf-bit-dri-mgd         .
           move      fil-nri-mgd          to   rf-bit-nri-mgd         .
           move      fil-nps-sdb          to   rf-bit-nps-sdb         .
           move      fil-ctr-sdb          to   rf-bit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       dec-fis-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-420.
           move      fil-iva-cod (w-c01)  to   rf-bit-iva-cod (w-c01) .
           move      fil-iva-ibl (w-c01)  to   rf-bit-iva-ibl (w-c01) .
           move      fil-iva-imp (w-c01)  to   rf-bit-iva-imp (w-c01) .
           go to     dec-fis-log-400.
       dec-fis-log-420.
           move      fil-iva-tdo          to   rf-bit-iva-tdo         .
           move      zero                 to   w-c01                  .
       dec-fis-log-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to dec-fis-log-520.
           move      fil-ctp-cod (w-c01)  to   rf-bit-ctp-cod (w-c01) .
           move      fil-ctp-imp (w-c01)  to   rf-bit-ctp-imp (w-c01) .
           go to     dec-fis-log-500.
       dec-fis-log-520.
           move      fil-ctr-stp          to   rf-bit-ctr-stp         .
           move      fil-flg-blx (1)      to   rf-bit-flg-blx (1)     .
           move      fil-flg-blx (2)      to   rf-bit-flg-blx (2)     .
           move      fil-flg-blx (3)      to   rf-bit-flg-blx (3)     .
           move      fil-flg-blx (4)      to   rf-bit-flg-blx (4)     .
           move      fil-flg-blx (5)      to   rf-bit-flg-blx (5)     .
           move      fil-flg-blx (6)      to   rf-bit-flg-blx (6)     .
           move      fil-flg-blx (7)      to   rf-bit-flg-blx (7)     .
           move      fil-flg-nbx (1)      to   rf-bit-flg-nbx (1)     .
           move      fil-flg-nbx (2)      to   rf-bit-flg-nbx (2)     .
           move      fil-flg-nbx (3)      to   rf-bit-flg-nbx (3)     .
           move      fil-flg-pul          to   rf-bit-flg-pul         .
           move      fil-flg-rfp          to   rf-bit-flg-rfp         .
           move      fil-alx-exp          to   rf-bit-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-bit               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-bit
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

