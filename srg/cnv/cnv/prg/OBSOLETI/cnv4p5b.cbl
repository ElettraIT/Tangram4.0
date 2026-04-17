       Identification Division.
       Program-Id.                                 cnv4p5b            .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    cnv                 *
      *                                   Fase:    cnv4p5b             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 15/04/05    *
      *                       Ultima revisione:    NdK del 15/04/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per versione 4.5 bis            *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Aggiornamento file [bft] : 15/04/05         *
      *                    Aggiornamento file [bit] : 15/04/05         *
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
      *    * File Control [bft]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bft   assign to disk           f-bft-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bft-k01
                   alternate record key   is bft-k02
                   alternate record key   is bft-k03
                   alternate record key   is bft-k04
                   alternate record key   is bft-k05
                   alternate record key   is bft-k06
                             file status  is                f-bft-sts .

      *    *===========================================================*
      *    * File Control [bit]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bit   assign to disk           f-bit-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bit-k01
                   alternate record key   is bit-k02
                   alternate record key   is bit-k03
                   alternate record key   is bit-k04
                   alternate record key   is bit-k05
                   alternate record key   is bit-k06
                   alternate record key   is bit-k07
                             file status  is                f-bit-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [bft]                                    *
      *    *-----------------------------------------------------------*
       fd  bft       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bft-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bft-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bft-k01.
                   15  bft-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  bft-k02.
                   15  bft-ide-dat        pic  9(07)       comp-3     .
                   15  bft-dat-reg        pic  9(07)       comp-3     .
                   15  bft-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  bft-k03.
                   15  bft-dat-reg-3      pic  9(07)       comp-3     .
                   15  bft-cod-dpz        pic  9(02)                  .
                   15  bft-cod-tmb        pic  x(05)                  .
                   15  bft-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  bft-k04.
                   15  bft-cod-dpz-4      pic  9(02)                  .
                   15  bft-tip-arc        pic  x(01)                  .
                   15  bft-cod-arc        pic  9(07)       comp-3     .
                   15  bft-dat-reg-4      pic  9(07)       comp-3     .
                   15  bft-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  bft-k05.
                   15  bft-cod-dpz-5      pic  9(02)                  .
                   15  bft-cod-tmb-5      pic  x(05)                  .
                   15  bft-dat-reg-5      pic  9(07)       comp-3     .
                   15  bft-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZBCH                         *
      *            *---------------------------------------------------*
               10  bft-k06.
                   15  bft-cod-dpz-6      pic  9(02)                  .
                   15  bft-flg-bch        pic  x(01)                  .
                   15  bft-num-prt-6      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bft-dat.
               10  bft-ide-ute            pic  x(08)                  .
               10  bft-ide-fas            pic  x(06)                  .
               10  bft-prt-ftf            pic  9(11)       comp-3     .
               10  bft-prt-mag            pic  9(11)       comp-3     .
               10  bft-pr2-mag            pic  9(11)       comp-3     .
               10  bft-int-ftr            pic  9(02)                  .
               10  bft-tmo-ftr            pic  x(05)                  .
               10  bft-dpz-arc            pic  x(04)                  .
               10  bft-arc-plf            pic  9(07)       comp-3     .
               10  bft-dpz-plf            pic  x(04)                  .
               10  bft-tip-ids            pic  9(02)                  .
               10  bft-dat-doc            pic  9(07)       comp-3     .
               10  bft-num-doc            pic  x(10)                  .
               10  bft-cod-lng            pic  x(03)                  .
               10  bft-vpf.
                   15  bft-sgl-vpf        pic  x(03)                  .
                   15  bft-dec-vpf        pic  9(01)                  .
                   15  bft-tdc-vpf        pic  x(01)                  .
                   15  bft-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bft-ass-iva            pic  9(05)       comp-3     .
               10  bft-ctp-acq            pic  9(07)       comp-3     .
               10  bft-inl-pgt            pic  9(02)                  .
               10  bft-cod-lst            pic  x(03)                  .
               10  bft-csr-aaf            pic  9(05)       comp-3     .
               10  bft-psr-aaf occurs 05  pic  9(02)v9(01) comp-3     .
               10  bft-csc-aaf            pic  9(05)       comp-3     .
               10  bft-psc-aaf            pic  9(02)v9(01) comp-3     .
               10  bft-voc-des occurs 06  pic  x(03)                  .
               10  bft-cod-fop            pic  9(07)       comp-3     .
               10  bft-scp-aap            pic  9(02)v9(01) comp-3     .
               10  bft-nos-ban            pic  x(10)                  .
               10  bft-cod-abi            pic  9(05)       comp-3     .
               10  bft-cod-cab            pic  9(05)       comp-3     .
               10  bft-ccc-app            pic  x(12)                  .
               10  bft-ccp-app            pic  x(12)                  .
               10  bft-add-spi            pic  x(03)                  .
               10  bft-add-spb            pic  x(03)                  .
               10  bft-ipr-iel            pic  9(02)                  .
               10  bft-pag-dsm            pic  9(07)       comp-3     .
               10  bft-pag-qaf            pic  9(09)       comp-3     .
               10  bft-pag-act            pic  9(09)       comp-3     .
               10  bft-cod-aqt            pic  9(07)       comp-3     .
               10  bft-pvf-aqt            pic  9(11)       comp-3     .
               10  bft-cod-ime            pic  9(07)       comp-3     .
               10  bft-pvf-ime            pic  9(11)       comp-3     .
               10  bft-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  bft-tot-scc            pic s9(11)       comp-3     .
               10  bft-per-scc            pic  9(02)v9(01) comp-3     .
               10  bft-civ-scc            pic  9(05)       comp-3     .
               10  bft-ccp-scc            pic  9(07)       comp-3     .
               10  bft-tot-scp            pic s9(11)       comp-3     .
               10  bft-per-scp            pic  9(02)v9(01) comp-3     .
               10  bft-civ-scp            pic  9(05)       comp-3     .
               10  bft-ccp-scp            pic  9(07)       comp-3     .
               10  bft-spe-add occurs 06.
                   15  bft-spe-snx        pic  9(01)                  .
                   15  bft-spe-mad        pic  9(01)                  .
                   15  bft-spe-per        pic  9(02)v9(01) comp-3     .
                   15  bft-spe-ibl        pic  9(02)                  .
                   15  bft-ibt-spe.
                       20  bft-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  bft-spe-imp        pic s9(09)       comp-3     .
                   15  bft-spe-civ        pic  9(05)       comp-3     .
                   15  bft-spe-ccp        pic  9(07)       comp-3     .
               10  bft-civ-spi            pic  9(05)       comp-3     .
               10  bft-ccp-spi            pic  9(07)       comp-3     .
               10  bft-tot-sic            pic s9(09)       comp-3     .
               10  bft-tot-sia            pic s9(09)       comp-3     .
               10  bft-tot-spb            pic s9(09)       comp-3     .
               10  bft-civ-spb            pic  9(05)       comp-3     .
               10  bft-ccp-spb            pic  9(07)       comp-3     .
               10  bft-prt-mgd            pic  9(07)       comp-3     .
               10  bft-nrg-mgd            pic  9(02)                  .
               10  bft-dri-mgd            pic  9(07)       comp-3     .
               10  bft-nri-mgd            pic  x(10)                  .
               10  bft-nps-sdb            pic  9(11)                  .
               10  bft-ctr-sdb            pic  9(02)                  .
               10  bft-iva-cst.
                   15  bft-iva-rig occurs 06.
                       20  bft-iva-cod    pic  9(05)       comp-3     .
                       20  bft-iva-ibl    pic s9(11)       comp-3     .
               10  bft-ctp-cst.
                   15  bft-ctp-rig occurs 10.
                       20  bft-ctp-cod    pic  9(07)       comp-3     .
                       20  bft-ctp-imp    pic s9(11)       comp-3     .
               10  bft-flg-ela.
                   15  bft-flg-blo.
                       20  bft-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bft-flg-nbl.
                       20  bft-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bft-flg-pul            pic  x(01)                  .
               10  bft-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [bit]                                    *
      *    *-----------------------------------------------------------*
       fd  bit       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bit-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bit-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bit-k01.
                   15  bit-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  bit-k02.
                   15  bit-ide-dat        pic  9(07)       comp-3     .
                   15  bit-dat-doc        pic  9(07)       comp-3     .
                   15  bit-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  bit-k03.
                   15  bit-dat-doc-3      pic  9(07)       comp-3     .
                   15  bit-cod-dpz        pic  9(02)                  .
                   15  bit-num-doc        pic  9(11)       comp-3     .
                   15  bit-cod-tmb        pic  x(05)                  .
                   15  bit-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  bit-k04.
                   15  bit-snx-fac        pic  x(01)                  .
                   15  bit-scl-ann        pic  9(03)       comp-3     .
                   15  bit-cod-dpz-4      pic  9(02)                  .
                   15  bit-sgl-num        pic  x(03)                  .
                   15  bit-num-doc-4      pic  9(11)       comp-3     .
                   15  bit-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  bit-k05.
                   15  bit-cod-dpz-5      pic  9(02)                  .
                   15  bit-tip-arc        pic  x(01)                  .
                   15  bit-cod-arc        pic  9(07)       comp-3     .
                   15  bit-dat-doc-5      pic  9(07)       comp-3     .
                   15  bit-num-doc-5      pic  9(11)       comp-3     .
                   15  bit-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  bit-k06.
                   15  bit-cod-dpz-6      pic  9(02)                  .
                   15  bit-cod-tmb-6      pic  x(05)                  .
                   15  bit-dat-doc-6      pic  9(07)       comp-3     .
                   15  bit-num-doc-6      pic  9(11)       comp-3     .
                   15  bit-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : FATARC                         *
      *            *---------------------------------------------------*
               10  bit-k07.
                   15  bit-fat-snx        pic  x(01)                  .
                   15  bit-fat-dat        pic  9(07)       comp-3     .
                   15  bit-fat-num        pic  9(11)       comp-3     .
                   15  bit-fat-npb        pic  9(03)       comp-3     .
                   15  bit-tip-arc-7      pic  x(01)                  .
                   15  bit-arc-plf-7      pic  9(07)       comp-3     .
                   15  bit-num-prt-7      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bit-dat.
               10  bit-ide-ute            pic  x(08)                  .
               10  bit-ide-fas            pic  x(06)                  .
               10  bit-prt-fti            pic  9(11)       comp-3     .
               10  bit-prt-mag            pic  9(11)       comp-3     .
               10  bit-pr2-mag            pic  9(11)       comp-3     .
               10  bit-int-ftr            pic  9(02)                  .
               10  bit-tmo-ftr            pic  x(05)                  .
               10  bit-snx-acm            pic  x(01)                  .
               10  bit-dpz-arc            pic  x(04)                  .
               10  bit-tip-frn            pic  9(02)                  .
               10  bit-arc-plf            pic  9(07)       comp-3     .
               10  bit-dpz-plf            pic  x(04)                  .
               10  bit-tip-ftz            pic  9(02)                  .
               10  bit-tip-ids            pic  9(02)                  .
               10  bit-cod-lng            pic  x(03)                  .
               10  bit-vpf.
                   15  bit-sgl-vpf        pic  x(03)                  .
                   15  bit-dec-vpf        pic  9(01)                  .
                   15  bit-tdc-vpf        pic  x(01)                  .
                   15  bit-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bit-ass-iva            pic  9(05)       comp-3     .
               10  bit-ctp-ven            pic  9(07)       comp-3     .
               10  bit-fat-sep            pic  x(01)                  .
               10  bit-inl-dcm            pic  9(02)                  .
               10  bit-inl-pgt            pic  9(02)                  .
               10  bit-cod-lst            pic  x(03)                  .
               10  bit-csr-aac            pic  9(05)       comp-3     .
               10  bit-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  bit-csc-aac            pic  9(05)       comp-3     .
               10  bit-psc-aac            pic  9(02)v9(01) comp-3     .
               10  bit-cpv-aac            pic  9(05)       comp-3     .
               10  bit-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  bit-voc-des occurs 06  pic  x(03)                  .
               10  bit-cod-fop            pic  9(07)       comp-3     .
               10  bit-scp-aap            pic  9(02)v9(01) comp-3     .
               10  bit-cod-abi            pic  9(05)       comp-3     .
               10  bit-cod-cab            pic  9(05)       comp-3     .
               10  bit-ccc-app            pic  x(12)                  .
               10  bit-nos-ban            pic  x(10)                  .
               10  bit-nos-ccp            pic  x(10)                  .
               10  bit-add-spi            pic  x(03)                  .
               10  bit-add-spb            pic  x(03)                  .
               10  bit-ipr-iel            pic  9(02)                  .
               10  bit-pag-dsm            pic  9(07)       comp-3     .
               10  bit-pag-qaf            pic  9(09)       comp-3     .
               10  bit-pag-act            pic  9(09)       comp-3     .
               10  bit-cod-age            pic  9(07)       comp-3     .
               10  bit-fsp-doc            pic  9(02)                  .
               10  bit-pvf-age            pic  9(11)       comp-3     .
               10  bit-tip-vpa            pic  9(02)                  .
               10  bit-cpv-aaa            pic  9(05)       comp-3     .
               10  bit-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  bit-cod-ime            pic  9(07)       comp-3     .
               10  bit-pvf-ime            pic  9(11)       comp-3     .
               10  bit-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  bit-tot-scc            pic s9(11)       comp-3     .
               10  bit-per-scc            pic  9(02)v9(01) comp-3     .
               10  bit-civ-scc            pic  9(05)       comp-3     .
               10  bit-ccp-scc            pic  9(07)       comp-3     .
               10  bit-tot-scp            pic s9(11)       comp-3     .
               10  bit-per-scp            pic  9(02)v9(01) comp-3     .
               10  bit-civ-scp            pic  9(05)       comp-3     .
               10  bit-ccp-scp            pic  9(07)       comp-3     .
               10  bit-spe-add occurs 06.
                   15  bit-spe-snx        pic  9(01)                  .
                   15  bit-spe-mad        pic  9(01)                  .
                   15  bit-spe-per        pic  9(02)v9(01) comp-3     .
                   15  bit-spe-ibl        pic  9(02)                  .
                   15  bit-ibt-spe.
                       20  bit-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  bit-spe-imp        pic s9(09)       comp-3     .
                   15  bit-spe-civ        pic  9(05)       comp-3     .
                   15  bit-spe-ccp        pic  9(07)       comp-3     .
               10  bit-civ-spi            pic  9(05)       comp-3     .
               10  bit-ccp-spi            pic  9(07)       comp-3     .
               10  bit-tot-sic            pic s9(09)       comp-3     .
               10  bit-tot-sia            pic s9(09)       comp-3     .
               10  bit-tot-spb            pic s9(09)       comp-3     .
               10  bit-civ-spb            pic  9(05)       comp-3     .
               10  bit-ccp-spb            pic  9(07)       comp-3     .
               10  bit-dtr-tra.
                   15  bit-tra-cur        pic  9(02)                  .
                   15  bit-cau-tra        pic  x(03)                  .
                   15  bit-asp-ben        pic  x(03)                  .
                   15  bit-num-col        pic  9(05)       comp-3     .
                   15  bit-pes-tot        pic  9(06)v9(03) comp-3     .
                   15  bit-dat-itr        pic  9(07)       comp-3     .
                   15  bit-ora-itr        pic  9(04)                  .
                   15  bit-cod-vet        pic  9(07)       comp-3     .
                   15  bit-cod-vt2        pic  9(07)       comp-3     .
                   15  bit-cod-vt3        pic  9(07)       comp-3     .
                   15  bit-cod-ant        pic  9(03)       comp-3     .
               10  bit-prt-mgd            pic  9(07)       comp-3     .
               10  bit-nrg-mgd            pic  9(02)                  .
               10  bit-dri-mgd            pic  9(07)       comp-3     .
               10  bit-nri-mgd            pic  x(10)                  .
               10  bit-nps-sdb            pic  9(11)                  .
               10  bit-ctr-sdb            pic  9(02)                  .
               10  bit-iva-cst.
                   15  bit-iva-rig occurs 06.
                       20  bit-iva-cod    pic  9(05)       comp-3     .
                       20  bit-iva-ibl    pic s9(11)       comp-3     .
                       20  bit-iva-imp    pic s9(11)       comp-3     .
                   15  bit-iva-tdo        pic s9(11)       comp-3     .
               10  bit-ctp-cst.
                   15  bit-ctp-rig occurs 10.
                       20  bit-ctp-cod    pic  9(07)       comp-3     .
                       20  bit-ctp-imp    pic s9(11)       comp-3     .
               10  bit-ctr-stp            pic  9(02)                  .
               10  bit-flg-ela.
                   15  bit-flg-blo.
                       20  bit-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bit-flg-nbl.
                       20  bit-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bit-flg-pul            pic  x(01)                  .
               10  bit-alx-exp.
                   15  filler occurs 40   pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [bft]                                       *
      *    *-----------------------------------------------------------*
       01  f-bft.
           05  f-bft-nam                  pic  x(04)                  .
           05  f-bft-pat                  pic  x(40)                  .
           05  f-bft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bit]                                       *
      *    *-----------------------------------------------------------*
       01  f-bit.
           05  f-bit-nam                  pic  x(04)                  .
           05  f-bit-pat                  pic  x(40)                  .
           05  f-bit-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .

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
                     "cnv4p5"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnv4p5b "                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONE PER RELEASE 4.5 (B)   "       .

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
      *              * Conversione [bft]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bft-000      thru exe-cnv-bft-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bit]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bit-000      thru exe-cnv-bit-999        .
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
      *    * Conversione [bft]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bft-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bft "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bft-999.
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
       exe-cnv-bft-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-bft-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-bft-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-bft]                         *
      *                  *---------------------------------------------*
           open      i-o    bft                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-bft]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       exe-cnv-bft-200.
      *              *-------------------------------------------------*
      *              * Start su [old-bft]                              *
      *              *-------------------------------------------------*
           move      low-values           to   bft-k01                .
           start     bft    key not less
                            bft-k01
                            invalid key
                            go to exe-cnv-bft-800.
       exe-cnv-bft-250.
      *              *-------------------------------------------------*
      *              * Next su [old-bft]                               *
      *              *-------------------------------------------------*
           read      bft    next
                            with no lock
                            at end
                            go to exe-cnv-bft-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bft-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-bft]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       exe-cnv-bft-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-bft]                          *
      *              *-------------------------------------------------*
       exe-cnv-bft-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bft                 .
           move      bft-ide-dat          to   rf-bft-ide-dat         .
           move      bft-ide-ute          to   rf-bft-ide-ute         .
           move      bft-ide-fas          to   rf-bft-ide-fas         .
           move      bft-num-prt          to   rf-bft-num-prt         .
           move      bft-cod-tmb          to   rf-bft-cod-tmb         .
           move      bft-prt-ftf          to   rf-bft-prt-ftf         .
           move      bft-prt-mag          to   rf-bft-prt-mag         .
           move      bft-pr2-mag          to   rf-bft-pr2-mag         .
           move      bft-int-ftr          to   rf-bft-int-ftr         .
           move      bft-tmo-ftr          to   rf-bft-tmo-ftr         .
           move      bft-cod-dpz          to   rf-bft-cod-dpz         .
           move      bft-dat-reg          to   rf-bft-dat-reg         .
           move      bft-tip-arc          to   rf-bft-tip-arc         .
           move      bft-cod-arc          to   rf-bft-cod-arc         .
           move      bft-dpz-arc          to   rf-bft-dpz-arc         .
           move      bft-arc-plf          to   rf-bft-arc-plf         .
           move      bft-dpz-plf          to   rf-bft-dpz-plf         .
           move      bft-tip-ids          to   rf-bft-tip-ids         .
           move      bft-dat-doc          to   rf-bft-dat-doc         .
           move      bft-num-doc          to   rf-bft-num-doc         .
           move      bft-cod-lng          to   rf-bft-cod-lng         .
           move      bft-sgl-vpf          to   rf-bft-sgl-vpf         .
           move      bft-dec-vpf          to   rf-bft-dec-vpf         .
           move      bft-tdc-vpf          to   rf-bft-tdc-vpf         .
           move      bft-cdc-vpf          to   rf-bft-cdc-vpf         .
           move      bft-ass-iva          to   rf-bft-ass-iva         .
           move      bft-ctp-acq          to   rf-bft-ctp-acq         .
           move      bft-inl-pgt          to   rf-bft-inl-pgt         .
           move      bft-cod-lst          to   rf-bft-cod-lst         .
           move      bft-csr-aaf          to   rf-bft-csr-aaf         .
           move      bft-psr-aaf (1)      to   rf-bft-psr-aaf (1)     .
           move      bft-psr-aaf (2)      to   rf-bft-psr-aaf (2)     .
           move      bft-psr-aaf (3)      to   rf-bft-psr-aaf (3)     .
           move      bft-psr-aaf (4)      to   rf-bft-psr-aaf (4)     .
           move      bft-psr-aaf (5)      to   rf-bft-psr-aaf (5)     .
           move      bft-csc-aaf          to   rf-bft-csc-aaf         .
           move      bft-psc-aaf          to   rf-bft-psc-aaf         .
           move      bft-voc-des (1)      to   rf-bft-voc-des (1)     .
           move      bft-voc-des (2)      to   rf-bft-voc-des (2)     .
           move      bft-voc-des (3)      to   rf-bft-voc-des (3)     .
           move      bft-voc-des (4)      to   rf-bft-voc-des (4)     .
           move      bft-voc-des (5)      to   rf-bft-voc-des (5)     .
           move      bft-voc-des (6)      to   rf-bft-voc-des (6)     .
           move      bft-cod-fop          to   rf-bft-cod-fop         .
           move      bft-scp-aap          to   rf-bft-scp-aap         .
           move      bft-nos-ban          to   rf-bft-nos-ban         .
           move      bft-cod-abi          to   rf-bft-cod-abi         .
           move      bft-cod-cab          to   rf-bft-cod-cab         .
           move      bft-ccc-app          to   rf-bft-ccc-app         .
           move      bft-ccp-app          to   rf-bft-ccp-app         .
           move      bft-add-spi          to   rf-bft-add-spi         .
           move      bft-add-spb          to   rf-bft-add-spb         .
           move      bft-ipr-iel          to   rf-bft-ipr-iel         .
           move      bft-pag-dsm          to   rf-bft-pag-dsm         .
           move      bft-pag-qaf          to   rf-bft-pag-qaf         .
           move      bft-pag-act          to   rf-bft-pag-act         .
           move      bft-cod-aqt          to   rf-bft-cod-aqt         .
           move      bft-pvf-aqt          to   rf-bft-pvf-aqt         .
           move      bft-cod-ime          to   rf-bft-cod-ime         .
           move      bft-pvf-ime          to   rf-bft-pvf-ime         .
           move      bft-tot-rig (1)      to   rf-bft-tot-rig (1)     .
           move      bft-tot-rig (2)      to   rf-bft-tot-rig (2)     .
           move      bft-tot-rig (3)      to   rf-bft-tot-rig (3)     .
           move      bft-tot-rig (4)      to   rf-bft-tot-rig (4)     .
           move      bft-tot-rig (5)      to   rf-bft-tot-rig (5)     .
           move      bft-tot-rig (6)      to   rf-bft-tot-rig (6)     .
           move      bft-tot-rig (7)      to   rf-bft-tot-rig (7)     .
           move      bft-tot-rig (8)      to   rf-bft-tot-rig (8)     .
           move      bft-tot-rig (9)      to   rf-bft-tot-rig (9)     .
           move      bft-tot-scc          to   rf-bft-tot-scc         .
           move      bft-per-scc          to   rf-bft-per-scc         .
           move      bft-civ-scc          to   rf-bft-civ-scc         .
           move      bft-ccp-scc          to   rf-bft-ccp-scc         .
           move      bft-tot-scp          to   rf-bft-tot-scp         .
           move      bft-per-scp          to   rf-bft-per-scp         .
           move      bft-civ-scp          to   rf-bft-civ-scp         .
           move      bft-ccp-scp          to   rf-bft-ccp-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-bft-480.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-540.
           move      bft-spe-snx (w-c01)  to   rf-bft-spe-snx (w-c01) .
           move      bft-spe-mad (w-c01)  to   rf-bft-spe-mad (w-c01) .
           move      bft-spe-per (w-c01)  to   rf-bft-spe-per (w-c01) .
           move      bft-spe-ibl (w-c01)  to   rf-bft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-bft-500.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-bft-520.
           move      bft-ibx-spe
                    (w-c01, w-c02)        to   rf-bft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-bft-500.
       exe-cnv-bft-520.
           move      bft-spe-imp (w-c01)  to   rf-bft-spe-imp (w-c01) .
           move      bft-spe-civ (w-c01)  to   rf-bft-spe-civ (w-c01) .
           move      bft-spe-ccp (w-c01)  to   rf-bft-spe-ccp (w-c01) .
           go to     exe-cnv-bft-480.
       exe-cnv-bft-540.
           move      bft-civ-spi          to   rf-bft-civ-spi         .
           move      bft-ccp-spi          to   rf-bft-ccp-spi         .
           move      bft-tot-sic          to   rf-bft-tot-sic         .
           move      bft-tot-sia          to   rf-bft-tot-sia         .
           move      bft-tot-spb          to   rf-bft-tot-spb         .
           move      bft-civ-spb          to   rf-bft-civ-spb         .
           move      bft-ccp-spb          to   rf-bft-ccp-spb         .
           move      bft-prt-mgd          to   rf-bft-prt-mgd         .
           move      bft-nrg-mgd          to   rf-bft-nrg-mgd         .
           move      bft-dri-mgd          to   rf-bft-dri-mgd         .
           move      bft-nri-mgd          to   rf-bft-nri-mgd         .
           move      bft-nps-sdb          to   rf-bft-nps-sdb         .
           move      bft-ctr-sdb          to   rf-bft-ctr-sdb         .
           move      zero                 to   w-c01                  .
       exe-cnv-bft-560.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-580.
           move      bft-iva-cod (w-c01)  to   rf-bft-iva-cod (w-c01) .
           move      bft-iva-ibl (w-c01)  to   rf-bft-iva-ibl (w-c01) .
           go to     exe-cnv-bft-560.
       exe-cnv-bft-580.
           move      zero                 to   w-c01                  .
       exe-cnv-bft-600.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bft-620.
           move      bft-ctp-cod (w-c01)  to   rf-bft-ctp-cod (w-c01) .
           move      bft-ctp-imp (w-c01)  to   rf-bft-ctp-imp (w-c01) .
           go to     exe-cnv-bft-600.
       exe-cnv-bft-620.
           move      bft-flg-bch          to   rf-bft-flg-bch         .
           move      bft-flg-blx (1)      to   rf-bft-flg-blx (1)     .
           move      bft-flg-blx (2)      to   rf-bft-flg-blx (2)     .
           move      bft-flg-blx (3)      to   rf-bft-flg-blx (3)     .
           move      bft-flg-blx (4)      to   rf-bft-flg-blx (4)     .
           move      bft-flg-blx (5)      to   rf-bft-flg-blx (5)     .
           move      bft-flg-blx (6)      to   rf-bft-flg-blx (6)     .
           move      bft-flg-blx (7)      to   rf-bft-flg-blx (7)     .
           move      bft-flg-nbx (1)      to   rf-bft-flg-nbx (1)     .
           move      bft-flg-nbx (2)      to   rf-bft-flg-nbx (2)     .
           move      bft-flg-nbx (3)      to   rf-bft-flg-nbx (3)     .
           move      bft-flg-pul          to   rf-bft-flg-pul         .
           move      bft-alx-exp          to   rf-bft-alx-exp         .
       exe-cnv-bft-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-bft]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bft-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bft-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bft-250.
       exe-cnv-bft-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-bft]                        *
      *                  *---------------------------------------------*
           close     bft                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-bft]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       exe-cnv-bft-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bft-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-bft] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-bft-999.
           exit.

      *    *===========================================================*
      *    * Conversione [bit]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bit-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bit "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bit-999.
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
       exe-cnv-bit-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-bit-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-bit-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-bit]                         *
      *                  *---------------------------------------------*
           open      i-o    bit                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-bit]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
       exe-cnv-bit-200.
      *              *-------------------------------------------------*
      *              * Start su [old-bit]                              *
      *              *-------------------------------------------------*
           move      low-values           to   bit-k01                .
           start     bit    key not less
                            bit-k01
                            invalid key
                            go to exe-cnv-bit-800.
       exe-cnv-bit-250.
      *              *-------------------------------------------------*
      *              * Next su [old-bit]                               *
      *              *-------------------------------------------------*
           read      bit    next
                            with no lock
                            at end
                            go to exe-cnv-bit-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bit-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-bit]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
       exe-cnv-bit-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-bit]                          *
      *              *-------------------------------------------------*
       exe-cnv-bit-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bit                 .
           move      bit-ide-dat          to   rf-bit-ide-dat         .
           move      bit-ide-ute          to   rf-bit-ide-ute         .
           move      bit-ide-fas          to   rf-bit-ide-fas         .
           move      bit-num-prt          to   rf-bit-num-prt         .
           move      bit-cod-tmb          to   rf-bit-cod-tmb         .
           move      bit-prt-fti          to   rf-bit-prt-fti         .
           move      bit-prt-mag          to   rf-bit-prt-mag         .
           move      bit-pr2-mag          to   rf-bit-pr2-mag         .
           move      bit-int-ftr          to   rf-bit-int-ftr         .
           move      bit-tmo-ftr          to   rf-bit-tmo-ftr         .
           move      bit-fat-snx          to   rf-bit-fat-snx         .
           move      bit-fat-dat          to   rf-bit-fat-dat         .
           move      bit-fat-num          to   rf-bit-fat-num         .
           move      bit-fat-npb          to   rf-bit-fat-npb         .
           move      bit-snx-acm          to   rf-bit-snx-acm         .
           move      bit-cod-dpz          to   rf-bit-cod-dpz         .
           move      bit-dat-doc          to   rf-bit-dat-doc         .
           move      bit-num-doc          to   rf-bit-num-doc         .
           move      bit-snx-fac          to   rf-bit-snx-fac         .
           move      bit-scl-ann          to   rf-bit-scl-ann         .
           move      bit-sgl-num          to   rf-bit-sgl-num         .
           move      bit-tip-arc          to   rf-bit-tip-arc         .
           move      bit-cod-arc          to   rf-bit-cod-arc         .
           move      bit-dpz-arc          to   rf-bit-dpz-arc         .
           move      bit-tip-frn          to   rf-bit-tip-frn         .
           move      bit-arc-plf          to   rf-bit-arc-plf         .
           move      bit-dpz-plf          to   rf-bit-dpz-plf         .
           move      bit-tip-ftz          to   rf-bit-tip-ftz         .
           move      bit-tip-ids          to   rf-bit-tip-ids         .
           move      bit-cod-lng          to   rf-bit-cod-lng         .
           move      bit-sgl-vpf          to   rf-bit-sgl-vpf         .
           move      bit-dec-vpf          to   rf-bit-dec-vpf         .
           move      bit-tdc-vpf          to   rf-bit-tdc-vpf         .
           move      bit-cdc-vpf          to   rf-bit-cdc-vpf         .
           move      bit-ass-iva          to   rf-bit-ass-iva         .
           move      bit-ctp-ven          to   rf-bit-ctp-ven         .
           move      bit-fat-sep          to   rf-bit-fat-sep         .
           move      bit-inl-dcm          to   rf-bit-inl-dcm         .
           move      bit-inl-pgt          to   rf-bit-inl-pgt         .
           move      bit-cod-lst          to   rf-bit-cod-lst         .
           move      bit-csr-aac          to   rf-bit-csr-aac         .
           move      bit-psr-aac (1)      to   rf-bit-psr-aac (1)     .
           move      bit-psr-aac (2)      to   rf-bit-psr-aac (2)     .
           move      bit-psr-aac (3)      to   rf-bit-psr-aac (3)     .
           move      bit-psr-aac (4)      to   rf-bit-psr-aac (4)     .
           move      bit-psr-aac (5)      to   rf-bit-psr-aac (5)     .
           move      bit-csc-aac          to   rf-bit-csc-aac         .
           move      bit-psc-aac          to   rf-bit-psc-aac         .
           move      bit-cpv-aac          to   rf-bit-cpv-aac         .
           move      bit-ppv-aac (1)      to   rf-bit-ppv-aac (1)     .
           move      bit-ppv-aac (2)      to   rf-bit-ppv-aac (2)     .
           move      bit-ppv-aac (3)      to   rf-bit-ppv-aac (3)     .
           move      bit-voc-des (1)      to   rf-bit-voc-des (1)     .
           move      bit-voc-des (2)      to   rf-bit-voc-des (2)     .
           move      bit-voc-des (3)      to   rf-bit-voc-des (3)     .
           move      bit-voc-des (4)      to   rf-bit-voc-des (4)     .
           move      bit-voc-des (5)      to   rf-bit-voc-des (5)     .
           move      bit-voc-des (6)      to   rf-bit-voc-des (6)     .
           move      bit-cod-fop          to   rf-bit-cod-fop         .
           move      bit-scp-aap          to   rf-bit-scp-aap         .
           move      bit-cod-abi          to   rf-bit-cod-abi         .
           move      bit-cod-cab          to   rf-bit-cod-cab         .
           move      bit-ccc-app          to   rf-bit-ccc-app         .
           move      bit-nos-ban          to   rf-bit-nos-ban         .
           move      bit-nos-ccp          to   rf-bit-nos-ccp         .
           move      bit-add-spi          to   rf-bit-add-spi         .
           move      bit-add-spb          to   rf-bit-add-spb         .
           move      bit-ipr-iel          to   rf-bit-ipr-iel         .
           move      bit-pag-dsm          to   rf-bit-pag-dsm         .
           move      bit-pag-qaf          to   rf-bit-pag-qaf         .
           move      bit-pag-act          to   rf-bit-pag-act         .
           move      bit-cod-age          to   rf-bit-cod-age         .
           move      bit-fsp-doc          to   rf-bit-fsp-doc         .
           move      bit-pvf-age          to   rf-bit-pvf-age         .
           move      bit-tip-vpa          to   rf-bit-tip-vpa         .
           move      bit-cpv-aaa          to   rf-bit-cpv-aaa         .
           move      bit-ppv-aaa (1)      to   rf-bit-ppv-aaa (1)     .
           move      bit-ppv-aaa (2)      to   rf-bit-ppv-aaa (2)     .
           move      bit-ppv-aaa (3)      to   rf-bit-ppv-aaa (3)     .
           move      bit-cod-ime          to   rf-bit-cod-ime         .
           move      bit-pvf-ime          to   rf-bit-pvf-ime         .
           move      bit-tot-rig (1)      to   rf-bit-tot-rig (1)     .
           move      bit-tot-rig (2)      to   rf-bit-tot-rig (2)     .
           move      bit-tot-rig (3)      to   rf-bit-tot-rig (3)     .
           move      bit-tot-rig (4)      to   rf-bit-tot-rig (4)     .
           move      bit-tot-rig (5)      to   rf-bit-tot-rig (5)     .
           move      bit-tot-rig (6)      to   rf-bit-tot-rig (6)     .
           move      bit-tot-rig (7)      to   rf-bit-tot-rig (7)     .
           move      bit-tot-rig (8)      to   rf-bit-tot-rig (8)     .
           move      bit-tot-rig (9)      to   rf-bit-tot-rig (9)     .
           move      bit-tot-scc          to   rf-bit-tot-scc         .
           move      bit-per-scc          to   rf-bit-per-scc         .
           move      bit-civ-scc          to   rf-bit-civ-scc         .
           move      bit-ccp-scc          to   rf-bit-ccp-scc         .
           move      bit-tot-scp          to   rf-bit-tot-scp         .
           move      bit-per-scp          to   rf-bit-per-scp         .
           move      bit-civ-scp          to   rf-bit-civ-scp         .
           move      bit-ccp-scp          to   rf-bit-ccp-scp         .
           move      zero                 to   w-c01                  .
       dec-fis-log-200.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-340.
           move      bit-spe-snx (w-c01)  to   rf-bit-spe-snx (w-c01) .
           move      bit-spe-mad (w-c01)  to   rf-bit-spe-mad (w-c01) .
           move      bit-spe-per (w-c01)  to   rf-bit-spe-per (w-c01) .
           move      bit-spe-ibl (w-c01)  to   rf-bit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       dec-fis-log-300.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to dec-fis-log-320.
           move      bit-ibx-spe
                    (w-c01, w-c02)        to   rf-bit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     dec-fis-log-300.
       dec-fis-log-320.
           move      bit-spe-imp (w-c01)  to   rf-bit-spe-imp (w-c01) .
           move      bit-spe-civ (w-c01)  to   rf-bit-spe-civ (w-c01) .
           move      bit-spe-ccp (w-c01)  to   rf-bit-spe-ccp (w-c01) .
           go to     dec-fis-log-200.
       dec-fis-log-340.
           move      bit-civ-spi          to   rf-bit-civ-spi         .
           move      bit-ccp-spi          to   rf-bit-ccp-spi         .
           move      bit-tot-sic          to   rf-bit-tot-sic         .
           move      bit-tot-sia          to   rf-bit-tot-sia         .
           move      bit-tot-spb          to   rf-bit-tot-spb         .
           move      bit-civ-spb          to   rf-bit-civ-spb         .
           move      bit-ccp-spb          to   rf-bit-ccp-spb         .
           move      bit-tra-cur          to   rf-bit-tra-cur         .
           move      bit-cau-tra          to   rf-bit-cau-tra         .
           move      bit-asp-ben          to   rf-bit-asp-ben         .
           move      bit-num-col          to   rf-bit-num-col         .
           move      bit-pes-tot          to   rf-bit-pes-tot         .
           move      bit-dat-itr          to   rf-bit-dat-itr         .
           move      bit-ora-itr          to   rf-bit-ora-itr         .
           move      bit-cod-vet          to   rf-bit-cod-vet         .
           move      bit-cod-vt2          to   rf-bit-cod-vt2         .
           move      bit-cod-vt3          to   rf-bit-cod-vt3         .
           move      bit-cod-ant          to   rf-bit-cod-ant         .
           move      bit-prt-mgd          to   rf-bit-prt-mgd         .
           move      bit-nrg-mgd          to   rf-bit-nrg-mgd         .
           move      bit-dri-mgd          to   rf-bit-dri-mgd         .
           move      bit-nri-mgd          to   rf-bit-nri-mgd         .
           move      bit-nps-sdb          to   rf-bit-nps-sdb         .
           move      bit-ctr-sdb          to   rf-bit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       dec-fis-log-400.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-420.
           move      bit-iva-cod (w-c01)  to   rf-bit-iva-cod (w-c01) .
           move      bit-iva-ibl (w-c01)  to   rf-bit-iva-ibl (w-c01) .
           move      bit-iva-imp (w-c01)  to   rf-bit-iva-imp (w-c01) .
           go to     dec-fis-log-400.
       dec-fis-log-420.
           move      bit-iva-tdo          to   rf-bit-iva-tdo         .
           move      zero                 to   w-c01                  .
       dec-fis-log-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to dec-fis-log-520.
           move      bit-ctp-cod (w-c01)  to   rf-bit-ctp-cod (w-c01) .
           move      bit-ctp-imp (w-c01)  to   rf-bit-ctp-imp (w-c01) .
           go to     dec-fis-log-500.
       dec-fis-log-520.
           move      bit-ctr-stp          to   rf-bit-ctr-stp         .
           move      bit-flg-blx (1)      to   rf-bit-flg-blx (1)     .
           move      bit-flg-blx (2)      to   rf-bit-flg-blx (2)     .
           move      bit-flg-blx (3)      to   rf-bit-flg-blx (3)     .
           move      bit-flg-blx (4)      to   rf-bit-flg-blx (4)     .
           move      bit-flg-blx (5)      to   rf-bit-flg-blx (5)     .
           move      bit-flg-blx (6)      to   rf-bit-flg-blx (6)     .
           move      bit-flg-blx (7)      to   rf-bit-flg-blx (7)     .
           move      bit-flg-nbx (1)      to   rf-bit-flg-nbx (1)     .
           move      bit-flg-nbx (2)      to   rf-bit-flg-nbx (2)     .
           move      bit-flg-nbx (3)      to   rf-bit-flg-nbx (3)     .
           move      bit-flg-pul          to   rf-bit-flg-pul         .
           move      bit-alx-exp          to   rf-bit-alx-exp         .
       exe-cnv-bit-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-bit]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bit-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bit-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bit-250.
       exe-cnv-bit-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-bit]                        *
      *                  *---------------------------------------------*
           close     bit                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-bit]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
       exe-cnv-bit-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bit-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-bit] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-bit-999.
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

