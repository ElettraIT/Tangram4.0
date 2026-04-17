       Identification Division.
       Program-Id.                                 cnv3p1             .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    cnv                 *
      *                                   Fase:    cnv3p1              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 06/03/95    *
      *                       Ultima revisione:    NdK del 06/03/95    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per versione 3.0                *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Aggiornamento file [oct] : 10/04/95         *
      *                    Aggiornamento file [ocr] : 06/03/95         *
      *                    Aggiornamento file [bit] : 20/03/95         *
      *                    Aggiornamento file [bir] : 09/03/95         *
      *                    Aggiornamento file [fir] : 13/03/95         *
      *                    Aggiornamento file [dcp] : 10/03/95         *
      *                    Aggiornamento file [lic] : 21/03/95         *
      *                    Aggiornamento file [oft] : 12/04/95         *
      *                    Aggiornamento file [ofr] : 12/04/95         *
      *                    Aggiornamento file [aaf] : 13/04/95         *
      *                    Aggiornamento file [bft] : 18/04/95         *
      *                    Aggiornamento file [bfr] : 18/04/95         *
      *                    Aggiornamento file [fft] : 20/04/95         *
      *                    Aggiornamento file [ffr] : 20/04/95         *
      *                    Aggiornamento file [age] : 11/05/95         *
      *                    Aggiornamento file [vet] : 15/05/95         *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      * Note per la conversione : Attenzione ai seguenti campi per i   *
      *                           quali si dovra' provvedere ad una    *
      *                           conversione specifica dai clienti :  *
      *                                                                *
      *                           [dcp] - Tipo acquisizione prodotto   *
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
      *    * File Control [oct]                                        *
      *    *-----------------------------------------------------------*
           select  optional  oct   assign to disk           f-oct-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is oct-k01
                   alternate record key   is oct-k02
                   alternate record key   is oct-k03
                   alternate record key   is oct-k04
                   alternate record key   is oct-k05
                   alternate record key   is oct-k06
                   alternate record key   is oct-k07
                             file status  is                f-oct-sts .

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

      *    *===========================================================*
      *    * File Control [bir]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bir   assign to disk           f-bir-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bir-k01
                   alternate record key   is bir-k02
                   alternate record key   is bir-k03
                   alternate record key   is bir-k04
                             file status  is                f-bir-sts .

      *    *===========================================================*
      *    * File Control [fir]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fir   assign to disk           f-fir-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fir-k01
                   alternate record key   is fir-k02
                   alternate record key   is fir-k03
                             file status  is                f-fir-sts .

      *    *===========================================================*
      *    * File Control [dcp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  dcp   assign to disk           f-dcp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is dcp-k01
                   alternate record key   is dcp-k02
                   alternate record key   is dcp-k03
                   alternate record key   is dcp-k04
                   alternate record key   is dcp-k05
                   alternate record key   is dcp-k06
                   alternate record key   is dcp-k07
                             file status  is                f-dcp-sts .

      *    *===========================================================*
      *    * File Control [lic]                                        *
      *    *-----------------------------------------------------------*
           select  optional  lic   assign to disk           f-lic-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is lic-k01
                             file status  is                f-lic-sts .

      *    *===========================================================*
      *    * File Control [oft]                                        *
      *    *-----------------------------------------------------------*
           select  optional  oft   assign to disk           f-oft-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is oft-k01
                   alternate record key   is oft-k02
                   alternate record key   is oft-k03
                   alternate record key   is oft-k04
                   alternate record key   is oft-k05
                   alternate record key   is oft-k06
                   alternate record key   is oft-k07
                             file status  is                f-oft-sts .

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
      *    * File Control [aaf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  aaf   assign to disk           f-aaf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is aaf-k01
                   alternate record key   is aaf-k02
                   alternate record key   is aaf-k03
                   alternate record key   is aaf-k04
                   alternate record key   is aaf-k05
                             file status  is                f-aaf-sts .

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
      *    * File Control [bfr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  bfr   assign to disk           f-bfr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is bfr-k01
                   alternate record key   is bfr-k02
                   alternate record key   is bfr-k03
                   alternate record key   is bfr-k04
                   alternate record key   is bfr-k05
                             file status  is                f-bfr-sts .

      *    *===========================================================*
      *    * File Control [fft]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fft   assign to disk           f-fft-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fft-k01
                   alternate record key   is fft-k02
                   alternate record key   is fft-k03
                   alternate record key   is fft-k04
                   alternate record key   is fft-k05
                             file status  is                f-fft-sts .

      *    *===========================================================*
      *    * File Control [ffr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ffr   assign to disk           f-ffr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ffr-k01
                   alternate record key   is ffr-k02
                   alternate record key   is ffr-k03
                   alternate record key   is ffr-k04
                             file status  is                f-ffr-sts .

      *    *===========================================================*
      *    * File Control [age]                                        *
      *    *-----------------------------------------------------------*
           select  optional  age   assign to disk           f-age-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is age-k01
                   alternate record key   is age-k02
                   alternate record key   is age-k03
                   alternate record key   is age-k04
                   alternate record key   is age-k05
                   alternate record key   is age-k06
                             file status  is                f-age-sts .

      *    *===========================================================*
      *    * File Control [vet]                                        *
      *    *-----------------------------------------------------------*
           select  optional  vet   assign to disk           f-vet-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is vet-k01
                   alternate record key   is vet-k02
                   alternate record key   is vet-k03
                   alternate record key   is vet-k04
                   alternate record key   is vet-k05
                   alternate record key   is vet-k06
                             file status  is                f-vet-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [oct]                                    *
      *    *-----------------------------------------------------------*
       fd  oct       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  oct-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  oct-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  oct-k01.
                   15  oct-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  oct-k02.
                   15  oct-ide-dat        pic  9(07)       comp-3     .
                   15  oct-dat-doc        pic  9(07)       comp-3     .
                   15  oct-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  oct-k03.
                   15  oct-dat-doc-3      pic  9(07)       comp-3     .
                   15  oct-cod-dpz        pic  9(02)                  .
                   15  oct-num-doc        pic  9(11)       comp-3     .
                   15  oct-tmo-orc        pic  x(05)                  .
                   15  oct-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  oct-k04.
                   15  oct-scl-ann        pic  9(03)       comp-3     .
                   15  oct-cod-dpz-4      pic  9(02)                  .
                   15  oct-sgl-num        pic  x(03)                  .
                   15  oct-num-doc-4      pic  9(11)       comp-3     .
                   15  oct-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  oct-k05.
                   15  oct-cod-dpz-5      pic  9(02)                  .
                   15  oct-tip-arc        pic  x(01)                  .
                   15  oct-cod-arc        pic  9(07)       comp-3     .
                   15  oct-dat-doc-5      pic  9(07)       comp-3     .
                   15  oct-num-doc-5      pic  9(11)       comp-3     .
                   15  oct-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMODAT                      *
      *            *---------------------------------------------------*
               10  oct-k06.
                   15  oct-cod-dpz-6      pic  9(02)                  .
                   15  oct-tmo-orc-6      pic  x(05)                  .
                   15  oct-dat-doc-6      pic  9(07)       comp-3     .
                   15  oct-num-doc-6      pic  9(11)       comp-3     .
                   15  oct-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZOCH                         *
      *            *---------------------------------------------------*
               10  oct-k07.
                   15  oct-cod-dpz-7      pic  9(02)                  .
                   15  oct-flg-och        pic  x(01)                  .
                   15  oct-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  oct-dat.
               10  oct-ide-ute            pic  x(08)                  .
               10  oct-ide-fas            pic  x(06)                  .
               10  oct-dpz-arc            pic  x(04)                  .
               10  oct-tip-frn            pic  9(02)                  .
               10  oct-arc-plf            pic  9(07)       comp-3     .
               10  oct-dpz-plf            pic  x(04)                  .
               10  oct-tip-ftz            pic  9(02)                  .
               10  oct-tip-ids            pic  9(02)                  .
               10  oct-ocl-dat            pic  9(07)       comp-3     .
               10  oct-ocl-num            pic  x(10)                  .
               10  oct-ocl-rif            pic  x(40)                  .
               10  oct-dat-cns            pic  9(07)       comp-3     .
               10  oct-fds-dtc            pic  9(02)                  .
               10  oct-tip-eva            pic  9(02)                  .
               10  oct-pri-eva            pic  x(02)                  .
               10  oct-cod-cdv            pic  9(03)       comp-3     .
               10  oct-com-int.
                   15  oct-com-rig occurs 03
                                          pic  x(40)                  .
               10  oct-cod-lng            pic  x(03)                  .
               10  oct-vpf.
                   15  oct-sgl-vpf        pic  x(03)                  .
                   15  oct-dec-vpf        pic  9(01)                  .
                   15  oct-tdc-vpf        pic  x(01)                  .
                   15  oct-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  oct-ass-iva            pic  9(05)       comp-3     .
               10  oct-ctp-ven            pic  9(07)       comp-3     .
               10  oct-fat-sep            pic  x(01)                  .
               10  oct-inl-dcm            pic  9(02)                  .
               10  oct-inl-pgt            pic  9(02)                  .
               10  oct-cod-lst            pic  x(03)                  .
               10  oct-csr-aac            pic  9(05)       comp-3     .
               10  oct-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  oct-csc-aac            pic  9(05)       comp-3     .
               10  oct-psc-aac            pic  9(02)v9(01) comp-3     .
               10  oct-cpv-aac            pic  9(05)       comp-3     .
               10  oct-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  oct-voc-des occurs 06  pic  x(03)                  .
               10  oct-cod-fop            pic  9(07)       comp-3     .
               10  oct-scp-aap            pic  9(02)v9(01) comp-3     .
               10  oct-cod-abi            pic  9(05)       comp-3     .
               10  oct-cod-cab            pic  9(05)       comp-3     .
               10  oct-ccc-app            pic  x(12)                  .
               10  oct-nos-ban            pic  x(10)                  .
               10  oct-nos-ccp            pic  x(10)                  .
               10  oct-add-spi            pic  x(03)                  .
               10  oct-add-spb            pic  x(03)                  .
               10  oct-ipr-iel            pic  9(02)                  .
               10  oct-pag-dsm            pic  9(07)       comp-3     .
               10  oct-pag-qaf            pic  9(09)       comp-3     .
               10  oct-pag-act            pic  9(09)       comp-3     .
               10  oct-cod-age            pic  9(07)       comp-3     .
               10  oct-fsp-doc            pic  9(02)                  .
               10  oct-pvf-age            pic  9(11)       comp-3     .
               10  oct-tip-vpa            pic  9(02)                  .
               10  oct-cpv-aaa            pic  9(05)       comp-3     .
               10  oct-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  oct-cod-ime            pic  9(07)       comp-3     .
               10  oct-pvf-ime            pic  9(11)       comp-3     .
               10  oct-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  oct-tot-scc            pic s9(11)       comp-3     .
               10  oct-per-scc            pic  9(02)v9(01) comp-3     .
               10  oct-tot-scp            pic s9(11)       comp-3     .
               10  oct-per-scp            pic  9(02)v9(01) comp-3     .
               10  oct-spe-add occurs 06.
                   15  oct-spe-snx        pic  9(01)                  .
                   15  oct-spe-mad        pic  9(01)                  .
                   15  oct-spe-per        pic  9(02)v9(01) comp-3     .
                   15  oct-spe-ibl        pic  9(02)                  .
                   15  oct-ibt-spe.
                       20  oct-ibx-spe  occurs 09
                                          pic  x(01)                  .
                   15  oct-spe-imp        pic s9(09)       comp-3     .
               10  oct-tot-doc            pic s9(11)       comp-3     .
               10  oct-ctr-stp            pic  9(02)                  .
               10  oct-flg-ela.
                   15  oct-flg-blo.
                       20  oct-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  oct-flg-nbl.
                       20  oct-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  oct-flg-pul            pic  x(01)                  .
               10  oct-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .

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
                   15  ocr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  ocr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  ocr-plm-vpl        pic  9(02)v9(01) comp-3     .
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
               10  ocr-dat-cns            pic  9(07)                  .
               10  ocr-pri-eva            pic  x(02)                  .
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
               10  ocr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

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

      *    *===========================================================*
      *    * File Description [bir]                                    *
      *    *-----------------------------------------------------------*
       fd  bir       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bir-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bir-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bir-k01.
                   15  bir-num-prt        pic  9(11)       comp-3     .
                   15  bir-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  bir-k02.
                   15  bir-cod-dpz        pic  9(02)                  .
                   15  bir-tip-mag        pic  9(02)                  .
                   15  bir-num-pro        pic  9(07)       comp-3     .
                   15  bir-dat-doc        pic  9(07)       comp-3     .
                   15  bir-num-doc        pic  9(11)       comp-3     .
                   15  bir-num-prt-2      pic  9(11)       comp-3     .
                   15  bir-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFORC                         *
      *            *---------------------------------------------------*
               10  bir-k03.
                   15  bir-cod-dpz-3      pic  9(02)                  .
                   15  bir-coc-prt        pic  9(11)       comp-3     .
                   15  bir-coc-prg        pic  9(05)       comp-3     .
                   15  bir-num-prt-3      pic  9(11)       comp-3     .
                   15  bir-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RIFOSC                         *
      *            *---------------------------------------------------*
               10  bir-k04.
                   15  bir-cod-dpz-4      pic  9(02)                  .
                   15  bir-osc-prt        pic  9(11)       comp-3     .
                   15  bir-coc-prt-4      pic  9(11)       comp-3     .
                   15  bir-coc-prg-4      pic  9(05)       comp-3     .
                   15  bir-num-prt-4      pic  9(11)       comp-3     .
                   15  bir-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bir-dat.
               10  bir-cod-tmb            pic  x(05)                  .
               10  bir-tip-arc            pic  x(01)                  .
               10  bir-cod-arc            pic  9(07)       comp-3     .
               10  bir-dpz-arc            pic  x(04)                  .
               10  bir-cod-lng            pic  x(03)                  .
               10  bir-vpf.
                   15  bir-sgl-vpf        pic  x(03)                  .
                   15  bir-dec-vpf        pic  9(01)                  .
                   15  bir-tdc-vpf        pic  x(01)                  .
                   15  bir-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  bir-bld-flb            pic  9(01)                  .
               10  bir-bld-tpb            pic  9(01)                  .
               10  bir-bld-rgb            pic  9(01)                  .
               10  bir-tip-rig            pic  x(05)                  .
               10  bir-alf-pro            pic  x(14)                  .
               10  bir-sgl-vrn            pic  x(14)                  .
               10  bir-des-ext            pic  9(01)                  .
               10  bir-des-rig            pic  x(40)                  .
               10  bir-tip-pro            pic  9(02)                  .
               10  bir-cod-iva            pic  9(05)       comp-3     .
               10  bir-ctp-ven            pic  9(07)       comp-3     .
               10  bir-umi-ven            pic  x(03)                  .
               10  bir-dec-qta            pic  9(01)                  .
               10  bir-qta-ven            pic s9(06)v9(03) comp-3     .
               10  bir-cod-dsl            pic  x(07)                  .
               10  bir-snx-2qt            pic  9(01)                  .
               10  bir-dec-2qt            pic  9(01)                  .
               10  bir-qta-a02            pic s9(06)v9(03) comp-3     .
               10  bir-snx-3qt            pic  9(01)                  .
               10  bir-dec-3qt            pic  9(01)                  .
               10  bir-qta-a03            pic s9(06)v9(03) comp-3     .
               10  bir-dec-prz            pic  9(01)                  .
               10  bir-vps.
                   15  bir-sgl-vps        pic  x(03)                  .
                   15  bir-dec-vps        pic  9(01)                  .
                   15  bir-tdc-vps        pic  x(01)                  .
                   15  bir-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  bir-prz-lrs            pic  9(09)       comp-3     .
               10  bir-prz-nts            pic  9(09)       comp-3     .
               10  bir-vpp.
                   15  bir-sgl-vpp        pic  x(03)                  .
                   15  bir-dec-vpp        pic  9(01)                  .
                   15  bir-tdc-vpp        pic  x(01)                  .
                   15  bir-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  bir-prz-ven            pic  9(09)       comp-3     .
               10  bir-snx-2pz            pic  9(01)                  .
               10  bir-prz-a02            pic  9(09)       comp-3     .
               10  bir-vpl.
                   15  bir-sgl-vpl        pic  x(03)                  .
                   15  bir-dec-vpl        pic  9(01)                  .
                   15  bir-tdc-vpl        pic  x(01)                  .
                   15  bir-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  bir-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  bir-plm-vpl        pic  9(02)v9(01) comp-3     .
                   15  bir-tlm-vpl        pic  x(01)                  .
                   15  bir-map-vpl        pic  x(01)                  .
               10  bir-epz-rgf            pic  9(01)                  .
               10  bir-csr-aap            pic  9(05)       comp-3     .
               10  bir-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  bir-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  bir-prz-net            pic  9(09)       comp-3     .
               10  bir-epz-pes            pic  9(02)                  .
               10  bir-vpc.
                   15  bir-sgl-vpc        pic  x(03)                  .
                   15  bir-dec-vpc        pic  9(01)                  .
                   15  bir-tdc-vpc        pic  x(01)                  .
                   15  bir-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  bir-dec-cos            pic  9(01)                  .
               10  bir-cos-rif            pic  9(09)       comp-3     .
               10  bir-imp-rig            pic s9(11)       comp-3     .
               10  bir-iau-rig            pic s9(11)       comp-3     .
               10  bir-cpv-aap            pic  9(05)       comp-3     .
               10  bir-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  bir-fsp-rig            pic  9(02)                  .
               10  bir-cpv-rig            pic  9(05)       comp-3     .
               10  bir-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  bir-pvf-rig            pic s9(11)       comp-3     .
               10  bir-ocl-dat            pic  9(07)       comp-3     .
               10  bir-ocl-num            pic  x(10)                  .
               10  bir-cmc-tip            pic  x(05)                  .
               10  bir-cmc-dat            pic  9(07)       comp-3     .
               10  bir-cmc-num            pic  9(11)       comp-3     .
               10  bir-coc-tip            pic  x(05)                  .
               10  bir-coc-dat            pic  9(07)       comp-3     .
               10  bir-coc-num            pic  9(11)       comp-3     .
               10  bir-coc-fzs            pic  x(01)                  .
               10  bir-fat-snx            pic  x(01)                  .
               10  bir-fat-dat            pic  9(07)       comp-3     .
               10  bir-fat-num            pic  9(11)       comp-3     .
               10  bir-fat-npb            pic  9(03)       comp-3     .
               10  bir-flg-ela.
                   15  bir-flg-blo.
                       20  bir-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bir-flg-nbl.
                       20  bir-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bir-flg-pul            pic  x(01)                  .
               10  bir-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [fir]                                    *
      *    *-----------------------------------------------------------*
       fd  fir       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fir-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fir-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fir-k01.
                   15  fir-num-prt        pic  9(09)       comp-3     .
                   15  fir-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRODAT                         *
      *            *---------------------------------------------------*
               10  fir-k02.
                   15  fir-tip-mag        pic  9(02)                  .
                   15  fir-num-pro        pic  9(07)       comp-3     .
                   15  fir-dat-doc        pic  9(07)       comp-3     .
                   15  fir-num-prt-2      pic  9(09)       comp-3     .
                   15  fir-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATPRO                         *
      *            *---------------------------------------------------*
               10  fir-k03.
                   15  fir-dat-doc-3      pic  9(07)       comp-3     .
                   15  fir-tip-mag-3      pic  9(02)                  .
                   15  fir-num-pro-3      pic  9(07)       comp-3     .
                   15  fir-num-prt-3      pic  9(09)       comp-3     .
                   15  fir-num-prg-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fir-dat.
               10  fir-cod-tmo            pic  x(05)                  .
               10  fir-cod-dpz            pic  9(02)                  .
               10  fir-num-doc            pic  9(11)       comp-3     .
               10  fir-cod-cli            pic  9(07)       comp-3     .
               10  fir-dpz-cli            pic  x(04)                  .
               10  fir-cli-pls            pic  9(07)       comp-3     .
               10  fir-dpc-pls            pic  x(04)                  .
               10  fir-cod-lng            pic  x(03)                  .
               10  fir-vpf.
                   15  fir-sgl-vpf        pic  x(03)                  .
                   15  fir-dec-vpf        pic  9(01)                  .
                   15  fir-tdc-vpf        pic  x(01)                  .
                   15  fir-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fir-bld-flb            pic  9(01)                  .
               10  fir-bld-tpb            pic  9(01)                  .
               10  fir-bld-rgb            pic  9(01)                  .
               10  fir-tip-rig            pic  x(05)                  .
               10  fir-alf-pro            pic  x(14)                  .
               10  fir-sgl-vrn            pic  x(14)                  .
               10  fir-des-ext            pic  9(01)                  .
               10  fir-des-rig            pic  x(40)                  .
               10  fir-tip-pro            pic  9(02)                  .
               10  fir-cod-iva            pic  9(05)       comp-3     .
               10  fir-ctp-ven            pic  9(07)       comp-3     .
               10  fir-umi-ven            pic  x(03)                  .
               10  fir-dec-qta            pic  9(01)                  .
               10  fir-qta-ven            pic s9(06)v9(03) comp-3     .
               10  fir-snx-2qt            pic  9(01)                  .
               10  fir-dec-2qt            pic  9(01)                  .
               10  fir-qta-a02            pic s9(06)v9(03) comp-3     .
               10  fir-snx-3qt            pic  9(01)                  .
               10  fir-dec-3qt            pic  9(01)                  .
               10  fir-qta-a03            pic s9(06)v9(03) comp-3     .
               10  fir-dec-prz            pic  9(01)                  .
               10  fir-vps.
                   15  fir-sgl-vps        pic  x(03)                  .
                   15  fir-dec-vps        pic  9(01)                  .
                   15  fir-tdc-vps        pic  x(01)                  .
                   15  fir-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  fir-prz-lrs            pic  9(09)       comp-3     .
               10  fir-prz-nts            pic  9(09)       comp-3     .
               10  fir-vpp.
                   15  fir-sgl-vpp        pic  x(03)                  .
                   15  fir-dec-vpp        pic  9(01)                  .
                   15  fir-tdc-vpp        pic  x(01)                  .
                   15  fir-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  fir-prz-ven            pic  9(09)       comp-3     .
               10  fir-snx-2pz            pic  9(01)                  .
               10  fir-prz-a02            pic  9(09)       comp-3     .
               10  fir-vpl.
                   15  fir-sgl-vpl        pic  x(03)                  .
                   15  fir-dec-vpl        pic  9(01)                  .
                   15  fir-tdc-vpl        pic  x(01)                  .
                   15  fir-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  fir-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  fir-plm-vpl        pic  9(02)v9(01) comp-3     .
                   15  fir-tlm-vpl        pic  x(01)                  .
                   15  fir-map-vpl        pic  x(01)                  .
               10  fir-epz-rgf            pic  9(01)                  .
               10  fir-csr-aap            pic  9(05)       comp-3     .
               10  fir-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  fir-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  fir-prz-net            pic  9(09)       comp-3     .
               10  fir-epz-pes            pic  9(02)                  .
               10  fir-vpc.
                   15  fir-sgl-vpc        pic  x(03)                  .
                   15  fir-dec-vpc        pic  9(01)                  .
                   15  fir-tdc-vpc        pic  x(01)                  .
                   15  fir-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  fir-dec-cos            pic  9(01)                  .
               10  fir-cos-rif            pic  9(09)       comp-3     .
               10  fir-imp-rig            pic s9(11)       comp-3     .
               10  fir-iau-rig            pic s9(11)       comp-3     .
               10  fir-cpv-aap            pic  9(05)       comp-3     .
               10  fir-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  fir-fsp-rig            pic  9(02)                  .
               10  fir-cpv-rig            pic  9(05)       comp-3     .
               10  fir-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  fir-pvf-rig            pic s9(11)       comp-3     .
               10  fir-ocl-dat            pic  9(07)       comp-3     .
               10  fir-ocl-num            pic  x(10)                  .
               10  fir-cmc-tip            pic  x(05)                  .
               10  fir-cmc-dat            pic  9(07)       comp-3     .
               10  fir-cmc-num            pic  9(11)       comp-3     .
               10  fir-coc-tip            pic  x(05)                  .
               10  fir-coc-dat            pic  9(07)       comp-3     .
               10  fir-coc-num            pic  9(11)       comp-3     .
               10  fir-bcc-tip            pic  x(05)                  .
               10  fir-bcc-dat            pic  9(07)       comp-3     .
               10  fir-bcc-num            pic  9(11)       comp-3     .
               10  fir-flg-ela.
                   15  fir-flg-blo.
                       20  fir-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fir-flg-nbl.
                       20  fir-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fir-flg-pul            pic  x(01)                  .
               10  fir-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [dcp]                                    *
      *    *-----------------------------------------------------------*
       fd  dcp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  dcp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  dcp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k01.
                   15  dcp-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  dcp-k02.
                   15  dcp-ide-dat        pic  9(07)       comp-3     .
                   15  dcp-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  dcp-k03.
                   15  dcp-des-key        pic  x(40)                  .
                   15  dcp-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k04.
                   15  dcp-alf-pro        pic  x(14)                  .
                   15  dcp-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k05.
                   15  dcp-syn-pro        pic  x(13)                  .
                   15  dcp-num-pro-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  dcp-k06.
                   15  dcp-cla-pro        pic  9(05)       comp-3     .
                   15  dcp-gru-pro        pic  9(05)       comp-3     .
                   15  dcp-sgr-pro        pic  9(05)       comp-3     .
                   15  dcp-des-key-6      pic  x(40)                  .
                   15  dcp-num-pro-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  dcp-k07.
                   15  dcp-cla-pro-7      pic  9(05)       comp-3     .
                   15  dcp-gru-pro-7      pic  9(05)       comp-3     .
                   15  dcp-sgr-pro-7      pic  9(05)       comp-3     .
                   15  dcp-alf-pro-7      pic  x(14)                  .
                   15  dcp-num-pro-7      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  dcp-dat.
               10  dcp-inf-gen.
                   15  dcp-ide-ute        pic  x(08)                  .
                   15  dcp-ide-fas        pic  x(06)                  .
                   15  dcp-des-pro        pic  x(40)                  .
                   15  dcp-des-pdx        pic  9(01)                  .
                   15  dcp-tip-pro        pic  9(02)                  .
               10  dcp-inf-fis.
                   15  dcp-tip-cfz        pic  9(02)                  .
                   15  dcp-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  dcp-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  dcp-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  dcp-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  dcp-dim-pro.
                       20  dcp-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  dcp-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  dcp-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  dcp-pcl-fis        pic  x(10)                  .
                   15  dcp-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  dcp-coe-div        pic  9(04)v9(03) comp-3     .
               10  dcp-inf-fat.
                   15  dcp-cod-iva        pic  9(05)       comp-3     .
                   15  dcp-ctp-ven        pic  9(07)       comp-3     .
                   15  dcp-umi-ven        pic  x(03)                  .
                   15  dcp-dec-qta        pic  9(01)                  .
                   15  dcp-sgl-vlt        pic  x(03)                  .
                   15  dcp-dec-vlt        pic  9(01)                  .
                   15  dcp-prz-lst        pic  9(09)       comp-3     .
                   15  dcp-epz-rgf        pic  9(01)                  .
                   15  dcp-snx-2qt        pic  9(01)                  .
                   15  dcp-dec-2qt        pic  9(01)                  .
                   15  dcp-snx-3qt        pic  9(01)                  .
                   15  dcp-dec-3qt        pic  9(01)                  .
                   15  dcp-snx-2pz        pic  9(01)                  .
                   15  dcp-aut-lst        pic  x(03)                  .
                   15  dcp-tip-vve        pic  x(03)                  .
               10  dcp-inf-cdv.
                   15  dcp-cat-scr        pic  9(05)       comp-3     .
                   15  dcp-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  dcp-inf-gag.
                   15  dcp-cat-pvg        pic  9(05)       comp-3     .
                   15  dcp-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
                   15  dcp-amm-pvg        pic  9(09)       comp-3     .
               10  dcp-inf-pcs.
                   15  dcp-cod-s01        pic  9(05)       comp-3     .
                   15  dcp-cod-s02        pic  9(05)       comp-3     .
                   15  dcp-cod-s03        pic  9(05)       comp-3     .
               10  dcp-inf-bdg.
                   15  dcp-cla-bdg        pic  9(05)       comp-3     .
               10  dcp-inf-aps.
                   15  dcp-alx-exp.
                       20  filler occurs 80
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [lic]                                    *
      *    *-----------------------------------------------------------*
       fd  lic       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  lic-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  lic-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  lic-k01.
                   15  lic-cod-cli        pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  lic-dat.
               10  lic-ann-rif            pic  9(03)       comp-3     .
               10  lic-num-lic            pic  x(10)                  .
               10  lic-dat-lic            pic  9(07)       comp-3     .
               10  lic-prt-int            pic  x(10)                  .
               10  lic-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [oft]                                    *
      *    *-----------------------------------------------------------*
       fd  oft       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  oft-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  oft-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  oft-k01.
                   15  oft-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  oft-k02.
                   15  oft-ide-dat        pic  9(07)       comp-3     .
                   15  oft-dat-doc        pic  9(07)       comp-3     .
                   15  oft-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  oft-k03.
                   15  oft-dat-doc-3      pic  9(07)       comp-3     .
                   15  oft-cod-dpz        pic  9(02)                  .
                   15  oft-num-doc        pic  9(11)       comp-3     .
                   15  oft-tmo-orf        pic  x(05)                  .
                   15  oft-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  oft-k04.
                   15  oft-scl-ann        pic  9(03)       comp-3     .
                   15  oft-cod-dpz-4      pic  9(02)                  .
                   15  oft-sgl-num        pic  x(03)                  .
                   15  oft-num-doc-4      pic  9(11)       comp-3     .
                   15  oft-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  oft-k05.
                   15  oft-cod-dpz-5      pic  9(02)                  .
                   15  oft-tip-arc        pic  x(01)                  .
                   15  oft-cod-arc        pic  9(07)       comp-3     .
                   15  oft-dat-doc-5      pic  9(07)       comp-3     .
                   15  oft-num-doc-5      pic  9(11)       comp-3     .
                   15  oft-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZTMODAT                      *
      *            *---------------------------------------------------*
               10  oft-k06.
                   15  oft-cod-dpz-6      pic  9(02)                  .
                   15  oft-tmo-orf-6      pic  x(05)                  .
                   15  oft-dat-doc-6      pic  9(07)       comp-3     .
                   15  oft-num-doc-6      pic  9(11)       comp-3     .
                   15  oft-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZOCH                         *
      *            *---------------------------------------------------*
               10  oft-k07.
                   15  oft-cod-dpz-7      pic  9(02)                  .
                   15  oft-flg-och        pic  x(01)                  .
                   15  oft-num-prt-7      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  oft-dat.
               10  oft-ide-ute            pic  x(08)                  .
               10  oft-ide-fas            pic  x(06)                  .
               10  oft-dpz-arc            pic  x(04)                  .
               10  oft-arc-plf            pic  9(07)       comp-3     .
               10  oft-dpz-plf            pic  x(04)                  .
               10  oft-tip-ids            pic  9(02)                  .
               10  oft-cof-dat            pic  9(07)       comp-3     .
               10  oft-cof-num            pic  x(10)                  .
               10  oft-cof-rif            pic  x(40)                  .
               10  oft-dat-cns            pic  9(07)       comp-3     .
               10  oft-fds-dtc            pic  9(02)                  .
               10  oft-tip-eva            pic  9(02)                  .
               10  oft-pri-eva            pic  x(02)                  .
               10  oft-cod-cda            pic  9(03)       comp-3     .
               10  oft-com-int.
                   15  oft-com-rig occurs 03
                                          pic  x(40)                  .
               10  oft-sgl-vlt            pic  x(03)                  .
               10  oft-dec-vlt            pic  9(01)                  .
               10  oft-tdc-vlt            pic  x(01)                  .
               10  oft-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  oft-ass-iva            pic  9(05)       comp-3     .
               10  oft-ctp-acq            pic  9(07)       comp-3     .
               10  oft-inl-dcm            pic  9(02)                  .
               10  oft-inl-pgt            pic  9(02)                  .
               10  oft-cod-lst            pic  x(03)                  .
               10  oft-csr-aaf            pic  9(05)       comp-3     .
               10  oft-psr-aaf occurs 05  pic  9(02)v9(01) comp-3     .
               10  oft-csc-aaf            pic  9(05)       comp-3     .
               10  oft-psc-aaf            pic  9(02)v9(01) comp-3     .
               10  oft-voc-des occurs 06  pic  x(03)                  .
               10  oft-cod-fop            pic  9(07)       comp-3     .
               10  oft-scp-aap            pic  9(02)v9(01) comp-3     .
               10  oft-nos-ban            pic  x(10)                  .
               10  oft-cod-abi            pic  9(05)       comp-3     .
               10  oft-cod-cab            pic  9(05)       comp-3     .
               10  oft-ccc-app            pic  x(12)                  .
               10  oft-ccp-app            pic  x(12)                  .
               10  oft-add-spi            pic  x(03)                  .
               10  oft-add-spb            pic  x(03)                  .
               10  oft-ipr-iel            pic  9(02)                  .
               10  oft-pag-dsm            pic  9(07)       comp-3     .
               10  oft-pag-qaf            pic  9(09)       comp-3     .
               10  oft-pag-act            pic  9(09)       comp-3     .
               10  oft-cod-aqt            pic  9(07)       comp-3     .
               10  oft-pvf-aqt            pic  9(11)       comp-3     .
               10  oft-cod-ime            pic  9(07)       comp-3     .
               10  oft-pvf-ime            pic  9(11)       comp-3     .
               10  oft-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  oft-tot-scc            pic s9(11)       comp-3     .
               10  oft-per-scc            pic  9(02)v9(01) comp-3     .
               10  oft-tot-scp            pic s9(11)       comp-3     .
               10  oft-per-scp            pic  9(02)v9(01) comp-3     .
               10  oft-spe-add occurs 06.
                   15  oft-spe-snx        pic  9(01)                  .
                   15  oft-spe-mad        pic  9(01)                  .
                   15  oft-spe-per        pic  9(02)v9(01) comp-3     .
                   15  oft-spe-ibl        pic  9(02)                  .
                   15  oft-ibt-spe.
                       20  oft-ibx-spe  occurs 09
                                          pic  x(01)                  .
                   15  oft-spe-imp        pic s9(09)       comp-3     .
               10  oft-tot-doc            pic s9(11)       comp-3     .
               10  oft-ctr-stp            pic  9(02)                  .
               10  oft-flg-ela.
                   15  oft-flg-blo.
                       20  oft-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  oft-flg-nbl.
                       20  oft-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  oft-flg-pul            pic  x(01)                  .
               10  oft-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

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
               10  ofr-cof-dat            pic  9(07)       comp-3     .
               10  ofr-cof-num            pic  x(10)                  .
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
               10  ofr-sgl-vlt            pic  x(03)                  .
               10  ofr-dec-vlt            pic  9(01)                  .
               10  ofr-tdc-vlt            pic  x(01)                  .
               10  ofr-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  ofr-lgv-vlt            pic  x(03)                  .
               10  ofr-lgv-dcv            pic  9(01)                  .
               10  ofr-lgv-tdc            pic  x(01)                  .
               10  ofr-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  ofr-lgv-pdt            pic  9(02)v9(01) comp-3     .
               10  ofr-dec-prz            pic  9(01)                  .
               10  ofr-prz-acq            pic  9(09)       comp-3     .
               10  ofr-snx-2pz            pic  9(01)                  .
               10  ofr-dec-2pz            pic  9(01)                  .
               10  ofr-prz-a02            pic  9(09)       comp-3     .
               10  ofr-epz-rgo            pic  9(01)                  .
               10  ofr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  ofr-imp-rig            pic s9(11)       comp-3     .
               10  ofr-iau-rig            pic s9(11)       comp-3     .
               10  ofr-cpv-aap            pic  9(05)       comp-3     .
               10  ofr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  ofr-fsp-rig            pic  9(02)                  .
               10  ofr-cpv-rig            pic  9(05)       comp-3     .
               10  ofr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  ofr-pvf-rig            pic s9(11)       comp-3     .
               10  ofr-dat-cns            pic  9(07)                  .
               10  ofr-pri-eva            pic  x(02)                  .
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
      *    * File Description [aaf]                                    *
      *    *-----------------------------------------------------------*
       fd  aaf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  aaf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  aaf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRFNFM                         *
      *            *---------------------------------------------------*
               10  aaf-k01.
                   15  aaf-tip-mag        pic  9(02)                  .
                   15  aaf-num-pro        pic  9(07)       comp-3     .
                   15  aaf-cod-dcf        pic  9(07)       comp-3     .
                   15  aaf-fda-pif        pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  aaf-k02.
                   15  aaf-ide-dat        pic  9(07)       comp-3     .
                   15  aaf-tip-mag-2      pic  9(02)                  .
                   15  aaf-num-pro-2      pic  9(07)       comp-3     .
                   15  aaf-cod-dcf-2      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-2      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNPRFM                         *
      *            *---------------------------------------------------*
               10  aaf-k03.
                   15  aaf-cod-dcf-3      pic  9(07)       comp-3     .
                   15  aaf-tip-mag-3      pic  9(02)                  .
                   15  aaf-num-pro-3      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-3      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PRFMFN                         *
      *            *---------------------------------------------------*
               10  aaf-k04.
                   15  aaf-tip-mag-4      pic  9(02)                  .
                   15  aaf-num-pro-4      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-4      pic  x(14)                  .
                   15  aaf-cod-dcf-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : COPSFN                         *
      *            *---------------------------------------------------*
               10  aaf-k05.
                   15  aaf-cod-dcf-5      pic  9(07)       comp-3     .
                   15  aaf-cop-sfn        pic  x(14)                  .
                   15  aaf-tip-mag-5      pic  9(02)                  .
                   15  aaf-num-pro-5      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-5      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  aaf-dat.
               10  aaf-ide-ute            pic  x(08)                  .
               10  aaf-ide-fas            pic  x(06)                  .
               10  aaf-dep-sfn            pic  x(40)                  .
               10  aaf-xdp-sfn            pic  9(01)                  .
               10  aaf-snx-tum            pic  x(01)                  .
               10  aaf-umf-tum            pic  x(03)                  .
               10  aaf-nde-tum            pic  9(01)                  .
               10  aaf-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  aaf-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  aaf-dpz-dcf            pic  x(04)                  .
               10  aaf-ann-not            pic  x(40)                  .
               10  aaf-tmp-cns            pic  9(03)       comp-3     .
               10  aaf-sgl-vlt            pic  x(03)                  .
               10  aaf-dec-vlt            pic  9(01)                  .
               10  aaf-dec-prz            pic  9(01)                  .
               10  aaf-tip-pza            pic  9(02)                  .
               10  aaf-lot-acq            pic  9(06)v9(03) comp-3     .
               10  aaf-tap-pes            pic  9(02)                  .
               10  aaf-uda-pes            pic  9(07)       comp-3     .
               10  aaf-lgv-vlt            pic  x(03)                  .
               10  aaf-lgv-dcv            pic  9(01)                  .
               10  aaf-lgv-tdc            pic  x(01)                  .
               10  aaf-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  aaf-lgv-pdt            pic  9(02)v9(01) comp-3     .
               10  aaf-tbl-pes.
                   15  aaf-ele-pes occurs 06.
                       20  aaf-qta-pes    pic  9(06)v9(03) comp-3     .
                       20  aaf-prz-pes    pic  9(09)       comp-3     .
                       20  aaf-csr-pes    pic  9(05)       comp-3     .
                       20  aaf-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  aaf-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

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
               10  bft-sgl-vlt            pic  x(03)                  .
               10  bft-dec-vlt            pic  9(01)                  .
               10  bft-tdc-vlt            pic  x(01)                  .
               10  bft-cdc-vlt            pic  9(06)v9(05) comp-3     .
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
      *    * File Description [bfr]                                    *
      *    *-----------------------------------------------------------*
       fd  bfr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  bfr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  bfr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  bfr-k01.
                   15  bfr-num-prt        pic  9(11)       comp-3     .
                   15  bfr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  bfr-k02.
                   15  bfr-cod-dpz        pic  9(02)                  .
                   15  bfr-tip-mag        pic  9(02)                  .
                   15  bfr-num-mag        pic  9(07)       comp-3     .
                   15  bfr-dat-reg        pic  9(07)       comp-3     .
                   15  bfr-num-prt-2      pic  9(11)       comp-3     .
                   15  bfr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFORF                         *
      *            *---------------------------------------------------*
               10  bfr-k03.
                   15  bfr-cod-dpz-3      pic  9(02)                  .
                   15  bfr-orf-prt        pic  9(11)       comp-3     .
                   15  bfr-orf-prg        pic  9(05)       comp-3     .
                   15  bfr-num-prt-3      pic  9(11)       comp-3     .
                   15  bfr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  bfr-k04.
                   15  bfr-cod-dpz-4      pic  9(02)                  .
                   15  bfr-flg-rch        pic  x(01)                  .
                   15  bfr-tip-mag-4      pic  9(02)                  .
                   15  bfr-num-mag-4      pic  9(07)       comp-3     .
                   15  bfr-num-prt-4      pic  9(11)       comp-3     .
                   15  bfr-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : RCHARC                         *
      *            *---------------------------------------------------*
               10  bfr-k05.
                   15  bfr-cod-dpz-5      pic  9(02)                  .
                   15  bfr-flg-rch-5      pic  x(01)                  .
                   15  bfr-tip-arc        pic  x(01)                  .
                   15  bfr-cod-arc        pic  9(07)       comp-3     .
                   15  bfr-num-prt-5      pic  9(11)       comp-3     .
                   15  bfr-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  bfr-dat.
               10  bfr-cod-tmb            pic  x(05)                  .
               10  bfr-dpz-arc            pic  x(04)                  .
               10  bfr-dat-doc            pic  9(07)                  .
               10  bfr-num-doc            pic  x(10)                  .
               10  bfr-bld-flb            pic  9(01)                  .
               10  bfr-bld-tpb            pic  9(01)                  .
               10  bfr-bld-rgb            pic  9(01)                  .
               10  bfr-tip-rig            pic  x(05)                  .
               10  bfr-alf-mag            pic  x(14)                  .
               10  bfr-sgl-vrn            pic  x(14)                  .
               10  bfr-fda-pif            pic  x(14)                  .
               10  bfr-cop-sfn            pic  x(14)                  .
               10  bfr-snx-tum            pic  x(01)                  .
               10  bfr-umf-tum            pic  x(03)                  .
               10  bfr-nde-tum            pic  9(01)                  .
               10  bfr-cmo-tum            pic  9(06)v9(03)            .
               10  bfr-cdi-tum            pic  9(06)v9(03)            .
               10  bfr-des-ext            pic  9(01)                  .
               10  bfr-des-rig            pic  x(40)                  .
               10  bfr-tip-pro            pic  9(02)                  .
               10  bfr-cod-iva            pic  9(05)       comp-3     .
               10  bfr-ctp-acq            pic  9(07)       comp-3     .
               10  bfr-umi-acq            pic  x(03)                  .
               10  bfr-dec-qta            pic  9(01)                  .
               10  bfr-qta-fda            pic s9(06)v9(03) comp-3     .
               10  bfr-qta-acq            pic s9(06)v9(03) comp-3     .
               10  bfr-cod-dsl            pic  x(07)                  .
               10  bfr-snx-2qt            pic  9(01)                  .
               10  bfr-dec-2qt            pic  9(01)                  .
               10  bfr-qta-a02            pic s9(06)v9(03) comp-3     .
               10  bfr-snx-3qt            pic  9(01)                  .
               10  bfr-dec-3qt            pic  9(01)                  .
               10  bfr-qta-a03            pic s9(06)v9(03) comp-3     .
               10  bfr-sgl-vlt            pic  x(03)                  .
               10  bfr-dec-vlt            pic  9(01)                  .
               10  bfr-tdc-vlt            pic  x(01)                  .
               10  bfr-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  bfr-lgv-vlt            pic  x(03)                  .
               10  bfr-lgv-dcv            pic  9(01)                  .
               10  bfr-lgv-tdc            pic  x(01)                  .
               10  bfr-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  bfr-lgv-pdt            pic  9(02)v9(01) comp-3     .
               10  bfr-dec-prz            pic  9(01)                  .
               10  bfr-prz-acq            pic  9(09)       comp-3     .
               10  bfr-snx-2pz            pic  9(01)                  .
               10  bfr-dec-2pz            pic  9(01)                  .
               10  bfr-prz-a02            pic  9(09)       comp-3     .
               10  bfr-epz-rgf            pic  9(01)                  .
               10  bfr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  bfr-imp-rig            pic s9(11)       comp-3     .
               10  bfr-iau-rig            pic s9(11)       comp-3     .
               10  bfr-cpv-aap            pic  9(05)       comp-3     .
               10  bfr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  bfr-fsp-rig            pic  9(02)                  .
               10  bfr-cpv-rig            pic  9(05)       comp-3     .
               10  bfr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  bfr-pvf-rig            pic s9(11)       comp-3     .
               10  bfr-rdo-tip            pic  x(05)                  .
               10  bfr-rdo-dat            pic  9(07)       comp-3     .
               10  bfr-rdo-num            pic  9(11)       comp-3     .
               10  bfr-orf-tip            pic  x(05)                  .
               10  bfr-orf-dat            pic  9(07)       comp-3     .
               10  bfr-orf-num            pic  9(11)       comp-3     .
               10  bfr-orf-fzs            pic  x(01)                  .
               10  bfr-orm-prt            pic  9(11)       comp-3     .
               10  bfr-flg-ela.
                   15  bfr-flg-blo.
                       20  bfr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  bfr-flg-nbl.
                       20  bfr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  bfr-flg-pul            pic  x(01)                  .
               10  bfr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [fft]                                    *
      *    *-----------------------------------------------------------*
       fd  fft       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fft-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fft-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fft-k01.
                   15  fft-num-prt-1      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fft-k02.
                   15  fft-ide-dat-2      pic  9(07)                  .
                   15  fft-dat-reg-2      pic  9(07)                  .
                   15  fft-num-prt-2      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fft-k03.
                   15  fft-dat-reg-3      pic  9(07)                  .
                   15  fft-cod-dpz-3      pic  9(02)                  .
                   15  fft-cod-tmf-3      pic  x(05)                  .
                   15  fft-num-prt-3      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DPZARCDAT                      *
      *            *---------------------------------------------------*
               10  fft-k04.
                   15  fft-cod-dpz-4      pic  9(02)                  .
                   15  fft-tip-arc-4      pic  x(01)                  .
                   15  fft-cod-arc-4      pic  9(07)                  .
                   15  fft-dat-reg-4      pic  9(07)                  .
                   15  fft-num-prt-4      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DPZTMBDAT                      *
      *            *---------------------------------------------------*
               10  fft-k05.
                   15  fft-cod-dpz-5      pic  9(02)                  .
                   15  fft-cod-tmf-5      pic  x(05)                  .
                   15  fft-dat-reg-5      pic  9(07)                  .
                   15  fft-num-prt-5      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fft-dat.
               10  fft-ide-dat            pic  9(07)                  .
               10  fft-ide-ute            pic  x(08)                  .
               10  fft-ide-fas            pic  x(06)                  .
               10  fft-num-prt            pic  9(11)                  .
               10  fft-cod-tmf            pic  x(05)                  .
               10  fft-tip-doc            pic  9(02)                  .
               10  fft-tpv-doc            pic  9(02)                  .
               10  fft-snx-ird            pic  x(01)                  .
               10  fft-cod-dpz            pic  9(02)                  .
               10  fft-dat-reg            pic  9(07)                  .
               10  fft-tip-arc            pic  x(01)                  .
               10  fft-cod-arc            pic  9(07)                  .
               10  fft-dpz-arc            pic  x(04)                  .
               10  fft-arc-plf            pic  9(07)                  .
               10  fft-dpz-plf            pic  x(04)                  .
               10  fft-dat-doc            pic  9(07)                  .
               10  fft-num-doc            pic  x(10)                  .
               10  fft-sgl-vlt            pic  x(03)                  .
               10  fft-dec-vlt            pic  9(01)                  .
               10  fft-tdc-vlt            pic  x(01)                  .
               10  fft-cdc-vlt            pic  9(06)v9(05)            .
               10  fft-ass-iva            pic  9(05)                  .
               10  fft-ctp-acq            pic  9(07)                  .
               10  fft-inl-pgt            pic  9(02)                  .
               10  fft-cod-lst            pic  x(03)                  .
               10  fft-csr-aaf            pic  9(05)                  .
               10  fft-psr-aaf occurs 05  pic  9(02)v9(01)            .
               10  fft-csc-aaf            pic  9(05)                  .
               10  fft-psc-aaf            pic  9(02)v9(01)            .
               10  fft-voc-des occurs 06  pic  x(03)                  .
               10  fft-iiv-doc            pic s9(11)                  .
               10  fft-imp-doc            pic s9(11)                  .
               10  fft-cod-fop            pic  9(07)                  .
               10  fft-scp-aap            pic  9(02)v9(01)            .
               10  fft-nos-ban            pic  x(10)                  .
               10  fft-cod-abi            pic  9(05)                  .
               10  fft-cod-cab            pic  9(05)                  .
               10  fft-ccc-app            pic  x(12)                  .
               10  fft-ccp-app            pic  x(12)                  .
               10  fft-cod-bef            pic  9(05)                  .
               10  fft-add-spi            pic  x(03)                  .
               10  fft-add-spb            pic  x(03)                  .
               10  fft-ipr-iel            pic  9(02)                  .
               10  fft-pag-dsm            pic  9(07)                  .
               10  fft-pag-qaf            pic  9(09)                  .
               10  fft-pag-act            pic  9(09)                  .
               10  fft-cod-aqt            pic  9(07)                  .
               10  fft-pvf-aqt            pic  9(11)                  .
               10  fft-cod-ime            pic  9(07)                  .
               10  fft-pvf-ime            pic  9(11)                  .
               10  fft-tot-rig occurs 09  pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
               10  fft-tot-scc            pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
               10  fft-per-scc            pic  9(02)v9(01)            .
               10  fft-civ-scc            pic  9(05)                  .
               10  fft-ccp-scc            pic  9(07)                  .
               10  fft-tot-scp            pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
               10  fft-per-scp            pic  9(02)v9(01)            .
               10  fft-civ-scp            pic  9(05)                  .
               10  fft-ccp-scp            pic  9(07)                  .
               10  fft-spe-add occurs 06.
                   15  fft-spe-snx        pic  9(01)                  .
                   15  fft-spe-mad        pic  9(01)                  .
                   15  fft-spe-per        pic  9(02)v9(01)            .
                   15  fft-spe-ibl        pic  9(02)                  .
                   15  fft-ibt-spe.
                       20  fft-ibx-spe  occurs 09
                                          pic  x(01)                  .
                   15  fft-spe-imp        pic s9(09)       sign is
                                                           trailing
                                                           separate
                                                           character  .
                   15  fft-spe-civ        pic  9(05)                  .
                   15  fft-spe-ccp        pic  9(07)                  .
               10  fft-tot-spi            pic s9(09)       sign is
                                                           trailing
                                                           separate
                                                           character  .
               10  fft-civ-spi            pic  9(05)                  .
               10  fft-ccp-spi            pic  9(07)                  .
               10  fft-tot-spb            pic s9(09)       sign is
                                                           trailing
                                                           separate
                                                           character  .
               10  fft-civ-spb            pic  9(05)                  .
               10  fft-ccp-spb            pic  9(07)                  .
               10  fft-des-cau.
                   15  fft-rig-cau occurs 04
                                          pic  x(40)                  .
               10  fft-num-giv            pic  9(02)                  .
               10  fft-npt-iva            pic  9(07)                  .
               10  fft-iva-cst.
                   15  fft-iva-rig occurs 06.
                       20  fft-iva-cod    pic  9(05)                  .
                       20  fft-iva-ibv    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
                       20  fft-iva-ibl    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
                       20  fft-iva-imp    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
                   15  fft-iva-tdo        pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
               10  fft-dat-mgd            pic  9(07)                  .
               10  fft-prt-mgd            pic  9(07)                  .
               10  fft-nrg-mgd            pic  9(02)                  .
               10  fft-dri-mgd            pic  9(07)                  .
               10  fft-nri-mgd            pic  x(10)                  .
               10  fft-prt-scf            pic  9(11)                  .
               10  fft-nps-scf            pic  9(11)                  .
               10  fft-ctr-scf            pic  9(02)                  .
               10  fft-prt-mag            pic  9(11)                  .
               10  fft-flg-ela.
                   15  fft-flg-blo.
                       20  fft-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fft-flg-nbl.
                       20  fft-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fft-flg-pul            pic  x(01)                  .
               10  fft-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ffr]                                    *
      *    *-----------------------------------------------------------*
       fd  ffr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ffr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ffr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  ffr-k01.
                   15  ffr-num-prt-1      pic  9(11)       comp-3     .
                   15  ffr-num-prg-1      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  ffr-k02.
                   15  ffr-cod-dpz-2      pic  9(02)                  .
                   15  ffr-tip-mag-2      pic  9(02)                  .
                   15  ffr-num-mag-2      pic  9(07)       comp-3     .
                   15  ffr-dat-reg-2      pic  9(07)       comp-3     .
                   15  ffr-num-prt-2      pic  9(11)       comp-3     .
                   15  ffr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATMAG                         *
      *            *---------------------------------------------------*
               10  ffr-k03.
                   15  ffr-cod-dpz-3      pic  9(02)                  .
                   15  ffr-dat-reg-3      pic  9(07)       comp-3     .
                   15  ffr-tip-mag-3      pic  9(02)                  .
                   15  ffr-num-mag-3      pic  9(07)       comp-3     .
                   15  ffr-num-prt-3      pic  9(11)       comp-3     .
                   15  ffr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RIFBFO                         *
      *            *---------------------------------------------------*
               10  ffr-k04.
                   15  ffr-cod-dpz-4      pic  9(02)                  .
                   15  ffr-bfo-prt-4      pic  9(11)       comp-3     .
                   15  ffr-bfo-prg-4      pic  9(05)       comp-3     .
                   15  ffr-num-prt-4      pic  9(11)       comp-3     .
                   15  ffr-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ffr-dat.
               10  ffr-num-prt            pic  9(11)       comp-3     .
               10  ffr-num-prg            pic  9(05)       comp-3     .
               10  ffr-cod-tmf            pic  x(05)                  .
               10  ffr-cod-dpz            pic  9(02)                  .
               10  ffr-dat-reg            pic  9(07)       comp-3     .
               10  ffr-tip-arc            pic  x(01)                  .
               10  ffr-cod-arc            pic  9(07)       comp-3     .
               10  ffr-dpz-arc            pic  x(04)                  .
               10  ffr-dat-doc            pic  9(07)       comp-3     .
               10  ffr-num-doc            pic  x(10)                  .
               10  ffr-bld-flb            pic  9(01)                  .
               10  ffr-bld-tpb            pic  9(01)                  .
               10  ffr-bld-rgb            pic  9(01)                  .
               10  ffr-tip-rig            pic  x(05)                  .
               10  ffr-tip-mag            pic  9(02)                  .
               10  ffr-num-mag            pic  9(07)       comp-3     .
               10  ffr-alf-mag            pic  x(14)                  .
               10  ffr-sgl-vrn            pic  x(14)                  .
               10  ffr-fda-pif            pic  x(14)                  .
               10  ffr-cop-sfn            pic  x(14)                  .
               10  ffr-snx-tum            pic  x(01)                  .
               10  ffr-umf-tum            pic  x(03)                  .
               10  ffr-nde-tum            pic  9(01)                  .
               10  ffr-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  ffr-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  ffr-des-ext            pic  9(01)                  .
               10  ffr-des-rig            pic  x(40)                  .
               10  ffr-tip-pro            pic  9(02)                  .
               10  ffr-cod-iva            pic  9(05)       comp-3     .
               10  ffr-ctp-acq            pic  9(07)       comp-3     .
               10  ffr-umi-acq            pic  x(03)                  .
               10  ffr-dec-qta            pic  9(01)                  .
               10  ffr-qta-fda            pic s9(06)v9(03) comp-3     .
               10  ffr-qta-acq            pic s9(06)v9(03) comp-3     .
               10  ffr-cod-dsl            pic  x(07)                  .
               10  ffr-snx-2qt            pic  9(01)                  .
               10  ffr-dec-2qt            pic  9(01)                  .
               10  ffr-qta-a02            pic s9(06)v9(03) comp-3     .
               10  ffr-snx-3qt            pic  9(01)                  .
               10  ffr-dec-3qt            pic  9(01)                  .
               10  ffr-qta-a03            pic s9(06)v9(03) comp-3     .
               10  ffr-sgl-vlt            pic  x(03)                  .
               10  ffr-dec-vlt            pic  9(01)                  .
               10  ffr-tdc-vlt            pic  x(01)                  .
               10  ffr-cdc-vlt            pic  9(06)v9(05) comp-3     .
               10  ffr-lgv-vlt            pic  x(03)                  .
               10  ffr-lgv-dcv            pic  9(01)                  .
               10  ffr-lgv-tdc            pic  x(01)                  .
               10  ffr-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  ffr-lgv-pdt            pic  9(02)v9(01) comp-3     .
               10  ffr-dec-prz            pic  9(01)                  .
               10  ffr-prz-acq            pic  9(09)       comp-3     .
               10  ffr-snx-2pz            pic  9(01)                  .
               10  ffr-dec-2pz            pic  9(01)                  .
               10  ffr-prz-a02            pic  9(09)       comp-3     .
               10  ffr-epz-rgf            pic  9(01)                  .
               10  ffr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  ffr-imp-rig            pic s9(11)       comp-3     .
               10  ffr-iau-rig            pic s9(11)       comp-3     .
               10  ffr-cpv-aap            pic  9(05)       comp-3     .
               10  ffr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  ffr-fsp-rig            pic  9(02)                  .
               10  ffr-cpv-rig            pic  9(05)       comp-3     .
               10  ffr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  ffr-pvf-rig            pic s9(11)       comp-3     .
               10  ffr-bfo-tip            pic  x(05)                  .
               10  ffr-bfo-dat            pic  9(07)       comp-3     .
               10  ffr-bfo-prt            pic  9(11)       comp-3     .
               10  ffr-bfo-prg            pic  9(05)       comp-3     .
               10  ffr-bfo-ddo            pic  9(07)       comp-3     .
               10  ffr-bfo-ndo            pic  x(10)                  .
               10  ffr-flg-ela.
                   15  ffr-flg-blo.
                       20  ffr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  ffr-flg-nbl.
                       20  ffr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  ffr-flg-pul            pic  x(01)                  .
               10  ffr-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [age]                                    *
      *    *-----------------------------------------------------------*
       fd  age       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  age-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  age-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODAGE                         *
      *            *---------------------------------------------------*
               10  age-k01.
                   15  age-cod-age        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  age-k02.
                   15  age-ide-dat        pic  9(07)       comp-3     .
                   15  age-cod-age-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  age-k03.
                   15  age-rag-key        pic  x(40)                  .
                   15  age-cod-age-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  age-k04.
                   15  age-cod-mne        pic  x(10)                  .
                   15  age-cod-age-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : NOMKEY                         *
      *            *---------------------------------------------------*
               10  age-k05.
                   15  age-nom-key        pic  x(20)                  .
                   15  age-cod-age-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : SUPAGE                         *
      *            *---------------------------------------------------*
               10  age-k06.
                   15  age-sup-age        pic  9(07)       comp-3     .
                   15  age-cod-age-6      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  age-dat.
               10  age-ide-ute            pic  x(08)                  .
               10  age-ide-fas            pic  x(06)                  .
               10  age-nom-age            pic  x(20)                  .
               10  age-rag-soc            pic  x(40)                  .
               10  age-via-age            pic  x(40)                  .
               10  age-loc-age            pic  x(40)                  .
               10  age-cod-naz            pic  x(03)                  .
               10  age-cod-cmn            pic  9(05)       comp-3     .
               10  age-cod-fzn            pic  9(03)       comp-3     .
               10  age-cod-lct            pic  9(03)       comp-3     .
               10  age-num-tel            pic  x(20)                  .
               10  age-num-fax            pic  x(20)                  .
               10  age-num-tlx            pic  x(20)                  .
               10  age-nom-int            pic  x(30)                  .
               10  age-prt-iva            pic  9(11)       comp-3     .
               10  age-cod-fis            pic  x(16)                  .
               10  age-cod-cge            pic  9(07)       comp-3     .
               10  age-cat-pvg            pic  9(05)       comp-3     .
               10  age-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  age-cla-bdg            pic  9(05)       comp-3     .
               10  age-tip-mat            pic  x(01)                  .
               10  age-flg-cpv.
                   15  age-flg-ele occurs 09
                                          pic  x(01)                  .
               10  age-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [vet]                                    *
      *    *-----------------------------------------------------------*
       fd  vet       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  vet-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  vet-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODVET                         *
      *            *---------------------------------------------------*
               10  vet-k01.
                   15  vet-cod-vet        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  vet-k02.
                   15  vet-ide-dat        pic  9(07)       comp-3     .
                   15  vet-cod-vet-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  vet-k03.
                   15  vet-rag-key        pic  x(40)                  .
                   15  vet-cod-vet-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  vet-k04.
                   15  vet-cod-mne        pic  x(10)                  .
                   15  vet-cod-vet-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PRTIVA                         *
      *            *---------------------------------------------------*
               10  vet-k05.
                   15  vet-prt-iva        pic  9(11)       comp-3     .
                   15  vet-cod-vet-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CODFIS                         *
      *            *---------------------------------------------------*
               10  vet-k06.
                   15  vet-cod-fis        pic  x(16)                  .
                   15  vet-cod-vet-6      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  vet-dat.
               10  vet-ide-ute            pic  x(08)                  .
               10  vet-ide-fas            pic  x(06)                  .
               10  vet-rag-soc            pic  x(40)                  .
               10  vet-via-vet            pic  x(40)                  .
               10  vet-loc-vet            pic  x(40)                  .
               10  vet-cod-naz            pic  x(03)                  .
               10  vet-cod-cmn            pic  9(05)       comp-3     .
               10  vet-cod-fzn            pic  9(03)       comp-3     .
               10  vet-cod-lct            pic  9(03)       comp-3     .
               10  vet-num-tel            pic  x(20)                  .
               10  vet-num-fax            pic  x(20)                  .
               10  vet-num-tlx            pic  x(20)                  .
               10  vet-nom-int            pic  x(30)                  .
               10  vet-tip-vet            pic  x(01)                  .
               10  vet-alx-exp.
                   15  filler  occurs 79  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [oct]                                       *
      *    *-----------------------------------------------------------*
       01  f-oct.
           05  f-oct-nam                  pic  x(04)                  .
           05  f-oct-pat                  pic  x(40)                  .
           05  f-oct-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ocr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ocr.
           05  f-ocr-nam                  pic  x(04)                  .
           05  f-ocr-pat                  pic  x(40)                  .
           05  f-ocr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bit]                                       *
      *    *-----------------------------------------------------------*
       01  f-bit.
           05  f-bit-nam                  pic  x(04)                  .
           05  f-bit-pat                  pic  x(40)                  .
           05  f-bit-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bir]                                       *
      *    *-----------------------------------------------------------*
       01  f-bir.
           05  f-bir-nam                  pic  x(04)                  .
           05  f-bir-pat                  pic  x(40)                  .
           05  f-bir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fir]                                       *
      *    *-----------------------------------------------------------*
       01  f-fir.
           05  f-fir-nam                  pic  x(04)                  .
           05  f-fir-pat                  pic  x(40)                  .
           05  f-fir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [dcp]                                       *
      *    *-----------------------------------------------------------*
       01  f-dcp.
           05  f-dcp-nam                  pic  x(04)                  .
           05  f-dcp-pat                  pic  x(40)                  .
           05  f-dcp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [lic]                                       *
      *    *-----------------------------------------------------------*
       01  f-lic.
           05  f-lic-nam                  pic  x(04)                  .
           05  f-lic-pat                  pic  x(40)                  .
           05  f-lic-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [oft]                                       *
      *    *-----------------------------------------------------------*
       01  f-oft.
           05  f-oft-nam                  pic  x(04)                  .
           05  f-oft-pat                  pic  x(40)                  .
           05  f-oft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ofr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ofr.
           05  f-ofr-nam                  pic  x(04)                  .
           05  f-ofr-pat                  pic  x(40)                  .
           05  f-ofr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [aaf]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaf.
           05  f-aaf-nam                  pic  x(04)                  .
           05  f-aaf-pat                  pic  x(40)                  .
           05  f-aaf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bft]                                       *
      *    *-----------------------------------------------------------*
       01  f-bft.
           05  f-bft-nam                  pic  x(04)                  .
           05  f-bft-pat                  pic  x(40)                  .
           05  f-bft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bfr]                                       *
      *    *-----------------------------------------------------------*
       01  f-bfr.
           05  f-bfr-nam                  pic  x(04)                  .
           05  f-bfr-pat                  pic  x(40)                  .
           05  f-bfr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fft]                                       *
      *    *-----------------------------------------------------------*
       01  f-fft.
           05  f-fft-nam                  pic  x(04)                  .
           05  f-fft-pat                  pic  x(40)                  .
           05  f-fft-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ffr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ffr.
           05  f-ffr-nam                  pic  x(04)                  .
           05  f-ffr-pat                  pic  x(40)                  .
           05  f-ffr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [age]                                       *
      *    *-----------------------------------------------------------*
       01  f-age.
           05  f-age-nam                  pic  x(04)                  .
           05  f-age-pat                  pic  x(40)                  .
           05  f-age-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [vet]                                       *
      *    *-----------------------------------------------------------*
       01  f-vet.
           05  f-vet-nam                  pic  x(04)                  .
           05  f-vet-pat                  pic  x(40)                  .
           05  f-vet-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
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
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [fft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rffft"                          .
      *        *-------------------------------------------------------*
      *        * [ffr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .

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
                     "cnv3p1"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnv3p1  "                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONE PER RELEASE 3.1       "       .

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
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det prezzo netto                             *
      *        *-------------------------------------------------------*
           05  w-det-prz-net.
               10  w-det-prz-net-prz      pic  9(09)                  .
               10  w-det-prz-net-psc occurs 05
                                          pic  9(02)v9(01)            .
               10  w-det-prz-net-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Cal                               *
      *    *-----------------------------------------------------------*
       01  w-cal.
      *        *-------------------------------------------------------*
      *        * Work per Cal importo scontato                         *
      *        *-------------------------------------------------------*
           05  w-cal-imp-sco.
               10  w-cal-imp-sco-iml      pic s9(11)                  .
               10  w-cal-imp-sco-psc      pic  9(02)v9(01)            .
               10  w-cal-imp-sco-w01      pic  9(03)v9(01)            .
               10  w-cal-imp-sco-w02      pic s9(14)v9(01)            .
               10  w-cal-imp-sco-imn      pic s9(11)                  .
               10  w-cal-imp-sco-ams      pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
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
      *              * Conversione [oct]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-oct-000      thru exe-cnv-oct-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ocr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ocr-000      thru exe-cnv-ocr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bit]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bit-000      thru exe-cnv-bit-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bir]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bir-000      thru exe-cnv-bir-999        .
      *              *-------------------------------------------------*
      *              * Conversione [fir]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-fir-000      thru exe-cnv-fir-999        .
      *              *-------------------------------------------------*
      *              * Conversione [dcp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-dcp-000      thru exe-cnv-dcp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [lic]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-lic-000      thru exe-cnv-lic-999        .
      *              *-------------------------------------------------*
      *              * Conversione [oft]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-oft-000      thru exe-cnv-oft-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ofr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ofr-000      thru exe-cnv-ofr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [aaf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaf-000      thru exe-cnv-aaf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bft]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bft-000      thru exe-cnv-bft-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bfr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bfr-000      thru exe-cnv-bfr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [fft]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-fft-000      thru exe-cnv-fft-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ffr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ffr-000      thru exe-cnv-ffr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [age]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-age-000      thru exe-cnv-age-999        .
      *              *-------------------------------------------------*
      *              * Conversione [vet]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-vet-000      thru exe-cnv-vet-999        .
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
      *    * Conversione [oct]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-oct-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "oct "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-oct-999.
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
       exe-cnv-oct-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-oct-pat              .
       exe-cnv-oct-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-oct]                         *
      *                  *---------------------------------------------*
           open      i-o    oct                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-oct]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-cnv-oct-200.
      *              *-------------------------------------------------*
      *              * Start su [old-oct]                              *
      *              *-------------------------------------------------*
           move      low-values           to   oct-k01                .
           start     oct    key not less
                            oct-k01
                            invalid key
                            go to exe-cnv-oct-800.
       exe-cnv-oct-250.
      *              *-------------------------------------------------*
      *              * Next su [old-oct]                               *
      *              *-------------------------------------------------*
           read      oct    next
                            with no lock
                            at end
                            go to exe-cnv-oct-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-oct-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-oct]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-cnv-oct-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-oct]                          *
      *              *-------------------------------------------------*
       exe-cnv-oct-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-oct                 .
           move      oct-ide-dat          to   rf-oct-ide-dat         .
           move      oct-ide-ute          to   rf-oct-ide-ute         .
           move      oct-ide-fas          to   rf-oct-ide-fas         .
           move      oct-num-prt          to   rf-oct-num-prt         .
           move      oct-tmo-orc          to   rf-oct-tmo-orc         .
           move      oct-cod-dpz          to   rf-oct-cod-dpz         .
           move      oct-dat-doc          to   rf-oct-dat-doc         .
           move      oct-num-doc          to   rf-oct-num-doc         .
           move      oct-scl-ann          to   rf-oct-scl-ann         .
           move      oct-sgl-num          to   rf-oct-sgl-num         .
           move      oct-tip-arc          to   rf-oct-tip-arc         .
           move      oct-cod-arc          to   rf-oct-cod-arc         .
           move      oct-dpz-arc          to   rf-oct-dpz-arc         .
           move      oct-tip-frn          to   rf-oct-tip-frn         .
           move      oct-arc-plf          to   rf-oct-arc-plf         .
           move      oct-dpz-plf          to   rf-oct-dpz-plf         .
           move      oct-tip-ftz          to   rf-oct-tip-ftz         .
           move      oct-tip-ids          to   rf-oct-tip-ids         .
           move      oct-ocl-dat          to   rf-oct-ocl-dat         .
           move      oct-ocl-num          to   rf-oct-ocl-num         .
           move      oct-ocl-rif          to   rf-oct-ocl-rif         .
           move      oct-dat-cns          to   rf-oct-dat-cns         .
           move      oct-fds-dtc          to   rf-oct-fds-dtc         .
           move      oct-tip-eva          to   rf-oct-tip-eva         .
           move      oct-pri-eva          to   rf-oct-pri-eva         .
           move      oct-cod-cdv          to   rf-oct-cod-cdv         .
           move      oct-com-int          to   rf-oct-com-int         .
           move      oct-cod-lng          to   rf-oct-cod-lng         .
           move      oct-sgl-vpf          to   rf-oct-sgl-vpf         .
           move      oct-dec-vpf          to   rf-oct-dec-vpf         .
           move      oct-tdc-vpf          to   rf-oct-tdc-vpf         .
           move      oct-cdc-vpf          to   rf-oct-cdc-vpf         .
           move      oct-ass-iva          to   rf-oct-ass-iva         .
           move      oct-ctp-ven          to   rf-oct-ctp-ven         .
           move      oct-fat-sep          to   rf-oct-fat-sep         .
           move      oct-inl-dcm          to   rf-oct-inl-dcm         .
           move      oct-inl-pgt          to   rf-oct-inl-pgt         .
           move      oct-cod-lst          to   rf-oct-cod-lst         .
           move      oct-csr-aac          to   rf-oct-csr-aac         .
           move      oct-psr-aac (1)      to   rf-oct-psr-aac (1)     .
           move      oct-psr-aac (2)      to   rf-oct-psr-aac (2)     .
           move      oct-psr-aac (3)      to   rf-oct-psr-aac (3)     .
           move      oct-psr-aac (4)      to   rf-oct-psr-aac (4)     .
           move      oct-psr-aac (5)      to   rf-oct-psr-aac (5)     .
           move      oct-csc-aac          to   rf-oct-csc-aac         .
           move      oct-psc-aac          to   rf-oct-psc-aac         .
           move      oct-cpv-aac          to   rf-oct-cpv-aac         .
           move      oct-ppv-aac (1)      to   rf-oct-ppv-aac (1)     .
           move      oct-ppv-aac (2)      to   rf-oct-ppv-aac (2)     .
           move      oct-ppv-aac (3)      to   rf-oct-ppv-aac (3)     .
           move      oct-voc-des (1)      to   rf-oct-voc-des (1)     .
           move      oct-voc-des (2)      to   rf-oct-voc-des (2)     .
           move      oct-voc-des (3)      to   rf-oct-voc-des (3)     .
           move      oct-voc-des (4)      to   rf-oct-voc-des (4)     .
           move      oct-voc-des (5)      to   rf-oct-voc-des (5)     .
           move      oct-voc-des (6)      to   rf-oct-voc-des (6)     .
           move      oct-cod-fop          to   rf-oct-cod-fop         .
           move      oct-scp-aap          to   rf-oct-scp-aap         .
           move      oct-cod-abi          to   rf-oct-cod-abi         .
           move      oct-cod-cab          to   rf-oct-cod-cab         .
           move      oct-ccc-app          to   rf-oct-ccc-app         .
           move      oct-nos-ban          to   rf-oct-nos-ban         .
           move      oct-nos-ccp          to   rf-oct-nos-ccp         .
           move      oct-add-spi          to   rf-oct-add-spi         .
           move      oct-add-spb          to   rf-oct-add-spb         .
           move      oct-ipr-iel          to   rf-oct-ipr-iel         .
           move      oct-pag-dsm          to   rf-oct-pag-dsm         .
           move      oct-pag-qaf          to   rf-oct-pag-qaf         .
           move      oct-pag-act          to   rf-oct-pag-act         .
           move      oct-cod-age          to   rf-oct-cod-age         .
           move      oct-fsp-doc          to   rf-oct-fsp-doc         .
           move      oct-pvf-age          to   rf-oct-pvf-age         .
           move      oct-tip-vpa          to   rf-oct-tip-vpa         .
           move      oct-cpv-aaa          to   rf-oct-cpv-aaa         .
           move      oct-ppv-aaa (1)      to   rf-oct-ppv-aaa (1)     .
           move      oct-ppv-aaa (2)      to   rf-oct-ppv-aaa (2)     .
           move      oct-ppv-aaa (3)      to   rf-oct-ppv-aaa (3)     .
           move      oct-cod-ime          to   rf-oct-cod-ime         .
           move      oct-pvf-ime          to   rf-oct-pvf-ime         .
           move      oct-tot-rig (1)      to   rf-oct-tot-rig (1)     .
           move      oct-tot-rig (2)      to   rf-oct-tot-rig (2)     .
           move      oct-tot-rig (3)      to   rf-oct-tot-rig (3)     .
           move      oct-tot-rig (4)      to   rf-oct-tot-rig (4)     .
           move      oct-tot-rig (5)      to   rf-oct-tot-rig (5)     .
           move      oct-tot-rig (6)      to   rf-oct-tot-rig (6)     .
           move      oct-tot-rig (7)      to   rf-oct-tot-rig (7)     .
           move      oct-tot-rig (8)      to   rf-oct-tot-rig (8)     .
           move      oct-tot-rig (9)      to   rf-oct-tot-rig (9)     .
           move      oct-tot-scc          to   rf-oct-tot-scc         .
           move      oct-per-scc          to   rf-oct-per-scc         .
           move      oct-tot-scp          to   rf-oct-tot-scp         .
           move      oct-per-scp          to   rf-oct-per-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-oct-420.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-oct-450.
           move      oct-spe-snx (w-c01)  to   rf-oct-spe-snx (w-c01) .
           move      oct-spe-mad (w-c01)  to   rf-oct-spe-mad (w-c01) .
           move      oct-spe-per (w-c01)  to   rf-oct-spe-per (w-c01) .
           move      oct-spe-ibl (w-c01)  to   rf-oct-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-oct-430.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-oct-440.
           move      oct-ibx-spe
                    (w-c01, w-c02)        to   rf-oct-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-oct-430.
       exe-cnv-oct-440.
           move      oct-spe-imp (w-c01)  to   rf-oct-spe-imp (w-c01) .
           go to     exe-cnv-oct-420.
       exe-cnv-oct-450.
           move      oct-tot-doc          to   rf-oct-tot-doc         .
           move      oct-ctr-stp          to   rf-oct-ctr-stp         .
           move      oct-flg-och          to   rf-oct-flg-och         .
           move      oct-flg-blx (1)      to   rf-oct-flg-blx (1)     .
           move      oct-flg-blx (2)      to   rf-oct-flg-blx (2)     .
           move      oct-flg-blx (3)      to   rf-oct-flg-blx (3)     .
           move      oct-flg-blx (4)      to   rf-oct-flg-blx (4)     .
           move      oct-flg-blx (5)      to   rf-oct-flg-blx (5)     .
           move      oct-flg-blx (6)      to   rf-oct-flg-blx (6)     .
           move      oct-flg-blx (7)      to   rf-oct-flg-blx (7)     .
           move      oct-flg-nbx (1)      to   rf-oct-flg-nbx (1)     .
           move      oct-flg-nbx (2)      to   rf-oct-flg-nbx (2)     .
           move      oct-flg-nbx (3)      to   rf-oct-flg-nbx (3)     .
           move      oct-flg-pul          to   rf-oct-flg-pul         .
           move      oct-alx-exp          to   rf-oct-alx-exp         .
       exe-cnv-oct-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-rsp = Codice responsabile ordine        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-oct-cod-rsp         .
       exe-cnv-oct-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-oct]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-oct-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-oct-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-oct-250.
       exe-cnv-oct-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-oct]                        *
      *                  *---------------------------------------------*
           close     oct                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-oct]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-cnv-oct-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-oct-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-oct] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-oct-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ocr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ocr-000.
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
           move      ocr-alx-exp          to   rf-ocr-alx-exp         .
       exe-cnv-ocr-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * prz-vpl = Prezzo di riferimento per legame  *
      *                  *           valutario                         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-ocr-prz-vpl         .
       exe-cnv-ocr-510.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * pri-eva = Priorita' di evasione             *
      *                  * dcn-ric = Data consegna richiesta           *
      *                  * dcn-prv = Data consegna prevista            *
      *                  * flg-cnf = Flag di conferma a cliente        *
      *                  *---------------------------------------------*
           move      ocr-pri-eva          to   rf-ocr-pri-eva         .
           move      ocr-dat-cns          to   rf-ocr-dcn-ric         .
           move      ocr-dat-cns          to   rf-ocr-dcn-prv         .
           move      spaces               to   rf-ocr-flg-cnf         .
       exe-cnv-ocr-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cop-scl = Codice prodotto secondo il clien- *
      *                  *           te                                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ocr-cop-scl         .
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
       exe-cnv-bit-420.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bit-450.
           move      bit-spe-snx (w-c01)  to   rf-bit-spe-snx (w-c01) .
           move      bit-spe-mad (w-c01)  to   rf-bit-spe-mad (w-c01) .
           move      bit-spe-per (w-c01)  to   rf-bit-spe-per (w-c01) .
           move      bit-spe-ibl (w-c01)  to   rf-bit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-bit-430.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-bit-440.
           move      bit-ibx-spe
                    (w-c01, w-c02)        to   rf-bit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-bit-430.
       exe-cnv-bit-440.
           move      bit-spe-imp (w-c01)  to   rf-bit-spe-imp (w-c01) .
           move      bit-spe-civ (w-c01)  to   rf-bit-spe-civ (w-c01) .
           move      bit-spe-ccp (w-c01)  to   rf-bit-spe-ccp (w-c01) .
           go to     exe-cnv-bit-420.
       exe-cnv-bit-450.
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
           move      bit-cod-ant          to   rf-bit-cod-ant         .
           move      bit-prt-mgd          to   rf-bit-prt-mgd         .
           move      bit-nrg-mgd          to   rf-bit-nrg-mgd         .
           move      bit-dri-mgd          to   rf-bit-dri-mgd         .
           move      bit-nri-mgd          to   rf-bit-nri-mgd         .
           move      bit-nps-sdb          to   rf-bit-nps-sdb         .
           move      bit-ctr-sdb          to   rf-bit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       exe-cnv-bit-460.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bit-470.
           move      bit-iva-cod (w-c01)  to   rf-bit-iva-cod (w-c01) .
           move      bit-iva-ibl (w-c01)  to   rf-bit-iva-ibl (w-c01) .
           move      bit-iva-imp (w-c01)  to   rf-bit-iva-imp (w-c01) .
           go to     exe-cnv-bit-460.
       exe-cnv-bit-470.
           move      bit-iva-tdo          to   rf-bit-iva-tdo         .
           move      zero                 to   w-c01                  .
       exe-cnv-bit-480.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bit-490.
           move      bit-ctp-cod (w-c01)  to   rf-bit-ctp-cod (w-c01) .
           move      bit-ctp-imp (w-c01)  to   rf-bit-ctp-imp (w-c01) .
           go to     exe-cnv-bit-480.
       exe-cnv-bit-490.
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
       exe-cnv-bit-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-vet = Codice primo vettore              *
      *                  * cod-vt2 = Codice secondo vettore            *
      *                  * cod-vt3 = Codice terzo vettore              *
      *                  *---------------------------------------------*
           move      bit-cod-vet          to   rf-bit-cod-vet         .
           move      zero                 to   rf-bit-cod-vt2         .
           move      zero                 to   rf-bit-cod-vt3         .
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
      *    * Conversione [bir]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bir-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bir "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bir-999.
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
       exe-cnv-bir-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-bir-pat              .
       exe-cnv-bir-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-bir]                         *
      *                  *---------------------------------------------*
           open      i-o    bir                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-bir]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       exe-cnv-bir-200.
      *              *-------------------------------------------------*
      *              * Start su [old-bir]                              *
      *              *-------------------------------------------------*
           move      low-values           to   bir-k01                .
           start     bir    key not less
                            bir-k01
                            invalid key
                            go to exe-cnv-bir-800.
       exe-cnv-bir-250.
      *              *-------------------------------------------------*
      *              * Next su [old-bir]                               *
      *              *-------------------------------------------------*
           read      bir    next
                            with no lock
                            at end
                            go to exe-cnv-bir-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bir-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-bir]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       exe-cnv-bir-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-bir]                          *
      *              *-------------------------------------------------*
       exe-cnv-bir-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      bir-num-prt          to   rf-bir-num-prt         .
           move      bir-num-prg          to   rf-bir-num-prg         .
           move      bir-cod-tmb          to   rf-bir-cod-tmb         .
           move      bir-cod-dpz          to   rf-bir-cod-dpz         .
           move      bir-dat-doc          to   rf-bir-dat-doc         .
           move      bir-num-doc          to   rf-bir-num-doc         .
           move      bir-tip-arc          to   rf-bir-tip-arc         .
           move      bir-cod-arc          to   rf-bir-cod-arc         .
           move      bir-dpz-arc          to   rf-bir-dpz-arc         .
           move      bir-cod-lng          to   rf-bir-cod-lng         .
           move      bir-sgl-vpf          to   rf-bir-sgl-vpf         .
           move      bir-dec-vpf          to   rf-bir-dec-vpf         .
           move      bir-tdc-vpf          to   rf-bir-tdc-vpf         .
           move      bir-cdc-vpf          to   rf-bir-cdc-vpf         .
           move      bir-bld-flb          to   rf-bir-bld-flb         .
           move      bir-bld-tpb          to   rf-bir-bld-tpb         .
           move      bir-bld-rgb          to   rf-bir-bld-rgb         .
           move      bir-tip-rig          to   rf-bir-tip-rig         .
           move      bir-tip-mag          to   rf-bir-tip-mag         .
           move      bir-num-pro          to   rf-bir-num-pro         .
           move      bir-alf-pro          to   rf-bir-alf-pro         .
           move      bir-sgl-vrn          to   rf-bir-sgl-vrn         .
           move      bir-des-ext          to   rf-bir-des-ext         .
           move      bir-des-rig          to   rf-bir-des-rig         .
           move      bir-tip-pro          to   rf-bir-tip-pro         .
           move      bir-cod-iva          to   rf-bir-cod-iva         .
           move      bir-ctp-ven          to   rf-bir-ctp-ven         .
           move      bir-umi-ven          to   rf-bir-umi-ven         .
           move      bir-dec-qta          to   rf-bir-dec-qta         .
           move      bir-qta-ven          to   rf-bir-qta-ven         .
           move      bir-cod-dsl          to   rf-bir-cod-dsl         .
           move      bir-snx-2qt          to   rf-bir-snx-2qt         .
           move      bir-dec-2qt          to   rf-bir-dec-2qt         .
           move      bir-qta-a02          to   rf-bir-qta-a02         .
           move      bir-snx-3qt          to   rf-bir-snx-3qt         .
           move      bir-dec-3qt          to   rf-bir-dec-3qt         .
           move      bir-qta-a03          to   rf-bir-qta-a03         .
           move      bir-dec-prz          to   rf-bir-dec-prz         .
           move      bir-sgl-vps          to   rf-bir-sgl-vps         .
           move      bir-dec-vps          to   rf-bir-dec-vps         .
           move      bir-tdc-vps          to   rf-bir-tdc-vps         .
           move      bir-cdc-vps          to   rf-bir-cdc-vps         .
           move      bir-prz-lrs          to   rf-bir-prz-lrs         .
           move      bir-prz-nts          to   rf-bir-prz-nts         .
           move      bir-sgl-vpp          to   rf-bir-sgl-vpp         .
           move      bir-dec-vpp          to   rf-bir-dec-vpp         .
           move      bir-tdc-vpp          to   rf-bir-tdc-vpp         .
           move      bir-cdc-vpp          to   rf-bir-cdc-vpp         .
           move      bir-prz-ven          to   rf-bir-prz-ven         .
           move      bir-snx-2pz          to   rf-bir-snx-2pz         .
           move      bir-prz-a02          to   rf-bir-prz-a02         .
           move      bir-sgl-vpl          to   rf-bir-sgl-vpl         .
           move      bir-dec-vpl          to   rf-bir-dec-vpl         .
           move      bir-tdc-vpl          to   rf-bir-tdc-vpl         .
           move      bir-cdc-vpl          to   rf-bir-cdc-vpl         .
           move      bir-ccr-vpl          to   rf-bir-ccr-vpl         .
           move      bir-plm-vpl          to   rf-bir-plm-vpl         .
           move      bir-tlm-vpl          to   rf-bir-tlm-vpl         .
           move      bir-map-vpl          to   rf-bir-map-vpl         .
           move      bir-epz-rgf          to   rf-bir-epz-rgf         .
           move      bir-csr-aap          to   rf-bir-csr-aap         .
           move      bir-psr-aap (1)      to   rf-bir-psr-aap (1)     .
           move      bir-psr-aap (2)      to   rf-bir-psr-aap (2)     .
           move      bir-psr-aap (3)      to   rf-bir-psr-aap (3)     .
           move      bir-psr-aap (4)      to   rf-bir-psr-aap (4)     .
           move      bir-psr-aap (5)      to   rf-bir-psr-aap (5)     .
           move      bir-per-scr (1)      to   rf-bir-per-scr (1)     .
           move      bir-per-scr (2)      to   rf-bir-per-scr (2)     .
           move      bir-per-scr (3)      to   rf-bir-per-scr (3)     .
           move      bir-per-scr (4)      to   rf-bir-per-scr (4)     .
           move      bir-per-scr (5)      to   rf-bir-per-scr (5)     .
           move      bir-prz-net          to   rf-bir-prz-net         .
           move      bir-epz-pes          to   rf-bir-epz-pes         .
           move      bir-sgl-vpc          to   rf-bir-sgl-vpc         .
           move      bir-dec-vpc          to   rf-bir-dec-vpc         .
           move      bir-tdc-vpc          to   rf-bir-tdc-vpc         .
           move      bir-cdc-vpc          to   rf-bir-cdc-vpc         .
           move      bir-dec-cos          to   rf-bir-dec-cos         .
           move      bir-cos-rif          to   rf-bir-cos-rif         .
           move      bir-imp-rig          to   rf-bir-imp-rig         .
           move      bir-iau-rig          to   rf-bir-iau-rig         .
           move      bir-cpv-aap          to   rf-bir-cpv-aap         .
           move      bir-ppv-aap (1)      to   rf-bir-ppv-aap (1)     .
           move      bir-ppv-aap (2)      to   rf-bir-ppv-aap (2)     .
           move      bir-ppv-aap (3)      to   rf-bir-ppv-aap (3)     .
           move      bir-fsp-rig          to   rf-bir-fsp-rig         .
           move      bir-cpv-rig          to   rf-bir-cpv-rig         .
           move      bir-ppv-rig (1)      to   rf-bir-ppv-rig (1)     .
           move      bir-ppv-rig (2)      to   rf-bir-ppv-rig (2)     .
           move      bir-ppv-rig (3)      to   rf-bir-ppv-rig (3)     .
           move      bir-pvf-rig          to   rf-bir-pvf-rig         .
           move      bir-ocl-dat          to   rf-bir-ocl-dat         .
           move      bir-ocl-num          to   rf-bir-ocl-num         .
           move      bir-cmc-tip          to   rf-bir-cmc-tip         .
           move      bir-cmc-dat          to   rf-bir-cmc-dat         .
           move      bir-cmc-num          to   rf-bir-cmc-num         .
           move      bir-coc-tip          to   rf-bir-coc-tip         .
           move      bir-coc-dat          to   rf-bir-coc-dat         .
           move      bir-coc-num          to   rf-bir-coc-num         .
           move      bir-coc-prt          to   rf-bir-coc-prt         .
           move      bir-coc-prg          to   rf-bir-coc-prg         .
           move      bir-coc-fzs          to   rf-bir-coc-fzs         .
           move      bir-fat-snx          to   rf-bir-fat-snx         .
           move      bir-fat-dat          to   rf-bir-fat-dat         .
           move      bir-fat-num          to   rf-bir-fat-num         .
           move      bir-fat-npb          to   rf-bir-fat-npb         .
           move      bir-flg-blx (1)      to   rf-bir-flg-blx (1)     .
           move      bir-flg-blx (2)      to   rf-bir-flg-blx (2)     .
           move      bir-flg-blx (3)      to   rf-bir-flg-blx (3)     .
           move      bir-flg-blx (4)      to   rf-bir-flg-blx (4)     .
           move      bir-flg-blx (5)      to   rf-bir-flg-blx (5)     .
           move      bir-flg-blx (6)      to   rf-bir-flg-blx (6)     .
           move      bir-flg-blx (7)      to   rf-bir-flg-blx (7)     .
           move      bir-flg-nbx (1)      to   rf-bir-flg-nbx (1)     .
           move      bir-flg-nbx (2)      to   rf-bir-flg-nbx (2)     .
           move      bir-flg-nbx (3)      to   rf-bir-flg-nbx (3)     .
           move      bir-flg-pul          to   rf-bir-flg-pul         .
           move      bir-alx-exp          to   rf-bir-alx-exp         .
       exe-cnv-bir-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * prz-vpl = Prezzo di riferimento per legame  *
      *                  *           valutario                         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-bir-prz-vpl         .
       exe-cnv-bir-510.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cop-scl = Codice prodotto secondo il clien- *
      *                  *           te                                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bir-cop-scl         .
       exe-cnv-bir-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * ods-prt = Numero protocollo ordine di spe-  *
      *                  *           zione                             *
      *                  * ods-prg = Numero progressivo riga nel pro-  *
      *                  *           tocollo ordine di spezione        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-bir-ods-prt         .
           move      zero                 to   rf-bir-ods-prg         .
       exe-cnv-bir-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-bir]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bir-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bir-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bir-250.
       exe-cnv-bir-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-bir]                        *
      *                  *---------------------------------------------*
           close     bir                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-bir]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       exe-cnv-bir-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bir-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-bir] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-bir-999.
           exit.

      *    *===========================================================*
      *    * Conversione [fir]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fir-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "fir "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-fir-999.
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
       exe-cnv-fir-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-fir-pat              .
       exe-cnv-fir-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-fir]                         *
      *                  *---------------------------------------------*
           open      i-o    fir                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-fir]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       exe-cnv-fir-200.
      *              *-------------------------------------------------*
      *              * Start su [old-fir]                              *
      *              *-------------------------------------------------*
           move      low-values           to   fir-k01                .
           start     fir    key not less
                            fir-k01
                            invalid key
                            go to exe-cnv-fir-800.
       exe-cnv-fir-250.
      *              *-------------------------------------------------*
      *              * Next su [old-fir]                               *
      *              *-------------------------------------------------*
           read      fir    next
                            with no lock
                            at end
                            go to exe-cnv-fir-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-fir-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-fir]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       exe-cnv-fir-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-fir]                          *
      *              *-------------------------------------------------*
       exe-cnv-fir-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      fir-num-prt          to   rf-fir-num-prt         .
           move      fir-num-prg          to   rf-fir-num-prg         .
           move      fir-cod-tmo          to   rf-fir-cod-tmo         .
           move      fir-cod-dpz          to   rf-fir-cod-dpz         .
           move      fir-dat-doc          to   rf-fir-dat-doc         .
           move      fir-num-doc          to   rf-fir-num-doc         .
           move      fir-cod-cli          to   rf-fir-cod-cli         .
           move      fir-dpz-cli          to   rf-fir-dpz-cli         .
           move      fir-cli-pls          to   rf-fir-cli-pls         .
           move      fir-dpc-pls          to   rf-fir-dpc-pls         .
           move      fir-cod-lng          to   rf-fir-cod-lng         .
           move      fir-sgl-vpf          to   rf-fir-sgl-vpf         .
           move      fir-dec-vpf          to   rf-fir-dec-vpf         .
           move      fir-tdc-vpf          to   rf-fir-tdc-vpf         .
           move      fir-cdc-vpf          to   rf-fir-cdc-vpf         .
           move      fir-bld-flb          to   rf-fir-bld-flb         .
           move      fir-bld-tpb          to   rf-fir-bld-tpb         .
           move      fir-bld-rgb          to   rf-fir-bld-rgb         .
           move      fir-tip-rig          to   rf-fir-tip-rig         .
           move      fir-tip-mag          to   rf-fir-tip-mag         .
           move      fir-num-pro          to   rf-fir-num-pro         .
           move      fir-alf-pro          to   rf-fir-alf-pro         .
           move      fir-sgl-vrn          to   rf-fir-sgl-vrn         .
           move      fir-des-ext          to   rf-fir-des-ext         .
           move      fir-des-rig          to   rf-fir-des-rig         .
           move      fir-tip-pro          to   rf-fir-tip-pro         .
           move      fir-cod-iva          to   rf-fir-cod-iva         .
           move      fir-ctp-ven          to   rf-fir-ctp-ven         .
           move      fir-umi-ven          to   rf-fir-umi-ven         .
           move      fir-dec-qta          to   rf-fir-dec-qta         .
           move      fir-qta-ven          to   rf-fir-qta-ven         .
           move      fir-snx-2qt          to   rf-fir-snx-2qt         .
           move      fir-dec-2qt          to   rf-fir-dec-2qt         .
           move      fir-qta-a02          to   rf-fir-qta-a02         .
           move      fir-snx-3qt          to   rf-fir-snx-3qt         .
           move      fir-dec-3qt          to   rf-fir-dec-3qt         .
           move      fir-qta-a03          to   rf-fir-qta-a03         .
           move      fir-dec-prz          to   rf-fir-dec-prz         .
           move      fir-sgl-vps          to   rf-fir-sgl-vps         .
           move      fir-dec-vps          to   rf-fir-dec-vps         .
           move      fir-tdc-vps          to   rf-fir-tdc-vps         .
           move      fir-cdc-vps          to   rf-fir-cdc-vps         .
           move      fir-prz-lrs          to   rf-fir-prz-lrs         .
           move      fir-prz-nts          to   rf-fir-prz-nts         .
           move      fir-sgl-vpp          to   rf-fir-sgl-vpp         .
           move      fir-dec-vpp          to   rf-fir-dec-vpp         .
           move      fir-tdc-vpp          to   rf-fir-tdc-vpp         .
           move      fir-cdc-vpp          to   rf-fir-cdc-vpp         .
           move      fir-prz-ven          to   rf-fir-prz-ven         .
           move      fir-snx-2pz          to   rf-fir-snx-2pz         .
           move      fir-prz-a02          to   rf-fir-prz-a02         .
           move      fir-sgl-vpl          to   rf-fir-sgl-vpl         .
           move      fir-dec-vpl          to   rf-fir-dec-vpl         .
           move      fir-tdc-vpl          to   rf-fir-tdc-vpl         .
           move      fir-cdc-vpl          to   rf-fir-cdc-vpl         .
           move      fir-ccr-vpl          to   rf-fir-ccr-vpl         .
           move      fir-plm-vpl          to   rf-fir-plm-vpl         .
           move      fir-tlm-vpl          to   rf-fir-tlm-vpl         .
           move      fir-map-vpl          to   rf-fir-map-vpl         .
           move      fir-epz-rgf          to   rf-fir-epz-rgf         .
           move      fir-csr-aap          to   rf-fir-csr-aap         .
           move      fir-psr-aap (1)      to   rf-fir-psr-aap (1)     .
           move      fir-psr-aap (2)      to   rf-fir-psr-aap (2)     .
           move      fir-psr-aap (3)      to   rf-fir-psr-aap (3)     .
           move      fir-psr-aap (4)      to   rf-fir-psr-aap (4)     .
           move      fir-psr-aap (5)      to   rf-fir-psr-aap (5)     .
           move      fir-per-scr (1)      to   rf-fir-per-scr (1)     .
           move      fir-per-scr (2)      to   rf-fir-per-scr (2)     .
           move      fir-per-scr (3)      to   rf-fir-per-scr (3)     .
           move      fir-per-scr (4)      to   rf-fir-per-scr (4)     .
           move      fir-per-scr (5)      to   rf-fir-per-scr (5)     .
           move      fir-prz-net          to   rf-fir-prz-net         .
           move      fir-epz-pes          to   rf-fir-epz-pes         .
           move      fir-sgl-vpc          to   rf-fir-sgl-vpc         .
           move      fir-dec-vpc          to   rf-fir-dec-vpc         .
           move      fir-tdc-vpc          to   rf-fir-tdc-vpc         .
           move      fir-cdc-vpc          to   rf-fir-cdc-vpc         .
           move      fir-dec-cos          to   rf-fir-dec-cos         .
           move      fir-cos-rif          to   rf-fir-cos-rif         .
           move      fir-imp-rig          to   rf-fir-imp-rig         .
           move      fir-iau-rig          to   rf-fir-iau-rig         .
           move      fir-cpv-aap          to   rf-fir-cpv-aap         .
           move      fir-ppv-aap (1)      to   rf-fir-ppv-aap (1)     .
           move      fir-ppv-aap (2)      to   rf-fir-ppv-aap (2)     .
           move      fir-ppv-aap (3)      to   rf-fir-ppv-aap (3)     .
           move      fir-fsp-rig          to   rf-fir-fsp-rig         .
           move      fir-cpv-rig          to   rf-fir-cpv-rig         .
           move      fir-ppv-rig (1)      to   rf-fir-ppv-rig (1)     .
           move      fir-ppv-rig (2)      to   rf-fir-ppv-rig (2)     .
           move      fir-ppv-rig (3)      to   rf-fir-ppv-rig (3)     .
           move      fir-pvf-rig          to   rf-fir-pvf-rig         .
           move      fir-ocl-dat          to   rf-fir-ocl-dat         .
           move      fir-ocl-num          to   rf-fir-ocl-num         .
           move      fir-cmc-tip          to   rf-fir-cmc-tip         .
           move      fir-cmc-dat          to   rf-fir-cmc-dat         .
           move      fir-cmc-num          to   rf-fir-cmc-num         .
           move      fir-coc-tip          to   rf-fir-coc-tip         .
           move      fir-coc-dat          to   rf-fir-coc-dat         .
           move      fir-coc-num          to   rf-fir-coc-num         .
           move      fir-bcc-tip          to   rf-fir-bcc-tip         .
           move      fir-bcc-dat          to   rf-fir-bcc-dat         .
           move      fir-bcc-num          to   rf-fir-bcc-num         .
           move      fir-flg-blx (1)      to   rf-fir-flg-blx (1)     .
           move      fir-flg-blx (2)      to   rf-fir-flg-blx (2)     .
           move      fir-flg-blx (3)      to   rf-fir-flg-blx (3)     .
           move      fir-flg-blx (4)      to   rf-fir-flg-blx (4)     .
           move      fir-flg-blx (5)      to   rf-fir-flg-blx (5)     .
           move      fir-flg-blx (6)      to   rf-fir-flg-blx (6)     .
           move      fir-flg-blx (7)      to   rf-fir-flg-blx (7)     .
           move      fir-flg-nbx (1)      to   rf-fir-flg-nbx (1)     .
           move      fir-flg-nbx (2)      to   rf-fir-flg-nbx (2)     .
           move      fir-flg-nbx (3)      to   rf-fir-flg-nbx (3)     .
           move      fir-flg-pul          to   rf-fir-flg-pul         .
           move      fir-alx-exp          to   rf-fir-alx-exp         .
       exe-cnv-fir-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * prz-vpl = Prezzo di riferimento per legame  *
      *                  *           valutario                         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-fir-prz-vpl         .
       exe-cnv-fir-510.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cop-scl = Codice prodotto secondo il clien- *
      *                  *           te                                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-fir-cop-scl         .
       exe-cnv-fir-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-fir]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-fir-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-fir-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-fir-250.
       exe-cnv-fir-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-fir]                        *
      *                  *---------------------------------------------*
           close     fir                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-fir]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       exe-cnv-fir-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-fir-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-fir] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-fir-999.
           exit.

      *    *===========================================================*
      *    * Conversione [dcp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-dcp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "dcp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-dcp-999.
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
       exe-cnv-dcp-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-dcp-pat              .
       exe-cnv-dcp-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-dcp]                         *
      *                  *---------------------------------------------*
           open      i-o    dcp                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-dcp]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       exe-cnv-dcp-200.
      *              *-------------------------------------------------*
      *              * Start su [old-dcp]                              *
      *              *-------------------------------------------------*
           move      low-values           to   dcp-k01                .
           start     dcp    key not less
                            dcp-k01
                            invalid key
                            go to exe-cnv-dcp-800.
       exe-cnv-dcp-250.
      *              *-------------------------------------------------*
      *              * Next su [old-dcp]                               *
      *              *-------------------------------------------------*
           read      dcp    next
                            with no lock
                            at end
                            go to exe-cnv-dcp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-dcp-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-dcp]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       exe-cnv-dcp-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-dcp]                          *
      *              *-------------------------------------------------*
       exe-cnv-dcp-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      dcp-ide-dat          to   rf-dcp-ide-dat         .
           move      dcp-ide-ute          to   rf-dcp-ide-ute         .
           move      dcp-ide-fas          to   rf-dcp-ide-fas         .
           move      dcp-num-pro          to   rf-dcp-num-pro         .
           move      dcp-alf-pro          to   rf-dcp-alf-pro         .
           move      dcp-syn-pro          to   rf-dcp-syn-pro         .
           move      dcp-des-key          to   rf-dcp-des-key         .
           move      dcp-des-pro          to   rf-dcp-des-pro         .
           move      dcp-des-pdx          to   rf-dcp-des-pdx         .
           move      dcp-cla-pro          to   rf-dcp-cla-pro         .
           move      dcp-gru-pro          to   rf-dcp-gru-pro         .
           move      dcp-sgr-pro          to   rf-dcp-sgr-pro         .
           move      dcp-tip-pro          to   rf-dcp-tip-pro         .
           move      dcp-tip-cfz          to   rf-dcp-tip-cfz         .
           move      dcp-qta-cfz          to   rf-dcp-qta-cfz         .
           move      dcp-pes-uni          to   rf-dcp-pes-uni         .
           move      dcp-pes-tar          to   rf-dcp-pes-tar         .
           move      dcp-vol-uni          to   rf-dcp-vol-uni         .
           move      dcp-dim-lar          to   rf-dcp-dim-lar         .
           move      dcp-dim-alt          to   rf-dcp-dim-alt         .
           move      dcp-dim-prf          to   rf-dcp-dim-prf         .
           move      dcp-pcl-fis          to   rf-dcp-pcl-fis         .
           move      dcp-coe-mol          to   rf-dcp-coe-mol         .
           move      dcp-coe-div          to   rf-dcp-coe-div         .
           move      dcp-cod-iva          to   rf-dcp-cod-iva         .
           move      dcp-ctp-ven          to   rf-dcp-ctp-ven         .
           move      dcp-umi-ven          to   rf-dcp-umi-ven         .
           move      dcp-dec-qta          to   rf-dcp-dec-qta         .
           move      dcp-sgl-vlt          to   rf-dcp-sgl-vlt         .
           move      dcp-dec-vlt          to   rf-dcp-dec-vlt         .
           move      dcp-prz-lst          to   rf-dcp-prz-lst         .
           move      dcp-epz-rgf          to   rf-dcp-epz-rgf         .
           move      dcp-snx-2qt          to   rf-dcp-snx-2qt         .
           move      dcp-dec-2qt          to   rf-dcp-dec-2qt         .
           move      dcp-snx-3qt          to   rf-dcp-snx-3qt         .
           move      dcp-dec-3qt          to   rf-dcp-dec-3qt         .
           move      dcp-snx-2pz          to   rf-dcp-snx-2pz         .
           move      dcp-aut-lst          to   rf-dcp-aut-lst         .
           move      dcp-tip-vve          to   rf-dcp-tip-vve         .
           move      dcp-cat-scr          to   rf-dcp-cat-scr         .
           move      dcp-per-scr (1)      to   rf-dcp-per-scr (1)     .
           move      dcp-per-scr (2)      to   rf-dcp-per-scr (2)     .
           move      dcp-per-scr (3)      to   rf-dcp-per-scr (3)     .
           move      dcp-per-scr (4)      to   rf-dcp-per-scr (4)     .
           move      dcp-per-scr (5)      to   rf-dcp-per-scr (5)     .
           move      dcp-cat-pvg          to   rf-dcp-cat-pvg         .
           move      dcp-per-pvg (1)      to   rf-dcp-per-pvg (1)     .
           move      dcp-per-pvg (2)      to   rf-dcp-per-pvg (2)     .
           move      dcp-per-pvg (3)      to   rf-dcp-per-pvg (3)     .
           move      dcp-amm-pvg          to   rf-dcp-amm-pvg         .
           move      dcp-cod-s01          to   rf-dcp-cod-s01         .
           move      dcp-cod-s02          to   rf-dcp-cod-s02         .
           move      dcp-cod-s03          to   rf-dcp-cod-s03         .
           move      dcp-cla-bdg          to   rf-dcp-cla-bdg         .
           move      dcp-alx-exp          to   rf-dcp-alx-exp         .
       exe-cnv-dcp-500.
      *                  *---------------------------------------------*
      *                  * Integrazione con nuovi campi (10/03/95)     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiave alfanumerica libera per il pro-  *
      *                      * dotto                                   *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-klb-pro         .
      *                      *-----------------------------------------*
      *                      * Tipo acquisizione prodotto              *
      *                      *-----------------------------------------*
           move      01                   to   rf-dcp-tip-acp         .
      *                      *-----------------------------------------*
      *                      * Nota generica sul prodotto nr. 1        *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-not-g01         .
      *                      *-----------------------------------------*
      *                      * Specifica libera per il prodotto        *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-spc-lib         .
      *                      *-----------------------------------------*
      *                      * Numero decimali per il prezzo           *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-dec-prz         .
      *                      *-----------------------------------------*
      *                      * Lotto di vendita                        *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-lot-ven         .
      *                      *-----------------------------------------*
      *                      * Segnale di prodotto gestito nei listini *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-snx-lst         .
      *                      *-----------------------------------------*
      *                      * Elementi da esporre per il prodotto     *
      *                      *-----------------------------------------*
           move      all "X"              to   rf-dcp-epz-lst         .
      *                      *-----------------------------------------*
      *                      * Pagina di riferimento                   *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-pag-lst         .
      *                      *-----------------------------------------*
      *                      * Riferimento libero                      *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-rfl-lst         .
      *                      *-----------------------------------------*
      *                      * Tempo medio di consegna                 *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-tmc-lst         .
      *                      *-----------------------------------------*
      *                      * Segnale di descrizione per i listini    *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-daa-lst         .
      *                      *-----------------------------------------*
      *                      * Data di inizio commercializzazione      *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-dat-icm         .
      *                      *-----------------------------------------*
      *                      * Status commerciale del prodotto         *
      *                      *-----------------------------------------*
           move      01                   to   rf-dcp-sta-tus         .
      *                      *-----------------------------------------*
      *                      * Data rilevazione status commerciale     *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-sta-tud         .
      *                      *-----------------------------------------*
      *                      * Codice numerico prodotto di riferimento *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-sta-tuc         .
      *                      *-----------------------------------------*
      *                      * Modalita' di trattamento statistiche    *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-sta-tux         .
      *                      *-----------------------------------------*
      *                      * Classe A..Z di importanza del prodotto  *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-cld-imp         .
      *                      *-----------------------------------------*
      *                      * Previsione di continuita'               *
      *                      *-----------------------------------------*
           move      99                   to   rf-dcp-pre-ctn         .
      *                      *-----------------------------------------*
      *                      * Grado di introduzione commerciale       *
      *                      *-----------------------------------------*
           move      99                   to   rf-dcp-gra-ico         .
      *                      *-----------------------------------------*
      *                      * Pericolosita' della concorrenza         *
      *                      *-----------------------------------------*
           move      99                   to   rf-dcp-pcl-ccz         .
      *                      *-----------------------------------------*
      *                      * Codice marketing per il prodotto        *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-cod-mkt         .
      *                      *-----------------------------------------*
      *                      * Indice libero marketing                 *
      *                      *-----------------------------------------*
           move      spaces               to   rf-dcp-ind-mkt         .
      *                      *-----------------------------------------*
      *                      * Segnale di prodotto con accettazione    *
      *                      * ordini bloccata                         *
      *                      *-----------------------------------------*
           move      01                   to   rf-dcp-for-blo         .
      *                      *-----------------------------------------*
      *                      * Data in cui e' avvenuto il blocco       *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-dor-blo         .
      *                      *-----------------------------------------*
      *                      * Segnale di prodotto con consegne        *
      *                      * bloccate                                *
      *                      *-----------------------------------------*
           move      01                   to   rf-dcp-fco-blo         .
      *                      *-----------------------------------------*
      *                      * Data in cui e' avvenuto il blocco       *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-dco-blo         .
      *                      *-----------------------------------------*
      *                      * Codice della nomenclatura combinata     *
      *                      *-----------------------------------------*
           move      zero                 to   rf-dcp-cdn-cdm         .
       exe-cnv-dcp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-dcp]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-dcp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-dcp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-dcp-250.
       exe-cnv-dcp-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-dcp]                        *
      *                  *---------------------------------------------*
           close     dcp                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-dcp]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       exe-cnv-dcp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-dcp-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-dcp] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-dcp-999.
           exit.

      *    *===========================================================*
      *    * Conversione [lic]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-lic-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "lic "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-lic-999.
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
       exe-cnv-lic-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-lic-pat              .
       exe-cnv-lic-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-lic]                         *
      *                  *---------------------------------------------*
           open      i-o    lic                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-lic]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-cnv-lic-200.
      *              *-------------------------------------------------*
      *              * Start su [old-lic]                              *
      *              *-------------------------------------------------*
           move      low-values           to   lic-k01                .
           start     lic    key not less
                            lic-k01
                            invalid key
                            go to exe-cnv-lic-800.
       exe-cnv-lic-250.
      *              *-------------------------------------------------*
      *              * Next su [old-lic]                               *
      *              *-------------------------------------------------*
           read      lic    next
                            with no lock
                            at end
                            go to exe-cnv-lic-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-lic-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-lic]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-cnv-lic-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-lic]                          *
      *              *-------------------------------------------------*
       exe-cnv-lic-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      lic-cod-cli          to   rf-lic-cod-cli         .
           move      lic-num-lic          to   rf-lic-num-lic         .
           move      lic-dat-lic          to   rf-lic-dat-lic         .
           move      lic-prt-int          to   rf-lic-prt-int         .
           move      lic-alx-exp          to   rf-lic-alx-exp         .
       exe-cnv-lic-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * drf-ini = Data riferimento iniziale         *
      *                  *---------------------------------------------*
           move      lic-ann-rif          to   s-saa                  .
           move      01                   to   s-gio                  .
           move      01                   to   s-mes                  .
           move      s-dat                to   rf-lic-drf-ini         .
       exe-cnv-lic-510.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * drf-fin = Data riferimento finale           *
      *                  *---------------------------------------------*
           move      lic-ann-rif          to   s-saa                  .
           move      31                   to   s-gio                  .
           move      12                   to   s-mes                  .
           move      s-dat                to   rf-lic-drf-fin         .
       exe-cnv-lic-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-lic]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-lic-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-lic-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-lic-250.
       exe-cnv-lic-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-lic]                        *
      *                  *---------------------------------------------*
           close     lic                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-lic]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-cnv-lic-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-lic-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-lic] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-lic-999.
           exit.

      *    *===========================================================*
      *    * Conversione [oft]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-oft-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "oft "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-oft-999.
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
       exe-cnv-oft-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-oft-pat              .
       exe-cnv-oft-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-oft]                         *
      *                  *---------------------------------------------*
           open      i-o    oft                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-oft]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
       exe-cnv-oft-200.
      *              *-------------------------------------------------*
      *              * Start su [old-oft]                              *
      *              *-------------------------------------------------*
           move      low-values           to   oft-k01                .
           start     oft    key not less
                            oft-k01
                            invalid key
                            go to exe-cnv-oft-800.
       exe-cnv-oft-250.
      *              *-------------------------------------------------*
      *              * Next su [old-oft]                               *
      *              *-------------------------------------------------*
           read      oft    next
                            with no lock
                            at end
                            go to exe-cnv-oft-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-oft-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-oft]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
       exe-cnv-oft-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-oft]                          *
      *              *-------------------------------------------------*
       exe-cnv-oft-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      oft-ide-dat          to   rf-oft-ide-dat         .
           move      oft-ide-ute          to   rf-oft-ide-ute         .
           move      oft-ide-fas          to   rf-oft-ide-fas         .
           move      oft-num-prt          to   rf-oft-num-prt         .
           move      oft-tmo-orf          to   rf-oft-tmo-orf         .
           move      oft-cod-dpz          to   rf-oft-cod-dpz         .
           move      oft-dat-doc          to   rf-oft-dat-doc         .
           move      oft-num-doc          to   rf-oft-num-doc         .
           move      oft-scl-ann          to   rf-oft-scl-ann         .
           move      oft-sgl-num          to   rf-oft-sgl-num         .
           move      oft-tip-arc          to   rf-oft-tip-arc         .
           move      oft-cod-arc          to   rf-oft-cod-arc         .
           move      oft-dpz-arc          to   rf-oft-dpz-arc         .
           move      oft-arc-plf          to   rf-oft-arc-plf         .
           move      oft-dpz-plf          to   rf-oft-dpz-plf         .
           move      oft-tip-ids          to   rf-oft-tip-ids         .
           move      oft-cof-dat          to   rf-oft-cof-dat         .
           move      oft-cof-num          to   rf-oft-cof-num         .
           move      oft-cof-rif          to   rf-oft-cof-rif         .
           move      oft-dat-cns          to   rf-oft-dat-cns         .
           move      oft-fds-dtc          to   rf-oft-fds-dtc         .
           move      oft-tip-eva          to   rf-oft-tip-eva         .
           move      oft-pri-eva          to   rf-oft-pri-eva         .
           move      oft-cod-cda          to   rf-oft-cod-cda         .
           move      oft-com-int          to   rf-oft-com-int         .
           move      oft-ass-iva          to   rf-oft-ass-iva         .
           move      oft-ctp-acq          to   rf-oft-ctp-acq         .
           move      oft-inl-dcm          to   rf-oft-inl-dcm         .
           move      oft-inl-pgt          to   rf-oft-inl-pgt         .
           move      oft-cod-lst          to   rf-oft-cod-lst         .
           move      oft-csr-aaf          to   rf-oft-csr-aaf         .
           move      oft-psr-aaf (1)      to   rf-oft-psr-aaf (1)     .
           move      oft-psr-aaf (2)      to   rf-oft-psr-aaf (2)     .
           move      oft-psr-aaf (3)      to   rf-oft-psr-aaf (3)     .
           move      oft-psr-aaf (4)      to   rf-oft-psr-aaf (4)     .
           move      oft-psr-aaf (5)      to   rf-oft-psr-aaf (5)     .
           move      oft-csc-aaf          to   rf-oft-csc-aaf         .
           move      oft-psc-aaf          to   rf-oft-psc-aaf         .
           move      oft-voc-des (1)      to   rf-oft-voc-des (1)     .
           move      oft-voc-des (2)      to   rf-oft-voc-des (2)     .
           move      oft-voc-des (3)      to   rf-oft-voc-des (3)     .
           move      oft-voc-des (4)      to   rf-oft-voc-des (4)     .
           move      oft-voc-des (5)      to   rf-oft-voc-des (5)     .
           move      oft-voc-des (6)      to   rf-oft-voc-des (6)     .
           move      oft-cod-fop          to   rf-oft-cod-fop         .
           move      oft-scp-aap          to   rf-oft-scp-aap         .
           move      oft-nos-ban          to   rf-oft-nos-ban         .
           move      oft-cod-abi          to   rf-oft-cod-abi         .
           move      oft-cod-cab          to   rf-oft-cod-cab         .
           move      oft-ccc-app          to   rf-oft-ccc-app         .
           move      oft-ccp-app          to   rf-oft-ccp-app         .
           move      oft-add-spi          to   rf-oft-add-spi         .
           move      oft-add-spb          to   rf-oft-add-spb         .
           move      oft-ipr-iel          to   rf-oft-ipr-iel         .
           move      oft-pag-dsm          to   rf-oft-pag-dsm         .
           move      oft-pag-qaf          to   rf-oft-pag-qaf         .
           move      oft-pag-act          to   rf-oft-pag-act         .
           move      oft-cod-aqt          to   rf-oft-cod-aqt         .
           move      oft-pvf-aqt          to   rf-oft-pvf-aqt         .
           move      oft-cod-ime          to   rf-oft-cod-ime         .
           move      oft-pvf-ime          to   rf-oft-pvf-ime         .
           move      oft-tot-rig (1)      to   rf-oft-tot-rig (1)     .
           move      oft-tot-rig (2)      to   rf-oft-tot-rig (2)     .
           move      oft-tot-rig (3)      to   rf-oft-tot-rig (3)     .
           move      oft-tot-rig (4)      to   rf-oft-tot-rig (4)     .
           move      oft-tot-rig (5)      to   rf-oft-tot-rig (5)     .
           move      oft-tot-rig (6)      to   rf-oft-tot-rig (6)     .
           move      oft-tot-rig (7)      to   rf-oft-tot-rig (7)     .
           move      oft-tot-rig (8)      to   rf-oft-tot-rig (8)     .
           move      oft-tot-rig (9)      to   rf-oft-tot-rig (9)     .
           move      oft-tot-scc          to   rf-oft-tot-scc         .
           move      oft-per-scc          to   rf-oft-per-scc         .
           move      oft-tot-scp          to   rf-oft-tot-scp         .
           move      oft-per-scp          to   rf-oft-per-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-oft-420.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-oft-450.
           move      oft-spe-snx (w-c01)  to   rf-oft-spe-snx (w-c01) .
           move      oft-spe-mad (w-c01)  to   rf-oft-spe-mad (w-c01) .
           move      oft-spe-per (w-c01)  to   rf-oft-spe-per (w-c01) .
           move      oft-spe-ibl (w-c01)  to   rf-oft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-oft-430.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-oft-440.
           move      oft-ibx-spe
                    (w-c01, w-c02)        to   rf-oft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-oft-430.
       exe-cnv-oft-440.
           move      oft-spe-imp (w-c01)  to   rf-oft-spe-imp (w-c01) .
           go to     exe-cnv-oft-420.
       exe-cnv-oft-450.
           move      oft-tot-doc          to   rf-oft-tot-doc         .
           move      oft-ctr-stp          to   rf-oft-ctr-stp         .
           move      oft-flg-och          to   rf-oft-flg-och         .
           move      oft-flg-blx (1)      to   rf-oft-flg-blx (1)     .
           move      oft-flg-blx (2)      to   rf-oft-flg-blx (2)     .
           move      oft-flg-blx (3)      to   rf-oft-flg-blx (3)     .
           move      oft-flg-blx (4)      to   rf-oft-flg-blx (4)     .
           move      oft-flg-blx (5)      to   rf-oft-flg-blx (5)     .
           move      oft-flg-blx (6)      to   rf-oft-flg-blx (6)     .
           move      oft-flg-blx (7)      to   rf-oft-flg-blx (7)     .
           move      oft-flg-nbx (1)      to   rf-oft-flg-nbx (1)     .
           move      oft-flg-nbx (2)      to   rf-oft-flg-nbx (2)     .
           move      oft-flg-nbx (3)      to   rf-oft-flg-nbx (3)     .
           move      oft-flg-pul          to   rf-oft-flg-pul         .
           move      oft-alx-exp          to   rf-oft-alx-exp         .
       exe-cnv-oft-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-rsp = Codice responsabile ordine        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-oft-cod-rsp         .
       exe-cnv-oft-510.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-lng = Codice lingua                     *
      *                  *---------------------------------------------*
           move      "I  "                to   rf-oft-cod-lng         .
       exe-cnv-oft-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpf = Sigla valuta per fatturazione     *
      *                  * dec-vpf = Decimali valuta per fatturazione  *
      *                  * tdc-vpf = Tipo di cambio valuta per fattu-  *
      *                  *           razione                           *
      *                  * cdc-vpf = Coefficiente di cambio valuta per *
      *                  *           fatturazione                      *
      *                  *---------------------------------------------*
           move      oft-sgl-vlt          to   rf-oft-sgl-vpf         .
           move      oft-dec-vlt          to   rf-oft-dec-vpf         .
           move      oft-tdc-vlt          to   rf-oft-tdc-vpf         .
           move      oft-cdc-vlt          to   rf-oft-cdc-vpf         .
       exe-cnv-oft-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-oft]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-oft-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-oft-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-oft-250.
       exe-cnv-oft-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-oft]                        *
      *                  *---------------------------------------------*
           close     oft                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-oft]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
       exe-cnv-oft-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-oft-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-oft] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-oft-999.
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
           move      ofr-num-prt          to   rf-ofr-num-prt         .
           move      ofr-num-prg          to   rf-ofr-num-prg         .
           move      ofr-tmo-orf          to   rf-ofr-tmo-orf         .
           move      ofr-cod-dpz          to   rf-ofr-cod-dpz         .
           move      ofr-dat-doc          to   rf-ofr-dat-doc         .
           move      ofr-num-doc          to   rf-ofr-num-doc         .
           move      ofr-tip-arc          to   rf-ofr-tip-arc         .
           move      ofr-cod-arc          to   rf-ofr-cod-arc         .
           move      ofr-dpz-arc          to   rf-ofr-dpz-arc         .
           move      ofr-cof-dat          to   rf-ofr-cof-dat         .
           move      ofr-cof-num          to   rf-ofr-cof-num         .
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
           move      ofr-prz-acq          to   rf-ofr-prz-acq         .
           move      ofr-snx-2pz          to   rf-ofr-snx-2pz         .
           move      ofr-dec-2pz          to   rf-ofr-dec-2pz         .
           move      ofr-prz-a02          to   rf-ofr-prz-a02         .
           move      ofr-epz-rgo          to   rf-ofr-epz-rgo         .
           move      ofr-per-scr (1)      to   rf-ofr-per-scr (1)     .
           move      ofr-per-scr (2)      to   rf-ofr-per-scr (2)     .
           move      ofr-per-scr (3)      to   rf-ofr-per-scr (3)     .
           move      ofr-per-scr (4)      to   rf-ofr-per-scr (4)     .
           move      ofr-per-scr (5)      to   rf-ofr-per-scr (5)     .
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
      *                  * cod-lng = Codice lingua                     *
      *                  *---------------------------------------------*
           move      "I   "               to   rf-ofr-cod-lng         .
       exe-cnv-ofr-510.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * pri-eva = Codice lingua                     *
      *                  *---------------------------------------------*
           move      ofr-pri-eva          to   rf-ofr-pri-eva         .
       exe-cnv-ofr-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpf = Sigla valuta per fatturazione     *
      *                  * dec-vpf = Decimali valuta per fatturazione  *
      *                  * tdc-vpf = Tipo di cambio valuta per fattu-  *
      *                  *           razione                           *
      *                  * cdc-vpf = Coefficiente di cambio valuta per *
      *                  *           fatturazione                      *
      *                  *---------------------------------------------*
           move      ofr-sgl-vlt          to   rf-ofr-sgl-vpf         .
           move      ofr-dec-vlt          to   rf-ofr-dec-vpf         .
           move      ofr-tdc-vlt          to   rf-ofr-tdc-vpf         .
           move      ofr-cdc-vlt          to   rf-ofr-cdc-vpf         .
       exe-cnv-ofr-530.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpp = Sigla valuta per il prezzo        *
      *                  * dec-vpp = Decimali valuta per il prezzo     *
      *                  * tdc-vpp = Tipo di cambio valuta per il      *
      *                  *           prezzo                            *
      *                  * cdc-vpp = Coefficiente di cambio valuta per *
      *                  *           il prezzo                         *
      *                  *---------------------------------------------*
           move      ofr-sgl-vlt          to   rf-ofr-sgl-vpp         .
           move      ofr-dec-vlt          to   rf-ofr-dec-vpp         .
           move      ofr-tdc-vlt          to   rf-ofr-tdc-vpp         .
           move      ofr-cdc-vlt          to   rf-ofr-cdc-vpp         .
       exe-cnv-ofr-540.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpl = Sigla valuta per legame valutario *
      *                  * dec-vpl = Decimali valuta per legame valu-  *
      *                  *           tario                             *
      *                  * tdc-vpl = Tipo di cambio valuta per legame  *
      *                  *           valutario                         *
      *                  * prz-vpl = Prezzo di riferimento per legame  *
      *                  *           valutario                         *
      *                  * cdc-vpl = Coefficiente di cambio valuta per *
      *                  *           legame valutario                  *
      *                  * ccr-vpl = Coefficiente di cambio di riferi- *
      *                  *           mento per legame valutario        *
      *                  * plm-vpl = Percentuale di limitazione per    *
      *                  *           legame valutario                  *
      *                  * tlm-vpl = Tipo di limitazione per legame    *
      *                  *           valutario                         *
      *                  * map-vpl = Momento di applicazione per lega- *
      *                  *           me valutario                      *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ofr-sgl-vpl         .
           move      zero                 to   rf-ofr-dec-vpl         .
           move      spaces               to   rf-ofr-tdc-vpl         .
           move      zero                 to   rf-ofr-prz-vpl         .
           move      zero                 to   rf-ofr-cdc-vpl         .
           move      zero                 to   rf-ofr-ccr-vpl         .
           move      zero                 to   rf-ofr-plm-vpl         .
           move      spaces               to   rf-ofr-tlm-vpl         .
           move      spaces               to   rf-ofr-map-vpl         .
       exe-cnv-ofr-550.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * csr-aap = Categoria di sconto in riga asso- *
      *                  *           ciato al prodotto                 *
      *                  * psr-aap = Percentuali di sconto in riga as- *
      *                  *           sociate al prodotto               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-ofr-csr-aap         .
           move      zero                 to   rf-ofr-psr-aap (1)     .
           move      zero                 to   rf-ofr-psr-aap (2)     .
           move      zero                 to   rf-ofr-psr-aap (3)     .
           move      zero                 to   rf-ofr-psr-aap (4)     .
           move      zero                 to   rf-ofr-psr-aap (5)     .
       exe-cnv-ofr-560.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * prz-net = Prezzo netto                      *
      *                  *---------------------------------------------*
           move      ofr-prz-acq          to   w-det-prz-net-prz      .
           move      ofr-per-scr (1)      to   w-det-prz-net-psc (1)  .
           move      ofr-per-scr (2)      to   w-det-prz-net-psc (2)  .
           move      ofr-per-scr (3)      to   w-det-prz-net-psc (3)  .
           move      ofr-per-scr (4)      to   w-det-prz-net-psc (4)  .
           move      ofr-per-scr (5)      to   w-det-prz-net-psc (5)  .
           perform   det-prz-net-000      thru det-prz-net-999        .
           move      w-det-prz-net-prz    to   rf-ofr-prz-net         .
       exe-cnv-ofr-570.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * dcn-ric = Data consegna richiesta           *
      *                  * fds-dcr = Formato di stampa data consegna   *
      *                  *           richiesta                         *
      *                  * dcn-prv = Data consegna prevista            *
      *                  * flg-cnf = Flag di conferma da fornitore     *
      *                  *---------------------------------------------*
           move      ofr-dat-cns          to   rf-ofr-dcn-ric         .
           move      spaces               to   rf-ofr-fds-dcr         .
           move      ofr-dat-cns          to   rf-ofr-dcn-prv         .
           move      spaces               to   rf-ofr-flg-cnf         .
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
      *    * Conversione [aaf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-aaf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "aaf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-aaf-999.
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
       exe-cnv-aaf-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-aaf-pat              .
       exe-cnv-aaf-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-aaf]                         *
      *                  *---------------------------------------------*
           open      i-o    aaf                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-aaf]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       exe-cnv-aaf-200.
      *              *-------------------------------------------------*
      *              * Start su [old-aaf]                              *
      *              *-------------------------------------------------*
           move      low-values           to   aaf-k01                .
           start     aaf    key not less
                            aaf-k01
                            invalid key
                            go to exe-cnv-aaf-800.
       exe-cnv-aaf-250.
      *              *-------------------------------------------------*
      *              * Next su [old-aaf]                               *
      *              *-------------------------------------------------*
           read      aaf    next
                            with no lock
                            at end
                            go to exe-cnv-aaf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-aaf-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-aaf]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       exe-cnv-aaf-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-aaf]                          *
      *              *-------------------------------------------------*
       exe-cnv-aaf-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      aaf-ide-dat          to   rf-aaf-ide-dat         .
           move      aaf-ide-ute          to   rf-aaf-ide-ute         .
           move      aaf-ide-fas          to   rf-aaf-ide-fas         .
           move      aaf-tip-mag          to   rf-aaf-tip-mag         .
           move      aaf-num-pro          to   rf-aaf-num-pro         .
           move      aaf-cod-dcf          to   rf-aaf-cod-dcf         .
           move      aaf-fda-pif          to   rf-aaf-fda-pif         .
           move      aaf-cop-sfn          to   rf-aaf-cop-sfn         .
           move      aaf-dep-sfn          to   rf-aaf-dep-sfn         .
           move      aaf-xdp-sfn          to   rf-aaf-xdp-sfn         .
           move      aaf-snx-tum          to   rf-aaf-snx-tum         .
           move      aaf-umf-tum          to   rf-aaf-umf-tum         .
           move      aaf-nde-tum          to   rf-aaf-nde-tum         .
           move      aaf-cmo-tum          to   rf-aaf-cmo-tum         .
           move      aaf-cdi-tum          to   rf-aaf-cdi-tum         .
           move      aaf-dpz-dcf          to   rf-aaf-dpz-dcf         .
           move      aaf-ann-not          to   rf-aaf-ann-not         .
           move      aaf-tmp-cns          to   rf-aaf-tmp-cns         .
           move      aaf-sgl-vlt          to   rf-aaf-sgl-vlt         .
           move      aaf-dec-vlt          to   rf-aaf-dec-vlt         .
           move      aaf-dec-prz          to   rf-aaf-dec-prz         .
           move      aaf-tip-pza          to   rf-aaf-tip-pza         .
           move      aaf-lot-acq          to   rf-aaf-lot-acq         .
           move      aaf-tap-pes          to   rf-aaf-tap-pes         .
           move      aaf-uda-pes          to   rf-aaf-uda-pes         .
           move      aaf-lgv-vlt          to   rf-aaf-lgv-vlt         .
           move      aaf-lgv-dcv          to   rf-aaf-lgv-dcv         .
           move      aaf-lgv-tdc          to   rf-aaf-lgv-tdc         .
           move      aaf-lgv-cdc          to   rf-aaf-lgv-cdc         .
           move      aaf-lgv-pdt          to   rf-aaf-lgv-pdt         .
           move      aaf-qta-pes (1)      to   rf-aaf-qta-pes (1)     .
           move      aaf-qta-pes (2)      to   rf-aaf-qta-pes (2)     .
           move      aaf-qta-pes (3)      to   rf-aaf-qta-pes (3)     .
           move      aaf-qta-pes (4)      to   rf-aaf-qta-pes (4)     .
           move      aaf-qta-pes (5)      to   rf-aaf-qta-pes (5)     .
           move      aaf-qta-pes (6)      to   rf-aaf-qta-pes (6)     .
           move      aaf-prz-pes (1)      to   rf-aaf-prz-pes (1)     .
           move      aaf-prz-pes (2)      to   rf-aaf-prz-pes (2)     .
           move      aaf-prz-pes (3)      to   rf-aaf-prz-pes (3)     .
           move      aaf-prz-pes (4)      to   rf-aaf-prz-pes (4)     .
           move      aaf-prz-pes (5)      to   rf-aaf-prz-pes (5)     .
           move      aaf-prz-pes (6)      to   rf-aaf-prz-pes (6)     .
           move      aaf-csr-pes (1)      to   rf-aaf-csr-pes (1)     .
           move      aaf-csr-pes (2)      to   rf-aaf-csr-pes (2)     .
           move      aaf-csr-pes (3)      to   rf-aaf-csr-pes (3)     .
           move      aaf-csr-pes (4)      to   rf-aaf-csr-pes (4)     .
           move      aaf-csr-pes (5)      to   rf-aaf-csr-pes (5)     .
           move      aaf-csr-pes (6)      to   rf-aaf-csr-pes (6)     .
           move      aaf-psr-pes (1, 1)   to   rf-aaf-psr-pes (1, 1)  .
           move      aaf-psr-pes (1, 2)   to   rf-aaf-psr-pes (1, 2)  .
           move      aaf-psr-pes (1, 3)   to   rf-aaf-psr-pes (1, 3)  .
           move      aaf-psr-pes (1, 4)   to   rf-aaf-psr-pes (1, 4)  .
           move      aaf-psr-pes (1, 5)   to   rf-aaf-psr-pes (1, 5)  .
           move      aaf-psr-pes (2, 1)   to   rf-aaf-psr-pes (2, 1)  .
           move      aaf-psr-pes (2, 2)   to   rf-aaf-psr-pes (2, 2)  .
           move      aaf-psr-pes (2, 3)   to   rf-aaf-psr-pes (2, 3)  .
           move      aaf-psr-pes (2, 4)   to   rf-aaf-psr-pes (2, 4)  .
           move      aaf-psr-pes (2, 5)   to   rf-aaf-psr-pes (2, 5)  .
           move      aaf-psr-pes (3, 1)   to   rf-aaf-psr-pes (3, 1)  .
           move      aaf-psr-pes (3, 2)   to   rf-aaf-psr-pes (3, 2)  .
           move      aaf-psr-pes (3, 3)   to   rf-aaf-psr-pes (3, 3)  .
           move      aaf-psr-pes (3, 4)   to   rf-aaf-psr-pes (3, 4)  .
           move      aaf-psr-pes (3, 5)   to   rf-aaf-psr-pes (3, 5)  .
           move      aaf-psr-pes (4, 1)   to   rf-aaf-psr-pes (4, 1)  .
           move      aaf-psr-pes (4, 2)   to   rf-aaf-psr-pes (4, 2)  .
           move      aaf-psr-pes (4, 3)   to   rf-aaf-psr-pes (4, 3)  .
           move      aaf-psr-pes (4, 4)   to   rf-aaf-psr-pes (4, 4)  .
           move      aaf-psr-pes (4, 5)   to   rf-aaf-psr-pes (4, 5)  .
           move      aaf-psr-pes (5, 1)   to   rf-aaf-psr-pes (5, 1)  .
           move      aaf-psr-pes (5, 2)   to   rf-aaf-psr-pes (5, 2)  .
           move      aaf-psr-pes (5, 3)   to   rf-aaf-psr-pes (5, 3)  .
           move      aaf-psr-pes (5, 4)   to   rf-aaf-psr-pes (5, 4)  .
           move      aaf-psr-pes (5, 5)   to   rf-aaf-psr-pes (5, 5)  .
           move      aaf-psr-pes (6, 1)   to   rf-aaf-psr-pes (6, 1)  .
           move      aaf-psr-pes (6, 2)   to   rf-aaf-psr-pes (6, 2)  .
           move      aaf-psr-pes (6, 3)   to   rf-aaf-psr-pes (6, 3)  .
           move      aaf-psr-pes (6, 4)   to   rf-aaf-psr-pes (6, 4)  .
           move      aaf-psr-pes (6, 5)   to   rf-aaf-psr-pes (6, 5)  .
           move      aaf-alx-exp          to   rf-aaf-alx-exp         .
       exe-cnv-aaf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-aaf]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-aaf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-aaf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-aaf-250.
       exe-cnv-aaf-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-aaf]                        *
      *                  *---------------------------------------------*
           close     aaf                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-aaf]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       exe-cnv-aaf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-aaf-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-aaf] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-aaf-999.
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
       exe-cnv-bft-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-440.
           move      bft-spe-snx (w-c01)  to   rf-bft-spe-snx (w-c01) .
           move      bft-spe-mad (w-c01)  to   rf-bft-spe-mad (w-c01) .
           move      bft-spe-per (w-c01)  to   rf-bft-spe-per (w-c01) .
           move      bft-spe-ibl (w-c01)  to   rf-bft-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-bft-420.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-bft-430.
           move      bft-ibx-spe
                    (w-c01, w-c02)        to   rf-bft-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-bft-420.
       exe-cnv-bft-430.
           move      bft-spe-imp (w-c01)  to   rf-bft-spe-imp (w-c01) .
           move      bft-spe-civ (w-c01)  to   rf-bft-spe-civ (w-c01) .
           move      bft-spe-ccp (w-c01)  to   rf-bft-spe-ccp (w-c01) .
           go to     exe-cnv-bft-410.
       exe-cnv-bft-440.
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
       exe-cnv-bft-450.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-bft-460.
           move      bft-iva-cod (w-c01)  to   rf-bft-iva-cod (w-c01) .
           move      bft-iva-ibl (w-c01)  to   rf-bft-iva-ibl (w-c01) .
           go to     exe-cnv-bft-450.
       exe-cnv-bft-460.
           move      zero                 to   w-c01                  .
       exe-cnv-bft-470.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-bft-480.
           move      bft-ctp-cod (w-c01)  to   rf-bft-ctp-cod (w-c01) .
           move      bft-ctp-imp (w-c01)  to   rf-bft-ctp-imp (w-c01) .
           go to     exe-cnv-bft-470.
       exe-cnv-bft-480.
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
       exe-cnv-bft-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-lng = Codice lingua                     *
      *                  *---------------------------------------------*
           move      "I  "                to   rf-bft-cod-lng         .
       exe-cnv-bft-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpf = Sigla valuta per fatturazione     *
      *                  * dec-vpf = Decimali valuta per fatturazione  *
      *                  * tdc-vpf = Tipo di cambio valuta per fattu-  *
      *                  *           razione                           *
      *                  * cdc-vpf = Coefficiente di cambio valuta per *
      *                  *           fatturazione                      *
      *                  *---------------------------------------------*
           move      bft-sgl-vlt          to   rf-bft-sgl-vpf         .
           move      bft-dec-vlt          to   rf-bft-dec-vpf         .
           move      bft-tdc-vlt          to   rf-bft-tdc-vpf         .
           move      bft-cdc-vlt          to   rf-bft-cdc-vpf         .
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
      *    * Conversione [bfr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-bfr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "bfr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-bfr-999.
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
       exe-cnv-bfr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-bfr-pat              .
       exe-cnv-bfr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-bfr]                         *
      *                  *---------------------------------------------*
           open      i-o    bfr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-bfr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       exe-cnv-bfr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-bfr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   bfr-k01                .
           start     bfr    key not less
                            bfr-k01
                            invalid key
                            go to exe-cnv-bfr-800.
       exe-cnv-bfr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-bfr]                               *
      *              *-------------------------------------------------*
           read      bfr    next
                            with no lock
                            at end
                            go to exe-cnv-bfr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-bfr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-bfr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       exe-cnv-bfr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-bfr]                          *
      *              *-------------------------------------------------*
       exe-cnv-bfr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      bfr-num-prt          to   rf-bfr-num-prt         .
           move      bfr-num-prg          to   rf-bfr-num-prg         .
           move      bfr-cod-tmb          to   rf-bfr-cod-tmb         .
           move      bfr-cod-dpz          to   rf-bfr-cod-dpz         .
           move      bfr-dat-reg          to   rf-bfr-dat-reg         .
           move      bfr-tip-arc          to   rf-bfr-tip-arc         .
           move      bfr-cod-arc          to   rf-bfr-cod-arc         .
           move      bfr-dpz-arc          to   rf-bfr-dpz-arc         .
           move      bfr-dat-doc          to   rf-bfr-dat-doc         .
           move      bfr-num-doc          to   rf-bfr-num-doc         .
           move      bfr-bld-flb          to   rf-bfr-bld-flb         .
           move      bfr-bld-tpb          to   rf-bfr-bld-tpb         .
           move      bfr-bld-rgb          to   rf-bfr-bld-rgb         .
           move      bfr-tip-rig          to   rf-bfr-tip-rig         .
           move      bfr-tip-mag          to   rf-bfr-tip-mag         .
           move      bfr-num-mag          to   rf-bfr-num-mag         .
           move      bfr-alf-mag          to   rf-bfr-alf-mag         .
           move      bfr-sgl-vrn          to   rf-bfr-sgl-vrn         .
           move      bfr-fda-pif          to   rf-bfr-fda-pif         .
           move      bfr-cop-sfn          to   rf-bfr-cop-sfn         .
           move      bfr-snx-tum          to   rf-bfr-snx-tum         .
           move      bfr-umf-tum          to   rf-bfr-umf-tum         .
           move      bfr-nde-tum          to   rf-bfr-nde-tum         .
           move      bfr-cmo-tum          to   rf-bfr-cmo-tum         .
           move      bfr-cdi-tum          to   rf-bfr-cdi-tum         .
           move      bfr-des-ext          to   rf-bfr-des-ext         .
           move      bfr-des-rig          to   rf-bfr-des-rig         .
           move      bfr-tip-pro          to   rf-bfr-tip-pro         .
           move      bfr-cod-iva          to   rf-bfr-cod-iva         .
           move      bfr-ctp-acq          to   rf-bfr-ctp-acq         .
           move      bfr-umi-acq          to   rf-bfr-umi-acq         .
           move      bfr-dec-qta          to   rf-bfr-dec-qta         .
           move      bfr-qta-fda          to   rf-bfr-qta-fda         .
           move      bfr-qta-acq          to   rf-bfr-qta-acq         .
           move      bfr-cod-dsl          to   rf-bfr-cod-dsl         .
           move      bfr-snx-2qt          to   rf-bfr-snx-2qt         .
           move      bfr-dec-2qt          to   rf-bfr-dec-2qt         .
           move      bfr-qta-a02          to   rf-bfr-qta-a02         .
           move      bfr-snx-3qt          to   rf-bfr-snx-3qt         .
           move      bfr-dec-3qt          to   rf-bfr-dec-3qt         .
           move      bfr-qta-a03          to   rf-bfr-qta-a03         .
           move      bfr-dec-prz          to   rf-bfr-dec-prz         .
           move      bfr-prz-acq          to   rf-bfr-prz-acq         .
           move      bfr-snx-2pz          to   rf-bfr-snx-2pz         .
           move      bfr-dec-2pz          to   rf-bfr-dec-2pz         .
           move      bfr-prz-a02          to   rf-bfr-prz-a02         .
           move      bfr-epz-rgf          to   rf-bfr-epz-rgf         .
           move      bfr-per-scr (1)      to   rf-bfr-per-scr (1)     .
           move      bfr-per-scr (2)      to   rf-bfr-per-scr (2)     .
           move      bfr-per-scr (3)      to   rf-bfr-per-scr (3)     .
           move      bfr-per-scr (4)      to   rf-bfr-per-scr (4)     .
           move      bfr-per-scr (5)      to   rf-bfr-per-scr (5)     .
           move      bfr-imp-rig          to   rf-bfr-imp-rig         .
           move      bfr-iau-rig          to   rf-bfr-iau-rig         .
           move      bfr-cpv-aap          to   rf-bfr-cpv-aap         .
           move      bfr-ppv-aap (1)      to   rf-bfr-ppv-aap (1)     .
           move      bfr-ppv-aap (2)      to   rf-bfr-ppv-aap (2)     .
           move      bfr-ppv-aap (3)      to   rf-bfr-ppv-aap (3)     .
           move      bfr-fsp-rig          to   rf-bfr-fsp-rig         .
           move      bfr-cpv-rig          to   rf-bfr-cpv-rig         .
           move      bfr-ppv-rig (1)      to   rf-bfr-ppv-rig (1)     .
           move      bfr-ppv-rig (2)      to   rf-bfr-ppv-rig (2)     .
           move      bfr-ppv-rig (3)      to   rf-bfr-ppv-rig (3)     .
           move      bfr-pvf-rig          to   rf-bfr-pvf-rig         .
           move      bfr-rdo-tip          to   rf-bfr-rdo-tip         .
           move      bfr-rdo-dat          to   rf-bfr-rdo-dat         .
           move      bfr-rdo-num          to   rf-bfr-rdo-num         .
           move      bfr-orf-tip          to   rf-bfr-orf-tip         .
           move      bfr-orf-dat          to   rf-bfr-orf-dat         .
           move      bfr-orf-num          to   rf-bfr-orf-num         .
           move      bfr-orf-prt          to   rf-bfr-orf-prt         .
           move      bfr-orf-prg          to   rf-bfr-orf-prg         .
           move      bfr-orf-fzs          to   rf-bfr-orf-fzs         .
           move      bfr-orm-prt          to   rf-bfr-orm-prt         .
           move      bfr-flg-rch          to   rf-bfr-flg-rch         .
           move      bfr-flg-blx (1)      to   rf-bfr-flg-blx (1)     .
           move      bfr-flg-blx (2)      to   rf-bfr-flg-blx (2)     .
           move      bfr-flg-blx (3)      to   rf-bfr-flg-blx (3)     .
           move      bfr-flg-blx (4)      to   rf-bfr-flg-blx (4)     .
           move      bfr-flg-blx (5)      to   rf-bfr-flg-blx (5)     .
           move      bfr-flg-blx (6)      to   rf-bfr-flg-blx (6)     .
           move      bfr-flg-blx (7)      to   rf-bfr-flg-blx (7)     .
           move      bfr-flg-nbx (1)      to   rf-bfr-flg-nbx (1)     .
           move      bfr-flg-nbx (2)      to   rf-bfr-flg-nbx (2)     .
           move      bfr-flg-nbx (3)      to   rf-bfr-flg-nbx (3)     .
           move      bfr-flg-pul          to   rf-bfr-flg-pul         .
           move      bfr-alx-exp          to   rf-bfr-alx-exp         .
       exe-cnv-bfr-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-lng = Codice lingua                     *
      *                  *---------------------------------------------*
           move      "I   "               to   rf-bfr-cod-lng         .
       exe-cnv-bfr-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpf = Sigla valuta per fatturazione     *
      *                  * dec-vpf = Decimali valuta per fatturazione  *
      *                  * tdc-vpf = Tipo di cambio valuta per fattu-  *
      *                  *           razione                           *
      *                  * cdc-vpf = Coefficiente di cambio valuta per *
      *                  *           fatturazione                      *
      *                  *---------------------------------------------*
           move      bfr-sgl-vlt          to   rf-bfr-sgl-vpf         .
           move      bfr-dec-vlt          to   rf-bfr-dec-vpf         .
           move      bfr-tdc-vlt          to   rf-bfr-tdc-vpf         .
           move      bfr-cdc-vlt          to   rf-bfr-cdc-vpf         .
       exe-cnv-bfr-530.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpp = Sigla valuta per il prezzo        *
      *                  * dec-vpp = Decimali valuta per il prezzo     *
      *                  * tdc-vpp = Tipo di cambio valuta per il      *
      *                  *           prezzo                            *
      *                  * cdc-vpp = Coefficiente di cambio valuta per *
      *                  *           il prezzo                         *
      *                  *---------------------------------------------*
           move      bfr-sgl-vlt          to   rf-bfr-sgl-vpp         .
           move      bfr-dec-vlt          to   rf-bfr-dec-vpp         .
           move      bfr-tdc-vlt          to   rf-bfr-tdc-vpp         .
           move      bfr-cdc-vlt          to   rf-bfr-cdc-vpp         .
       exe-cnv-bfr-540.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpl = Sigla valuta per legame valutario *
      *                  * dec-vpl = Decimali valuta per legame valu-  *
      *                  *           tario                             *
      *                  * tdc-vpl = Tipo di cambio valuta per legame  *
      *                  *           valutario                         *
      *                  * prz-vpl = Prezzo di riferimento per legame  *
      *                  *           valutario                         *
      *                  * cdc-vpl = Coefficiente di cambio valuta per *
      *                  *           legame valutario                  *
      *                  * ccr-vpl = Coefficiente di cambio di riferi- *
      *                  *           mento per legame valutario        *
      *                  * plm-vpl = Percentuale di limitazione per    *
      *                  *           legame valutario                  *
      *                  * tlm-vpl = Tipo di limitazione per legame    *
      *                  *           valutario                         *
      *                  * map-vpl = Momento di applicazione per lega- *
      *                  *           me valutario                      *
      *                  *---------------------------------------------*
           move      spaces               to   rf-bfr-sgl-vpl         .
           move      zero                 to   rf-bfr-dec-vpl         .
           move      spaces               to   rf-bfr-tdc-vpl         .
           move      zero                 to   rf-bfr-prz-vpl         .
           move      zero                 to   rf-bfr-cdc-vpl         .
           move      zero                 to   rf-bfr-ccr-vpl         .
           move      zero                 to   rf-bfr-plm-vpl         .
           move      spaces               to   rf-bfr-tlm-vpl         .
           move      spaces               to   rf-bfr-map-vpl         .
       exe-cnv-bfr-550.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * csr-aap = Categoria di sconto in riga asso- *
      *                  *           ciato al prodotto                 *
      *                  * psr-aap = Percentuali di sconto in riga as- *
      *                  *           sociate al prodotto               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-bfr-csr-aap         .
           move      zero                 to   rf-bfr-psr-aap (1)     .
           move      zero                 to   rf-bfr-psr-aap (2)     .
           move      zero                 to   rf-bfr-psr-aap (3)     .
           move      zero                 to   rf-bfr-psr-aap (4)     .
           move      zero                 to   rf-bfr-psr-aap (5)     .
       exe-cnv-bfr-560.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * prz-net = Prezzo netto                      *
      *                  *---------------------------------------------*
           move      bfr-prz-acq          to   w-det-prz-net-prz      .
           move      bfr-per-scr (1)      to   w-det-prz-net-psc (1)  .
           move      bfr-per-scr (2)      to   w-det-prz-net-psc (2)  .
           move      bfr-per-scr (3)      to   w-det-prz-net-psc (3)  .
           move      bfr-per-scr (4)      to   w-det-prz-net-psc (4)  .
           move      bfr-per-scr (5)      to   w-det-prz-net-psc (5)  .
           perform   det-prz-net-000      thru det-prz-net-999        .
           move      w-det-prz-net-prz    to   rf-bfr-prz-net         .
       exe-cnv-bfr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-bfr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-bfr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-bfr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-bfr-250.
       exe-cnv-bfr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-bfr]                        *
      *                  *---------------------------------------------*
           close     bfr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-bfr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       exe-cnv-bfr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-bfr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-bfr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-bfr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [fft]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fft-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "fft "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-fft-999.
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
       exe-cnv-fft-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-fft-pat              .
       exe-cnv-fft-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-fft]                         *
      *                  *---------------------------------------------*
           open      i-o    fft                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-fft]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
       exe-cnv-fft-200.
      *              *-------------------------------------------------*
      *              * Start su [old-fft]                              *
      *              *-------------------------------------------------*
           move      low-values           to   fft-k01                .
           start     fft    key not less
                            fft-k01
                            invalid key
                            go to exe-cnv-fft-800.
       exe-cnv-fft-250.
      *              *-------------------------------------------------*
      *              * Next su [old-fft]                               *
      *              *-------------------------------------------------*
           read      fft    next
                            with no lock
                            at end
                            go to exe-cnv-fft-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-fft-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-fft]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
       exe-cnv-fft-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-fft]                          *
      *              *-------------------------------------------------*
       exe-cnv-fft-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      fft-ide-dat          to   rf-fft-ide-dat         .
           move      fft-ide-ute          to   rf-fft-ide-ute         .
           move      fft-ide-fas          to   rf-fft-ide-fas         .
           move      fft-num-prt          to   rf-fft-num-prt         .
           move      fft-cod-tmf          to   rf-fft-cod-tmf         .
           move      fft-tip-doc          to   rf-fft-tip-doc         .
           move      fft-tpv-doc          to   rf-fft-tpv-doc         .
           move      fft-snx-ird          to   rf-fft-snx-ird         .
           move      fft-cod-dpz          to   rf-fft-cod-dpz         .
           move      fft-dat-reg          to   rf-fft-dat-reg         .
           move      fft-tip-arc          to   rf-fft-tip-arc         .
           move      fft-cod-arc          to   rf-fft-cod-arc         .
           move      fft-dpz-arc          to   rf-fft-dpz-arc         .
           move      fft-arc-plf          to   rf-fft-arc-plf         .
           move      fft-dpz-plf          to   rf-fft-dpz-plf         .
           move      fft-dat-doc          to   rf-fft-dat-doc         .
           move      fft-num-doc          to   rf-fft-num-doc         .
           move      fft-ass-iva          to   rf-fft-ass-iva         .
           move      fft-ctp-acq          to   rf-fft-ctp-acq         .
           move      fft-inl-pgt          to   rf-fft-inl-pgt         .
           move      fft-cod-lst          to   rf-fft-cod-lst         .
           move      fft-csr-aaf          to   rf-fft-csr-aaf         .
           move      fft-psr-aaf (1)      to   rf-fft-psr-aaf (1)     .
           move      fft-psr-aaf (2)      to   rf-fft-psr-aaf (2)     .
           move      fft-psr-aaf (3)      to   rf-fft-psr-aaf (3)     .
           move      fft-psr-aaf (4)      to   rf-fft-psr-aaf (4)     .
           move      fft-psr-aaf (5)      to   rf-fft-psr-aaf (5)     .
           move      fft-csc-aaf          to   rf-fft-csc-aaf         .
           move      fft-psc-aaf          to   rf-fft-psc-aaf         .
           move      fft-voc-des (1)      to   rf-fft-voc-des (1)     .
           move      fft-voc-des (2)      to   rf-fft-voc-des (2)     .
           move      fft-voc-des (3)      to   rf-fft-voc-des (3)     .
           move      fft-voc-des (4)      to   rf-fft-voc-des (4)     .
           move      fft-voc-des (5)      to   rf-fft-voc-des (5)     .
           move      fft-voc-des (6)      to   rf-fft-voc-des (6)     .
           move      fft-iiv-doc          to   rf-fft-iiv-doc         .
           move      fft-imp-doc          to   rf-fft-imp-doc         .
           move      fft-cod-fop          to   rf-fft-cod-fop         .
           move      fft-scp-aap          to   rf-fft-scp-aap         .
           move      fft-nos-ban          to   rf-fft-nos-ban         .
           move      fft-cod-abi          to   rf-fft-cod-abi         .
           move      fft-cod-cab          to   rf-fft-cod-cab         .
           move      fft-ccc-app          to   rf-fft-ccc-app         .
           move      fft-ccp-app          to   rf-fft-ccp-app         .
           move      fft-cod-bef          to   rf-fft-cod-bef         .
           move      fft-add-spi          to   rf-fft-add-spi         .
           move      fft-add-spb          to   rf-fft-add-spb         .
           move      fft-ipr-iel          to   rf-fft-ipr-iel         .
           move      fft-pag-dsm          to   rf-fft-pag-dsm         .
           move      fft-pag-qaf          to   rf-fft-pag-qaf         .
           move      fft-pag-act          to   rf-fft-pag-act         .
           move      fft-cod-aqt          to   rf-fft-cod-aqt         .
           move      fft-pvf-aqt          to   rf-fft-pvf-aqt         .
           move      fft-cod-ime          to   rf-fft-cod-ime         .
           move      fft-pvf-ime          to   rf-fft-pvf-ime         .
           move      fft-tot-rig (1)      to   rf-fft-tot-rig (1)     .
           move      fft-tot-rig (2)      to   rf-fft-tot-rig (2)     .
           move      fft-tot-rig (3)      to   rf-fft-tot-rig (3)     .
           move      fft-tot-rig (4)      to   rf-fft-tot-rig (4)     .
           move      fft-tot-rig (5)      to   rf-fft-tot-rig (5)     .
           move      fft-tot-rig (6)      to   rf-fft-tot-rig (6)     .
           move      fft-tot-rig (7)      to   rf-fft-tot-rig (7)     .
           move      fft-tot-rig (8)      to   rf-fft-tot-rig (8)     .
           move      fft-tot-rig (9)      to   rf-fft-tot-rig (9)     .
           move      fft-tot-scc          to   rf-fft-tot-scc         .
           move      fft-per-scc          to   rf-fft-per-scc         .
           move      fft-civ-scc          to   rf-fft-civ-scc         .
           move      fft-ccp-scc          to   rf-fft-ccp-scc         .
           move      fft-tot-scp          to   rf-fft-tot-scp         .
           move      fft-per-scp          to   rf-fft-per-scp         .
           move      fft-civ-scp          to   rf-fft-civ-scp         .
           move      fft-ccp-scp          to   rf-fft-ccp-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-fft-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-fft-420.
           move      fft-spe-snx (w-c01)  to   rf-fft-spe-snx (w-c01) .
           move      fft-spe-mad (w-c01)  to   rf-fft-spe-mad (w-c01) .
           move      fft-spe-per (w-c01)  to   rf-fft-spe-per (w-c01) .
           move      fft-spe-ibl (w-c01)  to   rf-fft-spe-ibl (w-c01) .
           move      fft-ibt-spe (w-c01)  to   rf-fft-ibt-spe (w-c01) .
           move      fft-spe-imp (w-c01)  to   rf-fft-spe-imp (w-c01) .
           move      fft-spe-civ (w-c01)  to   rf-fft-spe-civ (w-c01) .
           move      fft-spe-ccp (w-c01)  to   rf-fft-spe-ccp (w-c01) .
           go to     exe-cnv-fft-410.
       exe-cnv-fft-420.
           move      fft-tot-spi          to   rf-fft-tot-spi         .
           move      fft-civ-spi          to   rf-fft-civ-spi         .
           move      fft-ccp-spi          to   rf-fft-ccp-spi         .
           move      fft-tot-spb          to   rf-fft-tot-spb         .
           move      fft-civ-spb          to   rf-fft-civ-spb         .
           move      fft-ccp-spb          to   rf-fft-ccp-spb         .
           move      fft-des-cau          to   rf-fft-des-cau         .
           move      fft-num-giv          to   rf-fft-num-giv         .
           move      fft-npt-iva          to   rf-fft-npt-iva         .
           move      zero                 to   w-c01                  .
       exe-cnv-fft-430.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-fft-440.
           move      fft-iva-cod (w-c01)  to   rf-fft-iva-cod (w-c01) .
           move      fft-iva-ibv (w-c01)  to   rf-fft-iva-ibv (w-c01) .
           move      fft-iva-ibl (w-c01)  to   rf-fft-iva-ibl (w-c01) .
           move      fft-iva-imp (w-c01)  to   rf-fft-iva-imp (w-c01) .
           go to     exe-cnv-fft-430.
       exe-cnv-fft-440.
           move      fft-iva-tdo          to   rf-fft-iva-tdo         .
           move      fft-dat-mgd          to   rf-fft-dat-mgd         .
           move      fft-prt-mgd          to   rf-fft-prt-mgd         .
           move      fft-nrg-mgd          to   rf-fft-nrg-mgd         .
           move      fft-dri-mgd          to   rf-fft-dri-mgd         .
           move      fft-nri-mgd          to   rf-fft-nri-mgd         .
           move      fft-prt-scf          to   rf-fft-prt-scf         .
           move      fft-nps-scf          to   rf-fft-nps-scf         .
           move      fft-ctr-scf          to   rf-fft-ctr-scf         .
           move      fft-prt-mag          to   rf-fft-prt-mag         .
           move      fft-flg-ela          to   rf-fft-flg-ela         .
           move      fft-flg-pul          to   rf-fft-flg-pul         .
           move      fft-alx-exp          to   rf-fft-alx-exp         .
       exe-cnv-fft-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-lng = Codice lingua                     *
      *                  *---------------------------------------------*
           move      "I  "                to   rf-fft-cod-lng         .
       exe-cnv-fft-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpf = Sigla valuta per fatturazione     *
      *                  * dec-vpf = Decimali valuta per fatturazione  *
      *                  * tdc-vpf = Tipo di cambio valuta per fattu-  *
      *                  *           razione                           *
      *                  * cdc-vpf = Coefficiente di cambio valuta per *
      *                  *           fatturazione                      *
      *                  *---------------------------------------------*
           move      fft-sgl-vlt          to   rf-fft-sgl-vpf         .
           move      fft-dec-vlt          to   rf-fft-dec-vpf         .
           move      fft-tdc-vlt          to   rf-fft-tdc-vpf         .
           move      fft-cdc-vlt          to   rf-fft-cdc-vpf         .
       exe-cnv-fft-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-fft]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-fft-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-fft-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-fft-250.
       exe-cnv-fft-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-fft]                        *
      *                  *---------------------------------------------*
           close     fft                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-fft]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
       exe-cnv-fft-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-fft-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-fft] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-fft-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ffr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ffr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ffr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ffr-999.
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
       exe-cnv-ffr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-ffr-pat              .
       exe-cnv-ffr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-ffr]                         *
      *                  *---------------------------------------------*
           open      i-o    ffr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-ffr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
       exe-cnv-ffr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-ffr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   ffr-k01                .
           start     ffr    key not less
                            ffr-k01
                            invalid key
                            go to exe-cnv-ffr-800.
       exe-cnv-ffr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-ffr]                               *
      *              *-------------------------------------------------*
           read      ffr    next
                            with no lock
                            at end
                            go to exe-cnv-ffr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ffr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-ffr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
       exe-cnv-ffr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-ffr]                          *
      *              *-------------------------------------------------*
       exe-cnv-ffr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      ffr-num-prt          to   rf-ffr-num-prt         .
           move      ffr-num-prg          to   rf-ffr-num-prg         .
           move      ffr-cod-tmf          to   rf-ffr-cod-tmf         .
           move      ffr-cod-dpz          to   rf-ffr-cod-dpz         .
           move      ffr-dat-reg          to   rf-ffr-dat-reg         .
           move      ffr-tip-arc          to   rf-ffr-tip-arc         .
           move      ffr-cod-arc          to   rf-ffr-cod-arc         .
           move      ffr-dpz-arc          to   rf-ffr-dpz-arc         .
           move      ffr-dat-doc          to   rf-ffr-dat-doc         .
           move      ffr-num-doc          to   rf-ffr-num-doc         .
           move      ffr-bld-flb          to   rf-ffr-bld-flb         .
           move      ffr-bld-tpb          to   rf-ffr-bld-tpb         .
           move      ffr-bld-rgb          to   rf-ffr-bld-rgb         .
           move      ffr-tip-rig          to   rf-ffr-tip-rig         .
           move      ffr-tip-mag          to   rf-ffr-tip-mag         .
           move      ffr-num-mag          to   rf-ffr-num-mag         .
           move      ffr-alf-mag          to   rf-ffr-alf-mag         .
           move      ffr-sgl-vrn          to   rf-ffr-sgl-vrn         .
           move      ffr-fda-pif          to   rf-ffr-fda-pif         .
           move      ffr-cop-sfn          to   rf-ffr-cop-sfn         .
           move      ffr-snx-tum          to   rf-ffr-snx-tum         .
           move      ffr-umf-tum          to   rf-ffr-umf-tum         .
           move      ffr-nde-tum          to   rf-ffr-nde-tum         .
           move      ffr-cmo-tum          to   rf-ffr-cmo-tum         .
           move      ffr-cdi-tum          to   rf-ffr-cdi-tum         .
           move      ffr-des-ext          to   rf-ffr-des-ext         .
           move      ffr-des-rig          to   rf-ffr-des-rig         .
           move      ffr-tip-pro          to   rf-ffr-tip-pro         .
           move      ffr-cod-iva          to   rf-ffr-cod-iva         .
           move      ffr-ctp-acq          to   rf-ffr-ctp-acq         .
           move      ffr-umi-acq          to   rf-ffr-umi-acq         .
           move      ffr-dec-qta          to   rf-ffr-dec-qta         .
           move      ffr-qta-fda          to   rf-ffr-qta-fda         .
           move      ffr-qta-acq          to   rf-ffr-qta-acq         .
           move      ffr-cod-dsl          to   rf-ffr-cod-dsl         .
           move      ffr-snx-2qt          to   rf-ffr-snx-2qt         .
           move      ffr-dec-2qt          to   rf-ffr-dec-2qt         .
           move      ffr-qta-a02          to   rf-ffr-qta-a02         .
           move      ffr-snx-3qt          to   rf-ffr-snx-3qt         .
           move      ffr-dec-3qt          to   rf-ffr-dec-3qt         .
           move      ffr-qta-a03          to   rf-ffr-qta-a03         .
           move      ffr-dec-prz          to   rf-ffr-dec-prz         .
           move      ffr-prz-acq          to   rf-ffr-prz-acq         .
           move      ffr-snx-2pz          to   rf-ffr-snx-2pz         .
           move      ffr-dec-2pz          to   rf-ffr-dec-2pz         .
           move      ffr-prz-a02          to   rf-ffr-prz-a02         .
           move      ffr-epz-rgf          to   rf-ffr-epz-rgf         .
           move      ffr-per-scr (1)      to   rf-ffr-per-scr (1)     .
           move      ffr-per-scr (2)      to   rf-ffr-per-scr (2)     .
           move      ffr-per-scr (3)      to   rf-ffr-per-scr (3)     .
           move      ffr-per-scr (4)      to   rf-ffr-per-scr (4)     .
           move      ffr-per-scr (5)      to   rf-ffr-per-scr (5)     .
           move      ffr-imp-rig          to   rf-ffr-imp-rig         .
           move      ffr-iau-rig          to   rf-ffr-iau-rig         .
           move      ffr-cpv-aap          to   rf-ffr-cpv-aap         .
           move      ffr-ppv-aap (1)      to   rf-ffr-ppv-aap (1)     .
           move      ffr-ppv-aap (2)      to   rf-ffr-ppv-aap (2)     .
           move      ffr-ppv-aap (3)      to   rf-ffr-ppv-aap (3)     .
           move      ffr-fsp-rig          to   rf-ffr-fsp-rig         .
           move      ffr-cpv-rig          to   rf-ffr-cpv-rig         .
           move      ffr-ppv-rig (1)      to   rf-ffr-ppv-rig (1)     .
           move      ffr-ppv-rig (2)      to   rf-ffr-ppv-rig (2)     .
           move      ffr-ppv-rig (3)      to   rf-ffr-ppv-rig (3)     .
           move      ffr-pvf-rig          to   rf-ffr-pvf-rig         .
           move      ffr-bfo-tip          to   rf-ffr-bfo-tip         .
           move      ffr-bfo-dat          to   rf-ffr-bfo-dat         .
           move      ffr-bfo-prt          to   rf-ffr-bfo-prt         .
           move      ffr-bfo-prg          to   rf-ffr-bfo-prg         .
           move      ffr-bfo-ddo          to   rf-ffr-bfo-ddo         .
           move      ffr-bfo-ndo          to   rf-ffr-bfo-ndo         .
           move      ffr-flg-ela          to   rf-ffr-flg-ela         .
           move      ffr-flg-pul          to   rf-ffr-flg-pul         .
           move      ffr-alx-exp          to   rf-ffr-alx-exp         .
       exe-cnv-ffr-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * cod-lng = Codice lingua                     *
      *                  *---------------------------------------------*
           move      "I   "               to   rf-ffr-cod-lng         .
       exe-cnv-ffr-520.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpf = Sigla valuta per fatturazione     *
      *                  * dec-vpf = Decimali valuta per fatturazione  *
      *                  * tdc-vpf = Tipo di cambio valuta per fattu-  *
      *                  *           razione                           *
      *                  * cdc-vpf = Coefficiente di cambio valuta per *
      *                  *           fatturazione                      *
      *                  *---------------------------------------------*
           move      ffr-sgl-vlt          to   rf-ffr-sgl-vpf         .
           move      ffr-dec-vlt          to   rf-ffr-dec-vpf         .
           move      ffr-tdc-vlt          to   rf-ffr-tdc-vpf         .
           move      ffr-cdc-vlt          to   rf-ffr-cdc-vpf         .
       exe-cnv-ffr-530.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpp = Sigla valuta per il prezzo        *
      *                  * dec-vpp = Decimali valuta per il prezzo     *
      *                  * tdc-vpp = Tipo di cambio valuta per il      *
      *                  *           prezzo                            *
      *                  * cdc-vpp = Coefficiente di cambio valuta per *
      *                  *           il prezzo                         *
      *                  *---------------------------------------------*
           move      ffr-sgl-vlt          to   rf-ffr-sgl-vpp         .
           move      ffr-dec-vlt          to   rf-ffr-dec-vpp         .
           move      ffr-tdc-vlt          to   rf-ffr-tdc-vpp         .
           move      ffr-cdc-vlt          to   rf-ffr-cdc-vpp         .
       exe-cnv-ffr-540.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * sgl-vpl = Sigla valuta per legame valutario *
      *                  * dec-vpl = Decimali valuta per legame valu-  *
      *                  *           tario                             *
      *                  * tdc-vpl = Tipo di cambio valuta per legame  *
      *                  *           valutario                         *
      *                  * prz-vpl = Prezzo di riferimento per legame  *
      *                  *           valutario                         *
      *                  * cdc-vpl = Coefficiente di cambio valuta per *
      *                  *           legame valutario                  *
      *                  * ccr-vpl = Coefficiente di cambio di riferi- *
      *                  *           mento per legame valutario        *
      *                  * plm-vpl = Percentuale di limitazione per    *
      *                  *           legame valutario                  *
      *                  * tlm-vpl = Tipo di limitazione per legame    *
      *                  *           valutario                         *
      *                  * map-vpl = Momento di applicazione per lega- *
      *                  *           me valutario                      *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ffr-sgl-vpl         .
           move      zero                 to   rf-ffr-dec-vpl         .
           move      spaces               to   rf-ffr-tdc-vpl         .
           move      zero                 to   rf-ffr-prz-vpl         .
           move      zero                 to   rf-ffr-cdc-vpl         .
           move      zero                 to   rf-ffr-ccr-vpl         .
           move      zero                 to   rf-ffr-plm-vpl         .
           move      spaces               to   rf-ffr-tlm-vpl         .
           move      spaces               to   rf-ffr-map-vpl         .
       exe-cnv-ffr-550.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * csr-aap = Categoria di sconto in riga asso- *
      *                  *           ciato al prodotto                 *
      *                  * psr-aap = Percentuali di sconto in riga as- *
      *                  *           sociate al prodotto               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-ffr-csr-aap         .
           move      zero                 to   rf-ffr-psr-aap (1)     .
           move      zero                 to   rf-ffr-psr-aap (2)     .
           move      zero                 to   rf-ffr-psr-aap (3)     .
           move      zero                 to   rf-ffr-psr-aap (4)     .
           move      zero                 to   rf-ffr-psr-aap (5)     .
       exe-cnv-ffr-560.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * prz-net = Prezzo netto                      *
      *                  *---------------------------------------------*
           move      ffr-prz-acq          to   w-det-prz-net-prz      .
           move      ffr-per-scr (1)      to   w-det-prz-net-psc (1)  .
           move      ffr-per-scr (2)      to   w-det-prz-net-psc (2)  .
           move      ffr-per-scr (3)      to   w-det-prz-net-psc (3)  .
           move      ffr-per-scr (4)      to   w-det-prz-net-psc (4)  .
           move      ffr-per-scr (5)      to   w-det-prz-net-psc (5)  .
           perform   det-prz-net-000      thru det-prz-net-999        .
           move      w-det-prz-net-prz    to   rf-ffr-prz-net         .
       exe-cnv-ffr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-ffr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ffr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ffr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ffr-250.
       exe-cnv-ffr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-ffr]                        *
      *                  *---------------------------------------------*
           close     ffr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-ffr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
       exe-cnv-ffr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ffr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-ffr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-ffr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [age]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-age-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "age "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-age-999.
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
       exe-cnv-age-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-age-pat              .
       exe-cnv-age-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-age]                         *
      *                  *---------------------------------------------*
           open      i-o    age                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-age]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       exe-cnv-age-200.
      *              *-------------------------------------------------*
      *              * Start su [old-age]                              *
      *              *-------------------------------------------------*
           move      low-values           to   age-k01                .
           start     age    key not less
                            age-k01
                            invalid key
                            go to exe-cnv-age-800.
       exe-cnv-age-250.
      *              *-------------------------------------------------*
      *              * Next su [old-age]                               *
      *              *-------------------------------------------------*
           read      age    next
                            with no lock
                            at end
                            go to exe-cnv-age-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-age-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-age]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       exe-cnv-age-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-age]                          *
      *              *-------------------------------------------------*
       exe-cnv-age-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-age                 .
           move      age-ide-dat          to   rf-age-ide-dat         .
           move      age-ide-ute          to   rf-age-ide-ute         .
           move      age-ide-fas          to   rf-age-ide-fas         .
           move      age-cod-age          to   rf-age-cod-age         .
           move      age-cod-mne          to   rf-age-cod-mne         .
           move      age-nom-age          to   rf-age-nom-age         .
           move      age-nom-key          to   rf-age-nom-key         .
           move      age-rag-key          to   rf-age-rag-key         .
           move      age-rag-soc          to   rf-age-rag-soc         .
           move      age-via-age          to   rf-age-via-age         .
           move      age-loc-age          to   rf-age-loc-age         .
           move      age-cod-naz          to   rf-age-cod-naz         .
           move      age-cod-cmn          to   rf-age-cod-cmn         .
           move      age-cod-fzn          to   rf-age-cod-fzn         .
           move      age-cod-lct          to   rf-age-cod-lct         .
           move      age-num-tel          to   rf-age-num-tel         .
           move      age-num-fax          to   rf-age-num-fax         .
           move      age-num-tlx          to   rf-age-num-tlx         .
           move      age-nom-int          to   rf-age-nom-int         .
           move      age-prt-iva          to   rf-age-prt-iva         .
           move      age-cod-fis          to   rf-age-cod-fis         .
           move      age-cod-cge          to   rf-age-cod-cge         .
           move      age-sup-age          to   rf-age-sup-age         .
           move      age-cat-pvg          to   rf-age-cat-pvg         .
           move      age-per-pvg (1)      to   rf-age-per-pvg (1)     .
           move      age-per-pvg (2)      to   rf-age-per-pvg (2)     .
           move      age-per-pvg (3)      to   rf-age-per-pvg (3)     .
           move      age-cla-bdg          to   rf-age-cla-bdg         .
           move      age-tip-mat          to   rf-age-tip-mat         .
           move      age-flg-cpv          to   rf-age-flg-cpv         .
           move      age-alx-exp          to   rf-age-alx-exp         .
       exe-cnv-age-500.
      *                  *---------------------------------------------*
      *                  * Integrazione per :                          *
      *                  *                                             *
      *                  * ctp-ven = Contropartita per le vendite      *
      *                  * ctp-rsv = Contropartita per resi su vendite *
      *                  *---------------------------------------------*
           move      zero                 to   rf-age-ctp-ven         .
           move      zero                 to   rf-age-ctp-rsv         .
       exe-cnv-age-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-age]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-age-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-age-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-age-250.
       exe-cnv-age-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-age]                        *
      *                  *---------------------------------------------*
           close     age                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-age]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
       exe-cnv-age-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-age-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-age] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-age-999.
           exit.

      *    *===========================================================*
      *    * Conversione [vet]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-vet-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "vet "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-vet-999.
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
       exe-cnv-vet-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-vet-pat              .
       exe-cnv-vet-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-vet]                         *
      *                  *---------------------------------------------*
           open      i-o    vet                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-vet]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       exe-cnv-vet-200.
      *              *-------------------------------------------------*
      *              * Start su [old-vet]                              *
      *              *-------------------------------------------------*
           move      low-values           to   vet-k01                .
           start     vet    key not less
                            vet-k01
                            invalid key
                            go to exe-cnv-vet-800.
       exe-cnv-vet-250.
      *              *-------------------------------------------------*
      *              * Next su [old-vet]                               *
      *              *-------------------------------------------------*
           read      vet    next
                            with no lock
                            at end
                            go to exe-cnv-vet-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-vet-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-vet]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       exe-cnv-vet-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-vet]                          *
      *              *-------------------------------------------------*
       exe-cnv-vet-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-vet                 .
           move      vet-ide-dat          to   rf-vet-ide-dat         .
           move      vet-ide-ute          to   rf-vet-ide-ute         .
           move      vet-ide-fas          to   rf-vet-ide-fas         .
           move      vet-cod-vet          to   rf-vet-cod-vet         .
           move      vet-cod-mne          to   rf-vet-cod-mne         .
           move      vet-rag-key          to   rf-vet-rag-key         .
           move      vet-rag-soc          to   rf-vet-rag-soc         .
           move      vet-via-vet          to   rf-vet-via-vet         .
           move      vet-loc-vet          to   rf-vet-loc-vet         .
           move      vet-cod-naz          to   rf-vet-cod-naz         .
           move      vet-cod-cmn          to   rf-vet-cod-cmn         .
           move      vet-cod-fzn          to   rf-vet-cod-fzn         .
           move      vet-cod-lct          to   rf-vet-cod-lct         .
           move      vet-num-tel          to   rf-vet-num-tel         .
           move      vet-num-fax          to   rf-vet-num-fax         .
           move      vet-num-tlx          to   rf-vet-num-tlx         .
           move      vet-nom-int          to   rf-vet-nom-int         .
           move      vet-prt-iva          to   rf-vet-prt-iva         .
           move      vet-cod-fis          to   rf-vet-cod-fis         .
           move      vet-tip-vet          to   rf-vet-tip-vet         .
           move      vet-alx-exp          to   rf-vet-alx-exp         .
       exe-cnv-vet-500.
      *                  *---------------------------------------------*
      *                  * Aggiornamento per :                         *
      *                  *                                             *
      *                  * tip-vet = Tipo di vettore                   *
      *                  *---------------------------------------------*
           move      "C"                  to   rf-vet-tip-vet         .
       exe-cnv-vet-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-vet]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-vet-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-vet-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-vet-250.
       exe-cnv-vet-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-vet]                        *
      *                  *---------------------------------------------*
           close     vet                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-vet]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
       exe-cnv-vet-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-vet-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-vet] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-vet-999.
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

      *    *===========================================================*
      *    * Determinazione prezzo netto                               *
      *    *-----------------------------------------------------------*
       det-prz-net-000.
      *              *-------------------------------------------------*
      *              * Se valore in input a zero : uscita              *
      *              *-------------------------------------------------*
           if        w-det-prz-net-prz    =    zero
                     go to det-prz-net-999.
      *              *-------------------------------------------------*
      *              * Se percentuali di sconto tutte a zero, prezzo   *
      *              * netto pari al valore in input : uscita          *
      *              *-------------------------------------------------*
           if        w-det-prz-net-psc (1)
                                          =    zero and
                     w-det-prz-net-psc (2)
                                          =    zero and
                     w-det-prz-net-psc (3)
                                          =    zero and
                     w-det-prz-net-psc (4)
                                          =    zero and
                     w-det-prz-net-psc (5)
                                          =    zero
                     go to det-prz-net-999.
      *              *-------------------------------------------------*
      *              * Ciclo per abbattimento con sconti               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-prz-net-ctr      .
       det-prz-net-100.
           add       1                    to   w-det-prz-net-ctr      .
           if        w-det-prz-net-ctr    >    5
                     go to det-prz-net-999.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di calcolo                 *
      *                  *---------------------------------------------*
           move      w-det-prz-net-prz    to   w-cal-imp-sco-iml      .
           move      w-det-prz-net-psc
                    (w-det-prz-net-ctr)   to   w-cal-imp-sco-psc      .
           perform   cal-imp-sco-000      thru cal-imp-sco-999        .
           move      w-cal-imp-sco-imn    to   w-det-prz-net-prz      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-prz-net-100.
       det-prz-net-999.
           exit.

      *    *===========================================================*
      *    * Routine di calcolo importo scontato                       *
      *    *                                                           *
      *    * Input  : w-cal-imp-sco-iml = importo lordo da scontare    *
      *    *        : w-cal-imp-sco-psc = percentuale di sconto        *
      *    *                                                           *
      *    * Output : w-cal-imp-sco-imn = importo netto scontato       *
      *    *          w-cal-imp-sco-ams = ammontare dello sconto       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-imp-sco-000.
      *              *-------------------------------------------------*
      *              * Se percentuale di sconto a zero : uscita        *
      *              *-------------------------------------------------*
           if        w-cal-imp-sco-psc    =    zero
                     move  w-cal-imp-sco-iml
                                          to   w-cal-imp-sco-imn
                     move  zero           to   w-cal-imp-sco-ams
                     go to cal-imp-sco-999.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           subtract  w-cal-imp-sco-psc    from 100,0
                                        giving w-cal-imp-sco-w01      .
           multiply  w-cal-imp-sco-iml    by   w-cal-imp-sco-w01
                                        giving w-cal-imp-sco-w02      .
           divide    100                  into w-cal-imp-sco-w02
                                        giving w-cal-imp-sco-imn
                                               rounded                .
           subtract  w-cal-imp-sco-imn    from w-cal-imp-sco-iml
                                        giving w-cal-imp-sco-ams      .
       cal-imp-sco-999.
           exit.

