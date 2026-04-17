       Identification Division.
       Program-Id.                                 cnv4p6             .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    cnv                 *
      *                                   Fase:    cnv4p6              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/01/06    *
      *                       Ultima revisione:    NdK del 27/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversioni per versione 4.6                *
      *                                                                *
      *                    IN LAVORAZIONE                              *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                  AMPLIAMENTO QUANTITA'                         *
      *                                                                *
      *                    Aggiornamento file [bfr] : 13/01/06  v      *
      *                    Aggiornamento file [bir] : 13/01/06  v      *
      *                    Aggiornamento file [cdp] : 13/01/06  v      *
      *                    Aggiornamento file [fir] : 13/01/06  v      *
      *                    Aggiornamento file [ffr] : 13/01/06  v      *
      *                    Aggiornamento file [osr] : 13/01/06  v      *
      *                    Aggiornamento file [ocp] : 13/01/06  v      *
      *                    Aggiornamento file [ocr] : 13/01/06  v      *
      *                    Aggiornamento file [ofr] : 13/01/06  v      *
      *                                                                *
      *                    Aggiornamento file [dcp] : 16/02/06  v      *
      *                    Aggiornamento file [aaq] : 16/02/06  v      *
      *                    Aggiornamento file [aaf] : 16/02/06  v      *
      *                    Aggiornamento file [fbs] : 16/02/06  v      *
      *                                                                *
      *                  AMPLIAMENTO AREA LIBERA                       *
      *                                                                *
      *                    Aggiornamento file [mgr] : 16/02/06  v      *
      *                    Aggiornamento file [mgs] : 16/02/06  v      *
      *                    Aggiornamento file [mgt] : 16/02/06  v      *
      *                    Aggiornamento file [mgi] : 16/02/06 (moi) v *
      *                    Aggiornamento file [ivp] : 16/02/06  v      *
      *                                                                *
      *                  IMPLEMENTAZIONE 'STATUS' TIPI MOVIMENTO       *
      *                                                                *
      *                    Aggiornamento file [zoc] : 16/02/06  v      *
      *                    Aggiornamento file [zsc] : 16/02/06  v      *
      *                    Aggiornamento file [zbi] : 16/02/06  v      *
      *                    Aggiornamento file [zfi] : 16/02/06  v      *
      *                                                                *
      *                    Aggiornamento file [yof] : 20/02/06  v      *
      *                    Aggiornamento file [ybf] : 20/02/06  v      *
      *                    Aggiornamento file [yff] : 20/02/06  v      *
      *                                                                *
      *                  MIGLIORIA 'STATUS' FILTRI 'DCP'               *
      *                                                                *
      *                    Aggiornamento file [zos] : 20/02/06  v      *
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
      *    * File Control [cdp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  cdp   assign to disk           f-cdp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is cdp-k01
                   alternate record key   is cdp-k02
                   alternate record key   is cdp-k03
                   alternate record key   is cdp-k04
                   alternate record key   is cdp-k05
                   alternate record key   is cdp-k06
                   alternate record key   is cdp-k07
                   alternate record key   is cdp-k08
                   alternate record key   is cdp-k09
                   alternate record key   is cdp-k10
                   alternate record key   is cdp-k11
                             file status  is                f-cdp-sts .

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
      *    * File Control [osr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  osr   assign to disk           f-osr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is osr-k01
                   alternate record key   is osr-k02
                   alternate record key   is osr-k03
                   alternate record key   is osr-k04
                   alternate record key   is osr-k05
                             file status  is                f-osr-sts .

      *    *===========================================================*
      *    * File Control [ocp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ocp   assign to disk           f-ocp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ocp-k01
                             file status  is                f-ocp-sts .

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
                   alternate record key   is ocr-k05
                   alternate record key   is ocr-k06
                   alternate record key   is ocr-k07
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
      *    * File Control [mgr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  mgr   assign to disk           f-mgr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is mgr-k01
                   alternate record key   is mgr-k02
                   alternate record key   is mgr-k03
                   alternate record key   is mgr-k04
                   alternate record key   is mgr-k05
                             file status  is                f-mgr-sts .

      *    *===========================================================*
      *    * File Control [mgs]                                        *
      *    *-----------------------------------------------------------*
           select  optional  mgs   assign to disk           f-mgs-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is mgs-k01
                   alternate record key   is mgs-k02
                             file status  is                f-mgs-sts .

      *    *===========================================================*
      *    * File Control [mgt]                                        *
      *    *-----------------------------------------------------------*
           select  optional  mgt   assign to disk           f-mgt-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is mgt-k01
                   alternate record key   is mgt-k02
                             file status  is                f-mgt-sts .

      *    *===========================================================*
      *    * File Control [moi]                                        *
      *    *-----------------------------------------------------------*
           select  optional  moi   assign to disk           f-moi-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is moi-k01
                   alternate record key   is moi-k02
                             file status  is                f-moi-sts .

      *    *===========================================================*
      *    * File Control [ivp]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ivp   assign to disk           f-ivp-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ivp-k01
                             file status  is                f-ivp-sts .

      *    *===========================================================*
      *    * File Control [zoc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  zoc   assign to disk           f-zoc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is zoc-k01
                   alternate record key   is zoc-k02
                             file status  is                f-zoc-sts .

      *    *===========================================================*
      *    * File Control [zsc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  zsc   assign to disk           f-zsc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is zsc-k01
                   alternate record key   is zsc-k02
                             file status  is                f-zsc-sts .

      *    *===========================================================*
      *    * File Control [zbi]                                        *
      *    *-----------------------------------------------------------*
           select  optional  zbi   assign to disk           f-zbi-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is zbi-k01
                   alternate record key   is zbi-k02
                             file status  is                f-zbi-sts .

      *    *===========================================================*
      *    * File Control [zfi]                                        *
      *    *-----------------------------------------------------------*
           select  optional  zfi   assign to disk           f-zfi-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is zfi-k01
                   alternate record key   is zfi-k02
                             file status  is                f-zfi-sts .

      *    *===========================================================*
      *    * File Control [yof]                                        *
      *    *-----------------------------------------------------------*
           select  optional  yof   assign to disk           f-yof-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is yof-k01
                   alternate record key   is yof-k02
                             file status  is                f-yof-sts .

      *    *===========================================================*
      *    * File Control [ybf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ybf   assign to disk           f-ybf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is ybf-k01
                   alternate record key   is ybf-k02
                             file status  is                f-ybf-sts .

      *    *===========================================================*
      *    * File Control [yff]                                        *
      *    *-----------------------------------------------------------*
           select  optional  yff   assign to disk           f-yff-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is yff-k01
                   alternate record key   is yff-k02
                             file status  is                f-yff-sts .

      *    *===========================================================*
      *    * File Control [zos]                                        *
      *    *-----------------------------------------------------------*
           select  optional  zos   assign to disk           f-zos-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is zos-k01
                   alternate record key   is zos-k02
                   alternate record key   is zos-k03
                   alternate record key   is zos-k04
                             file status  is                f-zos-sts .

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
                   alternate record key   is dcp-k08
                             file status  is                f-dcp-sts .

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
                   alternate record key   is aaf-k06
                             file status  is                f-aaf-sts .

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
      *    * File Control [fbs]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fbs   assign to disk           f-fbs-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fbs-k01
                   alternate record key   is fbs-k02
                             file status  is                f-fbs-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

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
               10  bfr-cod-lng            pic  x(03)                  .
               10  bfr-vpf.
                   15  bfr-sgl-vpf        pic  x(03)                  .
                   15  bfr-dec-vpf        pic  9(01)                  .
                   15  bfr-tdc-vpf        pic  x(01)                  .
                   15  bfr-cdc-vpf        pic  9(06)v9(05) comp-3     .
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
               10  bfr-dec-prz            pic  9(01)                  .
               10  bfr-vpp.
                   15  bfr-sgl-vpp        pic  x(03)                  .
                   15  bfr-dec-vpp        pic  9(01)                  .
                   15  bfr-tdc-vpp        pic  x(01)                  .
                   15  bfr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  bfr-prz-acq            pic  9(09)       comp-3     .
               10  bfr-snx-2pz            pic  9(01)                  .
               10  bfr-dec-2pz            pic  9(01)                  .
               10  bfr-prz-a02            pic  9(09)       comp-3     .
               10  bfr-vpl.
                   15  bfr-sgl-vpl        pic  x(03)                  .
                   15  bfr-dec-vpl        pic  9(01)                  .
                   15  bfr-tdc-vpl        pic  x(01)                  .
                   15  bfr-prz-vpl        pic  9(09)       comp-3     .
                   15  bfr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  bfr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  bfr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  bfr-tlm-vpl        pic  x(01)                  .
                   15  bfr-map-vpl        pic  x(01)                  .
               10  bfr-epz-rgf            pic  9(01)                  .
               10  bfr-csr-aap            pic  9(05)       comp-3     .
               10  bfr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  bfr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  bfr-prz-net            pic  9(09)       comp-3     .
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
      *            * Chiave numero 04 : RIFODS                         *
      *            *---------------------------------------------------*
               10  bir-k04.
                   15  bir-cod-dpz-4      pic  9(02)                  .
                   15  bir-ods-prt        pic  9(11)       comp-3     .
                   15  bir-ods-prg        pic  9(05)       comp-3     .
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
               10  bir-cop-scl            pic  x(14)                  .
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
                   15  bir-prz-vpl        pic  9(09)       comp-3     .
                   15  bir-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  bir-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  bir-plm-vpl        pic  9(01)v9(02) comp-3     .
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
               10  bir-flg-puq            pic  x(01)                  .
               10  bir-alx-exp.
                   15  filler  occurs 19  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [cdp]                                    *
      *    *-----------------------------------------------------------*
       fd  cdp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  cdp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  cdp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  cdp-k01.
                   15  cdp-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  cdp-k02.
                   15  cdp-ide-dat        pic  9(07)       comp-3     .
                   15  cdp-dat-doc        pic  9(07)       comp-3     .
                   15  cdp-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  cdp-k03.
                   15  cdp-dat-doc-3      pic  9(07)       comp-3     .
                   15  cdp-cod-dpz        pic  9(02)                  .
                   15  cdp-num-doc        pic  9(11)       comp-3     .
                   15  cdp-tmo-cdp        pic  x(05)                  .
                   15  cdp-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  cdp-k04.
                   15  cdp-scl-ann        pic  9(03)       comp-3     .
                   15  cdp-cod-dpz-4      pic  9(02)                  .
                   15  cdp-sgl-num        pic  x(03)                  .
                   15  cdp-num-doc-4      pic  9(11)       comp-3     .
                   15  cdp-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : TMONUM                         *
      *            *---------------------------------------------------*
               10  cdp-k05.
                   15  cdp-cod-dpz-5      pic  9(02)                  .
                   15  cdp-tmo-cdp-5      pic  x(05)                  .
                   15  cdp-num-doc-5      pic  9(11)       comp-3     .
                   15  cdp-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZDSZ                         *
      *            *---------------------------------------------------*
               10  cdp-k06.
                   15  cdp-cod-dpz-6      pic  9(02)                  .
                   15  cdp-dsz-prd        pic  9(02)                  .
                   15  cdp-dsz-cod        pic  9(07)       comp-3     .
                   15  cdp-dat-doc-6      pic  9(07)       comp-3     .
                   15  cdp-num-doc-6      pic  9(11)       comp-3     .
                   15  cdp-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZESZ                         *
      *            *---------------------------------------------------*
               10  cdp-k07.
                   15  cdp-cod-dpz-7      pic  9(02)                  .
                   15  cdp-esz-prd        pic  9(02)                  .
                   15  cdp-esz-cod        pic  9(07)       comp-3     .
                   15  cdp-dat-doc-7      pic  9(07)       comp-3     .
                   15  cdp-num-doc-7      pic  9(11)       comp-3     .
                   15  cdp-num-prt-7      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : TMODAT                         *
      *            *---------------------------------------------------*
               10  cdp-k08.
                   15  cdp-cod-dpz-8      pic  9(02)                  .
                   15  cdp-tmo-cdp-8      pic  x(05)                  .
                   15  cdp-dat-doc-8      pic  9(07)       comp-3     .
                   15  cdp-num-doc-8      pic  9(11)       comp-3     .
                   15  cdp-num-prt-8      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : CCHDPZ                         *
      *            *---------------------------------------------------*
               10  cdp-k09.
                   15  cdp-flg-cch        pic  x(01)                  .
                   15  cdp-cod-dpz-9      pic  9(02)                  .
                   15  cdp-num-prt-9      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 10 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  cdp-k10.
                   15  cdp-cod-dpz-10     pic  9(02)                  .
                   15  cdp-tip-mag        pic  9(02)                  .
                   15  cdp-num-mag        pic  9(07)       comp-3     .
                   15  cdp-dat-doc-10     pic  9(07)       comp-3     .
                   15  cdp-num-doc-10     pic  9(11)       comp-3     .
                   15  cdp-num-prt-10     pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 11 : CCHMAG                         *
      *            *---------------------------------------------------*
               10  cdp-k11.
                   15  cdp-flg-cch-11     pic  x(01)                  .
                   15  cdp-cod-dpz-11     pic  9(02)                  .
                   15  cdp-tip-mag-11     pic  9(02)                  .
                   15  cdp-num-mag-11     pic  9(07)       comp-3     .
                   15  cdp-num-prt-11     pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  cdp-dat.
               10  cdp-ide-ute            pic  x(08)                  .
               10  cdp-ide-fas            pic  x(06)                  .
               10  cdp-dsz-dpz            pic  x(04)                  .
               10  cdp-esz-dpz            pic  x(04)                  .
               10  cdp-dat-cns            pic  9(07)       comp-3     .
               10  cdp-pri-prd            pic  x(02)                  .
               10  cdp-com-int.
                   15  cdp-com-rig occurs 03
                                          pic  x(40)                  .
               10  cdp-voc-des occurs 06  pic  x(03)                  .
               10  cdp-alf-mag            pic  x(14)                  .
               10  cdp-sgl-vrn            pic  x(14)                  .
               10  cdp-umi-prd            pic  x(03)                  .
               10  cdp-dec-qta            pic  9(01)                  .
               10  cdp-qta-dap            pic s9(06)v9(03) comp-3     .
               10  cdp-cod-dsl            pic  x(07)                  .
               10  cdp-snx-2qt            pic  9(01)                  .
               10  cdp-dec-2qt            pic  9(01)                  .
               10  cdp-qta-a02            pic s9(06)v9(03) comp-3     .
               10  cdp-snx-3qt            pic  9(01)                  .
               10  cdp-dec-3qt            pic  9(01)                  .
               10  cdp-qta-a03            pic s9(06)v9(03) comp-3     .
               10  cdp-ctr-stp            pic  9(02)                  .
               10  cdp-sdc-ccs            pic  x(01)                  .
               10  cdp-flg-ela.
                   15  cdp-flg-blo.
                       20  cdp-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  cdp-flg-nbl.
                       20  cdp-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  cdp-flg-pul            pic  x(01)                  .
               10  cdp-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
               10  fir-cop-scl            pic  x(14)                  .
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
                   15  fir-prz-vpl        pic  9(09)       comp-3     .
                   15  fir-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  fir-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  fir-plm-vpl        pic  9(01)v9(02) comp-3     .
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
               10  fir-flg-puq            pic  x(01)                  .
               10  fir-alx-exp.
                   15  filler  occurs 19  pic  x(01)                  .

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
               10  ffr-cod-lng            pic  x(03)                  .
               10  ffr-vpf.
                   15  ffr-sgl-vpf        pic  x(03)                  .
                   15  ffr-dec-vpf        pic  9(01)                  .
                   15  ffr-tdc-vpf        pic  x(01)                  .
                   15  ffr-cdc-vpf        pic  9(06)v9(05) comp-3     .
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
               10  ffr-dec-prz            pic  9(01)                  .
               10  ffr-vpp.
                   15  ffr-sgl-vpp        pic  x(03)                  .
                   15  ffr-dec-vpp        pic  9(01)                  .
                   15  ffr-tdc-vpp        pic  x(01)                  .
                   15  ffr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  ffr-prz-acq            pic  9(09)       comp-3     .
               10  ffr-snx-2pz            pic  9(01)                  .
               10  ffr-dec-2pz            pic  9(01)                  .
               10  ffr-prz-a02            pic  9(09)       comp-3     .
               10  ffr-vpl.
                   15  ffr-sgl-vpl        pic  x(03)                  .
                   15  ffr-dec-vpl        pic  9(01)                  .
                   15  ffr-tdc-vpl        pic  x(01)                  .
                   15  ffr-prz-vpl        pic  9(09)       comp-3     .
                   15  ffr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  ffr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  ffr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  ffr-tlm-vpl        pic  x(01)                  .
                   15  ffr-map-vpl        pic  x(01)                  .
               10  ffr-epz-rgf            pic  9(01)                  .
               10  ffr-csr-aap            pic  9(05)       comp-3     .
               10  ffr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  ffr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  ffr-prz-net            pic  9(09)       comp-3     .
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
      *    * File Description [osr]                                    *
      *    *-----------------------------------------------------------*
       fd  osr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  osr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  osr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  osr-k01.
                   15  osr-num-prt-1      pic  9(11)       comp-3     .
                   15  osr-num-prg-1      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  osr-k02.
                   15  osr-cod-dpz-2      pic  9(02)                  .
                   15  osr-tip-mag-2      pic  9(02)                  .
                   15  osr-num-pro-2      pic  9(07)       comp-3     .
                   15  osr-dat-doc-2      pic  9(07)       comp-3     .
                   15  osr-num-prt-2      pic  9(11)       comp-3     .
                   15  osr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RIFORC                         *
      *            *---------------------------------------------------*
               10  osr-k03.
                   15  osr-cod-dpz-3      pic  9(02)                  .
                   15  osr-coc-prt-3      pic  9(11)       comp-3     .
                   15  osr-coc-prg-3      pic  9(05)       comp-3     .
                   15  osr-num-prt-3      pic  9(11)       comp-3     .
                   15  osr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : RCHMAG                         *
      *            *---------------------------------------------------*
               10  osr-k04.
                   15  osr-cod-dpz-4      pic  9(02)                  .
                   15  osr-flg-rch-4      pic  x(01)                  .
                   15  osr-tip-mag-4      pic  9(02)                  .
                   15  osr-num-pro-4      pic  9(07)       comp-3     .
                   15  osr-num-prt-4      pic  9(11)       comp-3     .
                   15  osr-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : RCHARC                         *
      *            *---------------------------------------------------*
               10  osr-k05.
                   15  osr-cod-dpz-5      pic  9(02)                  .
                   15  osr-flg-rch-5      pic  x(01)                  .
                   15  osr-tip-arc-5      pic  x(01)                  .
                   15  osr-cod-arc-5      pic  9(07)       comp-3     .
                   15  osr-num-prt-5      pic  9(11)       comp-3     .
                   15  osr-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  osr-dat.
               10  osr-num-prt            pic  9(11)       comp-3     .
               10  osr-num-prg            pic  9(05)       comp-3     .
               10  osr-cod-tms            pic  x(05)                  .
               10  osr-cod-dpz            pic  9(02)                  .
               10  osr-dat-doc            pic  9(07)       comp-3     .
               10  osr-tip-arc            pic  x(01)                  .
               10  osr-cod-arc            pic  9(07)       comp-3     .
               10  osr-dpz-arc            pic  x(04)                  .
               10  osr-cod-lng            pic  x(03)                  .
               10  osr-vpf.
                   15  osr-sgl-vpf        pic  x(03)                  .
                   15  osr-dec-vpf        pic  9(01)                  .
                   15  osr-tdc-vpf        pic  x(01)                  .
                   15  osr-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  osr-bld-flb            pic  9(01)                  .
               10  osr-bld-tpb            pic  9(01)                  .
               10  osr-bld-rgb            pic  9(01)                  .
               10  osr-tip-rig            pic  x(05)                  .
               10  osr-tip-mag            pic  9(02)                  .
               10  osr-num-pro            pic  9(07)       comp-3     .
               10  osr-alf-pro            pic  x(14)                  .
               10  osr-sgl-vrn            pic  x(14)                  .
               10  osr-cop-scl            pic  x(14)                  .
               10  osr-des-ext            pic  9(01)                  .
               10  osr-des-rig            pic  x(40)                  .
               10  osr-tip-pro            pic  9(02)                  .
               10  osr-cod-iva            pic  9(05)       comp-3     .
               10  osr-ctp-ven            pic  9(07)       comp-3     .
               10  osr-umi-ven            pic  x(03)                  .
               10  osr-dec-qta            pic  9(01)                  .
               10  osr-qta-ven            pic s9(06)v9(03) comp-3     .
               10  osr-snx-2qt            pic  9(01)                  .
               10  osr-dec-2qt            pic  9(01)                  .
               10  osr-qta-a02            pic s9(06)v9(03) comp-3     .
               10  osr-snx-3qt            pic  9(01)                  .
               10  osr-dec-3qt            pic  9(01)                  .
               10  osr-qta-a03            pic s9(06)v9(03) comp-3     .
               10  osr-dec-prz            pic  9(01)                  .
               10  osr-vps.
                   15  osr-sgl-vps        pic  x(03)                  .
                   15  osr-dec-vps        pic  9(01)                  .
                   15  osr-tdc-vps        pic  x(01)                  .
                   15  osr-cdc-vps        pic  9(06)v9(05) comp-3     .
               10  osr-prz-lrs            pic  9(09)       comp-3     .
               10  osr-prz-nts            pic  9(09)       comp-3     .
               10  osr-vpp.
                   15  osr-sgl-vpp        pic  x(03)                  .
                   15  osr-dec-vpp        pic  9(01)                  .
                   15  osr-tdc-vpp        pic  x(01)                  .
                   15  osr-cdc-vpp        pic  9(06)v9(05) comp-3     .
               10  osr-prz-ven            pic  9(09)       comp-3     .
               10  osr-snx-2pz            pic  9(01)                  .
               10  osr-prz-a02            pic  9(09)       comp-3     .
               10  osr-vpl.
                   15  osr-sgl-vpl        pic  x(03)                  .
                   15  osr-dec-vpl        pic  9(01)                  .
                   15  osr-tdc-vpl        pic  x(01)                  .
                   15  osr-prz-vpl        pic  9(09)       comp-3     .
                   15  osr-cdc-vpl        pic  9(06)v9(05) comp-3     .
                   15  osr-ccr-vpl        pic  9(06)v9(05) comp-3     .
                   15  osr-plm-vpl        pic  9(01)v9(02) comp-3     .
                   15  osr-tlm-vpl        pic  x(01)                  .
                   15  osr-map-vpl        pic  x(01)                  .
               10  osr-epz-rgf            pic  9(01)                  .
               10  osr-csr-aap            pic  9(05)       comp-3     .
               10  osr-psr-aap occurs 05  pic  9(02)v9(01) comp-3     .
               10  osr-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  osr-prz-net            pic  9(09)       comp-3     .
               10  osr-epz-pes            pic  9(02)                  .
               10  osr-vpc.
                   15  osr-sgl-vpc        pic  x(03)                  .
                   15  osr-dec-vpc        pic  9(01)                  .
                   15  osr-tdc-vpc        pic  x(01)                  .
                   15  osr-cdc-vpc        pic  9(06)v9(05) comp-3     .
               10  osr-dec-cos            pic  9(01)                  .
               10  osr-cos-rif            pic  9(09)       comp-3     .
               10  osr-imp-rig            pic s9(11)       comp-3     .
               10  osr-iau-rig            pic s9(11)       comp-3     .
               10  osr-cpv-aap            pic  9(05)       comp-3     .
               10  osr-ppv-aap occurs 03  pic  9(02)v9(01) comp-3     .
               10  osr-fsp-rig            pic  9(02)                  .
               10  osr-cpv-rig            pic  9(05)       comp-3     .
               10  osr-ppv-rig occurs 03  pic  9(02)v9(01) comp-3     .
               10  osr-pvf-rig            pic s9(11)       comp-3     .
               10  osr-ocl-dat            pic  9(07)       comp-3     .
               10  osr-ocl-num            pic  x(10)                  .
               10  osr-cmc-tip            pic  x(05)                  .
               10  osr-cmc-dat            pic  9(07)       comp-3     .
               10  osr-cmc-num            pic  9(11)       comp-3     .
               10  osr-coc-tip            pic  x(05)                  .
               10  osr-coc-dat            pic  9(07)       comp-3     .
               10  osr-coc-num            pic  9(11)       comp-3     .
               10  osr-coc-prt            pic  9(11)       comp-3     .
               10  osr-coc-prg            pic  9(05)       comp-3     .
               10  osr-coc-fzs            pic  x(01)                  .
               10  osr-flg-rch            pic  x(01)                  .
               10  osr-flg-ela.
                   15  osr-flg-blo.
                       20  osr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  osr-flg-nbl.
                       20  osr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  osr-flg-pul            pic  x(01)                  .
               10  osr-flg-puq            pic  x(01)                  .
               10  osr-alx-exp.
                   15  filler  occurs 19  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ocp]                                    *
      *    *-----------------------------------------------------------*
       fd  ocp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ocp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ocp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  ocp-k01.
                   15  ocp-num-prt        pic  9(11)       comp-3     .
                   15  ocp-num-prg        pic  9(05)       comp-3     .
                   15  ocp-prg-frm        pic  9(03)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ocp-dat.
               10  ocp-qta-ass            pic s9(06)v9(03) comp-3     .
               10  ocp-dat-ass            pic  9(07)       comp-3     .
               10  ocp-prt-orf            pic  9(11)       comp-3     .
               10  ocp-prg-orf            pic  9(05)       comp-3     .
               10  ocp-flg-ela.
                   15  ocp-flg-blo.
                       20  ocp-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  ocp-flg-nbl.
                       20  ocp-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  ocp-flg-pul            pic  x(01)                  .
               10  ocp-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

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
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : ARCNDO                         *
      *            *---------------------------------------------------*
               10  ocr-k05.
                   15  ocr-cod-dpz-5      pic  9(02)                  .
                   15  ocr-tip-arc-5      pic  x(01)                  .
                   15  ocr-cod-arc-5      pic  9(07)       comp-3     .
                   15  ocr-num-doc-5      pic  9(11)       comp-3     .
                   15  ocr-num-prt-5      pic  9(11)       comp-3     .
                   15  ocr-num-prg-5      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : ARCDCR                         *
      *            *---------------------------------------------------*
               10  ocr-k06.
                   15  ocr-cod-dpz-6      pic  9(02)                  .
                   15  ocr-tip-arc-6      pic  x(01)                  .
                   15  ocr-cod-arc-6      pic  9(07)       comp-3     .
                   15  ocr-dcn-ric-6      pic  9(07)       comp-3     .
                   15  ocr-num-prt-6      pic  9(11)       comp-3     .
                   15  ocr-num-prg-6      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : ARCDEP                         *
      *            *---------------------------------------------------*
               10  ocr-k07.
                   15  ocr-cod-dpz-7      pic  9(02)                  .
                   15  ocr-tip-arc-7      pic  x(01)                  .
                   15  ocr-cod-arc-7      pic  9(07)       comp-3     .
                   15  ocr-des-rig-7      pic  x(40)                  .
                   15  ocr-dcn-ric-7      pic  9(07)       comp-3     .
                   15  ocr-num-prt-7      pic  9(11)       comp-3     .
                   15  ocr-num-prg-7      pic  9(05)       comp-3     .
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
               10  ocr-dec-2pz            pic  9(01)                  .
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
               10  ocr-dcn-cnf            pic  9(07)       comp-3     .
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
                   15  filler  occurs 40  pic  x(01)                  .

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
               10  ofr-csr-aap            pic  9(05)       comp-3     .
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
               10  ofr-dcn-ric            pic  9(07)       comp-3     .
               10  ofr-fds-dcr            pic  x(01)                  .
               10  ofr-dcn-prv            pic  9(07)       comp-3     .
               10  ofr-dcn-cnf            pic  9(07)       comp-3     .
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
                   15  filler  occurs 40  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [mgr]                                    *
      *    *-----------------------------------------------------------*
       fd  mgr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  mgr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  mgr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : DATREG                         *
      *            *---------------------------------------------------*
               10  mgr-k01.
                   15  mgr-dat-reg        pic  9(07)       comp-3     .
                   15  mgr-num-prt        pic  9(07)       comp-3     .
                   15  mgr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PDCDAT                         *
      *            *---------------------------------------------------*
               10  mgr-k02.
                   15  mgr-cod-pdc        pic  9(07)       comp-3     .
                   15  mgr-dat-reg-2      pic  9(07)       comp-3     .
                   15  mgr-num-prt-2      pic  9(07)       comp-3     .
                   15  mgr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : ARCDAT                         *
      *            *---------------------------------------------------*
               10  mgr-k03.
                   15  mgr-tip-arc        pic  x(01)                  .
                   15  mgr-cod-arc        pic  9(07)       comp-3     .
                   15  mgr-dat-reg-3      pic  9(07)       comp-3     .
                   15  mgr-num-prt-3      pic  9(07)       comp-3     .
                   15  mgr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ARCRIF                         *
      *            *---------------------------------------------------*
               10  mgr-k04.
                   15  mgr-tip-arc-4      pic  x(01)                  .
                   15  mgr-cod-arc-4      pic  9(07)       comp-3     .
                   15  mgr-dat-rif        pic  9(07)       comp-3     .
                   15  mgr-num-rif        pic  x(10)                  .
                   15  mgr-dat-reg-4      pic  9(07)       comp-3     .
                   15  mgr-num-prt-4      pic  9(07)       comp-3     .
                   15  mgr-num-prg-4      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : TARDOC                         *
      *            *---------------------------------------------------*
               10  mgr-k05.
                   15  mgr-tip-arc-5      pic  x(01)                  .
                   15  mgr-dat-doc-5      pic  9(07)       comp-3     .
                   15  mgr-num-doc-5      pic  x(10)                  .
                   15  mgr-cod-arc-5      pic  9(07)       comp-3     .
                   15  mgr-dat-reg-5      pic  9(07)       comp-3     .
                   15  mgr-num-prt-5      pic  9(07)       comp-3     .
                   15  mgr-num-prg-5      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  mgr-dat.
               10  mgr-cod-cau            pic  9(03)       comp-3     .
               10  mgr-snx-mob            pic  x(01)                  .
               10  mgr-tip-iva            pic  x(01)                  .
               10  mgr-com-rig            pic  x(40)                  .
               10  mgr-dat-doc            pic  9(07)       comp-3     .
               10  mgr-num-doc            pic  x(10)                  .
               10  mgr-dar-ave            pic  x(01)                  .
               10  mgr-imp-mov            pic s9(13)       comp-3     .
               10  mgr-flg-pge            pic  x(01)                  .
               10  mgr-flg-pcf            pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [mgs]                                    *
      *    *-----------------------------------------------------------*
       fd  mgs       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  mgs-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  mgs-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : "ESECOD"                       *
      *            *---------------------------------------------------*
               10  mgs-k01.
                   15  mgs-ann-ese        pic  9(03)                  .
                   15  mgs-tip-rec        pic  x(01)                  .
                   15  mgs-cod-con        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : "CODESE"                       *
      *            *---------------------------------------------------*
               10  mgs-k02.
                   15  mgs-tip-rec-2      pic  x(01)                  .
                   15  mgs-cod-con-2      pic  9(07)       comp-3     .
                   15  mgs-ann-ese-2      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  mgs-dat.
               10  mgs-sdo-ini            pic s9(13)       comp-3     .
               10  mgs-dti-mes occurs 12.
                   15  mgs-dar-mes        pic s9(13)       comp-3     .
                   15  mgs-ave-mes        pic s9(13)       comp-3     .
                   15  mgs-dav-ret        pic  x(01)                  .
                   15  mgs-imp-ret        pic s9(13)       comp-3     .
               10  mgs-dar-bil            pic s9(13)       comp-3     .
               10  mgs-ave-bil            pic s9(13)       comp-3     .
               10  mgs-dat-chi            pic  9(07)       comp-3     .

      *    *===========================================================*
      *    * File Description [mgt]                                    *
      *    *-----------------------------------------------------------*
       fd  mgt       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  mgt-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  mgt-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : DATREG                         *
      *            *---------------------------------------------------*
               10  mgt-k01.
                   15  mgt-dat-reg        pic  9(07)       comp-3     .
                   15  mgt-num-prt        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  mgt-k02.
                   15  mgt-ide-dat        pic  9(07)       comp-3     .
                   15  mgt-dat-reg-2      pic  9(07)       comp-3     .
                   15  mgt-num-prt-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  mgt-dat.
               10  mgt-ide-ute            pic  x(08)                  .
               10  mgt-ide-fas            pic  x(06)                  .
               10  mgt-cod-cau            pic  9(03)       comp-3     .
               10  mgt-snx-mob            pic  x(01)                  .
               10  mgt-tip-iva            pic  x(01)                  .
               10  mgt-des-cau.
                   15  filler occurs 160  pic  x(01)                  .
               10  mgt-dat-doc            pic  9(07)       comp-3     .
               10  mgt-num-doc            pic  x(10)                  .
               10  mgt-cod-num            pic  9(02)                  .
               10  mgt-prt-iva            pic  9(07)       comp-3     .
               10  mgt-flg-gio            pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [moi]                                    *
      *    *-----------------------------------------------------------*
       fd  moi       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  moi-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  moi-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : DATPRT                         *
      *            *---------------------------------------------------*
               10  moi-k01.
                   15  moi-dat-reg        pic  9(07)       comp-3     .
                   15  moi-num-prt        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATDOC                         *
      *            *---------------------------------------------------*
               10  moi-k02.
                   15  moi-tip-rec        pic  x(01)                  .
                   15  moi-dat-reg-2      pic  9(07)       comp-3     .
                   15  moi-cod-num        pic  9(02)                  .
                   15  moi-prt-iva        pic  9(11)       comp-3     .
                   15  moi-num-prt-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  moi-dat.
               10  moi-dat-doc            pic  9(07)       comp-3     .
               10  moi-num-doc            pic  x(10)                  .
               10  moi-cod-cau            pic  9(03)       comp-3     .
               10  moi-tip-iva            pic  x(01)                  .
               10  moi-cst-iva   occurs 06.
                   15  moi-ibl-iva        pic s9(11)       comp-3     .
                   15  moi-cod-iva        pic  9(05)       comp-3     .
                   15  moi-imp-iva        pic s9(11)       comp-3     .
               10  moi-tot-doc            pic s9(11)       comp-3     .
               10  moi-cod-arc            pic  9(07)       comp-3     .
               10  moi-flg-gio            pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ivp]                                    *
      *    *-----------------------------------------------------------*
       fd  ivp       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ivp-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ivp-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : "ANNMESALI"                    *
      *            *---------------------------------------------------*
               10  ivp-k01.
                   15  ivp-ann-ivp        pic  9(03)       comp-3     .
                   15  ivp-mes-ivp        pic  9(02)                  .
                   15  ivp-cod-iva        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ivp-dat.
               10  ivp-ibl-acq            pic s9(13)       comp-3     .
               10  ivp-imp-acq            pic s9(13)       comp-3     .
               10  ivp-ibl-ven            pic s9(13)       comp-3     .
               10  ivp-imp-ven            pic s9(13)       comp-3     .
               10  ivp-ibl-cor            pic s9(13)       comp-3     .
               10  ivp-imp-cor            pic s9(13)       comp-3     .

      *    *===========================================================*
      *    * File Description [zoc]                                    *
      *    *-----------------------------------------------------------*
       fd  zoc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  zoc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  zoc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTOC                         *
      *            *---------------------------------------------------*
               10  zoc-k01.
                   15  zoc-cod-toc        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  zoc-k02.
                   15  zoc-des-key        pic  x(30)                  .
                   15  zoc-cod-toc-2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  zoc-dat.
               10  zoc-des-toc            pic  x(30)                  .
               10  zoc-pwd-toc            pic  x(08)                  .
               10  zoc-vld-dpz            pic  9(02)                  .
               10  zoc-cod-dpz            pic  9(02)                  .
               10  zoc-org-doc            pic  9(02)                  .
               10  zoc-prv-doc            pic  9(02)                  .
               10  zoc-sgl-num            pic  x(03)                  .
               10  zoc-des-stp            pic  x(25)                  .
               10  zoc-snx-prz            pic  9(02)                  .
               10  zoc-snx-sco            pic  9(02)                  .
               10  zoc-snx-dtc            pic  9(02)                  .
               10  zoc-def-tpr            pic  x(05)                  .
               10  zoc-snx-age            pic  x(01)                  .
               10  zoc-snx-sto            pic  x(01)                  .
               10  zoc-tip-ord            pic  x(01)                  .
               10  zoc-def-fds            pic  9(02)                  .
               10  zoc-def-tar            pic  x(01)                  .
               10  zoc-alx-exp.
                   15  filler  occurs 69  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [zsc]                                    *
      *    *-----------------------------------------------------------*
       fd  zsc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  zsc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  zsc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTOS                         *
      *            *---------------------------------------------------*
               10  zsc-k01.
                   15  zsc-cod-tos-1      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  zsc-k02.
                   15  zsc-des-key-2      pic  x(30)                  .
                   15  zsc-cod-tos-2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  zsc-dat.
               10  zsc-cod-tos            pic  x(05)                  .
               10  zsc-des-key            pic  x(30)                  .
               10  zsc-des-tos            pic  x(30)                  .
               10  zsc-pwd-tos            pic  x(08)                  .
               10  zsc-vld-dpz            pic  9(02)                  .
               10  zsc-cod-dpz            pic  9(02)                  .
               10  zsc-mov-afd            pic  9(02)                  .
               10  zsc-def-tmf            pic  x(05)                  .
               10  zsc-tip-arc            pic  x(01)                  .
               10  zsc-tmo-btz            pic  x(05)                  .
               10  zsc-def-tpr            pic  x(05)                  .
               10  zsc-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [zbi]                                    *
      *    *-----------------------------------------------------------*
       fd  zbi       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  zbi-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  zbi-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTMB                         *
      *            *---------------------------------------------------*
               10  zbi-k01.
                   15  zbi-cod-tmb        pic  x(05)                  .
                   15  zbi-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  zbi-k02.
                   15  zbi-des-key        pic  x(30)                  .
                   15  zbi-cod-tmb-2      pic  x(05)                  .
                   15  zbi-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  zbi-dat.
               10  zbi-dti-gen.
                   15  zbi-des-tmb        pic  x(30)                  .
                   15  zbi-des-stp        pic  x(25)                  .
                   15  zbi-pwd-tmb        pic  x(08)                  .
                   15  zbi-int-ftr        pic  9(02)                  .
                   15  zbi-tmo-ftr        pic  x(05)                  .
                   15  zbi-snx-acm        pic  x(01)                  .
                   15  zbi-def-tac        pic  9(02)                  .
                   15  zbi-def-ctr        pic  x(03)                  .
                   15  zbi-cau-mag        pic  9(05)       comp-3     .
                   15  zbi-cod-mic        pic  x(03)                  .
                   15  zbi-cam-agg        pic  9(05)       comp-3     .
                   15  zbi-def-tar        pic  x(01)                  .
                   15  zbi-snv-tar        pic  x(01)                  .
                   15  zbi-lst-tar        pic  x(04)                  .
                   15  zbi-org-doc        pic  9(02)                  .
                   15  zbi-prv-doc        pic  9(02)                  .
                   15  zbi-sgl-num        pic  x(03)                  .
                   15  zbi-mov-afd        pic  9(02)                  .
                   15  zbi-def-tmf        pic  x(05)                  .
                   15  zbi-snx-prz        pic  9(02)                  .
                   15  zbi-snx-sco        pic  9(02)                  .
                   15  zbi-snx-imp        pic  9(02)                  .
                   15  zbi-snx-civ        pic  9(02)                  .
                   15  zbi-snx-ttd        pic  9(02)                  .
                   15  zbi-snx-dct        pic  9(02)                  .
                   15  zbi-des-dct.
                       20  zbi-rig-dct    occurs 04
                                          pic  x(60)                  .
                   15  zbi-tip-sql        pic  9(02)                  .
                   15  zbi-pos-sql        pic  9(03)       comp-3     .
                   15  zbi-snx-par        pic  x(01)                  .
                   15  zbi-vld-dpz        pic  x(01)                  .
                   15  zbi-snx-age        pic  x(01)                  .
                   15  zbi-snx-ndp        pic  x(01)                  .
                   15  zbi-snx-fop        pic  x(01)                  .
                   15  zbi-snx-lib        pic  x(01)                  .
                   15  zbi-def-tpr        pic  x(05)                  .
                   15  zbi-snx-nmm        pic  x(01)                  .
                   15  zbi-tmo-ft2        pic  x(05)                  .
                   15  zbi-snx-ncv        pic  x(01)                  .
                   15  zbi-alx-gen.
                       20  filler  occurs 23
                                          pic  x(01)                  .
               10  zbi-dti-dpz.
                   15  zbi-cod-dsl        pic  x(07)                  .
                   15  zbi-alx-dpz.
                       20  filler  occurs 40
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [zfi]                                    *
      *    *-----------------------------------------------------------*
       fd  zfi       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  zfi-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  zfi-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTMO                         *
      *            *---------------------------------------------------*
               10  zfi-k01.
                   15  zfi-cod-tmo        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  zfi-k02.
                   15  zfi-des-key        pic  x(30)                  .
                   15  zfi-cod-tmo-2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  zfi-dat.
               10  zfi-des-tmo            pic  x(30)                  .
               10  zfi-pwd-tmo            pic  x(08)                  .
               10  zfi-vld-dpz            pic  9(02)                  .
               10  zfi-cod-dpz            pic  9(02)                  .
               10  zfi-tip-doc            pic  9(02)                  .
               10  zfi-org-doc            pic  9(02)                  .
               10  zfi-prv-doc            pic  9(02)                  .
               10  zfi-sgl-num            pic  x(03)                  .
               10  zfi-num-giv            pic  9(02)                  .
               10  zfi-des-stp            pic  x(25)                  .
               10  zfi-cau-cge            pic  9(03)                  .
               10  zfi-ctp-ivv            pic  9(07)                  .
               10  zfi-ctp-ven            pic  9(07)                  .
               10  zfi-def-tpr            pic  x(05)                  .
               10  zfi-snx-age            pic  x(01)                  .
               10  zfi-snx-rco            pic  x(01)                  .
               10  zfi-snx-rdt            pic  x(01)                  .
               10  zfi-cod-dct            pic  9(03)                  .
               10  zfi-alx-exp.
                   15  filler  occurs 69  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [yof]                                    *
      *    *-----------------------------------------------------------*
       fd  yof       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  yof-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  yof-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTOF                         *
      *            *---------------------------------------------------*
               10  yof-k01.
                   15  yof-cod-tof        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  yof-k02.
                   15  yof-des-key        pic  x(30)                  .
                   15  yof-cod-tof-2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  yof-dat.
               10  yof-des-tof            pic  x(30)                  .
               10  yof-pwd-tof            pic  x(08)                  .
               10  yof-vld-dpz            pic  9(02)                  .
               10  yof-cod-dpz            pic  9(02)                  .
               10  yof-org-doc            pic  9(02)                  .
               10  yof-prv-doc            pic  9(02)                  .
               10  yof-sgl-num            pic  x(03)                  .
               10  yof-des-stp            pic  x(25)                  .
               10  yof-snx-prz            pic  9(02)                  .
               10  yof-snx-sco            pic  9(02)                  .
               10  yof-snx-dtc            pic  9(02)                  .
               10  yof-def-tpr            pic  x(05)                  .
               10  yof-snx-sto            pic  x(01)                  .
               10  yof-snx-stv            pic  x(01)                  .
               10  yof-alx-exp.
                   15  filler  occurs 73  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ybf]                                    *
      *    *-----------------------------------------------------------*
       fd  ybf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  ybf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  ybf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTMB                         *
      *            *---------------------------------------------------*
               10  ybf-k01.
                   15  ybf-cod-tmb        pic  x(05)                  .
                   15  ybf-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  ybf-k02.
                   15  ybf-des-key        pic  x(30)                  .
                   15  ybf-cod-tmb-2      pic  x(05)                  .
                   15  ybf-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  ybf-dat.
               10  ybf-dti-gen.
                   15  ybf-des-tmb        pic  x(30)                  .
                   15  ybf-pwd-tmb        pic  x(08)                  .
                   15  ybf-int-ftr        pic  9(02)                  .
                   15  ybf-tmo-ftr        pic  x(05)                  .
                   15  ybf-cau-mag        pic  9(05)                  .
                   15  ybf-cod-mic        pic  x(03)                  .
                   15  ybf-cam-agg        pic  9(05)                  .
                   15  ybf-def-tar        pic  x(01)                  .
                   15  ybf-snv-tar        pic  x(01)                  .
                   15  ybf-lst-tar        pic  x(04)                  .
                   15  ybf-org-doc        pic  9(02)                  .
                   15  ybf-prv-doc        pic  9(02)                  .
                   15  ybf-mov-afd        pic  9(02)                  .
                   15  ybf-def-tmf        pic  x(05)                  .
                   15  ybf-vld-dpz        pic  x(01)                  .
                   15  ybf-def-tpr        pic  x(05)                  .
                   15  ybf-alx-gen.
                       20  filler  occurs 34
                                          pic  x(01)                  .
               10  ybf-dti-dpz.
                   15  ybf-cod-dsl        pic  x(07)                  .
                   15  ybf-alx-dpz.
                       20  filler  occurs 40
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [yff]                                    *
      *    *-----------------------------------------------------------*
       fd  yff       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  yff-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  yff-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODTMF                         *
      *            *---------------------------------------------------*
               10  yff-k01.
                   15  yff-cod-tmf-1      pic  x(05)                  .
                   15  yff-cod-dpz-1      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  yff-k02.
                   15  yff-des-key-2      pic  x(30)                  .
                   15  yff-cod-tmf-2      pic  x(05)                  .
                   15  yff-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  yff-dat.
               10  yff-cod-tmf            pic  x(05)                  .
               10  yff-cod-dpz            pic  9(02)                  .
               10  yff-dti-gen.
                   15  yff-des-key        pic  x(30)                  .
                   15  yff-des-tmf        pic  x(30)                  .
                   15  yff-pwd-tmf        pic  x(08)                  .
                   15  yff-tip-doc        pic  9(02)                  .
                   15  yff-tpv-doc        pic  9(02)                  .
                   15  yff-snx-ird        pic  x(01)                  .
                   15  yff-cau-cge        pic  9(03)                  .
                   15  yff-cau-mag        pic  9(05)                  .
                   15  yff-mov-afd        pic  9(02)                  .
                   15  yff-def-tmf        pic  x(05)                  .
                   15  yff-vld-dpz        pic  x(01)                  .
                   15  yff-def-tpr        pic  x(05)                  .
                   15  yff-num-giv        pic  9(02)                  .
                   15  yff-alx-gen.
                       20  filler occurs 38
                                          pic  x(01)                  .
               10  yff-dti-dpz.
                   15  yff-cod-dsl        pic  x(07)                  .
                   15  yff-alx-dpz.
                       20  filler occurs 40
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [zos]                                    *
      *    *-----------------------------------------------------------*
       fd  zos       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  zos-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  zos-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODFLT                         *
      *            *---------------------------------------------------*
               10  zos-k01.
                   15  zos-tip-rec        pic  x(04)                  .
                   15  zos-cod-flt        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  zos-k02.
                   15  zos-ide-dat        pic  9(07)                  .
                   15  zos-tip-rec-2      pic  x(04)                  .
                   15  zos-cod-flt-2      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  zos-k03.
                   15  zos-tip-rec-3      pic  x(04)                  .
                   15  zos-des-key        pic  x(40)                  .
                   15  zos-cod-flt-3      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  zos-k04.
                   15  zos-tip-rec-4      pic  x(04)                  .
                   15  zos-cod-mne        pic  x(10)                  .
                   15  zos-cod-flt-4      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  zos-dat.
               10  zos-inf-gen.
                   15  zos-ide-ute        pic  x(08)                  .
                   15  zos-ide-fas        pic  x(06)                  .
                   15  zos-des-flt        pic  x(40)                  .
                   15  zos-ult-cod        pic  9(07)                  .
               10  zos-dat-flt.
                   15  filler occurs 512  pic  x(01)                  .

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
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : KLBPRO                         *
      *            *---------------------------------------------------*
               10  dcp-k08.
                   15  dcp-klb-pro        pic  x(13)                  .
                   15  dcp-num-pro-8      pic  9(07)       comp-3     .
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
                   15  dcp-tip-acp        pic  9(02)                  .
                   15  dcp-not-g01        pic  x(40)                  .
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
                   15  dcp-spc-lib        pic  x(20)                  .
               10  dcp-inf-fat.
                   15  dcp-cod-iva        pic  9(05)       comp-3     .
                   15  dcp-ctp-ven        pic  9(07)       comp-3     .
                   15  dcp-umi-ven        pic  x(03)                  .
                   15  dcp-dec-qta        pic  9(01)                  .
                   15  dcp-sgl-vlt        pic  x(03)                  .
                   15  dcp-dec-vlt        pic  9(01)                  .
                   15  dcp-dec-prz        pic  9(01)                  .
                   15  dcp-prz-lst        pic  9(09)       comp-3     .
                   15  dcp-lot-ven        pic  9(06)v9(03) comp-3     .
                   15  dcp-epz-rgf        pic  9(01)                  .
                   15  dcp-snx-2qt        pic  9(01)                  .
                   15  dcp-dec-2qt        pic  9(01)                  .
                   15  dcp-snx-3qt        pic  9(01)                  .
                   15  dcp-dec-3qt        pic  9(01)                  .
                   15  dcp-snx-2pz        pic  9(01)                  .
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
               10  dcp-inf-lst.
                   15  dcp-snx-lst        pic  9(01)                  .
                   15  dcp-epz-lst.
                       20  dcp-epz-ele occurs 20
                                          pic  x(01)                  .
                   15  dcp-pag-lst        pic  x(10)                  .
                   15  dcp-rfl-lst        pic  x(12)                  .
                   15  dcp-tmc-lst        pic  9(03)       comp-3     .
                   15  dcp-daa-lst        pic  9(01)                  .
                   15  dcp-aut-lst        pic  x(03)                  .
               10  dcp-inf-pcs.
                   15  dcp-cod-s01        pic  9(05)       comp-3     .
                   15  dcp-cod-s02        pic  9(05)       comp-3     .
                   15  dcp-cod-s03        pic  9(05)       comp-3     .
                   15  dcp-dat-icm        pic  9(07)       comp-3     .
                   15  dcp-sta-tus        pic  9(02)                  .
                   15  dcp-sta-tud        pic  9(07)       comp-3     .
                   15  dcp-sta-tuc        pic  9(07)       comp-3     .
                   15  dcp-sta-tux        pic  9(02)                  .
               10  dcp-inf-mkt.
                   15  dcp-cld-imp        pic  x(01)                  .
                   15  dcp-pre-ctn        pic  9(02)                  .
                   15  dcp-gra-ico        pic  9(02)                  .
                   15  dcp-pcl-ccz        pic  9(02)                  .
                   15  dcp-cod-mkt        pic  9(05)       comp-3     .
                   15  dcp-ind-mkt        pic  x(10)                  .
               10  dcp-inf-bdg.
                   15  dcp-cla-bdg        pic  9(05)       comp-3     .
               10  dcp-inf-bol.
                   15  dcp-for-blo        pic  9(02)                  .
                   15  dcp-dor-blo        pic  9(07)       comp-3     .
                   15  dcp-fco-blo        pic  9(02)                  .
                   15  dcp-dco-blo        pic  9(07)       comp-3     .
               10  dcp-inf-iic.
                   15  dcp-cdn-cdm        pic  9(08)                  .
               10  dcp-inf-aps.
                   15  dcp-alx-exp.
                       20  filler occurs 80
                                          pic  x(01)                  .

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
      *            * Chiave numero 05 : DCFCOPSFN                      *
      *            *---------------------------------------------------*
               10  aaf-k05.
                   15  aaf-cod-dcf-5      pic  9(07)       comp-3     .
                   15  aaf-cop-sfn        pic  x(14)                  .
                   15  aaf-tip-mag-5      pic  9(02)                  .
                   15  aaf-num-pro-5      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-5      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : COPSFN                         *
      *            *---------------------------------------------------*
               10  aaf-k06.
                   15  aaf-cop-sfn-6      pic  x(14)                  .
                   15  aaf-tip-mag-6      pic  9(02)                  .
                   15  aaf-cod-dcf-6      pic  9(07)       comp-3     .
                   15  aaf-num-pro-6      pic  9(07)       comp-3     .
                   15  aaf-fda-pif-6      pic  x(14)                  .
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
               10  aaf-lgv-pdt            pic  9(01)v9(02) comp-3     .
               10  aaf-tbl-pes.
                   15  aaf-ele-pes occurs 06.
                       20  aaf-qta-pes    pic  9(06)v9(03) comp-3     .
                       20  aaf-prz-pes    pic  9(09)       comp-3     .
                       20  aaf-csr-pes    pic  9(05)       comp-3     .
                       20  aaf-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  aaf-per-mpa            pic  9(02)v9(01)            .
               10  aaf-alx-exp.
                   15  filler  occurs 17  pic  x(01)                  .

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
                   15  aaq-cdp-pdt        pic  x(40)                  .
                   15  aaq-tip-mag-3      pic  9(02)                  .
                   15  aaq-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PDTCDP                         *
      *            *---------------------------------------------------*
               10  aaq-k04.
                   15  aaq-cod-pdt        pic  9(07)       comp-3     .
                   15  aaq-cdp-pdt-4      pic  x(40)                  .
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
      *    * File Description [fbs]                                    *
      *    *-----------------------------------------------------------*
       fd  fbs       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fbs-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fbs-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : MAGDPZ                         *
      *            *---------------------------------------------------*
               10  fbs-k01.
                   15  fbs-tip-mag        pic  9(02)                  .
                   15  fbs-num-mag        pic  9(07)       comp-3     .
                   15  fbs-var-mag        pic  x(14)                  .
                   15  fbs-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DPZMAG                         *
      *            *---------------------------------------------------*
               10  fbs-k02.
                   15  fbs-cod-dpz-2      pic  9(02)                  .
                   15  fbs-tip-mag-2      pic  9(02)                  .
                   15  fbs-num-mag-2      pic  9(07)       comp-3     .
                   15  fbs-var-mag-2      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fbs-dat.
               10  fbs-tip-stg            pic  x(03)                  .
               10  fbs-sco-stg occurs 4.
                   15  fbs-sco-min        pic  9(06)v9(03) comp-3     .
                   15  fbs-dua-min        pic  9(07)       comp-3     .
                   15  fbs-sco-sic        pic  9(06)v9(03) comp-3     .
                   15  fbs-dua-sic        pic  9(07)       comp-3     .
                   15  fbs-sco-max        pic  9(06)v9(03) comp-3     .
                   15  fbs-dua-max        pic  9(07)       comp-3     .
               10  fbs-sco-not            pic  x(20)                  .
               10  fbs-exp-alf.
                   15  filler occurs 20   pic  x(01)                  .
               10  fbs-exp-num.
                   15  filler occurs 20   pic  9(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [bfr]                                       *
      *    *-----------------------------------------------------------*
       01  f-bfr.
           05  f-bfr-nam                  pic  x(04)                  .
           05  f-bfr-pat                  pic  x(40)                  .
           05  f-bfr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [bir]                                       *
      *    *-----------------------------------------------------------*
       01  f-bir.
           05  f-bir-nam                  pic  x(04)                  .
           05  f-bir-pat                  pic  x(40)                  .
           05  f-bir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [cdp]                                       *
      *    *-----------------------------------------------------------*
       01  f-cdp.
           05  f-cdp-nam                  pic  x(04)                  .
           05  f-cdp-pat                  pic  x(40)                  .
           05  f-cdp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fir]                                       *
      *    *-----------------------------------------------------------*
       01  f-fir.
           05  f-fir-nam                  pic  x(04)                  .
           05  f-fir-pat                  pic  x(40)                  .
           05  f-fir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ffr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ffr.
           05  f-ffr-nam                  pic  x(04)                  .
           05  f-ffr-pat                  pic  x(40)                  .
           05  f-ffr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [osr]                                       *
      *    *-----------------------------------------------------------*
       01  f-osr.
           05  f-osr-nam                  pic  x(04)                  .
           05  f-osr-pat                  pic  x(40)                  .
           05  f-osr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ocp]                                       *
      *    *-----------------------------------------------------------*
       01  f-ocp.
           05  f-ocp-nam                  pic  x(04)                  .
           05  f-ocp-pat                  pic  x(40)                  .
           05  f-ocp-sts                  pic  x(02)                  .

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
      *    * File area per [mgr]                                       *
      *    *-----------------------------------------------------------*
       01  f-mgr.
           05  f-mgr-nam                  pic  x(04)                  .
           05  f-mgr-pat                  pic  x(40)                  .
           05  f-mgr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mgs]                                       *
      *    *-----------------------------------------------------------*
       01  f-mgs.
           05  f-mgs-nam                  pic  x(04)                  .
           05  f-mgs-pat                  pic  x(40)                  .
           05  f-mgs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mgt]                                       *
      *    *-----------------------------------------------------------*
       01  f-mgt.
           05  f-mgt-nam                  pic  x(04)                  .
           05  f-mgt-pat                  pic  x(40)                  .
           05  f-mgt-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [moi]                                       *
      *    *-----------------------------------------------------------*
       01  f-moi.
           05  f-moi-nam                  pic  x(04)                  .
           05  f-moi-pat                  pic  x(40)                  .
           05  f-moi-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ivp]                                       *
      *    *-----------------------------------------------------------*
       01  f-ivp.
           05  f-ivp-nam                  pic  x(04)                  .
           05  f-ivp-pat                  pic  x(40)                  .
           05  f-ivp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zoc]                                       *
      *    *-----------------------------------------------------------*
       01  f-zoc.
           05  f-zoc-nam                  pic  x(04)                  .
           05  f-zoc-pat                  pic  x(40)                  .
           05  f-zoc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zsc]                                       *
      *    *-----------------------------------------------------------*
       01  f-zsc.
           05  f-zsc-nam                  pic  x(04)                  .
           05  f-zsc-pat                  pic  x(40)                  .
           05  f-zsc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zbi]                                       *
      *    *-----------------------------------------------------------*
       01  f-zbi.
           05  f-zbi-nam                  pic  x(04)                  .
           05  f-zbi-pat                  pic  x(40)                  .
           05  f-zbi-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zfi]                                       *
      *    *-----------------------------------------------------------*
       01  f-zfi.
           05  f-zfi-nam                  pic  x(04)                  .
           05  f-zfi-pat                  pic  x(40)                  .
           05  f-zfi-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [yof]                                       *
      *    *-----------------------------------------------------------*
       01  f-yof.
           05  f-yof-nam                  pic  x(04)                  .
           05  f-yof-pat                  pic  x(40)                  .
           05  f-yof-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [ybf]                                       *
      *    *-----------------------------------------------------------*
       01  f-ybf.
           05  f-ybf-nam                  pic  x(04)                  .
           05  f-ybf-pat                  pic  x(40)                  .
           05  f-ybf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [yff]                                       *
      *    *-----------------------------------------------------------*
       01  f-yff.
           05  f-yff-nam                  pic  x(04)                  .
           05  f-yff-pat                  pic  x(40)                  .
           05  f-yff-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [zos]                                       *
      *    *-----------------------------------------------------------*
       01  f-zos.
           05  f-zos-nam                  pic  x(04)                  .
           05  f-zos-pat                  pic  x(40)                  .
           05  f-zos-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [dcp]                                       *
      *    *-----------------------------------------------------------*
       01  f-dcp.
           05  f-dcp-nam                  pic  x(04)                  .
           05  f-dcp-pat                  pic  x(40)                  .
           05  f-dcp-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [aaf]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaf.
           05  f-aaf-nam                  pic  x(04)                  .
           05  f-aaf-pat                  pic  x(40)                  .
           05  f-aaf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [aaq]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaq.
           05  f-aaq-nam                  pic  x(04)                  .
           05  f-aaq-pat                  pic  x(40)                  .
           05  f-aaq-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fbs]                                       *
      *    *-----------------------------------------------------------*
       01  f-fbs.
           05  f-fbs-nam                  pic  x(04)                  .
           05  f-fbs-pat                  pic  x(40)                  .
           05  f-fbs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [cdp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfcdp"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [ffr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [ocp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocp"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgi"                          .
      *        *-------------------------------------------------------*
      *        * [ivp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfivp"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [zsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfzsc"                          .
      *        *-------------------------------------------------------*
      *        * [zbi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .
      *        *-------------------------------------------------------*
      *        * [yof]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyof"                          .
      *        *-------------------------------------------------------*
      *        * [ybf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .
      *        *-------------------------------------------------------*
      *        * [yff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfyff"                          .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [fbs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rffbs"                          .

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
                     "cnv4p6"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnv4p5b "                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONE PER RELEASE 4.6       "       .

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
               10  w-det-rec-fil-s02      pic s9(03)                  .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dcp '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/wzosdcp0.wkl"                   .

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
      *              * Test se visualizzazione necessaria              *
      *              *-------------------------------------------------*
           if        w-det-rec-fil-s02    =    zero
                     move  w-det-rec-fil-v02
                                          to   w-det-rec-fil-s02
                     go to inc-rec-let-700.
           if        w-det-rec-fil-s02    =    w-det-rec-fil-v02
                     go to inc-rec-let-999.
           move      w-det-rec-fil-v02    to   w-det-rec-fil-s02      .
       inc-rec-let-700.
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
      *              * Normalizzazione comodi                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rec-fil-s02      .
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
      *              * Conversione [bfr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bfr-000      thru exe-cnv-bfr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [bir]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-bir-000      thru exe-cnv-bir-999        .
      *              *-------------------------------------------------*
      *              * Conversione [cdp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-cdp-000      thru exe-cnv-cdp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [fir]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-fir-000      thru exe-cnv-fir-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ffr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ffr-000      thru exe-cnv-ffr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [osr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-osr-000      thru exe-cnv-osr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ocp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ocp-000      thru exe-cnv-ocp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ocr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ocr-000      thru exe-cnv-ocr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ofr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ofr-000      thru exe-cnv-ofr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [mgr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-mgr-000      thru exe-cnv-mgr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [mgs]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-mgs-000      thru exe-cnv-mgs-999        .
      *              *-------------------------------------------------*
      *              * Conversione [mgt]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-mgt-000      thru exe-cnv-mgt-999        .
      *              *-------------------------------------------------*
      *              * Conversione [moi] (mgi)                         *
      *              *-------------------------------------------------*
           perform   exe-cnv-moi-000      thru exe-cnv-moi-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ivp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ivp-000      thru exe-cnv-ivp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [zoc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-zoc-000      thru exe-cnv-zoc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [zsc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-zsc-000      thru exe-cnv-zsc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [zbi]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-zbi-000      thru exe-cnv-zbi-999        .
      *              *-------------------------------------------------*
      *              * Conversione [zfi]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-zfi-000      thru exe-cnv-zfi-999        .
      *              *-------------------------------------------------*
      *              * Conversione [yof]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-yof-000      thru exe-cnv-yof-999        .
      *              *-------------------------------------------------*
      *              * Conversione [ybf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ybf-000      thru exe-cnv-ybf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [yff]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-yff-000      thru exe-cnv-yff-999        .
      *              *-------------------------------------------------*
      *              * Conversione [zos]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-zos-000      thru exe-cnv-zos-999        .
      *              *-------------------------------------------------*
      *              * Conversione [dcp]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-dcp-000      thru exe-cnv-dcp-999        .
      *              *-------------------------------------------------*
      *              * Conversione [aaf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaf-000      thru exe-cnv-aaf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [aaq]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaq-000      thru exe-cnv-aaq-999        .
      *              *-------------------------------------------------*
      *              * Conversione [fbs]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-fbs-000      thru exe-cnv-fbs-999        .
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
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
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
           move      spaces               to   rf-bfr                 .
           move      spaces               to   rf-bfr                 .
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
           move      bfr-cod-lng          to   rf-bfr-cod-lng         .
           move      bfr-sgl-vpf          to   rf-bfr-sgl-vpf         .
           move      bfr-dec-vpf          to   rf-bfr-dec-vpf         .
           move      bfr-tdc-vpf          to   rf-bfr-tdc-vpf         .
           move      bfr-cdc-vpf          to   rf-bfr-cdc-vpf         .
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
           move      bfr-sgl-vpp          to   rf-bfr-sgl-vpp         .
           move      bfr-dec-vpp          to   rf-bfr-dec-vpp         .
           move      bfr-tdc-vpp          to   rf-bfr-tdc-vpp         .
           move      bfr-cdc-vpp          to   rf-bfr-cdc-vpp         .
           move      bfr-prz-acq          to   rf-bfr-prz-acq         .
           move      bfr-snx-2pz          to   rf-bfr-snx-2pz         .
           move      bfr-dec-2pz          to   rf-bfr-dec-2pz         .
           move      bfr-prz-a02          to   rf-bfr-prz-a02         .
           move      bfr-sgl-vpl          to   rf-bfr-sgl-vpl         .
           move      bfr-dec-vpl          to   rf-bfr-dec-vpl         .
           move      bfr-tdc-vpl          to   rf-bfr-tdc-vpl         .
           move      bfr-prz-vpl          to   rf-bfr-prz-vpl         .
           move      bfr-cdc-vpl          to   rf-bfr-cdc-vpl         .
           move      bfr-ccr-vpl          to   rf-bfr-ccr-vpl         .
           move      bfr-plm-vpl          to   rf-bfr-plm-vpl         .
           move      bfr-tlm-vpl          to   rf-bfr-tlm-vpl         .
           move      bfr-map-vpl          to   rf-bfr-map-vpl         .
           move      bfr-epz-rgf          to   rf-bfr-epz-rgf         .
           move      bfr-csr-aap          to   rf-bfr-csr-aap         .
           move      bfr-psr-aap (1)      to   rf-bfr-psr-aap (1)     .
           move      bfr-psr-aap (2)      to   rf-bfr-psr-aap (2)     .
           move      bfr-psr-aap (3)      to   rf-bfr-psr-aap (3)     .
           move      bfr-psr-aap (4)      to   rf-bfr-psr-aap (4)     .
           move      bfr-psr-aap (5)      to   rf-bfr-psr-aap (5)     .
           move      bfr-per-scr (1)      to   rf-bfr-per-scr (1)     .
           move      bfr-per-scr (2)      to   rf-bfr-per-scr (2)     .
           move      bfr-per-scr (3)      to   rf-bfr-per-scr (3)     .
           move      bfr-per-scr (4)      to   rf-bfr-per-scr (4)     .
           move      bfr-per-scr (5)      to   rf-bfr-per-scr (5)     .
           move      bfr-prz-net          to   rf-bfr-prz-net         .
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
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
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
           move      spaces               to   rf-bir                 .
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
           move      bir-cop-scl          to   rf-bir-cop-scl         .
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
           move      bir-prz-vpl          to   rf-bir-prz-vpl         .
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
           move      bir-ods-prt          to   rf-bir-ods-prt         .
           move      bir-ods-prg          to   rf-bir-ods-prg         .
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
           move      bir-flg-puq          to   rf-bir-flg-puq         .
           move      bir-alx-exp          to   rf-bir-alx-exp         .
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
      *    * Conversione [cdp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-cdp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "cdp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cdp/fls/ioc/obj/iofcdp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-cdp-999.
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
       exe-cnv-cdp-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-cdp-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-cdp-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-cdp]                         *
      *                  *---------------------------------------------*
           open      i-o    cdp                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-cdp]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
       exe-cnv-cdp-200.
      *              *-------------------------------------------------*
      *              * Start su [old-cdp]                              *
      *              *-------------------------------------------------*
           move      low-values           to   cdp-k01                .
           start     cdp    key not less
                            cdp-k01
                            invalid key
                            go to exe-cnv-cdp-800.
       exe-cnv-cdp-250.
      *              *-------------------------------------------------*
      *              * Next su [old-cdp]                               *
      *              *-------------------------------------------------*
           read      cdp    next
                            with no lock
                            at end
                            go to exe-cnv-cdp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-cdp-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-cdp]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
       exe-cnv-cdp-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-cdp]                          *
      *              *-------------------------------------------------*
       exe-cnv-cdp-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-cdp                 .
           move      cdp-ide-dat          to   rf-cdp-ide-dat         .
           move      cdp-ide-ute          to   rf-cdp-ide-ute         .
           move      cdp-ide-fas          to   rf-cdp-ide-fas         .
           move      cdp-num-prt          to   rf-cdp-num-prt         .
           move      cdp-tmo-cdp          to   rf-cdp-tmo-cdp         .
           move      cdp-cod-dpz          to   rf-cdp-cod-dpz         .
           move      cdp-dat-doc          to   rf-cdp-dat-doc         .
           move      cdp-num-doc          to   rf-cdp-num-doc         .
           move      cdp-scl-ann          to   rf-cdp-scl-ann         .
           move      cdp-sgl-num          to   rf-cdp-sgl-num         .
           move      cdp-dsz-prd          to   rf-cdp-dsz-prd         .
           move      cdp-dsz-cod          to   rf-cdp-dsz-cod         .
           move      cdp-dsz-dpz          to   rf-cdp-dsz-dpz         .
           move      cdp-esz-prd          to   rf-cdp-esz-prd         .
           move      cdp-esz-cod          to   rf-cdp-esz-cod         .
           move      cdp-esz-dpz          to   rf-cdp-esz-dpz         .
           move      cdp-dat-cns          to   rf-cdp-dat-cns         .
           move      cdp-pri-prd          to   rf-cdp-pri-prd         .
           move      cdp-com-int          to   rf-cdp-com-int         .
           move      cdp-voc-des (1)      to   rf-cdp-voc-des (1)     .
           move      cdp-voc-des (2)      to   rf-cdp-voc-des (2)     .
           move      cdp-voc-des (3)      to   rf-cdp-voc-des (3)     .
           move      cdp-voc-des (4)      to   rf-cdp-voc-des (4)     .
           move      cdp-voc-des (5)      to   rf-cdp-voc-des (5)     .
           move      cdp-voc-des (6)      to   rf-cdp-voc-des (6)     .
           move      cdp-tip-mag          to   rf-cdp-tip-mag         .
           move      cdp-num-mag          to   rf-cdp-num-mag         .
           move      cdp-alf-mag          to   rf-cdp-alf-mag         .
           move      cdp-sgl-vrn          to   rf-cdp-sgl-vrn         .
           move      cdp-umi-prd          to   rf-cdp-umi-prd         .
           move      cdp-dec-qta          to   rf-cdp-dec-qta         .
           move      cdp-qta-dap          to   rf-cdp-qta-dap         .
           move      cdp-cod-dsl          to   rf-cdp-cod-dsl         .
           move      cdp-snx-2qt          to   rf-cdp-snx-2qt         .
           move      cdp-dec-2qt          to   rf-cdp-dec-2qt         .
           move      cdp-qta-a02          to   rf-cdp-qta-a02         .
           move      cdp-snx-3qt          to   rf-cdp-snx-3qt         .
           move      cdp-dec-3qt          to   rf-cdp-dec-3qt         .
           move      cdp-qta-a03          to   rf-cdp-qta-a03         .
           move      cdp-ctr-stp          to   rf-cdp-ctr-stp         .
           move      cdp-sdc-ccs          to   rf-cdp-sdc-ccs         .
           move      cdp-flg-cch          to   rf-cdp-flg-cch         .
           move      cdp-flg-ela          to   rf-cdp-flg-ela         .
           move      cdp-flg-pul          to   rf-cdp-flg-pul         .
           move      cdp-alx-exp          to   rf-cdp-alx-exp         .
       exe-cnv-cdp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-cdp]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-cdp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-cdp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-cdp-250.
       exe-cnv-cdp-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-cdp]                        *
      *                  *---------------------------------------------*
           close     cdp                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-cdp]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cdp                 .
       exe-cnv-cdp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-cdp-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-cdp] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-cdp-999.
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
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
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
           move      spaces               to   rf-fir                 .
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
           move      fir-cop-scl          to   rf-fir-cop-scl         .
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
           move      fir-prz-vpl          to   rf-fir-prz-vpl         .
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
           move      fir-flg-puq          to   rf-fir-flg-puq         .
           move      fir-alx-exp          to   rf-fir-alx-exp         .
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
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
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
           move      spaces               to   rf-ffr                 .
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
           move      ffr-cod-lng          to   rf-ffr-cod-lng         .
           move      ffr-sgl-vpf          to   rf-ffr-sgl-vpf         .
           move      ffr-dec-vpf          to   rf-ffr-dec-vpf         .
           move      ffr-tdc-vpf          to   rf-ffr-tdc-vpf         .
           move      ffr-cdc-vpf          to   rf-ffr-cdc-vpf         .
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
           move      ffr-sgl-vpp          to   rf-ffr-sgl-vpp         .
           move      ffr-dec-vpp          to   rf-ffr-dec-vpp         .
           move      ffr-tdc-vpp          to   rf-ffr-tdc-vpp         .
           move      ffr-cdc-vpp          to   rf-ffr-cdc-vpp         .
           move      ffr-prz-acq          to   rf-ffr-prz-acq         .
           move      ffr-snx-2pz          to   rf-ffr-snx-2pz         .
           move      ffr-dec-2pz          to   rf-ffr-dec-2pz         .
           move      ffr-prz-a02          to   rf-ffr-prz-a02         .
           move      ffr-sgl-vpl          to   rf-ffr-sgl-vpl         .
           move      ffr-dec-vpl          to   rf-ffr-dec-vpl         .
           move      ffr-tdc-vpl          to   rf-ffr-tdc-vpl         .
           move      ffr-prz-vpl          to   rf-ffr-prz-vpl         .
           move      ffr-cdc-vpl          to   rf-ffr-cdc-vpl         .
           move      ffr-ccr-vpl          to   rf-ffr-ccr-vpl         .
           move      ffr-plm-vpl          to   rf-ffr-plm-vpl         .
           move      ffr-tlm-vpl          to   rf-ffr-tlm-vpl         .
           move      ffr-map-vpl          to   rf-ffr-map-vpl         .
           move      ffr-epz-rgf          to   rf-ffr-epz-rgf         .
           move      ffr-csr-aap          to   rf-ffr-csr-aap         .
           move      ffr-psr-aap (1)      to   rf-ffr-psr-aap (1)     .
           move      ffr-psr-aap (2)      to   rf-ffr-psr-aap (2)     .
           move      ffr-psr-aap (3)      to   rf-ffr-psr-aap (3)     .
           move      ffr-psr-aap (4)      to   rf-ffr-psr-aap (4)     .
           move      ffr-psr-aap (5)      to   rf-ffr-psr-aap (5)     .
           move      ffr-per-scr (1)      to   rf-ffr-per-scr (1)     .
           move      ffr-per-scr (2)      to   rf-ffr-per-scr (2)     .
           move      ffr-per-scr (3)      to   rf-ffr-per-scr (3)     .
           move      ffr-per-scr (4)      to   rf-ffr-per-scr (4)     .
           move      ffr-per-scr (5)      to   rf-ffr-per-scr (5)     .
           move      ffr-prz-net          to   rf-ffr-prz-net         .
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
      *    * Conversione [osr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-osr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "osr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-osr-999.
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
       exe-cnv-osr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-osr-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-osr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-osr]                         *
      *                  *---------------------------------------------*
           open      i-o    osr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-osr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
       exe-cnv-osr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-osr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   osr-k01                .
           start     osr    key not less
                            osr-k01
                            invalid key
                            go to exe-cnv-osr-800.
       exe-cnv-osr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-osr]                               *
      *              *-------------------------------------------------*
           read      osr    next
                            with no lock
                            at end
                            go to exe-cnv-osr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-osr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-osr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
       exe-cnv-osr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-osr]                          *
      *              *-------------------------------------------------*
       exe-cnv-osr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-osr                 .
           move      osr-num-prt          to   rf-osr-num-prt         .
           move      osr-num-prg          to   rf-osr-num-prg         .
           move      osr-cod-tms          to   rf-osr-cod-tms         .
           move      osr-cod-dpz          to   rf-osr-cod-dpz         .
           move      osr-dat-doc          to   rf-osr-dat-doc         .
           move      osr-tip-arc          to   rf-osr-tip-arc         .
           move      osr-cod-arc          to   rf-osr-cod-arc         .
           move      osr-dpz-arc          to   rf-osr-dpz-arc         .
           move      osr-cod-lng          to   rf-osr-cod-lng         .
           move      osr-sgl-vpf          to   rf-osr-sgl-vpf         .
           move      osr-dec-vpf          to   rf-osr-dec-vpf         .
           move      osr-tdc-vpf          to   rf-osr-tdc-vpf         .
           move      osr-cdc-vpf          to   rf-osr-cdc-vpf         .
           move      osr-bld-flb          to   rf-osr-bld-flb         .
           move      osr-bld-tpb          to   rf-osr-bld-tpb         .
           move      osr-bld-rgb          to   rf-osr-bld-rgb         .
           move      osr-tip-rig          to   rf-osr-tip-rig         .
           move      osr-tip-mag          to   rf-osr-tip-mag         .
           move      osr-num-pro          to   rf-osr-num-pro         .
           move      osr-alf-pro          to   rf-osr-alf-pro         .
           move      osr-sgl-vrn          to   rf-osr-sgl-vrn         .
           move      osr-cop-scl          to   rf-osr-cop-scl         .
           move      osr-des-ext          to   rf-osr-des-ext         .
           move      osr-des-rig          to   rf-osr-des-rig         .
           move      osr-tip-pro          to   rf-osr-tip-pro         .
           move      osr-cod-iva          to   rf-osr-cod-iva         .
           move      osr-ctp-ven          to   rf-osr-ctp-ven         .
           move      osr-umi-ven          to   rf-osr-umi-ven         .
           move      osr-dec-qta          to   rf-osr-dec-qta         .
           move      osr-qta-ven          to   rf-osr-qta-ven         .
           move      osr-snx-2qt          to   rf-osr-snx-2qt         .
           move      osr-dec-2qt          to   rf-osr-dec-2qt         .
           move      osr-qta-a02          to   rf-osr-qta-a02         .
           move      osr-snx-3qt          to   rf-osr-snx-3qt         .
           move      osr-dec-3qt          to   rf-osr-dec-3qt         .
           move      osr-qta-a03          to   rf-osr-qta-a03         .
           move      osr-dec-prz          to   rf-osr-dec-prz         .
           move      osr-sgl-vps          to   rf-osr-sgl-vps         .
           move      osr-dec-vps          to   rf-osr-dec-vps         .
           move      osr-tdc-vps          to   rf-osr-tdc-vps         .
           move      osr-cdc-vps          to   rf-osr-cdc-vps         .
           move      osr-prz-lrs          to   rf-osr-prz-lrs         .
           move      osr-prz-nts          to   rf-osr-prz-nts         .
           move      osr-sgl-vpp          to   rf-osr-sgl-vpp         .
           move      osr-dec-vpp          to   rf-osr-dec-vpp         .
           move      osr-tdc-vpp          to   rf-osr-tdc-vpp         .
           move      osr-cdc-vpp          to   rf-osr-cdc-vpp         .
           move      osr-prz-ven          to   rf-osr-prz-ven         .
           move      osr-snx-2pz          to   rf-osr-snx-2pz         .
           move      osr-prz-a02          to   rf-osr-prz-a02         .
           move      osr-sgl-vpl          to   rf-osr-sgl-vpl         .
           move      osr-dec-vpl          to   rf-osr-dec-vpl         .
           move      osr-tdc-vpl          to   rf-osr-tdc-vpl         .
           move      osr-prz-vpl          to   rf-osr-prz-vpl         .
           move      osr-cdc-vpl          to   rf-osr-cdc-vpl         .
           move      osr-ccr-vpl          to   rf-osr-ccr-vpl         .
           move      osr-plm-vpl          to   rf-osr-plm-vpl         .
           move      osr-tlm-vpl          to   rf-osr-tlm-vpl         .
           move      osr-map-vpl          to   rf-osr-map-vpl         .
           move      osr-epz-rgf          to   rf-osr-epz-rgf         .
           move      osr-csr-aap          to   rf-osr-csr-aap         .
           move      osr-psr-aap (1)      to   rf-osr-psr-aap (1)     .
           move      osr-psr-aap (2)      to   rf-osr-psr-aap (2)     .
           move      osr-psr-aap (3)      to   rf-osr-psr-aap (3)     .
           move      osr-psr-aap (4)      to   rf-osr-psr-aap (4)     .
           move      osr-psr-aap (5)      to   rf-osr-psr-aap (5)     .
           move      osr-per-scr (1)      to   rf-osr-per-scr (1)     .
           move      osr-per-scr (2)      to   rf-osr-per-scr (2)     .
           move      osr-per-scr (3)      to   rf-osr-per-scr (3)     .
           move      osr-per-scr (4)      to   rf-osr-per-scr (4)     .
           move      osr-per-scr (5)      to   rf-osr-per-scr (5)     .
           move      osr-prz-net          to   rf-osr-prz-net         .
           move      osr-epz-pes          to   rf-osr-epz-pes         .
           move      osr-sgl-vpc          to   rf-osr-sgl-vpc         .
           move      osr-dec-vpc          to   rf-osr-dec-vpc         .
           move      osr-tdc-vpc          to   rf-osr-tdc-vpc         .
           move      osr-cdc-vpc          to   rf-osr-cdc-vpc         .
           move      osr-dec-cos          to   rf-osr-dec-cos         .
           move      osr-cos-rif          to   rf-osr-cos-rif         .
           move      osr-imp-rig          to   rf-osr-imp-rig         .
           move      osr-iau-rig          to   rf-osr-iau-rig         .
           move      osr-cpv-aap          to   rf-osr-cpv-aap         .
           move      osr-ppv-aap (1)      to   rf-osr-ppv-aap (1)     .
           move      osr-ppv-aap (2)      to   rf-osr-ppv-aap (2)     .
           move      osr-ppv-aap (3)      to   rf-osr-ppv-aap (3)     .
           move      osr-fsp-rig          to   rf-osr-fsp-rig         .
           move      osr-cpv-rig          to   rf-osr-cpv-rig         .
           move      osr-ppv-rig (1)      to   rf-osr-ppv-rig (1)     .
           move      osr-ppv-rig (2)      to   rf-osr-ppv-rig (2)     .
           move      osr-ppv-rig (3)      to   rf-osr-ppv-rig (3)     .
           move      osr-pvf-rig          to   rf-osr-pvf-rig         .
           move      osr-ocl-dat          to   rf-osr-ocl-dat         .
           move      osr-ocl-num          to   rf-osr-ocl-num         .
           move      osr-cmc-tip          to   rf-osr-cmc-tip         .
           move      osr-cmc-dat          to   rf-osr-cmc-dat         .
           move      osr-cmc-num          to   rf-osr-cmc-num         .
           move      osr-coc-tip          to   rf-osr-coc-tip         .
           move      osr-coc-dat          to   rf-osr-coc-dat         .
           move      osr-coc-num          to   rf-osr-coc-num         .
           move      osr-coc-prt          to   rf-osr-coc-prt         .
           move      osr-coc-prg          to   rf-osr-coc-prg         .
           move      osr-coc-fzs          to   rf-osr-coc-fzs         .
           move      osr-flg-rch          to   rf-osr-flg-rch         .
           move      osr-flg-blx (1)      to   rf-osr-flg-blx (1)     .
           move      osr-flg-blx (2)      to   rf-osr-flg-blx (2)     .
           move      osr-flg-blx (3)      to   rf-osr-flg-blx (3)     .
           move      osr-flg-blx (4)      to   rf-osr-flg-blx (4)     .
           move      osr-flg-blx (5)      to   rf-osr-flg-blx (5)     .
           move      osr-flg-blx (6)      to   rf-osr-flg-blx (6)     .
           move      osr-flg-blx (7)      to   rf-osr-flg-blx (7)     .
           move      osr-flg-nbx (1)      to   rf-osr-flg-nbx (1)     .
           move      osr-flg-nbx (2)      to   rf-osr-flg-nbx (2)     .
           move      osr-flg-nbx (3)      to   rf-osr-flg-nbx (3)     .
           move      osr-flg-pul          to   rf-osr-flg-pul         .
           move      osr-flg-puq          to   rf-osr-flg-puq         .
           move      osr-alx-exp          to   rf-osr-alx-exp         .
       exe-cnv-osr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-osr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-osr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-osr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-osr-250.
       exe-cnv-osr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-osr]                        *
      *                  *---------------------------------------------*
           close     osr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-osr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
       exe-cnv-osr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-osr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-osr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-osr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ocp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ocp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ocp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orc/fls/ioc/obj/iofocp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ocp-999.
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
       exe-cnv-ocp-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-ocp-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-ocp-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-ocp]                         *
      *                  *---------------------------------------------*
           open      i-o    ocp                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-ocp]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
       exe-cnv-ocp-200.
      *              *-------------------------------------------------*
      *              * Start su [old-ocp]                              *
      *              *-------------------------------------------------*
           move      low-values           to   ocp-k01                .
           start     ocp    key not less
                            ocp-k01
                            invalid key
                            go to exe-cnv-ocp-800.
       exe-cnv-ocp-250.
      *              *-------------------------------------------------*
      *              * Next su [old-ocp]                               *
      *              *-------------------------------------------------*
           read      ocp    next
                            with no lock
                            at end
                            go to exe-cnv-ocp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ocp-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-ocp]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
       exe-cnv-ocp-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-ocp]                          *
      *              *-------------------------------------------------*
       exe-cnv-ocp-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ocp                 .
           move      ocp-num-prt          to   rf-ocp-num-prt         .
           move      ocp-num-prg          to   rf-ocp-num-prg         .
           move      ocp-prg-frm          to   rf-ocp-prg-frm         .
           move      ocp-qta-ass          to   rf-ocp-qta-ass         .
           move      ocp-dat-ass          to   rf-ocp-dat-ass         .
           move      ocp-prt-orf          to   rf-ocp-prt-orf         .
           move      ocp-prg-orf          to   rf-ocp-prg-orf         .
           move      ocp-flg-ela          to   rf-ocp-flg-ela         .
           move      ocp-flg-pul          to   rf-ocp-flg-pul         .
           move      ocp-alx-exp          to   rf-ocp-alx-exp         .
       exe-cnv-ocp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-ocp]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ocp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ocp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ocp-250.
       exe-cnv-ocp-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-ocp]                        *
      *                  *---------------------------------------------*
           close     ocp                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-ocp]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocp                 .
       exe-cnv-ocp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ocp-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-ocp] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-ocp-999.
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
           move      ocr-dec-2pz          to   rf-ocr-dec-2pz         .
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
           move      ocr-dcn-cnf          to   rf-ocr-dcn-cnf         .
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
           move      ofr-dcn-cnf          to   rf-ofr-dcn-cnf         .
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
      *    * Conversione [mgr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-mgr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "mgr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-mgr-999.
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
       exe-cnv-mgr-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-mgr-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-mgr-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-mgr]                         *
      *                  *---------------------------------------------*
           open      i-o    mgr                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-mgr]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       exe-cnv-mgr-200.
      *              *-------------------------------------------------*
      *              * Start su [old-mgr]                              *
      *              *-------------------------------------------------*
           move      low-values           to   mgr-k01                .
           start     mgr    key not less
                            mgr-k01
                            invalid key
                            go to exe-cnv-mgr-800.
       exe-cnv-mgr-250.
      *              *-------------------------------------------------*
      *              * Next su [old-mgr]                               *
      *              *-------------------------------------------------*
           read      mgr    next
                            with no lock
                            at end
                            go to exe-cnv-mgr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-mgr-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-mgr]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       exe-cnv-mgr-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-mgr]                          *
      *              *-------------------------------------------------*
       exe-cnv-mgr-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-mgr                 .
           move      mgr-dat-reg          to   rf-mgr-dat-reg         .
           move      mgr-num-prt          to   rf-mgr-num-prt         .
           move      mgr-num-prg          to   rf-mgr-num-prg         .
           move      mgr-cod-pdc          to   rf-mgr-cod-pdc         .
           move      mgr-tip-arc          to   rf-mgr-tip-arc         .
           move      mgr-cod-arc          to   rf-mgr-cod-arc         .
           move      mgr-cod-cau          to   rf-mgr-cod-cau         .
           move      mgr-snx-mob          to   rf-mgr-snx-mob         .
           move      mgr-tip-iva          to   rf-mgr-tip-iva         .
           move      mgr-com-rig          to   rf-mgr-com-rig         .
           move      mgr-dat-doc          to   rf-mgr-dat-doc         .
           move      mgr-num-doc          to   rf-mgr-num-doc         .
           move      mgr-dat-rif          to   rf-mgr-dat-rif         .
           move      mgr-num-rif          to   rf-mgr-num-rif         .
           move      mgr-dar-ave          to   rf-mgr-dar-ave         .
           move      mgr-imp-mov          to   rf-mgr-imp-mov         .
           move      mgr-flg-pge          to   rf-mgr-flg-pge         .
           move      mgr-flg-pcf          to   rf-mgr-flg-pcf         .
           move      spaces               to   rf-mgr-alx-exp         .
       exe-cnv-mgr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-mgr]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-mgr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-mgr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-mgr-250.
       exe-cnv-mgr-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-mgr]                        *
      *                  *---------------------------------------------*
           close     mgr                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-mgr]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       exe-cnv-mgr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-mgr-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-mgr] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-mgr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [mgs]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-mgs-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "mgs "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-mgs-999.
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
       exe-cnv-mgs-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-mgs-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-mgs-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-mgs]                         *
      *                  *---------------------------------------------*
           open      i-o    mgs                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-mgs]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       exe-cnv-mgs-200.
      *              *-------------------------------------------------*
      *              * Start su [old-mgs]                              *
      *              *-------------------------------------------------*
           move      low-values           to   mgs-k01                .
           start     mgs    key not less
                            mgs-k01
                            invalid key
                            go to exe-cnv-mgs-800.
       exe-cnv-mgs-250.
      *              *-------------------------------------------------*
      *              * Next su [old-mgs]                               *
      *              *-------------------------------------------------*
           read      mgs    next
                            with no lock
                            at end
                            go to exe-cnv-mgs-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-mgs-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-mgs]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       exe-cnv-mgs-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-mgs]                          *
      *              *-------------------------------------------------*
       exe-cnv-mgs-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-mgs                 .
           move      mgs-ann-ese          to   rf-mgs-ann-ese         .
           move      mgs-tip-rec          to   rf-mgs-tip-rec         .
           move      mgs-cod-con          to   rf-mgs-cod-con         .
           move      mgs-sdo-ini          to   rf-mgs-sdo-ini         .
           move      zero                 to   w-c01                  .
       exe-cnv-mgs-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    12
                     go to exe-cnv-mgs-420.
           move      mgs-dar-mes (w-c01)  to   rf-mgs-dar-mes (w-c01) .
           move      mgs-ave-mes (w-c01)  to   rf-mgs-ave-mes (w-c01) .
           move      mgs-dav-ret (w-c01)  to   rf-mgs-dav-ret (w-c01) .
           move      mgs-imp-ret (w-c01)  to   rf-mgs-imp-ret (w-c01) .
           go to     exe-cnv-mgs-410.
       exe-cnv-mgs-420.
           move      mgs-dar-bil          to   rf-mgs-dar-bil         .
           move      mgs-ave-bil          to   rf-mgs-ave-bil         .
           move      mgs-dat-chi          to   rf-mgs-dat-chi         .
           move      spaces               to   rf-mgs-alx-exp         .
       exe-cnv-mgs-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-mgs]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-mgs-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-mgs-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-mgs-250.
       exe-cnv-mgs-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-mgs]                        *
      *                  *---------------------------------------------*
           close     mgs                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-mgs]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       exe-cnv-mgs-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-mgs-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-mgs] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-mgs-999.
           exit.

      *    *===========================================================*
      *    * Conversione [mgt]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-mgt-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "mgt "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-mgt-999.
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
       exe-cnv-mgt-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-mgt-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-mgt-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-mgt]                         *
      *                  *---------------------------------------------*
           open      i-o    mgt                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-mgt]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       exe-cnv-mgt-200.
      *              *-------------------------------------------------*
      *              * Start su [old-mgt]                              *
      *              *-------------------------------------------------*
           move      low-values           to   mgt-k01                .
           start     mgt    key not less
                            mgt-k01
                            invalid key
                            go to exe-cnv-mgt-800.
       exe-cnv-mgt-250.
      *              *-------------------------------------------------*
      *              * Next su [old-mgt]                               *
      *              *-------------------------------------------------*
           read      mgt    next
                            with no lock
                            at end
                            go to exe-cnv-mgt-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-mgt-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-mgt]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       exe-cnv-mgt-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-mgt]                          *
      *              *-------------------------------------------------*
       exe-cnv-mgt-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-mgt                 .
           move      mgt-ide-dat          to   rf-mgt-ide-dat         .
           move      mgt-ide-ute          to   rf-mgt-ide-ute         .
           move      mgt-ide-fas          to   rf-mgt-ide-fas         .
           move      mgt-dat-reg          to   rf-mgt-dat-reg         .
           move      mgt-num-prt          to   rf-mgt-num-prt         .
           move      mgt-cod-cau          to   rf-mgt-cod-cau         .
           move      mgt-snx-mob          to   rf-mgt-snx-mob         .
           move      mgt-tip-iva          to   rf-mgt-tip-iva         .
           move      mgt-des-cau          to   rf-mgt-des-cau         .
           move      mgt-dat-doc          to   rf-mgt-dat-doc         .
           move      mgt-num-doc          to   rf-mgt-num-doc         .
           move      mgt-cod-num          to   rf-mgt-cod-num         .
           move      mgt-prt-iva          to   rf-mgt-prt-iva         .
           move      mgt-flg-gio          to   rf-mgt-flg-gio         .
           move      spaces               to   rf-mgt-alx-exp         .
       exe-cnv-mgt-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-mgt]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-mgt-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-mgt-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-mgt-250.
       exe-cnv-mgt-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-mgt]                        *
      *                  *---------------------------------------------*
           close     mgt                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-mgt]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       exe-cnv-mgt-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-mgt-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-mgt] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-mgt-999.
           exit.

      *    *===========================================================*
      *    * Conversione [moi]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-moi-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "moi "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofmoi"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-moi-999.
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
       exe-cnv-moi-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-moi-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-moi-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-moi]                         *
      *                  *---------------------------------------------*
           open      i-o    moi                                       .
      *                  *---------------------------------------------*
      *                  * Open file [mgi]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       exe-cnv-moi-200.
      *              *-------------------------------------------------*
      *              * Start su [old-moi]                              *
      *              *-------------------------------------------------*
           move      low-values           to   moi-k01                .
           start     moi    key not less
                            moi-k01
                            invalid key
                            go to exe-cnv-moi-800.
       exe-cnv-moi-250.
      *              *-------------------------------------------------*
      *              * Next su [old-moi]                               *
      *              *-------------------------------------------------*
           read      moi    next
                            with no lock
                            at end
                            go to exe-cnv-moi-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-moi-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [mgi]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       exe-cnv-moi-350.
      *              *-------------------------------------------------*
      *              * Composizione [mgi]                              *
      *              *-------------------------------------------------*
       exe-cnv-moi-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-mgi                 .
           move      moi-tip-rec          to   rf-mgi-tip-rec         .
           move      moi-dat-reg          to   rf-mgi-dat-reg         .
           move      moi-num-prt          to   rf-mgi-num-prt         .
           move      moi-dat-doc          to   rf-mgi-dat-doc         .
           move      moi-num-doc          to   rf-mgi-num-doc         .
           move      moi-cod-num          to   rf-mgi-cod-num         .
           move      moi-prt-iva          to   rf-mgi-prt-iva         .
           move      moi-cod-cau          to   rf-mgi-cod-cau         .
           move      moi-tip-iva          to   rf-mgi-tip-iva         .
           move      zero                 to   w-c01                  .
       exe-cnv-moi-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-moi-420.
           move      moi-ibl-iva (w-c01)  to   rf-mgi-ibl-iva (w-c01) .          
           move      moi-cod-iva (w-c01)  to   rf-mgi-cod-iva (w-c01) .          
           move      moi-imp-iva (w-c01)  to   rf-mgi-imp-iva (w-c01) .          
           go to     exe-cnv-moi-410.
       exe-cnv-moi-420.
           move      moi-tot-doc          to   rf-mgi-tot-doc         .
           move      moi-cod-arc          to   rf-mgi-cod-arc         .
           move      moi-flg-gio          to   rf-mgi-flg-gio         .
           move      spaces               to   rf-mgi-alx-exp         .
       exe-cnv-moi-700.
      *              *-------------------------------------------------*
      *              * Scrittura [mgi]                                 *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-moi-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-moi-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-moi-250.
       exe-cnv-moi-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-moi]                        *
      *                  *---------------------------------------------*
           close     moi                                              .
      *                  *---------------------------------------------*
      *                  * Close file [mgi]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       exe-cnv-moi-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-moi-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-moi] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-moi-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ivp]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ivp-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ivp "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofivp"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ivp-999.
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
       exe-cnv-ivp-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-ivp-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-ivp-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-ivp]                         *
      *                  *---------------------------------------------*
           open      i-o    ivp                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-ivp]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivp                 .
       exe-cnv-ivp-200.
      *              *-------------------------------------------------*
      *              * Start su [old-ivp]                              *
      *              *-------------------------------------------------*
           move      low-values           to   ivp-k01                .
           start     ivp    key not less
                            ivp-k01
                            invalid key
                            go to exe-cnv-ivp-800.
       exe-cnv-ivp-250.
      *              *-------------------------------------------------*
      *              * Next su [old-ivp]                               *
      *              *-------------------------------------------------*
           read      ivp    next
                            with no lock
                            at end
                            go to exe-cnv-ivp-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ivp-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-ivp]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivp                 .
       exe-cnv-ivp-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-ivp]                          *
      *              *-------------------------------------------------*
       exe-cnv-ivp-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ivp                 .
           move      ivp-ann-ivp          to   rf-ivp-ann-ivp         .
           move      ivp-mes-ivp          to   rf-ivp-mes-ivp         .
           move      ivp-cod-iva          to   rf-ivp-cod-iva         .
           move      ivp-ibl-acq          to   rf-ivp-ibl-acq         .
           move      ivp-imp-acq          to   rf-ivp-imp-acq         .
           move      ivp-ibl-ven          to   rf-ivp-ibl-ven         .
           move      ivp-imp-ven          to   rf-ivp-imp-ven         .
           move      ivp-ibl-cor          to   rf-ivp-ibl-cor         .
           move      ivp-imp-cor          to   rf-ivp-imp-cor         .
           move      spaces               to   rf-ivp-alx-exp         .
       exe-cnv-ivp-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-ivp]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivp                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ivp-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ivp-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ivp-250.
       exe-cnv-ivp-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-ivp]                        *
      *                  *---------------------------------------------*
           close     ivp                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-ivp]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ivp                 .
       exe-cnv-ivp-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ivp-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-ivp] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-ivp-999.
           exit.

      *    *===========================================================*
      *    * Conversione [zoc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-zoc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "zoc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-zoc-999.
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
       exe-cnv-zoc-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-zoc-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-zoc-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-zoc]                         *
      *                  *---------------------------------------------*
           open      i-o    zoc                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-zoc]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
       exe-cnv-zoc-200.
      *              *-------------------------------------------------*
      *              * Start su [old-zoc]                              *
      *              *-------------------------------------------------*
           move      low-values           to   zoc-k01                .
           start     zoc    key not less
                            zoc-k01
                            invalid key
                            go to exe-cnv-zoc-800.
       exe-cnv-zoc-250.
      *              *-------------------------------------------------*
      *              * Next su [old-zoc]                               *
      *              *-------------------------------------------------*
           read      zoc    next
                            with no lock
                            at end
                            go to exe-cnv-zoc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-zoc-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-zoc]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
       exe-cnv-zoc-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-zoc]                          *
      *              *-------------------------------------------------*
       exe-cnv-zoc-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-zoc                 .
           move      zoc-cod-toc          to   rf-zoc-cod-toc         .
           move      zoc-des-key          to   rf-zoc-des-key         .
           move      zoc-des-toc          to   rf-zoc-des-toc         .
           move      zoc-pwd-toc          to   rf-zoc-pwd-toc         .
           move      zoc-vld-dpz          to   rf-zoc-vld-dpz         .
           move      zoc-cod-dpz          to   rf-zoc-cod-dpz         .
           move      zoc-org-doc          to   rf-zoc-org-doc         .
           move      zoc-prv-doc          to   rf-zoc-prv-doc         .
           move      zoc-sgl-num          to   rf-zoc-sgl-num         .
           move      zoc-des-stp          to   rf-zoc-des-stp         .
           move      zoc-snx-prz          to   rf-zoc-snx-prz         .
           move      zoc-snx-sco          to   rf-zoc-snx-sco         .
           move      zoc-snx-dtc          to   rf-zoc-snx-dtc         .
           move      zoc-def-tpr          to   rf-zoc-def-tpr         .
           move      zoc-snx-age          to   rf-zoc-snx-age         .
           move      zoc-snx-sto          to   rf-zoc-snx-sto         .
           move      zoc-tip-ord          to   rf-zoc-tip-ord         .
           move      zoc-def-fds          to   rf-zoc-def-fds         .
           move      zoc-def-tar          to   rf-zoc-def-tar         .
           move      01                   to   rf-zoc-sta-tus         .
           move      zero                 to   rf-zoc-sta-tud         .
           move      spaces               to   rf-zoc-sta-tuc         .
           move      zero                 to   rf-zoc-sta-tux         .
           move      zoc-alx-exp          to   rf-zoc-alx-exp         .
       exe-cnv-zoc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-zoc]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-zoc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-zoc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-zoc-250.
       exe-cnv-zoc-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-zoc]                        *
      *                  *---------------------------------------------*
           close     zoc                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-zoc]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
       exe-cnv-zoc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-zoc-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-zoc] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-zoc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [zsc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-zsc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "zsc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-zsc-999.
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
       exe-cnv-zsc-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-zsc-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-zsc-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-zsc]                         *
      *                  *---------------------------------------------*
           open      i-o    zsc                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-zsc]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
       exe-cnv-zsc-200.
      *              *-------------------------------------------------*
      *              * Start su [old-zsc]                              *
      *              *-------------------------------------------------*
           move      low-values           to   zsc-k01                .
           start     zsc    key not less
                            zsc-k01
                            invalid key
                            go to exe-cnv-zsc-800.
       exe-cnv-zsc-250.
      *              *-------------------------------------------------*
      *              * Next su [old-zsc]                               *
      *              *-------------------------------------------------*
           read      zsc    next
                            with no lock
                            at end
                            go to exe-cnv-zsc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-zsc-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-zsc]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
       exe-cnv-zsc-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-zsc]                          *
      *              *-------------------------------------------------*
       exe-cnv-zsc-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-zsc                 .
           move      zsc-cod-tos          to   rf-zsc-cod-tos         .
           move      zsc-des-key          to   rf-zsc-des-key         .
           move      zsc-des-tos          to   rf-zsc-des-tos         .
           move      zsc-pwd-tos          to   rf-zsc-pwd-tos         .
           move      zsc-vld-dpz          to   rf-zsc-vld-dpz         .
           move      zsc-cod-dpz          to   rf-zsc-cod-dpz         .
           move      zsc-mov-afd          to   rf-zsc-mov-afd         .
           move      zsc-def-tmf          to   rf-zsc-def-tmf         .
           move      zsc-tip-arc          to   rf-zsc-tip-arc         .
           move      zsc-tmo-btz          to   rf-zsc-tmo-btz         .
           move      zsc-def-tpr          to   rf-zsc-def-tpr         .
           move      01                   to   rf-zsc-sta-tus         .
           move      zero                 to   rf-zsc-sta-tud         .
           move      spaces               to   rf-zsc-sta-tuc         .
           move      zero                 to   rf-zsc-sta-tux         .
           move      zsc-alx-exp          to   rf-zsc-alx-exp         .
       exe-cnv-zsc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-zsc]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-zsc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-zsc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-zsc-250.
       exe-cnv-zsc-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-zsc]                        *
      *                  *---------------------------------------------*
           close     zsc                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-zsc]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
       exe-cnv-zsc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-zsc-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-zsc] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-zsc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [zbi]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-zbi-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "zbi "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-zbi-999.
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
       exe-cnv-zbi-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-zbi-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-zbi-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-zbi]                         *
      *                  *---------------------------------------------*
           open      i-o    zbi                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-zbi]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       exe-cnv-zbi-200.
      *              *-------------------------------------------------*
      *              * Start su [old-zbi]                              *
      *              *-------------------------------------------------*
           move      low-values           to   zbi-k01                .
           start     zbi    key not less
                            zbi-k01
                            invalid key
                            go to exe-cnv-zbi-800.
       exe-cnv-zbi-250.
      *              *-------------------------------------------------*
      *              * Next su [old-zbi]                               *
      *              *-------------------------------------------------*
           read      zbi    next
                            with no lock
                            at end
                            go to exe-cnv-zbi-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-zbi-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-zbi]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       exe-cnv-zbi-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-zbi]                          *
      *              *-------------------------------------------------*
       exe-cnv-zbi-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-zbi                 .
           move      zbi-cod-tmb          to   rf-zbi-cod-tmb         .
           move      zbi-cod-dpz          to   rf-zbi-cod-dpz         .
           move      zbi-des-key          to   rf-zbi-des-key         .
           move      zbi-des-tmb          to   rf-zbi-des-tmb         .
           move      zbi-des-stp          to   rf-zbi-des-stp         .
           move      zbi-pwd-tmb          to   rf-zbi-pwd-tmb         .
           move      zbi-int-ftr          to   rf-zbi-int-ftr         .
           move      zbi-tmo-ftr          to   rf-zbi-tmo-ftr         .
           move      zbi-snx-acm          to   rf-zbi-snx-acm         .
           move      zbi-def-tac          to   rf-zbi-def-tac         .
           move      zbi-def-ctr          to   rf-zbi-def-ctr         .
           move      zbi-cau-mag          to   rf-zbi-cau-mag         .
           move      zbi-cod-mic          to   rf-zbi-cod-mic         .
           move      zbi-cam-agg          to   rf-zbi-cam-agg         .
           move      zbi-def-tar          to   rf-zbi-def-tar         .
           move      zbi-snv-tar          to   rf-zbi-snv-tar         .
           move      zbi-lst-tar          to   rf-zbi-lst-tar         .
           move      zbi-org-doc          to   rf-zbi-org-doc         .
           move      zbi-prv-doc          to   rf-zbi-prv-doc         .
           move      zbi-sgl-num          to   rf-zbi-sgl-num         .
           move      zbi-mov-afd          to   rf-zbi-mov-afd         .
           move      zbi-def-tmf          to   rf-zbi-def-tmf         .
           move      zbi-snx-prz          to   rf-zbi-snx-prz         .
           move      zbi-snx-sco          to   rf-zbi-snx-sco         .
           move      zbi-snx-imp          to   rf-zbi-snx-imp         .
           move      zbi-snx-civ          to   rf-zbi-snx-civ         .
           move      zbi-snx-ttd          to   rf-zbi-snx-ttd         .
           move      zbi-snx-dct          to   rf-zbi-snx-dct         .
           move      zbi-des-dct          to   rf-zbi-des-dct         .
           move      zbi-tip-sql          to   rf-zbi-tip-sql         .
           move      zbi-pos-sql          to   rf-zbi-pos-sql         .
           move      zbi-snx-par          to   rf-zbi-snx-par         .
           move      zbi-vld-dpz          to   rf-zbi-vld-dpz         .
           move      zbi-snx-age          to   rf-zbi-snx-age         .
           move      zbi-snx-ndp          to   rf-zbi-snx-ndp         .
           move      zbi-snx-fop          to   rf-zbi-snx-fop         .
           move      zbi-snx-lib          to   rf-zbi-snx-lib         .
           move      zbi-def-tpr          to   rf-zbi-def-tpr         .
           move      zbi-snx-nmm          to   rf-zbi-snx-nmm         .
           move      zbi-tmo-ft2          to   rf-zbi-tmo-ft2         .
           move      zbi-snx-ncv          to   rf-zbi-snx-ncv         .
           move      01                   to   rf-zbi-sta-tus         .
           move      zero                 to   rf-zbi-sta-tud         .
           move      spaces               to   rf-zbi-sta-tuc         .
           move      zero                 to   rf-zbi-sta-tux         .
           move      zbi-alx-gen          to   rf-zbi-alx-gen         .
           move      zbi-cod-dsl          to   rf-zbi-cod-dsl         .
           move      zbi-alx-dpz          to   rf-zbi-alx-dpz         .
       exe-cnv-zbi-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-zbi]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-zbi-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-zbi-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-zbi-250.
       exe-cnv-zbi-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-zbi]                        *
      *                  *---------------------------------------------*
           close     zbi                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-zbi]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
       exe-cnv-zbi-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-zbi-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-zbi] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-zbi-999.
           exit.

      *    *===========================================================*
      *    * Conversione [zfi]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-zfi-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "zfi "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-zfi-999.
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
       exe-cnv-zfi-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-zfi-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-zfi-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-zfi]                         *
      *                  *---------------------------------------------*
           open      i-o    zfi                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-zfi]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
       exe-cnv-zfi-200.
      *              *-------------------------------------------------*
      *              * Start su [old-zfi]                              *
      *              *-------------------------------------------------*
           move      low-values           to   zfi-k01                .
           start     zfi    key not less
                            zfi-k01
                            invalid key
                            go to exe-cnv-zfi-800.
       exe-cnv-zfi-250.
      *              *-------------------------------------------------*
      *              * Next su [old-zfi]                               *
      *              *-------------------------------------------------*
           read      zfi    next
                            with no lock
                            at end
                            go to exe-cnv-zfi-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-zfi-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-zfi]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
       exe-cnv-zfi-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-zfi]                          *
      *              *-------------------------------------------------*
       exe-cnv-zfi-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-zfi                 .
           move      zfi-cod-tmo          to   rf-zfi-cod-tmo         .
           move      zfi-des-tmo          to   rf-zfi-des-tmo         .
           move      zfi-des-key          to   rf-zfi-des-key         .
           move      zfi-pwd-tmo          to   rf-zfi-pwd-tmo         .
           move      zfi-vld-dpz          to   rf-zfi-vld-dpz         .
           move      zfi-cod-dpz          to   rf-zfi-cod-dpz         .
           move      zfi-tip-doc          to   rf-zfi-tip-doc         .
           move      zfi-org-doc          to   rf-zfi-org-doc         .
           move      zfi-prv-doc          to   rf-zfi-prv-doc         .
           move      zfi-sgl-num          to   rf-zfi-sgl-num         .
           move      zfi-num-giv          to   rf-zfi-num-giv         .
           move      zfi-des-stp          to   rf-zfi-des-stp         .
           move      zfi-cau-cge          to   rf-zfi-cau-cge         .
           move      zfi-ctp-ivv          to   rf-zfi-ctp-ivv         .
           move      zfi-ctp-ven          to   rf-zfi-ctp-ven         .
           move      zfi-def-tpr          to   rf-zfi-def-tpr         .
           move      zfi-snx-age          to   rf-zfi-snx-age         .
           move      zfi-snx-rco          to   rf-zfi-snx-rco         .
           move      zfi-snx-rdt          to   rf-zfi-snx-rdt         .
           move      zfi-cod-dct          to   rf-zfi-cod-dct         .
           move      01                   to   rf-zfi-sta-tus         .
           move      zero                 to   rf-zfi-sta-tud         .
           move      spaces               to   rf-zfi-sta-tuc         .
           move      zero                 to   rf-zfi-sta-tux         .
           move      zfi-alx-exp          to   rf-zfi-alx-exp         .
       exe-cnv-zfi-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-zfi]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-zfi-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-zfi-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-zfi-250.
       exe-cnv-zfi-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-zfi]                        *
      *                  *---------------------------------------------*
           close     zfi                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-zfi]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
       exe-cnv-zfi-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-zfi-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-zfi] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-zfi-999.
           exit.

      *    *===========================================================*
      *    * Conversione [yof]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-yof-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "yof "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/orf/fls/ioc/obj/iofyof"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-yof-999.
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
       exe-cnv-yof-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-yof-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-yof-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-yof]                         *
      *                  *---------------------------------------------*
           open      i-o    yof                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-yof]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
       exe-cnv-yof-200.
      *              *-------------------------------------------------*
      *              * Start su [old-yof]                              *
      *              *-------------------------------------------------*
           move      low-values           to   yof-k01                .
           start     yof    key not less
                            yof-k01
                            invalid key
                            go to exe-cnv-yof-800.
       exe-cnv-yof-250.
      *              *-------------------------------------------------*
      *              * Next su [old-yof]                               *
      *              *-------------------------------------------------*
           read      yof    next
                            with no lock
                            at end
                            go to exe-cnv-yof-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-yof-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-yof]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
       exe-cnv-yof-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-yof]                          *
      *              *-------------------------------------------------*
       exe-cnv-yof-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-yof                 .
           move      yof-cod-tof          to   rf-yof-cod-tof         .
           move      yof-des-key          to   rf-yof-des-key         .
           move      yof-des-tof          to   rf-yof-des-tof         .
           move      yof-pwd-tof          to   rf-yof-pwd-tof         .
           move      yof-vld-dpz          to   rf-yof-vld-dpz         .
           move      yof-cod-dpz          to   rf-yof-cod-dpz         .
           move      yof-org-doc          to   rf-yof-org-doc         .
           move      yof-prv-doc          to   rf-yof-prv-doc         .
           move      yof-sgl-num          to   rf-yof-sgl-num         .
           move      yof-des-stp          to   rf-yof-des-stp         .
           move      yof-snx-prz          to   rf-yof-snx-prz         .
           move      yof-snx-sco          to   rf-yof-snx-sco         .
           move      yof-snx-dtc          to   rf-yof-snx-dtc         .
           move      yof-def-tpr          to   rf-yof-def-tpr         .
           move      yof-snx-sto          to   rf-yof-snx-sto         .
           move      yof-snx-stv          to   rf-yof-snx-stv         .
           move      yof-alx-exp          to   rf-yof-alx-exp         .
       exe-cnv-yof-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-yof]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-yof-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-yof-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-yof-250.
       exe-cnv-yof-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-yof]                        *
      *                  *---------------------------------------------*
           close     yof                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-yof]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yof                 .
       exe-cnv-yof-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-yof-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-yof] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-yof-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ybf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-ybf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "ybf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-ybf-999.
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
       exe-cnv-ybf-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-ybf-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-ybf-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-ybf]                         *
      *                  *---------------------------------------------*
           open      i-o    ybf                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-ybf]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       exe-cnv-ybf-200.
      *              *-------------------------------------------------*
      *              * Start su [old-ybf]                              *
      *              *-------------------------------------------------*
           move      low-values           to   ybf-k01                .
           start     ybf    key not less
                            ybf-k01
                            invalid key
                            go to exe-cnv-ybf-800.
       exe-cnv-ybf-250.
      *              *-------------------------------------------------*
      *              * Next su [old-ybf]                               *
      *              *-------------------------------------------------*
           read      ybf    next
                            with no lock
                            at end
                            go to exe-cnv-ybf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-ybf-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-ybf]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       exe-cnv-ybf-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-ybf]                          *
      *              *-------------------------------------------------*
       exe-cnv-ybf-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-ybf                 .
           move      ybf-cod-tmb          to   rf-ybf-cod-tmb         .
           move      ybf-cod-dpz          to   rf-ybf-cod-dpz         .
           move      ybf-des-key          to   rf-ybf-des-key         .
           move      ybf-des-tmb          to   rf-ybf-des-tmb         .
           move      ybf-pwd-tmb          to   rf-ybf-pwd-tmb         .
           move      ybf-int-ftr          to   rf-ybf-int-ftr         .
           move      ybf-tmo-ftr          to   rf-ybf-tmo-ftr         .
           move      ybf-cau-mag          to   rf-ybf-cau-mag         .
           move      ybf-cod-mic          to   rf-ybf-cod-mic         .
           move      ybf-cam-agg          to   rf-ybf-cam-agg         .
           move      ybf-def-tar          to   rf-ybf-def-tar         .
           move      ybf-snv-tar          to   rf-ybf-snv-tar         .
           move      ybf-lst-tar          to   rf-ybf-lst-tar         .
           move      ybf-org-doc          to   rf-ybf-org-doc         .
           move      ybf-prv-doc          to   rf-ybf-prv-doc         .
           move      ybf-mov-afd          to   rf-ybf-mov-afd         .
           move      ybf-def-tmf          to   rf-ybf-def-tmf         .
           move      ybf-vld-dpz          to   rf-ybf-vld-dpz         .
           move      ybf-def-tpr          to   rf-ybf-def-tpr         .
           move      ybf-alx-gen          to   rf-ybf-alx-gen         .
           move      ybf-cod-dsl          to   rf-ybf-cod-dsl         .
           move      ybf-alx-dpz          to   rf-ybf-alx-dpz         .
       exe-cnv-ybf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-ybf]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-ybf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-ybf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-ybf-250.
       exe-cnv-ybf-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-ybf]                        *
      *                  *---------------------------------------------*
           close     ybf                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-ybf]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
       exe-cnv-ybf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-ybf-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-ybf] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-ybf-999.
           exit.

      *    *===========================================================*
      *    * Conversione [yff]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-yff-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "yff "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/ffo/fls/ioc/obj/iofyff"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-yff-999.
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
       exe-cnv-yff-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-yff-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-yff-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-yff]                         *
      *                  *---------------------------------------------*
           open      i-o    yff                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-yff]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yff                 .
       exe-cnv-yff-200.
      *              *-------------------------------------------------*
      *              * Start su [old-yff]                              *
      *              *-------------------------------------------------*
           move      low-values           to   yff-k01                .
           start     yff    key not less
                            yff-k01
                            invalid key
                            go to exe-cnv-yff-800.
       exe-cnv-yff-250.
      *              *-------------------------------------------------*
      *              * Next su [old-yff]                               *
      *              *-------------------------------------------------*
           read      yff    next
                            with no lock
                            at end
                            go to exe-cnv-yff-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-yff-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-yff]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yff                 .
       exe-cnv-yff-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-yff]                          *
      *              *-------------------------------------------------*
       exe-cnv-yff-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-yff                 .
           move      yff-cod-tmf          to   rf-yff-cod-tmf         .
           move      yff-cod-dpz          to   rf-yff-cod-dpz         .
           move      yff-des-key          to   rf-yff-des-key         .
           move      yff-des-tmf          to   rf-yff-des-tmf         .
           move      yff-pwd-tmf          to   rf-yff-pwd-tmf         .
           move      yff-tip-doc          to   rf-yff-tip-doc         .
           move      yff-tpv-doc          to   rf-yff-tpv-doc         .
           move      yff-snx-ird          to   rf-yff-snx-ird         .
           move      yff-cau-cge          to   rf-yff-cau-cge         .
           move      yff-cau-mag          to   rf-yff-cau-mag         .
           move      yff-mov-afd          to   rf-yff-mov-afd         .
           move      yff-def-tmf          to   rf-yff-def-tmf         .
           move      yff-vld-dpz          to   rf-yff-vld-dpz         .
           move      yff-def-tpr          to   rf-yff-def-tpr         .
           move      yff-num-giv          to   rf-yff-num-giv         .
           move      yff-alx-gen          to   rf-yff-alx-gen         .
           move      yff-cod-dsl          to   rf-yff-cod-dsl         .
           move      yff-alx-dpz          to   rf-yff-alx-dpz         .
       exe-cnv-yff-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-yff]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yff                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-yff-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-yff-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-yff-250.
       exe-cnv-yff-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-yff]                        *
      *                  *---------------------------------------------*
           close     yff                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-yff]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yff                 .
       exe-cnv-yff-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-yff-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-yff] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-yff-999.
           exit.

      *    *===========================================================*
      *    * Conversione [zos]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-zos-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "zos "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-zos-999.
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
       exe-cnv-zos-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-zos-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-zos-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-zos]                         *
      *                  *---------------------------------------------*
           open      i-o    zos                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-zos]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       exe-cnv-zos-200.
      *              *-------------------------------------------------*
      *              * Start su [old-zos]                              *
      *              *-------------------------------------------------*
           move      low-values           to   zos-k01                .
           start     zos    key not less
                            zos-k01
                            invalid key
                            go to exe-cnv-zos-800.
       exe-cnv-zos-250.
      *              *-------------------------------------------------*
      *              * Next su [old-zos]                               *
      *              *-------------------------------------------------*
           read      zos    next
                            with no lock
                            at end
                            go to exe-cnv-zos-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-zos-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-zos]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       exe-cnv-zos-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-zos]                          *
      *              *-------------------------------------------------*
       exe-cnv-zos-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-zos                 .
           move      zos-ide-dat          to   rf-zos-ide-dat         .
           move      zos-ide-ute          to   rf-zos-ide-ute         .
           move      zos-ide-fas          to   rf-zos-ide-fas         .
           move      zos-tip-rec          to   rf-zos-tip-rec         .
           move      zos-cod-flt          to   rf-zos-cod-flt         .
           move      zos-cod-mne          to   rf-zos-cod-mne         .
           move      zos-des-key          to   rf-zos-des-key         .
           move      zos-des-flt          to   rf-zos-des-flt         .
           move      zos-ult-cod          to   rf-zos-ult-cod         .
           move      zos-dat-flt          to   rf-zos-dat-flt         .
       exe-cnv-zos-500.
      *                  *---------------------------------------------*
      *                  * Update speciale per filtro prodotti         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se filtro prodotti                 *
      *                      *-----------------------------------------*
           if        zos-tip-rec          not  = "dcp "
                     go to exe-cnv-zos-700.
      *                      *-----------------------------------------*
      *                      * Ridefinizione per filtro prodotti       *
      *                      *-----------------------------------------*
           move      zos-dat-flt          to   w-zos-dcp              .
      *                      *-----------------------------------------*
      *                      * Trattamento nuovo status                *
      *                      *-----------------------------------------*
           if        w-zos-dcp-sta-tus    =    zero
                     move  "XXXXXXX"      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    01
                     move  "X      "      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    11
                     move  " X     "      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    21
                     move  "  X    "      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    51
                     move  "   X   "      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    52
                     move  "    X  "      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    71
                     move  "     X "      to   w-zos-dcp-sta-tuw
           else if   w-zos-dcp-sta-tus    =    72
                     move  "      X"      to   w-zos-dcp-sta-tuw
           else      move  "XXXXXXX"      to   w-zos-dcp-sta-tuw      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento filtro prodotti           *
      *                      *-----------------------------------------*
           move      w-zos-dcp            to   rf-zos-dat-flt         .
       exe-cnv-zos-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-zos]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-zos-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-zos-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-zos-250.
       exe-cnv-zos-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-zos]                        *
      *                  *---------------------------------------------*
           close     zos                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-zos]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       exe-cnv-zos-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-zos-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-zos] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-zos-999.
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
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
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
           move      spaces               to   rf-dcp                 .
           move      dcp-ide-dat          to   rf-dcp-ide-dat         .
           move      dcp-ide-ute          to   rf-dcp-ide-ute         .
           move      dcp-ide-fas          to   rf-dcp-ide-fas         .
           move      dcp-num-pro          to   rf-dcp-num-pro         .
           move      dcp-alf-pro          to   rf-dcp-alf-pro         .
           move      dcp-syn-pro          to   rf-dcp-syn-pro         .
           move      dcp-klb-pro          to   rf-dcp-klb-pro         .
           move      dcp-des-key          to   rf-dcp-des-key         .
           move      dcp-des-pro          to   rf-dcp-des-pro         .
           move      dcp-des-pdx          to   rf-dcp-des-pdx         .
           move      dcp-cla-pro          to   rf-dcp-cla-pro         .
           move      dcp-gru-pro          to   rf-dcp-gru-pro         .
           move      dcp-sgr-pro          to   rf-dcp-sgr-pro         .
           move      dcp-tip-pro          to   rf-dcp-tip-pro         .
           move      dcp-tip-acp          to   rf-dcp-tip-acp         .
           move      dcp-not-g01          to   rf-dcp-not-g01         .
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
           move      dcp-spc-lib          to   rf-dcp-spc-lib         .
           move      dcp-cod-iva          to   rf-dcp-cod-iva         .
           move      dcp-ctp-ven          to   rf-dcp-ctp-ven         .
           move      dcp-umi-ven          to   rf-dcp-umi-ven         .
           move      dcp-dec-qta          to   rf-dcp-dec-qta         .
           move      dcp-sgl-vlt          to   rf-dcp-sgl-vlt         .
           move      dcp-dec-vlt          to   rf-dcp-dec-vlt         .
           move      dcp-dec-prz          to   rf-dcp-dec-prz         .
           move      dcp-prz-lst          to   rf-dcp-prz-lst         .
           move      dcp-lot-ven          to   rf-dcp-lot-ven         .
           move      dcp-epz-rgf          to   rf-dcp-epz-rgf         .
           move      dcp-snx-2qt          to   rf-dcp-snx-2qt         .
           move      dcp-dec-2qt          to   rf-dcp-dec-2qt         .
           move      dcp-snx-3qt          to   rf-dcp-snx-3qt         .
           move      dcp-dec-3qt          to   rf-dcp-dec-3qt         .
           move      dcp-snx-2pz          to   rf-dcp-snx-2pz         .
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
           move      dcp-snx-lst          to   rf-dcp-snx-lst         .
           move      dcp-epz-lst          to   rf-dcp-epz-lst         .
           move      dcp-pag-lst          to   rf-dcp-pag-lst         .
           move      dcp-rfl-lst          to   rf-dcp-rfl-lst         .
           move      dcp-tmc-lst          to   rf-dcp-tmc-lst         .
           move      dcp-daa-lst          to   rf-dcp-daa-lst         .
           move      dcp-aut-lst          to   rf-dcp-aut-lst         .
           move      dcp-cod-s01          to   rf-dcp-cod-s01         .
           move      dcp-cod-s02          to   rf-dcp-cod-s02         .
           move      dcp-cod-s03          to   rf-dcp-cod-s03         .
           move      dcp-dat-icm          to   rf-dcp-dat-icm         .
           move      dcp-sta-tus          to   rf-dcp-sta-tus         .
           move      dcp-sta-tud          to   rf-dcp-sta-tud         .
           move      dcp-sta-tuc          to   rf-dcp-sta-tuc         .
           move      dcp-sta-tux          to   rf-dcp-sta-tux         .
           move      dcp-cld-imp          to   rf-dcp-cld-imp         .
           move      dcp-pre-ctn          to   rf-dcp-pre-ctn         .
           move      dcp-gra-ico          to   rf-dcp-gra-ico         .
           move      dcp-pcl-ccz          to   rf-dcp-pcl-ccz         .
           move      dcp-cod-mkt          to   rf-dcp-cod-mkt         .
           move      dcp-ind-mkt          to   rf-dcp-ind-mkt         .
           move      dcp-cla-bdg          to   rf-dcp-cla-bdg         .
           move      dcp-for-blo          to   rf-dcp-for-blo         .
           move      dcp-dor-blo          to   rf-dcp-dor-blo         .
           move      dcp-fco-blo          to   rf-dcp-fco-blo         .
           move      dcp-dco-blo          to   rf-dcp-dco-blo         .
           move      dcp-cdn-cdm          to   rf-dcp-cdn-cdm         .
           move      dcp-alx-exp          to   rf-dcp-alx-exp         .
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
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
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
           move      spaces               to   rf-aaf                 .
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
           move      aaf-per-mpa          to   rf-aaf-per-mpa         .
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
      *    * Conversione [fbs]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fbs-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "fbs "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-fbs-999.
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
       exe-cnv-fbs-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-fbs-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-fbs-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-fbs]                         *
      *                  *---------------------------------------------*
           open      i-o    fbs                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-fbs]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       exe-cnv-fbs-200.
      *              *-------------------------------------------------*
      *              * Start su [old-fbs]                              *
      *              *-------------------------------------------------*
           move      low-values           to   fbs-k01                .
           start     fbs    key not less
                            fbs-k01
                            invalid key
                            go to exe-cnv-fbs-800.
       exe-cnv-fbs-250.
      *              *-------------------------------------------------*
      *              * Next su [old-fbs]                               *
      *              *-------------------------------------------------*
           read      fbs    next
                            with no lock
                            at end
                            go to exe-cnv-fbs-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-fbs-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-fbs]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       exe-cnv-fbs-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-fbs]                          *
      *              *-------------------------------------------------*
       exe-cnv-fbs-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-fbs                 .
           move      fbs-cod-dpz          to   rf-fbs-cod-dpz         .
           move      fbs-tip-mag          to   rf-fbs-tip-mag         .
           move      fbs-num-mag          to   rf-fbs-num-mag         .
           move      fbs-var-mag          to   rf-fbs-var-mag         .
           move      fbs-tip-stg          to   rf-fbs-tip-stg         .
      *
           move      fbs-sco-min (1)      to   rf-fbs-sco-min (1)     .
           move      fbs-sco-min (2)      to   rf-fbs-sco-min (2)     .
           move      fbs-sco-min (3)      to   rf-fbs-sco-min (3)     .
           move      fbs-sco-min (4)      to   rf-fbs-sco-min (4)     .
      *
           move      fbs-dua-min (1)      to   rf-fbs-dua-min (1)     .
           move      fbs-dua-min (2)      to   rf-fbs-dua-min (2)     .
           move      fbs-dua-min (3)      to   rf-fbs-dua-min (3)     .
           move      fbs-dua-min (4)      to   rf-fbs-dua-min (4)     .
      *
           move      fbs-sco-sic (1)      to   rf-fbs-sco-sic (1)     .
           move      fbs-sco-sic (2)      to   rf-fbs-sco-sic (2)     .
           move      fbs-sco-sic (3)      to   rf-fbs-sco-sic (3)     .
           move      fbs-sco-sic (4)      to   rf-fbs-sco-sic (4)     .
      *
           move      fbs-dua-sic (1)      to   rf-fbs-dua-sic (1)     .
           move      fbs-dua-sic (2)      to   rf-fbs-dua-sic (2)     .
           move      fbs-dua-sic (3)      to   rf-fbs-dua-sic (3)     .
           move      fbs-dua-sic (4)      to   rf-fbs-dua-sic (4)     .
      *
           move      fbs-sco-max (1)      to   rf-fbs-sco-max (1)     .
           move      fbs-sco-max (2)      to   rf-fbs-sco-max (2)     .
           move      fbs-sco-max (3)      to   rf-fbs-sco-max (3)     .
           move      fbs-sco-max (4)      to   rf-fbs-sco-max (4)     .
      *
           move      fbs-dua-max (1)      to   rf-fbs-dua-max (1)     .
           move      fbs-dua-max (2)      to   rf-fbs-dua-max (2)     .
           move      fbs-dua-max (3)      to   rf-fbs-dua-max (3)     .
           move      fbs-dua-max (4)      to   rf-fbs-dua-max (4)     .
      *
           move      fbs-sco-not          to   rf-fbs-sco-not         .
           move      fbs-exp-alf          to   rf-fbs-exp-alf         .
           move      fbs-exp-num          to   rf-fbs-exp-num         .
       exe-cnv-fbs-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-fbs]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-fbs-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-fbs-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-fbs-250.
       exe-cnv-fbs-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-fbs]                        *
      *                  *---------------------------------------------*
           close     fbs                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-fbs]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       exe-cnv-fbs-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-fbs-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-fbs] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-fbs-999.
           exit.


