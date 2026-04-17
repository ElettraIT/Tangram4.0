       Identification Division.
       Program-Id.                                 cnvzatbo            .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    cnv                 *
      *                                   Fase:    cnvzatbo            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/11/09    *
      *                       Ultima revisione:    NdK del 20/11/09    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per distacco BOLOGNA - ZATTI    *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Aggiornamento file [cli] : 16/12/09         *
      *                    Aggiornamento file [dcc] : 16/12/09         *
      *                    Aggiornamento file [fit] : 30/12/09         *
      *                    Aggiornamento file [fir] : 30/12/09         *
      *                    Aggiornamento file [mms] : 30/12/09         *
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
      *    * File Control [cli]                                        *
      *    *-----------------------------------------------------------*
           select  optional  cli   assign to disk           f-cli-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is cli-k01
                   alternate record key   is cli-k02
                   alternate record key   is cli-k03
                   alternate record key   is cli-k04
                   alternate record key   is cli-k05
                   alternate record key   is cli-k06
                             file status  is                f-cli-sts .

      *    *===========================================================*
      *    * File Control [dcc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  dcc   assign to disk           f-dcc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is dcc-k01
                   alternate record key   is dcc-k02
                   alternate record key   is dcc-k03
                             file status  is                f-dcc-sts .

      *    *===========================================================*
      *    * File Control [mms]                                        *
      *    *-----------------------------------------------------------*
           select  optional  mms   assign to disk           f-mms-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is mms-k01
                   alternate record key   is mms-k02
                   alternate record key   is mms-k03
                             file status  is                f-mms-sts .

      *    *===========================================================*
      *    * File Control [fit]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fit   assign to disk           f-fit-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fit-k01
                   alternate record key   is fit-k02
                   alternate record key   is fit-k03
                   alternate record key   is fit-k04
                   alternate record key   is fit-k05
                             file status  is                f-fit-sts .

      *    *===========================================================*
      *    * File Control [fir]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fir   assign to disk           f-fir-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is fir-k01
                   alternate record key   is fir-k02
                   alternate record key   is fir-k03
                   alternate record key   is fir-k04
                             file status  is                f-fir-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*
      
      *    *===========================================================*
      *    * File Description [cli]                                    *
      *    *-----------------------------------------------------------*
       fd  cli       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  cli-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  cli-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  cli-k01.
                   15  cli-cod-cli        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  cli-k02.
                   15  cli-ide-dat        pic  9(07)       comp-3     .
                   15  cli-cod-cli-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  cli-k03.
                   15  cli-rag-key        pic  x(40)                  .
                   15  cli-cod-cli-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CODMNE                         *
      *            *---------------------------------------------------*
               10  cli-k04.
                   15  cli-cod-mne        pic  x(10)                  .
                   15  cli-cod-cli-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PRTIVA                         *
      *            *---------------------------------------------------*
               10  cli-k05.
                   15  cli-prt-iva        pic  9(11)       comp-3     .
                   15  cli-cod-cli-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CODFIS                         *
      *            *---------------------------------------------------*
               10  cli-k06.
                   15  cli-cod-fis        pic  x(16)                  .
                   15  cli-cod-cli-6      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  cli-dat.
               10  cli-ide-ute            pic  x(08)                  .
               10  cli-ide-fas            pic  x(06)                  .
               10  cli-rag-soc            pic  x(40)                  .
               10  cli-via-cli            pic  x(40)                  .
               10  cli-loc-cli            pic  x(40)                  .
               10  cli-cod-naz            pic  x(03)                  .
               10  cli-cod-cmn            pic  9(05)       comp-3     .
               10  cli-cod-fzn            pic  9(03)       comp-3     .
               10  cli-cod-lct            pic  9(03)       comp-3     .
               10  cli-num-tel            pic  x(20)                  .
               10  cli-num-fax            pic  x(20)                  .
               10  cli-num-tlx            pic  x(20)                  .
               10  cli-nom-int            pic  x(30)                  .
               10  cli-cod-iva            pic  9(05)       comp-3     .
               10  cli-snx-a13            pic  x(01)                  .
               10  cli-cod-cge            pic  9(07)       comp-3     .
               10  cli-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [dcc]                                    *
      *    *-----------------------------------------------------------*
       fd  dcc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  dcc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  dcc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  dcc-k01.
                   15  dcc-cod-cli        pic  9(07)       comp-3     .
                   15  dcc-dpz-cli        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  dcc-k02.
                   15  dcc-ide-dat        pic  9(07)       comp-3     .
                   15  dcc-cod-cli-2      pic  9(07)       comp-3     .
                   15  dcc-dpz-cli-2      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  dcc-k03.
                   15  dcc-rag-key        pic  x(40)                  .
                   15  dcc-cod-cli-3      pic  9(07)       comp-3     .
                   15  dcc-dpz-cli-3      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  dcc-dat.
               10  dcc-inf-gen.
                   15  dcc-ide-ute        pic  x(08)                  .
                   15  dcc-ide-fas        pic  x(06)                  .
                   15  dcc-rag-soc        pic  x(40)                  .
                   15  dcc-via-dcc        pic  x(40)                  .
                   15  dcc-loc-dcc        pic  x(40)                  .
                   15  dcc-cod-naz        pic  x(03)                  .
                   15  dcc-cod-cmn        pic  9(05)       comp-3     .
                   15  dcc-cod-fzn        pic  9(03)       comp-3     .
                   15  dcc-cod-lct        pic  9(03)       comp-3     .
                   15  dcc-rs1-doc        pic  x(40)                  .
                   15  dcc-rs2-doc        pic  x(40)                  .
                   15  dcc-not-g01        pic  x(40)                  .
                   15  dcc-not-g02        pic  x(40)                  .
                   15  dcc-not-g03        pic  x(40)                  .
                   15  dcc-num-tel        pic  x(20)                  .
                   15  dcc-tel-alt        pic  x(20)                  .
                   15  dcc-nom-int        pic  x(30)                  .
                   15  dcc-int-alt        pic  x(30)                  .
                   15  dcc-num-fax        pic  x(20)                  .
                   15  dcc-num-tlx        pic  x(20)                  .
                   15  dcc-num-ptp        pic  x(20)                  .
                   15  dcc-num-ptf        pic  x(20)                  .
                   15  dcc-tel-mdm        pic  x(20)                  .
                   15  dcc-idn-ema        pic  x(20)                  .
                   15  dcc-inl-dcm        pic  9(02)                  .
                   15  dcc-inl-pgt        pic  9(02)                  .
                   15  dcc-noi-fnt        pic  x(15)                  .
                   15  dcc-mod-sft        pic  9(02)                  .
               10  dcc-inf-fat.
                   15  dcc-tas-ivc        pic  9(02)                  .
                   15  dcc-ctp-ven        pic  9(07)       comp-3     .
                   15  dcc-tdp-plc        pic  9(02)                  .
                   15  dcc-per-fat        pic  9(02)                  .
                   15  dcc-rag-bft        pic  9(02)                  .
                   15  dcc-epz-pes        pic  9(02)                  .
                   15  dcc-cod-vlt        pic  x(03)                  .
                   15  dcc-mom-acv        pic  9(02)                  .
                   15  dcc-snx-rlv        pic  x(01)                  .
                   15  dcc-mom-alv        pic  9(02)                  .
                   15  dcc-cod-lng        pic  x(03)                  .
                   15  dcc-tip-frn        pic  9(02)                  .
                   15  dcc-arc-plf        pic  9(07)       comp-3     .
                   15  dcc-dpz-plf        pic  x(04)                  .
                   15  dcc-tip-ftz        pic  9(02)                  .
               10  dcc-inf-cdv.
                   15  dcc-cod-lst        pic  x(03)                  .
                   15  dcc-cat-scr        pic  9(05)       comp-3     .
                   15  dcc-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  dcc-cat-scc        pic  9(05)       comp-3     .
                   15  dcc-per-scc        pic  9(02)v9(01) comp-3     .
                   15  dcc-add-spe occurs 06.
                       20  dcc-snm-spe    pic  9(02)                  .
                       20  dcc-per-spe    pic  9(02)v9(01) comp-3     .
                       20  dcc-imp-spe    pic  9(09)       comp-3     .
                   15  dcc-vde-fat occurs 06.
                       20  dcc-vde-cod    pic  x(03)                  .
               10  dcc-inf-cdp.
                   15  dcc-cod-fop        pic  9(07)       comp-3     .
                   15  dcc-tip-esm        pic  9(02)                  .
                   15  dcc-ggg-alt        pic  9(02)                  .
                   15  dcc-mmm-e01        pic  9(02)                  .
                   15  dcc-mmm-e02        pic  9(02)                  .
                   15  dcc-add-spi        pic  x(03)                  .
                   15  dcc-add-spb        pic  x(03)                  .
                   15  dcc-cod-abi        pic  9(05)       comp-3     .
                   15  dcc-cod-cab        pic  9(05)       comp-3     .
                   15  dcc-ccc-app        pic  x(12)                  .
                   15  dcc-cod-cin        pic  x(01)                  .
                   15  dcc-nos-ban        pic  x(10)                  .
                   15  dcc-nos-bpe        pic  x(10)                  .
                   15  dcc-nos-ccp        pic  x(10)                  .
                   15  dcc-ipr-iel        pic  9(02)                  .
               10  dcc-inf-gag.
                   15  dcc-cod-age        pic  9(07)       comp-3     .
                   15  dcc-cat-pvg        pic  9(05)       comp-3     .
                   15  dcc-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
               10  dcc-inf-pcs.
                   15  dcc-cod-zon        pic  9(05)       comp-3     .
                   15  dcc-cod-cat        pic  9(05)       comp-3     .
                   15  dcc-cod-stt        pic  9(05)       comp-3     .
                   15  dcc-dat-aqz        pic  9(07)       comp-3     .
                   15  dcc-sta-tus        pic  9(02)                  .
                   15  dcc-sta-tud        pic  9(07)       comp-3     .
                   15  dcc-sta-tuc        pic  9(07)       comp-3     .
                   15  dcc-sta-tux        pic  9(02)                  .
               10  dcc-inf-mkt.
                   15  dcc-cld-imp        pic  x(01)                  .
                   15  dcc-tra-pco        pic  9(02)                  .
                   15  dcc-gra-ico        pic  9(02)                  .
                   15  dcc-pcl-ccz        pic  9(02)                  .
                   15  dcc-pre-ctn        pic  9(02)                  .
                   15  dcc-org-ctt        pic  9(02)                  .
                   15  dcc-cod-mkt        pic  9(05)       comp-3     .
                   15  dcc-ind-mkt        pic  x(10)                  .
               10  dcc-inf-bdg.
                   15  dcc-cla-bdg        pic  9(05)       comp-3     .
               10  dcc-inf-bol.
                   15  dcc-for-blo        pic  9(02)                  .
                   15  dcc-dor-blo        pic  9(07)       comp-3     .
                   15  dcc-fco-blo        pic  9(02)                  .
                   15  dcc-dco-blo        pic  9(07)       comp-3     .
                   15  dcc-dtt-acu        pic  9(02)                  .
                   15  dcc-cod-vet        pic  9(07)       comp-3     .
                   15  dcc-cod-vt2        pic  9(07)       comp-3     .
                   15  dcc-cod-vt3        pic  9(07)       comp-3     .
                   15  dcc-dst-klm        pic  9(04)v9(01) comp-3     .
                   15  dcc-dti-cpf        pic  9(07)       comp-3     .
                   15  dcc-dtf-cpf        pic  9(07)       comp-3     .
                   15  dcc-gdl-nls        pic  x(07)                  .
                   15  dcc-oam-oin        pic  9(04)                  .
                   15  dcc-oam-ofi        pic  9(04)                  .
                   15  dcc-oap-oin        pic  9(04)                  .
                   15  dcc-oap-ofi        pic  9(04)                  .
                   15  dcc-abn-vtt        pic  x(12)                  .
               10  dcc-inf-aps.
                   15  dcc-idn-em2        pic  x(20)                  .
                   15  dcc-cod-cdv        pic  9(03)                  .
                   15  dcc-cin-eur        pic  x(02)                  .
                   15  dcc-alx-exp.
                       20  filler occurs 43
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [mms]                                    *
      *    *-----------------------------------------------------------*
       fd  mms       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  mms-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  mms-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : DPMGDS                         *
      *            *---------------------------------------------------*
               10  mms-k01.
                   15  mms-ann-ese        pic  9(03)       comp-3     .
                   15  mms-cod-dpz        pic  9(02)                  .
                   15  mms-tip-mag        pic  9(02)                  .
                   15  mms-num-mag        pic  9(07)       comp-3     .
                   15  mms-var-mag        pic  x(14)                  .
                   15  mms-cod-dsl        pic  x(07)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DPDSMG                         *
      *            *---------------------------------------------------*
               10  mms-k02.
                   15  mms-ann-ese-2      pic  9(03)       comp-3     .
                   15  mms-cod-dpz-2      pic  9(02)                  .
                   15  mms-cod-dsl-2      pic  x(07)                  .
                   15  mms-tip-mag-2      pic  9(02)                  .
                   15  mms-num-mag-2      pic  9(07)       comp-3     .
                   15  mms-var-mag-2      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : MGDPDS                         *
      *            *---------------------------------------------------*
               10  mms-k03.
                   15  mms-ann-ese-3      pic  9(03)       comp-3     .
                   15  mms-tip-mag-3      pic  9(02)                  .
                   15  mms-num-mag-3      pic  9(07)       comp-3     .
                   15  mms-var-mag-3      pic  x(14)                  .
                   15  mms-cod-dpz-3      pic  9(02)                  .
                   15  mms-cod-dsl-3      pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  mms-dat.
               10  mms-qta-ini            pic s9(08)v9(03) comp-3     .
               10  mms-prg-mes occurs 12.
                   15  mms-prg-car        pic s9(08)v9(03) comp-3     .
                   15  mms-prg-sca        pic s9(08)v9(03) comp-3     .
               10  mms-dip-ife            pic s9(08)v9(03) comp-3     .
               10  mms-din-ife            pic s9(08)v9(03) comp-3     .
               10  mms-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [fit]                                    *
      *    *-----------------------------------------------------------*
       fd  fit       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  fit-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  fit-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fit-k01.
                   15  fit-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fit-k02.
                   15  fit-ide-dat        pic  9(07)       comp-3     .
                   15  fit-dat-doc        pic  9(07)       comp-3     .
                   15  fit-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fit-k03.
                   15  fit-dat-doc-3      pic  9(07)       comp-3     .
                   15  fit-cod-dpz        pic  9(02)                  .
                   15  fit-num-doc        pic  9(11)       comp-3     .
                   15  fit-cod-tmo        pic  x(05)                  .
                   15  fit-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  fit-k04.
                   15  fit-scl-ann        pic  9(03)       comp-3     .
                   15  fit-num-giv        pic  9(02)                  .
                   15  fit-sgl-num        pic  x(03)                  .
                   15  fit-num-doc-4      pic  9(11)       comp-3     .
                   15  fit-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CLIDAT                         *
      *            *---------------------------------------------------*
               10  fit-k05.
                   15  fit-cod-cli        pic  9(07)       comp-3     .
                   15  fit-dat-doc-5      pic  9(07)       comp-3     .
                   15  fit-num-doc-5      pic  9(11)       comp-3     .
                   15  fit-cod-tmo-5      pic  x(05)                  .
                   15  fit-num-prt-5      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fit-dat.
               10  fit-ide-ute            pic  x(08)                  .
               10  fit-ide-fas            pic  x(06)                  .
               10  fit-tip-doc            pic  9(02)                  .
               10  fit-org-doc            pic  9(02)                  .
               10  fit-dpz-cli            pic  x(04)                  .
               10  fit-tip-frn            pic  9(02)                  .
               10  fit-cli-plf            pic  9(07)       comp-3     .
               10  fit-dpc-plf            pic  x(04)                  .
               10  fit-cod-lng            pic  x(03)                  .
               10  fit-vpf.
                   15  fit-sgl-vpf        pic  x(03)                  .
                   15  fit-dec-vpf        pic  9(01)                  .
                   15  fit-tdc-vpf        pic  x(01)                  .
                   15  fit-cdc-vpf        pic  9(06)v9(05) comp-3     .
               10  fit-ass-iva            pic  9(05)       comp-3     .
               10  fit-ctp-ven            pic  9(07)       comp-3     .
               10  fit-inl-dcm            pic  9(02)                  .
               10  fit-inl-pgt            pic  9(02)                  .
               10  fit-cod-lst            pic  x(03)                  .
               10  fit-csr-aac            pic  9(05)       comp-3     .
               10  fit-psr-aac occurs 05  pic  9(02)v9(01) comp-3     .
               10  fit-csc-aac            pic  9(05)       comp-3     .
               10  fit-psc-aac            pic  9(02)v9(01) comp-3     .
               10  fit-cpv-aac            pic  9(05)       comp-3     .
               10  fit-ppv-aac occurs 03  pic  9(02)v9(01) comp-3     .
               10  fit-voc-des occurs 06  pic  x(03)                  .
               10  fit-cod-fop            pic  9(07)       comp-3     .
               10  fit-scp-aap            pic  9(02)v9(01) comp-3     .
               10  fit-cod-abi            pic  9(05)       comp-3     .
               10  fit-cod-cab            pic  9(05)       comp-3     .
               10  fit-ccc-app            pic  x(12)                  .
               10  fit-nos-ban            pic  x(10)                  .
               10  fit-nos-ccp            pic  x(10)                  .
               10  fit-add-spb            pic  x(03)                  .
               10  fit-ipr-iel            pic  9(02)                  .
               10  fit-pag-dsm            pic  9(07)       comp-3     .
               10  fit-pag-qaf            pic  9(09)       comp-3     .
               10  fit-pag-act            pic  9(09)       comp-3     .
               10  fit-cod-age            pic  9(07)       comp-3     .
               10  fit-fsp-doc            pic  9(02)                  .
               10  fit-pvf-age            pic  9(11)       comp-3     .
               10  fit-tip-vpa            pic  9(02)                  .
               10  fit-cpv-aaa            pic  9(05)       comp-3     .
               10  fit-ppv-aaa occurs 03  pic  9(02)v9(01) comp-3     .
               10  fit-cod-ime            pic  9(07)       comp-3     .
               10  fit-pvf-ime            pic  9(11)       comp-3     .
               10  fit-tot-rig occurs 09  pic s9(11)       comp-3     .
               10  fit-tot-scc            pic s9(11)       comp-3     .
               10  fit-per-scc            pic  9(02)v9(01) comp-3     .
               10  fit-civ-scc            pic  9(05)       comp-3     .
               10  fit-ccp-scc            pic  9(07)       comp-3     .
               10  fit-tot-scp            pic s9(11)       comp-3     .
               10  fit-per-scp            pic  9(02)v9(01) comp-3     .
               10  fit-civ-scp            pic  9(05)       comp-3     .
               10  fit-ccp-scp            pic  9(07)       comp-3     .
               10  fit-spe-add occurs 06.
                   15  fit-spe-snx        pic  9(01)                  .
                   15  fit-spe-mad        pic  9(01)                  .
                   15  fit-spe-per        pic  9(02)v9(01) comp-3     .
                   15  fit-spe-ibl        pic  9(02)                  .
                   15  fit-ibt-spe.
                       20  fit-ibx-spe occurs 09
                                          pic  x(01)                  .
                   15  fit-spe-imp        pic s9(09)       comp-3     .
                   15  fit-spe-civ        pic  9(05)       comp-3     .
                   15  fit-spe-ccp        pic  9(07)       comp-3     .
               10  fit-add-spi            pic  x(03)                  .
               10  fit-civ-spi            pic  9(05)       comp-3     .
               10  fit-ccp-spi            pic  9(07)       comp-3     .
               10  fit-tot-sic            pic s9(09)       comp-3     .
               10  fit-tot-sia            pic s9(09)       comp-3     .
               10  fit-tot-spb            pic s9(09)       comp-3     .
               10  fit-civ-spb            pic  9(05)       comp-3     .
               10  fit-ccp-spb            pic  9(07)       comp-3     .
               10  fit-prt-mgd            pic  9(07)       comp-3     .
               10  fit-nrg-mgd            pic  9(02)                  .
               10  fit-dri-mgd            pic  9(07)       comp-3     .
               10  fit-nri-mgd            pic  x(10)                  .
               10  fit-nps-sdb            pic  9(11)                  .
               10  fit-ctr-sdb            pic  9(02)                  .
               10  fit-iva-cst.
                   15  fit-iva-rig occurs 06.
                       20  fit-iva-cod    pic  9(05)       comp-3     .
                       20  fit-iva-ibl    pic s9(11)       comp-3     .
                       20  fit-iva-imp    pic s9(11)       comp-3     .
                   15  fit-iva-tdo        pic s9(11)       comp-3     .
               10  fit-ctp-cst.
                   15  fit-ctp-rig occurs 10.
                       20  fit-ctp-cod    pic  9(07)       comp-3     .
                       20  fit-ctp-imp    pic s9(11)       comp-3     .
               10  fit-ctr-stp            pic  9(02)                  .
               10  fit-flg-ela.
                   15  fit-flg-blo.
                       20  fit-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fit-flg-nbl.
                       20  fit-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fit-flg-pul            pic  x(01)                  .
               10  fit-alx-exp.
                   15  filler occurs 40   pic  x(01)                  .

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
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CLIPRO                         *
      *            *---------------------------------------------------*
               10  fir-k04.
                   15  fir-cod-cli        pic  9(07)       comp-3     .
                   15  fir-tip-mag-4      pic  9(02)                  .
                   15  fir-num-pro-4      pic  9(07)       comp-3     .
                   15  fir-dat-doc-4      pic  9(07)       comp-3     .
                   15  fir-num-prt-4      pic  9(09)       comp-3     .
                   15  fir-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fir-dat.
               10  fir-cod-tmo            pic  x(05)                  .
               10  fir-cod-dpz            pic  9(02)                  .
               10  fir-num-doc            pic  9(11)       comp-3     .
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
               10  fir-qta-ven            pic s9(10)v9(03) comp-3     .
               10  fir-snx-2qt            pic  9(01)                  .
               10  fir-dec-2qt            pic  9(01)                  .
               10  fir-qta-a02            pic s9(10)v9(03) comp-3     .
               10  fir-snx-3qt            pic  9(01)                  .
               10  fir-dec-3qt            pic  9(01)                  .
               10  fir-qta-a03            pic s9(10)v9(03) comp-3     .
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
                   15  filler  occurs 80  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [cli]                                       *
      *    *-----------------------------------------------------------*
       01  f-cli.
           05  f-cli-nam                  pic  x(04)                  .
           05  f-cli-pat                  pic  x(40)                  .
           05  f-cli-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [dcc]                                       *
      *    *-----------------------------------------------------------*
       01  f-dcc.
           05  f-dcc-nam                  pic  x(04)                  .
           05  f-dcc-pat                  pic  x(40)                  .
           05  f-dcc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [mms]                                       *
      *    *-----------------------------------------------------------*
       01  f-mms.
           05  f-mms-nam                  pic  x(04)                  .
           05  f-mms-pat                  pic  x(40)                  .
           05  f-mms-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fit]                                       *
      *    *-----------------------------------------------------------*
       01  f-fit.
           05  f-fit-nam                  pic  x(04)                  .
           05  f-fit-pat                  pic  x(40)                  .
           05  f-fit-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [fir]                                       *
      *    *-----------------------------------------------------------*
       01  f-fir.
           05  f-fir-nam                  pic  x(04)                  .
           05  f-fir-pat                  pic  x(40)                  .
           05  f-fir-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [mms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmms"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .

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
                     "cnvzat"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "cnvzatbo"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "CONVERSIONE PER ESTRAZIONE DATI BOLOGNA "       .

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
      *    * Work-area per contatori e indici                          *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Contatore 'I' di comodo                               *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-001              pic  9(03)                  .

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
      *              * Conversione [cli]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-cli-000      thru exe-cnv-cli-999        .
      *              *-------------------------------------------------*
      *              * Conversione [dcc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-dcc-000      thru exe-cnv-dcc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [mms]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-mms-000      thru exe-cnv-mms-999        .
      *              *-------------------------------------------------*
      *              * Conversione [fit]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-fit-000      thru exe-cnv-fit-999        .
      *              *-------------------------------------------------*
      *              * Conversione [fir]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-fir-000      thru exe-cnv-fir-999        .
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
      *    * Conversione [cli]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-cli-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "cli "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-cli-999.
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
       exe-cnv-cli-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-cli-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-cli-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-cli]                         *
      *                  *---------------------------------------------*
           open      i-o    cli                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-cli]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       exe-cnv-cli-200.
      *              *-------------------------------------------------*
      *              * Start su [old-cli]                              *
      *              *-------------------------------------------------*
           move      low-values           to   cli-k01                .
           start     cli    key not less
                            cli-k01
                            invalid key
                            go to exe-cnv-cli-800.
       exe-cnv-cli-250.
      *              *-------------------------------------------------*
      *              * Next su [old-cli]                               *
      *              *-------------------------------------------------*
           read      cli    next
                            with no lock
                            at end
                            go to exe-cnv-cli-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-cli-280.
      *              *-------------------------------------------------*
      *              * Eventuali selezioni su [old-cli]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni                                   *
      *                  *---------------------------------------------*
           if        cli-cod-cli          >    0028000 and
                     cli-cod-cli          <    0028999
                     go to exe-cnv-cli-300.
           if        cli-cod-cli          >    0029000 and
                     cli-cod-cli          <    0029999
                     go to exe-cnv-cli-300.
           if        cli-cod-cli          >    0050000 and
                     cli-cod-cli          <    0050999
                     go to exe-cnv-cli-300.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-cnv-cli-250.
       exe-cnv-cli-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-cli]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       exe-cnv-cli-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-cli]                          *
      *              *-------------------------------------------------*
       exe-cnv-cli-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-cli                 .
           move      cli-ide-dat          to   rf-cli-ide-dat         .
           move      cli-ide-ute          to   rf-cli-ide-ute         .
           move      cli-ide-fas          to   rf-cli-ide-fas         .
           move      cli-cod-cli          to   rf-cli-cod-cli         .
           move      cli-cod-mne          to   rf-cli-cod-mne         .
           move      cli-rag-key          to   rf-cli-rag-key         .
           move      cli-rag-soc          to   rf-cli-rag-soc         .
           move      cli-via-cli          to   rf-cli-via-cli         .
           move      cli-loc-cli          to   rf-cli-loc-cli         .
           move      cli-cod-naz          to   rf-cli-cod-naz         .
           move      cli-cod-cmn          to   rf-cli-cod-cmn         .
           move      cli-cod-fzn          to   rf-cli-cod-fzn         .
           move      cli-cod-lct          to   rf-cli-cod-lct         .
           move      cli-num-tel          to   rf-cli-num-tel         .
           move      cli-num-fax          to   rf-cli-num-fax         .
           move      cli-num-tlx          to   rf-cli-num-tlx         .
           move      cli-nom-int          to   rf-cli-nom-int         .
           move      cli-cod-iva          to   rf-cli-cod-iva         .
           move      cli-prt-iva          to   rf-cli-prt-iva         .
           move      cli-cod-fis          to   rf-cli-cod-fis         .
           move      cli-snx-a13          to   rf-cli-snx-a13         .
           move      cli-cod-cge          to   rf-cli-cod-cge         .
           move      cli-alx-exp          to   rf-cli-alx-exp         .
       exe-cnv-cli-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-cli]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-cli-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-cli-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-cli-250.
       exe-cnv-cli-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-cli]                        *
      *                  *---------------------------------------------*
           close     cli                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-cli]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       exe-cnv-cli-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-cli-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-cli] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-cli-999.
           exit.

      *    *===========================================================*
      *    * Conversione [dcc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-dcc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "dcc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-dcc-999.
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
       exe-cnv-dcc-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-dcc-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-dcc-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-dcc]                         *
      *                  *---------------------------------------------*
           open      i-o    dcc                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-dcc]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       exe-cnv-dcc-200.
      *              *-------------------------------------------------*
      *              * Start su [old-dcc]                              *
      *              *-------------------------------------------------*
           move      low-values           to   dcc-k01                .
           start     dcc    key not less
                            dcc-k01
                            invalid key
                            go to exe-cnv-dcc-800.
       exe-cnv-dcc-250.
      *              *-------------------------------------------------*
      *              * Next su [old-dcc]                               *
      *              *-------------------------------------------------*
           read      dcc    next
                            with no lock
                            at end
                            go to exe-cnv-dcc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-dcc-280.
      *              *-------------------------------------------------*
      *              * Eventuali selezioni su [old-dcc]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni                                   *
      *                  *---------------------------------------------*
           if        dcc-cod-cli          >    0028000 and
                     dcc-cod-cli          <    0028999
                     go to exe-cnv-dcc-300.
           if        dcc-cod-cli          >    0029000 and
                     dcc-cod-cli          <    0029999
                     go to exe-cnv-dcc-300.
           if        dcc-cod-cli          >    0050000 and
                     dcc-cod-cli          <    0050999
                     go to exe-cnv-dcc-300.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-cnv-dcc-250.
       exe-cnv-dcc-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-dcc]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       exe-cnv-dcc-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-dcc]                          *
      *              *-------------------------------------------------*
       exe-cnv-dcc-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc                 .
           move      dcc-ide-dat          to   rf-dcc-ide-dat         .
           move      dcc-ide-ute          to   rf-dcc-ide-ute         .
           move      dcc-ide-fas          to   rf-dcc-ide-fas         .
           move      dcc-cod-cli          to   rf-dcc-cod-cli         .
           move      dcc-dpz-cli          to   rf-dcc-dpz-cli         .
           move      dcc-rag-key          to   rf-dcc-rag-key         .
           move      dcc-rag-soc          to   rf-dcc-rag-soc         .
           move      dcc-via-dcc          to   rf-dcc-via-dcc         .
           move      dcc-loc-dcc          to   rf-dcc-loc-dcc         .
           move      dcc-cod-naz          to   rf-dcc-cod-naz         .
           move      dcc-cod-cmn          to   rf-dcc-cod-cmn         .
           move      dcc-cod-fzn          to   rf-dcc-cod-fzn         .
           move      dcc-cod-lct          to   rf-dcc-cod-lct         .
           move      dcc-rs1-doc          to   rf-dcc-rs1-doc         .
           move      dcc-rs2-doc          to   rf-dcc-rs2-doc         .
           move      dcc-not-g01          to   rf-dcc-not-g01         .
           move      dcc-not-g02          to   rf-dcc-not-g02         .
           move      dcc-not-g03          to   rf-dcc-not-g03         .
           move      dcc-num-tel          to   rf-dcc-num-tel         .
           move      dcc-tel-alt          to   rf-dcc-tel-alt         .
           move      dcc-nom-int          to   rf-dcc-nom-int         .
           move      dcc-int-alt          to   rf-dcc-int-alt         .
           move      dcc-num-fax          to   rf-dcc-num-fax         .
           move      dcc-num-tlx          to   rf-dcc-num-tlx         .
           move      dcc-num-ptp          to   rf-dcc-num-ptp         .
           move      dcc-num-ptf          to   rf-dcc-num-ptf         .
           move      dcc-tel-mdm          to   rf-dcc-tel-mdm         .
           move      dcc-idn-ema          to   rf-dcc-idn-ema         .
           move      dcc-inl-dcm          to   rf-dcc-inl-dcm         .
           move      dcc-inl-pgt          to   rf-dcc-inl-pgt         .
           move      dcc-noi-fnt          to   rf-dcc-noi-fnt         .
           move      dcc-mod-sft          to   rf-dcc-mod-sft         .
           move      dcc-tas-ivc          to   rf-dcc-tas-ivc         .
           move      dcc-ctp-ven          to   rf-dcc-ctp-ven         .
           move      dcc-tdp-plc          to   rf-dcc-tdp-plc         .
           move      dcc-per-fat          to   rf-dcc-per-fat         .
           move      dcc-rag-bft          to   rf-dcc-rag-bft         .
           move      dcc-epz-pes          to   rf-dcc-epz-pes         .
           move      dcc-cod-vlt          to   rf-dcc-cod-vlt         .
           move      dcc-mom-acv          to   rf-dcc-mom-acv         .
           move      dcc-snx-rlv          to   rf-dcc-snx-rlv         .
           move      dcc-mom-alv          to   rf-dcc-mom-alv         .
           move      dcc-cod-lng          to   rf-dcc-cod-lng         .
           move      dcc-tip-frn          to   rf-dcc-tip-frn         .
           move      dcc-arc-plf          to   rf-dcc-arc-plf         .
           move      dcc-dpz-plf          to   rf-dcc-dpz-plf         .
           move      dcc-tip-ftz          to   rf-dcc-tip-ftz         .
           move      dcc-cod-lst          to   rf-dcc-cod-lst         .
           move      dcc-cat-scr          to   rf-dcc-cat-scr         .
           move      dcc-per-scr (1)      to   rf-dcc-per-scr (1)     .
           move      dcc-per-scr (2)      to   rf-dcc-per-scr (2)     .
           move      dcc-per-scr (3)      to   rf-dcc-per-scr (3)     .
           move      dcc-per-scr (4)      to   rf-dcc-per-scr (4)     .
           move      dcc-per-scr (5)      to   rf-dcc-per-scr (5)     .
           move      dcc-cat-scc          to   rf-dcc-cat-scc         .
           move      dcc-per-scc          to   rf-dcc-per-scc         .
           move      dcc-snm-spe (1)      to   rf-dcc-snm-spe (1)     .
           move      dcc-snm-spe (2)      to   rf-dcc-snm-spe (2)     .
           move      dcc-snm-spe (3)      to   rf-dcc-snm-spe (3)     .
           move      dcc-snm-spe (4)      to   rf-dcc-snm-spe (4)     .
           move      dcc-snm-spe (5)      to   rf-dcc-snm-spe (5)     .
           move      dcc-snm-spe (6)      to   rf-dcc-snm-spe (6)     .
           move      dcc-per-spe (1)      to   rf-dcc-per-spe (1)     .
           move      dcc-per-spe (2)      to   rf-dcc-per-spe (2)     .
           move      dcc-per-spe (3)      to   rf-dcc-per-spe (3)     .
           move      dcc-per-spe (4)      to   rf-dcc-per-spe (4)     .
           move      dcc-per-spe (5)      to   rf-dcc-per-spe (5)     .
           move      dcc-per-spe (6)      to   rf-dcc-per-spe (6)     .
           move      dcc-imp-spe (1)      to   rf-dcc-imp-spe (1)     .
           move      dcc-imp-spe (2)      to   rf-dcc-imp-spe (2)     .
           move      dcc-imp-spe (3)      to   rf-dcc-imp-spe (3)     .
           move      dcc-imp-spe (4)      to   rf-dcc-imp-spe (4)     .
           move      dcc-imp-spe (5)      to   rf-dcc-imp-spe (5)     .
           move      dcc-imp-spe (6)      to   rf-dcc-imp-spe (6)     .
           move      dcc-vde-cod (1)      to   rf-dcc-vde-cod (1)     .
           move      dcc-vde-cod (2)      to   rf-dcc-vde-cod (2)     .
           move      dcc-vde-cod (3)      to   rf-dcc-vde-cod (3)     .
           move      dcc-vde-cod (4)      to   rf-dcc-vde-cod (4)     .
           move      dcc-vde-cod (5)      to   rf-dcc-vde-cod (5)     .
           move      dcc-vde-cod (6)      to   rf-dcc-vde-cod (6)     .
           move      dcc-cod-fop          to   rf-dcc-cod-fop         .
           move      dcc-tip-esm          to   rf-dcc-tip-esm         .
           move      dcc-ggg-alt          to   rf-dcc-ggg-alt         .
           move      dcc-mmm-e01          to   rf-dcc-mmm-e01         .
           move      dcc-mmm-e02          to   rf-dcc-mmm-e02         .
           move      dcc-add-spi          to   rf-dcc-add-spi         .
           move      dcc-add-spb          to   rf-dcc-add-spb         .
           move      dcc-cod-abi          to   rf-dcc-cod-abi         .
           move      dcc-cod-cab          to   rf-dcc-cod-cab         .
           move      dcc-ccc-app          to   rf-dcc-ccc-app         .
           move      dcc-cod-cin          to   rf-dcc-cod-cin         .
           move      dcc-nos-ban          to   rf-dcc-nos-ban         .
           move      dcc-nos-bpe          to   rf-dcc-nos-bpe         .
           move      dcc-nos-ccp          to   rf-dcc-nos-ccp         .
           move      dcc-ipr-iel          to   rf-dcc-ipr-iel         .
           move      dcc-cod-age          to   rf-dcc-cod-age         .
           move      dcc-cat-pvg          to   rf-dcc-cat-pvg         .
           move      dcc-per-pvg (1)      to   rf-dcc-per-pvg (1)     .
           move      dcc-per-pvg (2)      to   rf-dcc-per-pvg (2)     .
           move      dcc-per-pvg (3)      to   rf-dcc-per-pvg (3)     .
           move      dcc-cod-zon          to   rf-dcc-cod-zon         .
           move      dcc-cod-cat          to   rf-dcc-cod-cat         .
           move      dcc-cod-stt          to   rf-dcc-cod-stt         .
           move      dcc-dat-aqz          to   rf-dcc-dat-aqz         .
           move      dcc-sta-tus          to   rf-dcc-sta-tus         .
           move      dcc-sta-tud          to   rf-dcc-sta-tud         .
           move      dcc-sta-tuc          to   rf-dcc-sta-tuc         .
           move      dcc-sta-tux          to   rf-dcc-sta-tux         .
           move      dcc-cld-imp          to   rf-dcc-cld-imp         .
           move      dcc-tra-pco          to   rf-dcc-tra-pco         .
           move      dcc-gra-ico          to   rf-dcc-gra-ico         .
           move      dcc-pcl-ccz          to   rf-dcc-pcl-ccz         .
           move      dcc-pre-ctn          to   rf-dcc-pre-ctn         .
           move      dcc-org-ctt          to   rf-dcc-org-ctt         .
           move      dcc-cod-mkt          to   rf-dcc-cod-mkt         .
           move      dcc-ind-mkt          to   rf-dcc-ind-mkt         .
           move      dcc-cla-bdg          to   rf-dcc-cla-bdg         .
           move      dcc-for-blo          to   rf-dcc-for-blo         .
           move      dcc-dor-blo          to   rf-dcc-dor-blo         .
           move      dcc-fco-blo          to   rf-dcc-fco-blo         .
           move      dcc-dco-blo          to   rf-dcc-dco-blo         .
           move      dcc-dtt-acu          to   rf-dcc-dtt-acu         .
           move      dcc-cod-vet          to   rf-dcc-cod-vet         .
           move      dcc-cod-vt2          to   rf-dcc-cod-vt2         .
           move      dcc-cod-vt3          to   rf-dcc-cod-vt3         .
           move      dcc-abn-vtt          to   rf-dcc-abn-vtt         .
           move      dcc-dst-klm          to   rf-dcc-dst-klm         .
           move      dcc-dti-cpf          to   rf-dcc-dti-cpf         .
           move      dcc-dtf-cpf          to   rf-dcc-dtf-cpf         .
           move      dcc-gdl-nls          to   rf-dcc-gdl-nls         .
           move      dcc-oam-oin          to   rf-dcc-oam-oin         .
           move      dcc-oam-ofi          to   rf-dcc-oam-ofi         .
           move      dcc-oap-oin          to   rf-dcc-oap-oin         .
           move      dcc-oap-ofi          to   rf-dcc-oap-ofi         .
           move      dcc-idn-em2          to   rf-dcc-idn-em2         .
           move      dcc-cod-cdv          to   rf-dcc-cod-cdv         .
           move      dcc-cin-eur          to   rf-dcc-cin-eur         .
           move      dcc-alx-exp          to   rf-dcc-alx-exp         .
           move      dcc-alx-exp          to   rf-dcc-alx-exp         .
       exe-cnv-dcc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-dcc]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-dcc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-dcc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-dcc-250.
       exe-cnv-dcc-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-dcc]                        *
      *                  *---------------------------------------------*
           close     dcc                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-dcc]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       exe-cnv-dcc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-dcc-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-dcc] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-dcc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [mms]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-mms-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "mms "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-mms-999.
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
       exe-cnv-mms-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-mms-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-mms-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-mms]                         *
      *                  *---------------------------------------------*
           open      i-o    mms                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-mms]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       exe-cnv-mms-200.
      *              *-------------------------------------------------*
      *              * Start su [old-mms]                              *
      *              *-------------------------------------------------*
           move      low-values           to   mms-k01                .
           start     mms    key not less
                            mms-k01
                            invalid key
                            go to exe-cnv-mms-800.
       exe-cnv-mms-250.
      *              *-------------------------------------------------*
      *              * Next su [old-mms]                               *
      *              *-------------------------------------------------*
           read      mms    next
                            with no lock
                            at end
                            go to exe-cnv-mms-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-mms-280.
      *              *-------------------------------------------------*
      *              * Eventuali selezioni su [old-mms]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni                                   *
      *                  *---------------------------------------------*
           if        mms-cod-dpz          =    04
                     go to exe-cnv-mms-300.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-cnv-mms-250.
       exe-cnv-mms-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-mms]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       exe-cnv-mms-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-mms]                          *
      *              *-------------------------------------------------*
       exe-cnv-mms-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-mms                 .
           move      mms-ann-ese          to   rf-mms-ann-ese         .
           move      01                   to   rf-mms-cod-dpz         .
           move      mms-cod-dsl          to   rf-mms-cod-dsl         .
           move      mms-tip-mag          to   rf-mms-tip-mag         .
           move      mms-num-mag          to   rf-mms-num-mag         .
           move      mms-var-mag          to   rf-mms-var-mag         .
           move      mms-qta-ini          to   rf-mms-qta-ini         .
           move      zero                 to   w-cix-ctr-001          .
       exe-cnv-mms-420.
           add       1                    to   w-cix-ctr-001          .
           if        w-cix-ctr-001        >    12
                     go to exe-cnv-mms-440.
           move      mms-prg-car
                    (w-cix-ctr-001)       to   rf-mms-prg-car
                                              (w-cix-ctr-001)         .
           move      mms-prg-sca
                    (w-cix-ctr-001)       to   rf-mms-prg-sca
                                              (w-cix-ctr-001)         .
           go to     exe-cnv-mms-420.
       exe-cnv-mms-440.
           move      mms-dip-ife          to   rf-mms-dip-ife         .
           move      mms-din-ife          to   rf-mms-din-ife         .
           move      mms-alx-exp          to   rf-mms-alx-exp         .
       exe-cnv-mms-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-mms]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-mms-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-mms-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-mms-250.
       exe-cnv-mms-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-mms]                        *
      *                  *---------------------------------------------*
           close     mms                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-mms]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
       exe-cnv-mms-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-mms-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-mms] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-mms-999.
           exit.

      *    *===========================================================*
      *    * Conversione [fit]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-fit-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "fit "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-fit-999.
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
       exe-cnv-fit-050.
      *              *-------------------------------------------------*
      *              * Rename file originale ed estrazione pathname    *
      *              * file originale in f-xxx-pat, e prompts per      *
      *              * l'esecuzione                                    *
      *              *-------------------------------------------------*
           perform   fil-ren-ini-000      thru fil-ren-ini-999        .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      f-xxx-pat            to   f-fit-pat              .
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
           move      w-det-rec-fil-rec    to   f-xxx-nrf              .
       exe-cnv-fit-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [old-fit]                         *
      *                  *---------------------------------------------*
           open      i-o    fit                                       .
      *                  *---------------------------------------------*
      *                  * Open file [new-fit]                         *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       exe-cnv-fit-200.
      *              *-------------------------------------------------*
      *              * Start su [old-fit]                              *
      *              *-------------------------------------------------*
           move      low-values           to   fit-k01                .
           start     fit    key not less
                            fit-k01
                            invalid key
                            go to exe-cnv-fit-800.
       exe-cnv-fit-250.
      *              *-------------------------------------------------*
      *              * Next su [old-fit]                               *
      *              *-------------------------------------------------*
           read      fit    next
                            with no lock
                            at end
                            go to exe-cnv-fit-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-fit-280.
      *              *-------------------------------------------------*
      *              * Eventuali selezioni su [old-fit]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni                                   *
      *                  *---------------------------------------------*
           if        fit-cod-dpz          =    04
                     go to exe-cnv-fit-300.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-cnv-fit-250.
       exe-cnv-fit-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new-fit]           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       exe-cnv-fit-350.
      *              *-------------------------------------------------*
      *              * Composizione [new-fit]                          *
      *              *-------------------------------------------------*
       exe-cnv-fit-400.
      *                  *---------------------------------------------*
      *                  * Ripresa record precedente                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-fit                 .
           move      01                   to   rf-fit-cod-dpz         .
           move      fit-ide-dat          to   rf-fit-ide-dat         .
           move      fit-ide-ute          to   rf-fit-ide-ute         .
           move      fit-ide-fas          to   rf-fit-ide-fas         .
           move      fit-num-prt          to   rf-fit-num-prt         .
           move      fit-cod-tmo          to   rf-fit-cod-tmo         .
           move      fit-tip-doc          to   rf-fit-tip-doc         .
           move      fit-org-doc          to   rf-fit-org-doc         .
           move      fit-num-giv          to   rf-fit-num-giv         .
           move      fit-dat-doc          to   rf-fit-dat-doc         .
           move      fit-num-doc          to   rf-fit-num-doc         .
           move      fit-scl-ann          to   rf-fit-scl-ann         .
           move      fit-sgl-num          to   rf-fit-sgl-num         .
           move      fit-cod-cli          to   rf-fit-cod-cli         .
           move      fit-dpz-cli          to   rf-fit-dpz-cli         .
           move      fit-tip-frn          to   rf-fit-tip-frn         .
           move      fit-cli-plf          to   rf-fit-cli-plf         .
           move      fit-dpc-plf          to   rf-fit-dpc-plf         .
           move      fit-cod-lng          to   rf-fit-cod-lng         .
           move      fit-sgl-vpf          to   rf-fit-sgl-vpf         .
           move      fit-dec-vpf          to   rf-fit-dec-vpf         .
           move      fit-tdc-vpf          to   rf-fit-tdc-vpf         .
           move      fit-cdc-vpf          to   rf-fit-cdc-vpf         .
           move      fit-ass-iva          to   rf-fit-ass-iva         .
           move      fit-ctp-ven          to   rf-fit-ctp-ven         .
           move      fit-inl-dcm          to   rf-fit-inl-dcm         .
           move      fit-inl-pgt          to   rf-fit-inl-pgt         .
           move      fit-cod-lst          to   rf-fit-cod-lst         .
           move      fit-csr-aac          to   rf-fit-csr-aac         .
           move      fit-psr-aac (1)      to   rf-fit-psr-aac (1)     .
           move      fit-psr-aac (2)      to   rf-fit-psr-aac (2)     .
           move      fit-psr-aac (3)      to   rf-fit-psr-aac (3)     .
           move      fit-psr-aac (4)      to   rf-fit-psr-aac (4)     .
           move      fit-psr-aac (5)      to   rf-fit-psr-aac (5)     .
           move      fit-csc-aac          to   rf-fit-csc-aac         .
           move      fit-psc-aac          to   rf-fit-psc-aac         .
           move      fit-cpv-aac          to   rf-fit-cpv-aac         .
           move      fit-ppv-aac (1)      to   rf-fit-ppv-aac (1)     .
           move      fit-ppv-aac (2)      to   rf-fit-ppv-aac (2)     .
           move      fit-ppv-aac (3)      to   rf-fit-ppv-aac (3)     .
           move      fit-voc-des (1)      to   rf-fit-voc-des (1)     .
           move      fit-voc-des (2)      to   rf-fit-voc-des (2)     .
           move      fit-voc-des (3)      to   rf-fit-voc-des (3)     .
           move      fit-voc-des (4)      to   rf-fit-voc-des (4)     .
           move      fit-voc-des (5)      to   rf-fit-voc-des (5)     .
           move      fit-voc-des (6)      to   rf-fit-voc-des (6)     .
           move      fit-cod-fop          to   rf-fit-cod-fop         .
           move      fit-scp-aap          to   rf-fit-scp-aap         .
           move      fit-cod-abi          to   rf-fit-cod-abi         .
           move      fit-cod-cab          to   rf-fit-cod-cab         .
           move      fit-ccc-app          to   rf-fit-ccc-app         .
           move      fit-nos-ban          to   rf-fit-nos-ban         .
           move      fit-nos-ccp          to   rf-fit-nos-ccp         .
           move      fit-add-spb          to   rf-fit-add-spb         .
           move      fit-ipr-iel          to   rf-fit-ipr-iel         .
           move      fit-pag-dsm          to   rf-fit-pag-dsm         .
           move      fit-pag-qaf          to   rf-fit-pag-qaf         .
           move      fit-pag-act          to   rf-fit-pag-act         .
           move      fit-cod-age          to   rf-fit-cod-age         .
           move      fit-fsp-doc          to   rf-fit-fsp-doc         .
           move      fit-pvf-age          to   rf-fit-pvf-age         .
           move      fit-tip-vpa          to   rf-fit-tip-vpa         .
           move      fit-cpv-aaa          to   rf-fit-cpv-aaa         .
           move      fit-ppv-aaa (1)      to   rf-fit-ppv-aaa (1)     .
           move      fit-ppv-aaa (2)      to   rf-fit-ppv-aaa (2)     .
           move      fit-ppv-aaa (3)      to   rf-fit-ppv-aaa (3)     .
           move      fit-cod-ime          to   rf-fit-cod-ime         .
           move      fit-pvf-ime          to   rf-fit-pvf-ime         .
           move      fit-tot-rig (1)      to   rf-fit-tot-rig (1)     .
           move      fit-tot-rig (2)      to   rf-fit-tot-rig (2)     .
           move      fit-tot-rig (3)      to   rf-fit-tot-rig (3)     .
           move      fit-tot-rig (4)      to   rf-fit-tot-rig (4)     .
           move      fit-tot-rig (5)      to   rf-fit-tot-rig (5)     .
           move      fit-tot-rig (6)      to   rf-fit-tot-rig (6)     .
           move      fit-tot-rig (7)      to   rf-fit-tot-rig (7)     .
           move      fit-tot-rig (8)      to   rf-fit-tot-rig (8)     .
           move      fit-tot-rig (9)      to   rf-fit-tot-rig (9)     .
           move      fit-tot-scc          to   rf-fit-tot-scc         .
           move      fit-per-scc          to   rf-fit-per-scc         .
           move      fit-civ-scc          to   rf-fit-civ-scc         .
           move      fit-ccp-scc          to   rf-fit-ccp-scc         .
           move      fit-tot-scp          to   rf-fit-tot-scp         .
           move      fit-per-scp          to   rf-fit-per-scp         .
           move      fit-civ-scp          to   rf-fit-civ-scp         .
           move      fit-ccp-scp          to   rf-fit-ccp-scp         .
           move      zero                 to   w-c01                  .
       exe-cnv-fit-420.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-fit-450.
           move      fit-spe-snx (w-c01)  to   rf-fit-spe-snx (w-c01) .
           move      fit-spe-mad (w-c01)  to   rf-fit-spe-mad (w-c01) .
           move      fit-spe-per (w-c01)  to   rf-fit-spe-per (w-c01) .
           move      fit-spe-ibl (w-c01)  to   rf-fit-spe-ibl (w-c01) .
           move      zero                 to   w-c02                  .
       exe-cnv-fit-430.
           add       1                    to   w-c02                  .
           if        w-c02                >    9
                     go to exe-cnv-fit-440.
           move      fit-ibx-spe
                    (w-c01, w-c02)        to   rf-fit-ibx-spe
                                               (w-c01, w-c02)         .
           go to     exe-cnv-fit-430.
       exe-cnv-fit-440.
           move      fit-spe-imp (w-c01)  to   rf-fit-spe-imp (w-c01) .
           move      fit-spe-civ (w-c01)  to   rf-fit-spe-civ (w-c01) .
           move      fit-spe-ccp (w-c01)  to   rf-fit-spe-ccp (w-c01) .
           go to     exe-cnv-fit-420.
       exe-cnv-fit-450.
           move      fit-add-spi          to   rf-fit-add-spi         .
           move      fit-civ-spi          to   rf-fit-civ-spi         .
           move      fit-ccp-spi          to   rf-fit-ccp-spi         .
           move      fit-tot-sic          to   rf-fit-tot-sic         .
           move      fit-tot-sia          to   rf-fit-tot-sia         .
           move      fit-tot-spb          to   rf-fit-tot-spb         .
           move      fit-civ-spb          to   rf-fit-civ-spb         .
           move      fit-ccp-spb          to   rf-fit-ccp-spb         .
           move      fit-prt-mgd          to   rf-fit-prt-mgd         .
           move      fit-nrg-mgd          to   rf-fit-nrg-mgd         .
           move      fit-dri-mgd          to   rf-fit-dri-mgd         .
           move      fit-nri-mgd          to   rf-fit-nri-mgd         .
           move      fit-nps-sdb          to   rf-fit-nps-sdb         .
           move      fit-ctr-sdb          to   rf-fit-ctr-sdb         .
           move      zero                 to   w-c01                  .
       exe-cnv-fit-500.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to exe-cnv-fit-520.
           move      fit-iva-cod (w-c01)  to   rf-fit-iva-cod (w-c01) .
           move      fit-iva-ibl (w-c01)  to   rf-fit-iva-ibl (w-c01) .
           move      fit-iva-imp (w-c01)  to   rf-fit-iva-imp (w-c01) .
           go to     exe-cnv-fit-500.
       exe-cnv-fit-520.
           move      fit-iva-tdo          to   rf-fit-iva-tdo         .
           move      zero                 to   w-c01                  .
       exe-cnv-fit-600.
           add       1                    to   w-c01                  .
           if        w-c01                >    10
                     go to exe-cnv-fit-620.
           move      fit-ctp-cod (w-c01)  to   rf-fit-ctp-cod (w-c01) .
           move      fit-ctp-imp (w-c01)  to   rf-fit-ctp-imp (w-c01) .
           go to     exe-cnv-fit-600.
       exe-cnv-fit-620.
           move      fit-ctr-stp          to   rf-fit-ctr-stp         .
           move      fit-flg-blx (1)      to   rf-fit-flg-blx (1)     .
           move      fit-flg-blx (2)      to   rf-fit-flg-blx (2)     .
           move      fit-flg-blx (3)      to   rf-fit-flg-blx (3)     .
           move      fit-flg-blx (4)      to   rf-fit-flg-blx (4)     .
           move      fit-flg-blx (5)      to   rf-fit-flg-blx (5)     .
           move      fit-flg-blx (6)      to   rf-fit-flg-blx (6)     .
           move      fit-flg-blx (7)      to   rf-fit-flg-blx (7)     .
           move      fit-flg-nbx (1)      to   rf-fit-flg-nbx (1)     .
           move      fit-flg-nbx (2)      to   rf-fit-flg-nbx (2)     .
           move      fit-flg-nbx (3)      to   rf-fit-flg-nbx (3)     .
           move      fit-flg-pul          to   rf-fit-flg-pul         .
           move      fit-alx-exp          to   rf-fit-alx-exp         .
       exe-cnv-fit-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new-fit]                             *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-fit-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-fit-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-fit-250.
       exe-cnv-fit-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [old-fit]                        *
      *                  *---------------------------------------------*
           close     fit                                              .
      *                  *---------------------------------------------*
      *                  * Close file [new-fit]                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       exe-cnv-fit-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-fit-900.
      *              *-------------------------------------------------*
      *              * Delete file [old-fit] e scrittura rullino       *
      *              * messaggi                                        *
      *              *-------------------------------------------------*
           perform   fil-del-fin-000      thru fil-del-fin-999        .
       exe-cnv-fit-999.
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
       exe-cnv-fir-280.
      *              *-------------------------------------------------*
      *              * Eventuali selezioni su [old-fir]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezioni                                   *
      *                  *---------------------------------------------*
           if        fir-cod-dpz          =    04
                     go to exe-cnv-fir-300.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-cnv-fir-250.
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
           move      01                   to   rf-fir-cod-dpz         .
           move      fir-num-prt          to   rf-fir-num-prt         .
           move      fir-num-prg          to   rf-fir-num-prg         .
           move      fir-cod-tmo          to   rf-fir-cod-tmo         .
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
           if        f-xxx-sts            =    "S"
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

