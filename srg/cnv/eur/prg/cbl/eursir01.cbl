       Identification Division.
       Program-Id.                                 eursir01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    eur                 *
      *                                   Fase:    eursir01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/00    *
      *                       Ultima revisione:    NdK del 17/05/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per EURO - SIRI                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Copia archivi                               *
      *                                                                *
      * {agb}   [hrr]      Tabella richieste di selezione salvate      *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Conversioni                                 *
      *                                                                *
      * {orc} v [ocr]      Righe ordine cliente                        *
      * {agb} v [hfc]      Saldi per statistiche di vendita            *
      * {mag} v [hpr]      File di appoggio prodotti                   *
      * {orc} v [hoc]      File di appoggio giornaliero                *
      * {orc} v [hop]      File di appoggio assegnazione        [S.C.] *
      * {dcf} v [aaf]      Anagrafiche di acquisto                     *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Assestamento inevasi                        *
      *                                                                *
      * {ORC}   [ocr] v  Ordini clienti, righe                         *
      * {ORC}   [oct] v  Ordini clienti, testate                       *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Note           : [S.C.] = Senza conversioni                    *
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
      *    * File Control [ocr] per SIRI                               *
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
      *    * File Control [hfc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hfc   assign to disk           f-hfc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hfc-k01
                   alternate record key   is hfc-k02
                             file status  is                f-hfc-sts .

      *    *===========================================================*
      *    * File Control [hpr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hpr   assign to disk           f-hpr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hpr-k01
                             file status  is                f-hpr-sts .

      *    *===========================================================*
      *    * File Control [hoc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hoc   assign to disk           f-hoc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hoc-k01
                   alternate record key   is hoc-k02
                   alternate record key   is hoc-k03
                             file status  is                f-hoc-sts .

      *    *===========================================================*
      *    * File Control [hop]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hop   assign to disk           f-hop-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hop-k01
                             file status  is                f-hop-sts .

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
      *    * File Control [kk1]                                        *
      *    *-----------------------------------------------------------*
           select  optional  kk1   assign to disk           f-kk1-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is kk1-key
                             file status  is                f-kk1-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

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
      *    * File Description [hfc]                                    *
      *    *-----------------------------------------------------------*
       fd  hfc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hfc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hfc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : ESECLIAGE                      *
      *            *---------------------------------------------------*
               10  hfc-k01.
                   15  hfc-ann-ese        pic  9(03)       comp-3     .
                   15  hfc-cod-cli        pic  9(07)       comp-3     .
                   15  hfc-cod-age        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : ESEAGECLI                      *
      *            *---------------------------------------------------*
               10  hfc-k02.
                   15  hfc-ann-ese-2      pic  9(03)       comp-3     .
                   15  hfc-cod-age-2      pic  9(07)       comp-3     .
                   15  hfc-cod-cli-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hfc-dat.
               10  hfc-dat-mns.
                   15  hfc-mes-mns occurs 12.
                       20  hfc-fat-mns    pic s9(13)       comp-3     .
               10  hfc-ale-alf.
                   15  filler  occurs 20  pic  x(01)                  .
               10  hfc-ale-num.
                   15  filler  occurs 20  pic  9(01)                  .

      *    *===========================================================*
      *    * File Description [hpr]                                    *
      *    *-----------------------------------------------------------*
       fd  hpr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hpr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hpr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  hpr-k01.
                   15  hpr-num-pro        pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hpr-dat.
               10  hpr-dur-cmc            pic  9(07)       comp-3     .
               10  hpr-val-cmc            pic  9(11)       comp-3     .
               10  hpr-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hoc]                                    *
      *    *-----------------------------------------------------------*
       fd  hoc       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hoc-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hoc-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : DATPRT                         *
      *            *---------------------------------------------------*
               10  hoc-k01.
                   15  hoc-dat-ela-1      pic  9(07)       comp-3     .
                   15  hoc-num-prt-1      pic  9(11)       comp-3     .
                   15  hoc-num-prg-1      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRTDAT                         *
      *            *---------------------------------------------------*
               10  hoc-k02.
                   15  hoc-num-prt-2      pic  9(11)       comp-3     .
                   15  hoc-num-prg-2      pic  9(05)       comp-3     .
                   15  hoc-dat-ela-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : CDIPRT                         *
      *            *---------------------------------------------------*
               10  hoc-k03.
                   15  hoc-dat-ela-3      pic  9(07)       comp-3     .
                   15  hoc-flg-cdi-3      pic  x(01)                  .
                   15  hoc-num-prt-3      pic  9(11)       comp-3     .
                   15  hoc-num-prg-3      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hoc-dat.
               10  hoc-dat-ela            pic  9(07)       comp-3     .
               10  hoc-num-prt            pic  9(11)       comp-3     .
               10  hoc-num-prg            pic  9(05)       comp-3     .
               10  hoc-flg-cdi            pic  x(01)                  .
               10  hoc-num-pro            pic  9(07)       comp-3     .
               10  hoc-alf-pro            pic  x(14)                  .
               10  hoc-qta-ord            pic s9(06)v9(03) comp-3     .
               10  hoc-prz-ven            pic  9(09)       comp-3     .
               10  hoc-sgl-vpl            pic  x(03)                  .
               10  hoc-ccr-vpl            pic  9(06)v9(05) comp-3     .
               10  hoc-plm-vpl            pic  9(01)v9(02) comp-3     .
               10  hoc-tlm-vpl            pic  x(01)                  .
               10  hoc-per-scr occurs 05  pic  9(02)v9(01) comp-3     .
               10  hoc-prz-net            pic  9(09)       comp-3     .
               10  hoc-imp-rig            pic s9(11)       comp-3     .
               10  hoc-dcn-ric            pic  9(07)       comp-3     .
               10  hoc-dcn-prv            pic  9(07)       comp-3     .
               10  hoc-flg-cnf            pic  x(01)                  .
               10  hoc-flg-ela.
                   15  hoc-flg-blo.
                       20  hoc-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hoc-flg-nbl.
                       20  hoc-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hoc-flg-pul            pic  x(01)                  .
               10  hoc-ale-alf.
                   15  filler  occurs 20  pic  x(01)                  .
               10  hoc-ale-num.
                   15  filler  occurs 20  pic  9(01)                  .

      *    *===========================================================*
      *    * File Description [hop]                                    *
      *    *-----------------------------------------------------------*
       fd  hop       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hop-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hop-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : DATPRT                         *
      *            *---------------------------------------------------*
               10  hop-k01.
                   15  hop-dat-ela        pic  9(07)       comp-3     .
                   15  hop-num-prt        pic  9(11)       comp-3     .
                   15  hop-num-prg        pic  9(05)       comp-3     .
                   15  hop-prg-frm        pic  9(03)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hop-dat.
               10  hop-qta-ass            pic s9(06)v9(03) comp-3     .
               10  hop-dat-ass            pic  9(07)       comp-3     .
               10  hop-prt-orf            pic  9(11)       comp-3     .
               10  hop-prg-orf            pic  9(05)       comp-3     .
               10  hop-dcn-prv            pic  9(07)       comp-3     .
               10  hop-flg-cnf            pic  x(01)                  .
               10  hop-flg-ela.
                   15  hop-flg-blo.
                       20  hop-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hop-flg-nbl.
                       20  hop-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hop-flg-pul            pic  x(01)                  .
               10  hop-alx-exp.
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
      *    * File Description [kk1]                                    *
      *    *-----------------------------------------------------------*
       fd  kk1       label record standard                            .

      *    *===========================================================*
      *    * Record file di appoggio [kk1]                             *
      *    *-----------------------------------------------------------*
       01  kk1-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  kk1-key.
               10  kk1-num-prt            pic  9(11)                  .
               10  kk1-tip-rec            pic  x(01)                  .
               10  kk1-num-prg            pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  kk1-dat.
               10  filler                 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [ocr]                                       *
      *    *-----------------------------------------------------------*
       01  f-ocr.
           05  f-ocr-nam                  pic  x(04)                  .
           05  f-ocr-pat                  pic  x(40)                  .
           05  f-ocr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hfc]                                       *
      *    *-----------------------------------------------------------*
       01  f-hfc.
           05  f-hfc-nam                  pic  x(04)                  .
           05  f-hfc-pat                  pic  x(40)                  .
           05  f-hfc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hpr]                                       *
      *    *-----------------------------------------------------------*
       01  f-hpr.
           05  f-hpr-nam                  pic  x(04)                  .
           05  f-hpr-pat                  pic  x(40)                  .
           05  f-hpr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hoc]                                       *
      *    *-----------------------------------------------------------*
       01  f-hoc.
           05  f-hoc-nam                  pic  x(04)                  .
           05  f-hoc-pat                  pic  x(40)                  .
           05  f-hoc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hop]                                       *
      *    *-----------------------------------------------------------*
       01  f-hop.
           05  f-hop-nam                  pic  x(04)                  .
           05  f-hop-pat                  pic  x(40)                  .
           05  f-hop-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [aaf]                                       *
      *    *-----------------------------------------------------------*
       01  f-aaf.
           05  f-aaf-nam                  pic  x(04)                  .
           05  f-aaf-pat                  pic  x(40)                  .
           05  f-aaf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [kk1]                                       *
      *    *-----------------------------------------------------------*
       01  f-kk1.
           05  f-kk1-nam                  pic  x(04)                  .
           05  f-kk1-pat                  pic  x(40)                  .
           05  f-kk1-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [hfc]                                                 *
      *        *-------------------------------------------------------*
           copy      "mcm/agb/fls/rec/rfhfc"                          .
      *        *-------------------------------------------------------*
      *        * [hpr]                                                 *
      *        *-------------------------------------------------------*
           copy      "mcm/mag/fls/rec/rfhpr"                          .
      *        *-------------------------------------------------------*
      *        * [hoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "mcm/orc/fls/rec/rfhoc"                          .
      *        *-------------------------------------------------------*
      *        * [hop]                                                 *
      *        *-------------------------------------------------------*
           copy      "mcm/orc/fls/rec/rfhop"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .

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
                     "eursir"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "eursir01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONI PER EURO - Siri       "       .

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
           05  w-cdc                      pic  9(06)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Work per messaggi errore                              *
      *        *-------------------------------------------------------*
           05  w-ide-dat                  pic  x(08)                  .
           05  w-ide-saa                  pic  x(03)                  .
           05  w-ide-mes                  pic  x(02)                  .
           05  w-ide-arc                  pic  x(07)                  .
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
           05  w-wrk-cmd-prz              pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det quantita' secondaria                     *
      *        *-------------------------------------------------------*
           05  w-det-qta-a02.
      *            *---------------------------------------------------*
      *            * Quantita' impostata                       [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-a02-qim      pic s9(06)v9(03)            .
      *            *---------------------------------------------------*
      *            * Numero decimali                           [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-a02-ndu      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Coeff. moltiplicatore per trasformazione  [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-a02-cmu      pic  9(06)v9(03)            .
      *            *---------------------------------------------------*
      *            * Coeff. divisore per trasformazione        [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-a02-cdu      pic  9(06)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' secondaria determinata         [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-a02-qts      pic s9(06)v9(03)            .
      *            *---------------------------------------------------*
      *            * Work-area di lavoro                               *
      *            *---------------------------------------------------*
               10  w-det-qta-a02-wpq      pic s9(15)v9(03)            .
               10  w-det-qta-a02-wq0      pic s9(18)                  .
               10  w-det-qta-a02-wq1      pic s9(17)v9(01)            .
               10  w-det-qta-a02-wq2      pic s9(16)v9(02)            .
               10  w-det-qta-a02-wq3      pic s9(15)v9(03)            .

      *    *===========================================================*
      *    * Work area per tabella archivi                             *
      *    *-----------------------------------------------------------*
       01  w-arc.
           05  w-arc-ele.
               10  w-arc-ele-max          pic  9(03) value 001        .
               10  w-arc-ele-tbl.
                   15  filler             pic  x(55) value
             "[hrr]      Tabella richieste di selezione salvate      ".
               10  w-arc-ele-tbr redefines
                   w-arc-ele-tbl.
                   15  w-arc-ele-ele occurs 001.
                       20  filler         pic  x(01)                  .
                       20  w-arc-ele-nam  pic  x(03)                  .
                       20  filler         pic  x(05)                  .
                       20  w-arc-ele-des  pic  x(46)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dtl"                   .

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
      *              * Conversione [ocr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-ocr-000      thru exe-cnv-ocr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hfc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hfc-000      thru exe-cnv-hfc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hpr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hpr-000      thru exe-cnv-hpr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hoc]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hoc-000      thru exe-cnv-hoc-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hop]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hop-000      thru exe-cnv-hop-999        .
      *              *-------------------------------------------------*
      *              * Conversione [aaf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-aaf-000      thru exe-cnv-aaf-999        .
      *              *-------------------------------------------------*
      *              * Assestamento ordini clienti inevasi             *
      *              *-------------------------------------------------*
           perform   exe-ass-orc-000      thru exe-ass-orc-999        .
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
           move      zero                 to   v-dec                  .
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
       exe-cnv-ocr-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-ocr-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-ocr-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    ocr                                       .
       exe-cnv-ocr-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   ocr-k01                .
           start     ocr    key not less
                            ocr-k01
                            invalid key
                            go to exe-cnv-ocr-800.
       exe-cnv-ocr-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
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
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-ocr-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-cnv-ocr-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
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
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
       exe-cnv-ocr-600.
      *                  *---------------------------------------------*
      *                  * Importo provvigione a forfait, valuta base  *
      *                  *---------------------------------------------*
           move      ocr-pvf-rig          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ocr-pvf-rig         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-ocr-msg-000
                                          thru exe-cnv-ocr-msg-999    .
       exe-cnv-ocr-620.
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      ocr-sgl-vpf          to   w-exe-cnv-val-sgl      .
           move      ocr-dec-vpf          to   w-exe-cnv-val-dec      .
           move      ocr-tdc-vpf          to   w-exe-cnv-val-tdc      .
           move      ocr-cdc-vpf          to   w-exe-cnv-val-cdc      .
           move      ocr-dat-doc          to   w-exe-cnv-val-dat      .
      *
           perform   exe-cnv-val-000      thru exe-cnv-val-999        .
      *
           move      w-exe-cnv-val-sgl    to   rf-ocr-sgl-vpf         .
           move      w-exe-cnv-val-dec    to   rf-ocr-dec-vpf         .
           move      w-exe-cnv-val-tdc    to   rf-ocr-tdc-vpf         .
           move      w-exe-cnv-val-cdc    to   rf-ocr-cdc-vpf         .
      *                  *---------------------------------------------*
      *                  * Valuta per prezzo standard                  *
      *                  *---------------------------------------------*
           move      ocr-sgl-vps          to   w-exe-cnv-val-sgl      .
           move      ocr-dec-vps          to   w-exe-cnv-val-dec      .
           move      ocr-tdc-vps          to   w-exe-cnv-val-tdc      .
           move      ocr-cdc-vps          to   w-exe-cnv-val-cdc      .
           move      ocr-dat-doc          to   w-exe-cnv-val-dat      .
      *
           perform   exe-cnv-val-000      thru exe-cnv-val-999        .
      *
           move      w-exe-cnv-val-sgl    to   rf-ocr-sgl-vps         .
           move      w-exe-cnv-val-dec    to   rf-ocr-dec-vps         .
           move      w-exe-cnv-val-tdc    to   rf-ocr-tdc-vps         .
           move      w-exe-cnv-val-cdc    to   rf-ocr-cdc-vps         .
      *                  *---------------------------------------------*
      *                  * Valuta per prezzo di vendita                *
      *                  *---------------------------------------------*
           move      ocr-sgl-vpp          to   w-exe-cnv-val-sgl      .
           move      ocr-dec-vpp          to   w-exe-cnv-val-dec      .
           move      ocr-tdc-vpp          to   w-exe-cnv-val-tdc      .
           move      ocr-cdc-vpp          to   w-exe-cnv-val-cdc      .
           move      ocr-dat-doc          to   w-exe-cnv-val-dat      .
      *
           perform   exe-cnv-val-000      thru exe-cnv-val-999        .
      *
           move      w-exe-cnv-val-sgl    to   rf-ocr-sgl-vpp         .
           move      w-exe-cnv-val-dec    to   rf-ocr-dec-vpp         .
           move      w-exe-cnv-val-tdc    to   rf-ocr-tdc-vpp         .
           move      w-exe-cnv-val-cdc    to   rf-ocr-cdc-vpp         .
      *                  *---------------------------------------------*
      *                  * Valuta per il legame valutario              *
      *                  *---------------------------------------------*
           move      ocr-sgl-vpl          to   w-exe-cnv-lgv-sgl      .
           move      ocr-dec-vpl          to   w-exe-cnv-lgv-dec      .
           move      ocr-tdc-vpl          to   w-exe-cnv-lgv-tdc      .
           move      ocr-prz-vpl          to   w-exe-cnv-lgv-prz      .
           move      ocr-cdc-vpl          to   w-exe-cnv-lgv-cdc      .
           move      ocr-ccr-vpl          to   w-exe-cnv-lgv-ccr      .
           move      ocr-plm-vpl          to   w-exe-cnv-lgv-plm      .
           move      ocr-tlm-vpl          to   w-exe-cnv-lgv-tlm      .
           move      ocr-map-vpl          to   w-exe-cnv-lgv-map      .
           move      ocr-dat-doc          to   w-exe-cnv-lgv-dat      .
      *
           perform   exe-cnv-lgv-000      thru exe-cnv-lgv-999        .
      *
           move      w-exe-cnv-lgv-sgl    to   rf-ocr-sgl-vpl         .
           move      w-exe-cnv-lgv-dec    to   rf-ocr-dec-vpl         .
           move      w-exe-cnv-lgv-tdc    to   rf-ocr-tdc-vpl         .
           move      w-exe-cnv-lgv-prz    to   rf-ocr-prz-vpl         .
           move      w-exe-cnv-lgv-cdc    to   rf-ocr-cdc-vpl         .
           move      w-exe-cnv-lgv-ccr    to   rf-ocr-ccr-vpl         .
           move      w-exe-cnv-lgv-plm    to   rf-ocr-plm-vpl         .
           move      w-exe-cnv-lgv-tlm    to   rf-ocr-tlm-vpl         .
           move      w-exe-cnv-lgv-map    to   rf-ocr-map-vpl         .
      *                  *---------------------------------------------*
      *                  * Valuta per il costo di riferimento          *
      *                  *---------------------------------------------*
           move      ocr-sgl-vpc          to   w-exe-cnv-val-sgl      .
           move      ocr-dec-vpc          to   w-exe-cnv-val-dec      .
           move      ocr-tdc-vpc          to   w-exe-cnv-val-tdc      .
           move      ocr-cdc-vpc          to   w-exe-cnv-val-cdc      .
           move      ocr-dat-doc          to   w-exe-cnv-val-dat      .
      *
           perform   exe-cnv-val-000      thru exe-cnv-val-999        .
      *
           move      w-exe-cnv-val-sgl    to   rf-ocr-sgl-vpc         .
           move      w-exe-cnv-val-dec    to   rf-ocr-dec-vpc         .
           move      w-exe-cnv-val-tdc    to   rf-ocr-tdc-vpc         .
           move      w-exe-cnv-val-cdc    to   rf-ocr-cdc-vpc         .
       exe-cnv-ocr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
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
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     ocr                                              .
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
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
       exe-cnv-ocr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [ocr]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-ocr-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      ocr-dat-doc          to   v-num                  .
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
           move      ocr-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      ocr-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
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
                     " - "      delimited by   size
                     w-ide-prg  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-ocr-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hfc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hfc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hfc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "mcm/agb/fls/ioc/obj/iofhfc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hfc-999.
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
       exe-cnv-hfc-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hfc-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hfc-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hfc                                       .
       exe-cnv-hfc-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hfc-k01                .
           start     hfc    key not less
                            hfc-k01
                            invalid key
                            go to exe-cnv-hfc-800.
       exe-cnv-hfc-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hfc    next
                            with no lock
                            at end
                            go to exe-cnv-hfc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hfc-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hfc-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
       exe-cnv-hfc-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hfc                 .
           move      hfc-ann-ese          to   rf-hfc-ann-ese         .
           move      hfc-cod-cli          to   rf-hfc-cod-cli         .
           move      hfc-cod-age          to   rf-hfc-cod-age         .
           move      zero                 to   w-c01                  .
       exe-cnv-hfc-410.
           add       1                    to   w-c01                  .
           if        w-c01                >    12
                     go to exe-cnv-hfc-420.
           move      hfc-fat-mns (w-c01)  to   rf-hfc-fat-mns (w-c01) .
           go to     exe-cnv-hfc-410.
       exe-cnv-hfc-420.
           move      hfc-ale-alf          to   rf-hfc-ale-alf         .
           move      hfc-ale-num          to   rf-hfc-ale-num         .
       exe-cnv-hfc-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori mensili                              *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-cnv-hfc-520.
           add       1                    to   w-inx                  .
           if        w-inx                >    12
                     go to exe-cnv-hfc-700.
      *                  *---------------------------------------------*
      *                  * Totale mensile                              *
      *                  *---------------------------------------------*
           move      hfc-fat-mns (w-inx)  to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hfc-fat-mns (w-inx) .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hfc-msg-000
                                          thru exe-cnv-hfc-msg-999    .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-cnv-hfc-520.
       exe-cnv-hfc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hfc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hfc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hfc-250.
       exe-cnv-hfc-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hfc                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hfc                                              .
       exe-cnv-hfc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hfc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hfc-999.
       exe-cnv-hfc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hfc]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hfc-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing secolo / anno                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<9"                 to   v-edm                  .
           move      hfc-ann-ese          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-saa              .
      *                  *---------------------------------------------*
      *                  * Editing codice archivio                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hfc-cod-cli          to   v-num                  .
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
                     w-ide-saa  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-arc  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hfc-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hpr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hpr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hpr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "mcm/mag/fls/ioc/obj/iofhpr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hpr-999.
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
       exe-cnv-hpr-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpr                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hpr-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hpr-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hpr                                       .
       exe-cnv-hpr-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hpr-k01                .
           start     hpr    key not less
                            hpr-k01
                            invalid key
                            go to exe-cnv-hpr-800.
       exe-cnv-hpr-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hpr    next
                            with no lock
                            at end
                            go to exe-cnv-hpr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hpr-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hpr-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpr                 .
       exe-cnv-hpr-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hpr                 .
           move      hpr-num-pro          to   rf-hpr-num-pro         .
           move      hpr-dur-cmc          to   rf-hpr-dur-cmc         .
           move      hpr-val-cmc          to   rf-hpr-val-cmc         .
           move      hpr-alx-exp          to   rf-hpr-alx-exp         .
       exe-cnv-hpr-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore costo medio continuo                 *
      *                  *---------------------------------------------*
           move      hpr-val-cmc          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hpr-val-cmc         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hpr-msg-000
                                          thru exe-cnv-hpr-msg-999    .
       exe-cnv-hpr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hpr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hpr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hpr-250.
       exe-cnv-hpr-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hpr                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hpr                                              .
       exe-cnv-hpr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hpr-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hpr-999.
       exe-cnv-hpr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hpr]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hpr-msg-000.
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
           move      hpr-num-pro          to   v-num                  .
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
                     w-ide-saa  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-arc  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hpr-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hoc]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hoc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hoc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "mcm/orc/fls/ioc/obj/iofhoc"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hoc-999.
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
       exe-cnv-hoc-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hoc                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hoc-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hoc-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hoc                                       .
       exe-cnv-hoc-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hoc-k01                .
           start     hoc    key not less
                            hoc-k01
                            invalid key
                            go to exe-cnv-hoc-800.
       exe-cnv-hoc-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hoc    next
                            with no lock
                            at end
                            go to exe-cnv-hoc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hoc-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hoc-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hoc                 .
       exe-cnv-hoc-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hoc                 .
           move      hoc-dat-ela          to   rf-hoc-dat-ela         .
           move      hoc-num-prt          to   rf-hoc-num-prt         .
           move      hoc-num-prg          to   rf-hoc-num-prg         .
           move      hoc-flg-cdi          to   rf-hoc-flg-cdi         .
           move      hoc-num-pro          to   rf-hoc-num-pro         .
           move      hoc-alf-pro          to   rf-hoc-alf-pro         .
           move      hoc-qta-ord          to   rf-hoc-qta-ord         .
           move      hoc-prz-ven          to   rf-hoc-prz-ven         .
           move      hoc-sgl-vpl          to   rf-hoc-sgl-vpl         .
           move      hoc-ccr-vpl          to   rf-hoc-ccr-vpl         .
           move      hoc-plm-vpl          to   rf-hoc-plm-vpl         .
           move      hoc-tlm-vpl          to   rf-hoc-tlm-vpl         .
           move      hoc-per-scr (1)      to   rf-hoc-per-scr (1)     .
           move      hoc-per-scr (2)      to   rf-hoc-per-scr (2)     .
           move      hoc-per-scr (3)      to   rf-hoc-per-scr (3)     .
           move      hoc-per-scr (4)      to   rf-hoc-per-scr (4)     .
           move      hoc-per-scr (5)      to   rf-hoc-per-scr (5)     .
           move      hoc-prz-net          to   rf-hoc-prz-net         .
           move      hoc-imp-rig          to   rf-hoc-imp-rig         .
           move      hoc-dcn-ric          to   rf-hoc-dcn-ric         .
           move      hoc-dcn-prv          to   rf-hoc-dcn-prv         .
           move      hoc-flg-cnf          to   rf-hoc-flg-cnf         .
           move      hoc-flg-ela          to   rf-hoc-flg-ela         .
           move      hoc-flg-pul          to   rf-hoc-flg-pul         .
           move      hoc-ale-alf          to   rf-hoc-ale-alf         .
           move      hoc-ale-num          to   rf-hoc-ale-num         .
       exe-cnv-hoc-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per il legame valutario              *
      *                  *---------------------------------------------*
           move      hoc-sgl-vpl          to   w-exe-cnv-lgv-sgl      .
           move      hoc-ccr-vpl          to   w-exe-cnv-lgv-ccr      .
           move      hoc-plm-vpl          to   w-exe-cnv-lgv-plm      .
           move      hoc-tlm-vpl          to   w-exe-cnv-lgv-tlm      .
      *
           perform   exe-cnv-lgv-000      thru exe-cnv-lgv-999        .
      *
           move      w-exe-cnv-lgv-sgl    to   rf-hoc-sgl-vpl         .
           move      w-exe-cnv-lgv-ccr    to   rf-hoc-ccr-vpl         .
           move      w-exe-cnv-lgv-plm    to   rf-hoc-plm-vpl         .
           move      w-exe-cnv-lgv-tlm    to   rf-hoc-tlm-vpl         .
       exe-cnv-hoc-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hoc                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hoc-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hoc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hoc-250.
       exe-cnv-hoc-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hoc                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hoc                                              .
       exe-cnv-hoc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hoc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hoc-999.
       exe-cnv-hoc-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hoc]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hoc-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      hoc-dat-ela          to   v-num                  .
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
           move      hoc-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hoc-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-saa  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-arc  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hoc-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hop]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hop-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hop "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "mcm/orc/fls/ioc/obj/iofhop"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hop-999.
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
       exe-cnv-hop-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hop                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hop-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hop-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hop                                       .
       exe-cnv-hop-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hop-k01                .
           start     hop    key not less
                            hop-k01
                            invalid key
                            go to exe-cnv-hop-800.
       exe-cnv-hop-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hop    next
                            with no lock
                            at end
                            go to exe-cnv-hop-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hop-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hop-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hop                 .
       exe-cnv-hop-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hop                 .
           move      hop-dat-ela          to   rf-hop-dat-ela         .
           move      hop-num-prt          to   rf-hop-num-prt         .
           move      hop-num-prg          to   rf-hop-num-prg         .
           move      hop-prg-frm          to   rf-hop-prg-frm         .
           move      hop-qta-ass          to   rf-hop-qta-ass         .
           move      hop-dat-ass          to   rf-hop-dat-ass         .
           move      hop-prt-orf          to   rf-hop-prt-orf         .
           move      hop-prg-orf          to   rf-hop-prg-orf         .
           move      hop-dcn-prv          to   rf-hop-dcn-prv         .
           move      hop-flg-cnf          to   rf-hop-flg-cnf         .
           move      hop-flg-ela          to   rf-hop-flg-ela         .
           move      hop-flg-pul          to   rf-hop-flg-pul         .
           move      hop-alx-exp          to   rf-hop-alx-exp         .
       exe-cnv-hop-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
       exe-cnv-hop-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hop                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hop-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hop-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hop-250.
       exe-cnv-hop-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hop                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hop                                              .
       exe-cnv-hop-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hop-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hop-999.
       exe-cnv-hop-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hop]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hop-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data registrazione                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      hop-dat-ela          to   v-num                  .
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
           move      hop-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Editing progressivo                         *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hop-num-prg          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prg              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-saa  delimited by   spaces
                     " - "      delimited by   size
                     w-ide-arc  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hop-msg-999.
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
       exe-cnv-aaf-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-aaf-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-aaf-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    aaf                                       .
       exe-cnv-aaf-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   aaf-k01                .
           start     aaf    key not less
                            aaf-k01
                            invalid key
                            go to exe-cnv-aaf-800.
       exe-cnv-aaf-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
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
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-aaf-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
       exe-cnv-aaf-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
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
       exe-cnv-aaf-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per il legame valutario              *
      *                  *---------------------------------------------*
           move      aaf-lgv-vlt          to   w-exe-cnv-lgv-sgl      .
           move      aaf-lgv-dcv          to   w-exe-cnv-lgv-dec      .
           move      aaf-lgv-tdc          to   w-exe-cnv-lgv-tdc      .
           move      aaf-lgv-cdc          to   w-exe-cnv-lgv-cdc      .
           move      aaf-lgv-pdt          to   w-exe-cnv-lgv-plm      .
      *
           perform   exe-cnv-lgv-000      thru exe-cnv-lgv-999        .
      *
           move      w-exe-cnv-lgv-sgl    to   rf-aaf-lgv-vlt         .
           move      w-exe-cnv-lgv-dec    to   rf-aaf-lgv-dcv         .
           move      w-exe-cnv-lgv-tdc    to   rf-aaf-lgv-tdc         .
           move      w-exe-cnv-lgv-cdc    to   rf-aaf-lgv-cdc         .
           move      w-exe-cnv-lgv-plm    to   rf-aaf-lgv-pdt         .
      *                  *---------------------------------------------*
      *                  * Prezzi per 1.000                            *
      *                  *---------------------------------------------*
           if        aaf-sgl-vlt          not  = "LIT"
                     go to exe-cnv-aaf-700.
      *
           move      aaf-prz-pes (1)      to   w-wrk-cmd-prz          .
           multiply  1000                 by   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-aaf-prz-pes (1)     .
      *
           move      "P"                  to   rf-aaf-snx-tum         .
           move      "MGL"                to   rf-aaf-umf-tum         .
           move      3                    to   rf-aaf-nde-tum         .
           move      1000                 to   rf-aaf-cmo-tum         .
           move      1                    to   rf-aaf-cdi-tum         .
      *
           move      "EUR"                to   rf-aaf-sgl-vlt         .
           move      2                    to   rf-aaf-dec-vlt         .
       exe-cnv-aaf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
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
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     aaf                                              .
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
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-aaf-999.
       exe-cnv-aaf-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *-----------------------------------------------------------*
       exe-ass-orc-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "orc "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-ass-orc-999.
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
       exe-ass-orc-100.
      *              *-------------------------------------------------*
      *              * [kk1]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta di un pathname unico alla segre-  *
      *                  * teria                                       *
      *                  *---------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-kk1-pat              .
      *                  *---------------------------------------------*
      *                  * Open file                                   *
      *                  *---------------------------------------------*
           open      i-o    kk1                                       .
       exe-ass-orc-120.
      *              *-------------------------------------------------*
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine     *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-sts-orc-tip-ope      .
           move      "pgm/orc/prg/obj/dstsorc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sts-orc
                                               rf-oct                 .
       exe-ass-orc-150.
      *              *-------------------------------------------------*
      *              * Subroutine di bufferizzazione testate e righe   *
      *              * ordini inevasi                                  *
      *              *-------------------------------------------------*
           perform   exe-ass-orc-buf-000  thru exe-ass-orc-buf-999    .
       exe-ass-orc-180.
      *              *-------------------------------------------------*
      *              * Start su file [kk1]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione chiave                      *
      *                  *---------------------------------------------*
           move      zero                 to   kk1-num-prt            .
           move      spaces               to   kk1-tip-rec            .
           move      zero                 to   kk1-num-prg            .
      *                  *---------------------------------------------*
      *                  * Operazione di Start                         *
      *                  *---------------------------------------------*
           start     kk1    key not less
                            kk1-key
                            invalid key
                            go to exe-ass-orc-800.
       exe-ass-orc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file di appoggio [kk1]      *
      *              *-------------------------------------------------*
           read      kk1    next
                            with no lock
                            at end
                            go to exe-ass-orc-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-ass-orc-300.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo record          *
      *              *-------------------------------------------------*
           if        kk1-tip-rec          =    "R"
                     go to exe-ass-orc-400
           else if   kk1-tip-rec          =    "T"
                     go to exe-ass-orc-500
           else      go to exe-ass-orc-700.
       exe-ass-orc-400.
      *              *-------------------------------------------------*
      *              * Se record di riga                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-ass-orc-ocr-000  thru exe-ass-orc-ocr-999    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-ass-orc-700.
       exe-ass-orc-500.
      *              *-------------------------------------------------*
      *              * Se record di testata                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-ass-orc-oct-000  thru exe-ass-orc-oct-999    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-ass-orc-700.
       exe-ass-orc-700.
      *              *-------------------------------------------------*
      *              * Riciclo a Next su [kk1]                         *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-200.
       exe-ass-orc-800.
      *              *-------------------------------------------------*
      *              * [kk1]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file                                  *
      *                  *---------------------------------------------*
           close     kk1                                              .
      *                  *---------------------------------------------*
      *                  * Richiesta di delete di un pathname alla se- *
      *                  * greteria                                    *
      *                  *---------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-kk1-pat            to   s-pat                  .
           move      "K"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       exe-ass-orc-820.
      *              *-------------------------------------------------*
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-sts-orc-tip-ope      .
           move      "pgm/orc/prg/obj/dstsorc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sts-orc
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-sts-orc-tip-ope      .
           move      "pgm/orc/prg/obj/dstsorc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sts-orc
                                               rf-oct                 .
           if        d-sts-orc-exi-sts    not  = spaces
                     go to exe-ass-orc-850.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/orc/prg/obj/dstsorc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       exe-ass-orc-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-ass-orc-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-999.
       exe-ass-orc-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di bufferizzazione testate                     *
      *    *-----------------------------------------------------------*
       exe-ass-orc-buf-000.
      *              *-------------------------------------------------*
      *              * Start su [oct]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZOCH    "         to   f-key                  .
           move      01                   to   rf-oct-cod-dpz         .
           move      spaces               to   rf-oct-flg-och         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-orc-buf-900.
       exe-ass-orc-buf-200.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-orc-buf-900.
       exe-ass-orc-buf-300.
      *              *-------------------------------------------------*
      *              * Test Max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-oct-cod-dpz       not  = 01
                     go to exe-ass-orc-buf-900.
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-oct-flg-och       not  = spaces
                     go to exe-ass-orc-buf-900.
       exe-ass-orc-buf-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ordine inevaso                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione status                   *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-sts-orc-tip-ope      .
           move      "pgm/orc/prg/obj/dstsorc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sts-orc
                                               rf-oct                 .
      *                      *-----------------------------------------*
      *                      * Solo ordini inevasi                     *
      *                      *-----------------------------------------*
           if        d-sts-orc-sts-ord    =    02  or
                     d-sts-orc-sts-ord    =    04
                     go to exe-ass-orc-buf-600
           else      go to exe-ass-orc-buf-200.
       exe-ass-orc-buf-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Testata ordine                              *
      *                  *---------------------------------------------*
           move      rf-oct-num-prt       to   kk1-num-prt            .
           move      "T"                  to   kk1-tip-rec            .
           move      zero                 to   kk1-num-prg            .
      *                  *---------------------------------------------*
      *                  * Scrittura                                   *
      *                  *---------------------------------------------*
           write     kk1-rec                                          .
       exe-ass-orc-buf-610.
      *                  *---------------------------------------------*
      *                  * Righe ordine                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su [ocr]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-oct-num-prt       to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                          *-------------------------------------*
      *                          * Se Start errata                     *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-orc-buf-800.
       exe-ass-orc-buf-620.
      *                      *-----------------------------------------*
      *                      * Next su [ocr]                           *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                          *-------------------------------------*
      *                          * Se fine file                        *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-orc-buf-800.
       exe-ass-orc-buf-630.
      *                      *-----------------------------------------*
      *                      * Test Max su [ocr]                       *
      *                      *-----------------------------------------*
           if        rf-ocr-num-prt       not  = rf-oct-num-prt
                     go to exe-ass-orc-buf-800.
       exe-ass-orc-buf-640.
      *                      *-----------------------------------------*
      *                      * Selezioni su [ocr]                      *
      *                      *-----------------------------------------*
           if        rf-ocr-tip-rig (1 : 1)
                                          =    "C"
                     go to exe-ass-orc-buf-620.
       exe-ass-orc-buf-660.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-ocr-num-prt       to   kk1-num-prt            .
           move      "R"                  to   kk1-tip-rec            .
           move      rf-ocr-num-prg       to   kk1-num-prg            .
      *                      *-----------------------------------------*
      *                      * Scrittura                               *
      *                      *-----------------------------------------*
           write     kk1-rec                                          .
       exe-ass-orc-buf-700.
      *              *-------------------------------------------------*
      *              * Riciclo su [ocr]                                *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-buf-620.
       exe-ass-orc-buf-800.
      *              *-------------------------------------------------*
      *              * Riciclo su [oct]                                *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-buf-200.
       exe-ass-orc-buf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-buf-999.
       exe-ass-orc-buf-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *-----------------------------------------------------------*
       exe-ass-orc-ocr-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-ass-orc-ocr-200.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ocr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      kk1-num-prt          to   rf-ocr-num-prt         .
           move      kk1-num-prg          to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se non trovato : uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-orc-ocr-900.
       exe-ass-orc-ocr-400.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della valuta per la  *
      *                  * fatturazione                                *
      *                  *---------------------------------------------*
           if        rf-ocr-sgl-vpf       =    "LIT"
                     go to exe-ass-orc-ocr-420
           else if   rf-ocr-sgl-vpf       =    "EUR"
                     go to exe-ass-orc-ocr-430
           else if   rf-ocr-sgl-vpf       =    "DEM"
                     go to exe-ass-orc-ocr-440
           else      go to exe-ass-orc-ocr-450.
       exe-ass-orc-ocr-420.
      *                  *---------------------------------------------*
      *                  * Se Lire                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-ocr-lit-000  thru exe-ass-ocr-lit-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-ocr-700.
       exe-ass-orc-ocr-430.
      *                  *---------------------------------------------*
      *                  * Se Euro                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-ocr-eur-000  thru exe-ass-ocr-eur-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-ocr-700.
       exe-ass-orc-ocr-440.
      *                  *---------------------------------------------*
      *                  * Se Marchi                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-ocr-dem-000  thru exe-ass-ocr-dem-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-ocr-700.
       exe-ass-orc-ocr-450.
      *                  *---------------------------------------------*
      *                  * Se Altro                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A rilascio                              *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-ocr-850.
       exe-ass-orc-ocr-700.
      *              *-------------------------------------------------*
      *              * Fasi comuni                                     *
      *              *-------------------------------------------------*
       exe-ass-orc-ocr-800.
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Update record [ocr]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-ass-orc-ocr-850.
      *                  *---------------------------------------------*
      *                  * Rilascio record [ocr]                       *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       exe-ass-orc-ocr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-ocr-999.
       exe-ass-orc-ocr-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *                                                           *
      *    * Subroutine di conversione righe in Lire                   *
      *    *-----------------------------------------------------------*
       exe-ass-ocr-lit-000.
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vpf         .
           move      2                    to   rf-ocr-dec-vpf         .
           move      "*"                  to   rf-ocr-tdc-vpf         .
           move      000001,00000         to   rf-ocr-cdc-vpf         .
       exe-ass-ocr-lit-100.
      *              *-------------------------------------------------*
      *              * Segnale di gestione della seconda quantita'     *
      *              *-------------------------------------------------*
           move      1                    to   rf-ocr-snx-2qt         .
      *              *-------------------------------------------------*
      *              * Numero decimali per la seconda quantita'        *
      *              *-------------------------------------------------*
           move      3                    to   rf-ocr-dec-2qt         .
       exe-ass-ocr-lit-200.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' secondaria             *
      *              *-------------------------------------------------*
           move      rf-ocr-qta-ord       to   w-det-qta-a02-qim      .
           move      rf-ocr-dec-2qt       to   w-det-qta-a02-ndu      .
           move      1000                 to   w-det-qta-a02-cmu      .
           move      1                    to   w-det-qta-a02-cdu      .
           perform   det-qta-a02-000      thru det-qta-a02-999        .
           move      w-det-qta-a02-qts    to   rf-ocr-qta-a02         .
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi standard    *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vps         .
           move      2                    to   rf-ocr-dec-vps         .
           move      "*"                  to   rf-ocr-tdc-vps         .
           move      000001,00000         to   rf-ocr-cdc-vps         .
       exe-ass-ocr-lit-300.
      *              *-------------------------------------------------*
      *              * Prezzi                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo lordo standard                       *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-lrs       to   w-wrk-cmd-prz          .
           multiply  1000                 by   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ocr-prz-lrs         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto standard                       *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-nts       to   w-wrk-cmd-prz          .
           multiply  1000                 by   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ocr-prz-nts         .
      *                  *---------------------------------------------*
      *                  * Prezzo unitario di vendita                  *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-ven       to   w-wrk-cmd-prz          .
           multiply  1000                 by   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ocr-prz-ven         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto effettivo                      *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-net       to   w-wrk-cmd-prz          .
           multiply  1000                 by   w-wrk-cmd-prz          .
      *
           move      w-wrk-cmd-prz        to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ocr-prz-net         .
       exe-ass-ocr-lit-400.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi di vendita  *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vpp         .
           move      2                    to   rf-ocr-dec-vpp         .
           move      "*"                  to   rf-ocr-tdc-vpp         .
           move      000001,00000         to   rf-ocr-cdc-vpp         .
       exe-ass-ocr-lit-500.
      *              *-------------------------------------------------*
      *              * Legame valutario                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se presente                            *
      *                  *---------------------------------------------*
           if        rf-ocr-sgl-vpl       =    spaces
                     go to exe-ass-ocr-lit-600.
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento (quello netto)        *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-net       to   rf-ocr-prz-vpl         .
       exe-ass-ocr-lit-600.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione del costo riferimento  *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vpc         .
           move      2                    to   rf-ocr-dec-vpc         .
           move      "*"                  to   rf-ocr-tdc-vpc         .
           move      000001,00000         to   rf-ocr-cdc-vpc         .
       exe-ass-ocr-lit-700.
      *              *-------------------------------------------------*
      *              * Costo di riferimento                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento                       *
      *                  *---------------------------------------------*
           move      rf-ocr-cos-rif       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-ocr-cos-rif         .
       exe-ass-ocr-lit-800.
      *              *-------------------------------------------------*
      *              * Importi                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Importo definitivo per la riga              *
      *                  *---------------------------------------------*
           multiply  rf-ocr-qta-a02       by   rf-ocr-prz-net
                                        giving rf-ocr-imp-rig         .
      *
           if        rf-ocr-dec-prz       =    1
                     divide 10            into rf-ocr-imp-rig
           else if   rf-ocr-dec-prz       =    2
                     divide 100           into rf-ocr-imp-rig         .
      *                  *---------------------------------------------*
      *                  * Importo ausiliario per la riga              *
      *                  *---------------------------------------------*
           move      rf-ocr-imp-rig       to   rf-ocr-iau-rig         .
       exe-ass-ocr-lit-850.
      *              *-------------------------------------------------*
      *              * Flag di prezzo unitario                         *
      *              *-------------------------------------------------*
           move      "K"                  to   rf-ocr-flg-puq         .
       exe-ass-ocr-lit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-ocr-lit-999.
       exe-ass-ocr-lit-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *                                                           *
      *    * Subroutine di conversione righe in Euro                   *
      *    *-----------------------------------------------------------*
       exe-ass-ocr-eur-000.
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-ocr-cdc-vpf         .
       exe-ass-ocr-eur-100.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi standard    *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-ocr-cdc-vps         .
       exe-ass-ocr-eur-300.
      *              *-------------------------------------------------*
      *              * Prezzi                                          *
      *              *-------------------------------------------------*
       exe-ass-ocr-eur-400.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi di vendita  *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-ocr-cdc-vpp         .
       exe-ass-ocr-eur-500.
      *              *-------------------------------------------------*
      *              * Legame valutario                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento (quello netto)        *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-net       to   rf-ocr-prz-vpl         .
       exe-ass-ocr-eur-600.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione del costo riferimento  *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-ocr-cdc-vpc         .
       exe-ass-ocr-eur-700.
      *              *-------------------------------------------------*
      *              * Costo di riferimento                            *
      *              *-------------------------------------------------*
       exe-ass-ocr-eur-800.
      *              *-------------------------------------------------*
      *              * Importi                                         *
      *              *-------------------------------------------------*
       exe-ass-ocr-eur-850.
       exe-ass-ocr-eur-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-ocr-eur-999.
       exe-ass-ocr-eur-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione righe                           *
      *    *                                                           *
      *    * Subroutine di conversione righe in Marchi                 *
      *    *-----------------------------------------------------------*
       exe-ass-ocr-dem-000.
      *              *-------------------------------------------------*
      *              * Coefficiente fisso                              *
      *              *-------------------------------------------------*
           move      1,95583              to   w-cdc                  .
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vpf         .
           move      2                    to   rf-ocr-dec-vpf         .
           move      "*"                  to   rf-ocr-tdc-vpf         .
           move      000001,00000         to   rf-ocr-cdc-vpf         .
       exe-ass-ocr-dem-100.
      *              *-------------------------------------------------*
      *              * Segnale di gestione della seconda quantita'     *
      *              *-------------------------------------------------*
           move      1                    to   rf-ocr-snx-2qt         .
      *              *-------------------------------------------------*
      *              * Numero decimali per la seconda quantita'        *
      *              *-------------------------------------------------*
           move      3                    to   rf-ocr-dec-2qt         .
       exe-ass-ocr-dem-200.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' secondaria             *
      *              *-------------------------------------------------*
           move      rf-ocr-qta-ord       to   w-det-qta-a02-qim      .
           move      rf-ocr-dec-2qt       to   w-det-qta-a02-ndu      .
           move      1000                 to   w-det-qta-a02-cmu      .
           move      1                    to   w-det-qta-a02-cdu      .
           perform   det-qta-a02-000      thru det-qta-a02-999        .
           move      w-det-qta-a02-qts    to   rf-ocr-qta-a02         .
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi standard    *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vps         .
           move      2                    to   rf-ocr-dec-vps         .
           move      "*"                  to   rf-ocr-tdc-vps         .
           move      000001,00000         to   rf-ocr-cdc-vps         .
       exe-ass-ocr-dem-300.
      *              *-------------------------------------------------*
      *              * Prezzi                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo lordo standard                       *
      *                  *---------------------------------------------*
           multiply  1000                 by   rf-ocr-prz-lrs         .
      *
           if        rf-ocr-prz-lrs       not  = zero
                     divide  w-cdc        into rf-ocr-prz-lrs         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto standard                       *
      *                  *---------------------------------------------*
           multiply  1000                 by   rf-ocr-prz-nts         .
      *
           if        rf-ocr-prz-nts       not  = zero
                     divide  w-cdc        into rf-ocr-prz-nts         .
      *                  *---------------------------------------------*
      *                  * Prezzo unitario di vendita                  *
      *                  *---------------------------------------------*
           multiply  1000                 by   rf-ocr-prz-ven         .
      *
           if        rf-ocr-prz-ven       not  = zero
                     divide  w-cdc        into rf-ocr-prz-ven         .
      *                  *---------------------------------------------*
      *                  * Prezzo netto effettivo                      *
      *                  *---------------------------------------------*
           multiply  1000                 by   rf-ocr-prz-net         .
      *
           if        rf-ocr-prz-net       not  = zero
                     divide  w-cdc        into rf-ocr-prz-net         .
       exe-ass-ocr-dem-400.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione dei prezzi di vendita  *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vpp         .
           move      2                    to   rf-ocr-dec-vpp         .
           move      "*"                  to   rf-ocr-tdc-vpp         .
           move      000001,00000         to   rf-ocr-cdc-vpp         .
       exe-ass-ocr-dem-500.
      *              *-------------------------------------------------*
      *              * Legame valutario                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se presente                            *
      *                  *---------------------------------------------*
           if        rf-ocr-sgl-vpl       =    spaces
                     go to exe-ass-ocr-dem-600.
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento (quello netto)        *
      *                  *---------------------------------------------*
           move      rf-ocr-prz-net       to   rf-ocr-prz-vpl         .
       exe-ass-ocr-dem-600.
      *              *-------------------------------------------------*
      *              * Valuta per l'espressione del costo riferimento  *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-ocr-sgl-vpc         .
           move      2                    to   rf-ocr-dec-vpc         .
           move      "*"                  to   rf-ocr-tdc-vpc         .
           move      000001,00000         to   rf-ocr-cdc-vpc         .
       exe-ass-ocr-dem-700.
      *              *-------------------------------------------------*
      *              * Costo di riferimento                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento                       *
      *                  *---------------------------------------------*
           if        rf-ocr-cos-rif       not  = zero
                     divide  w-cdc        into rf-ocr-cos-rif         .
       exe-ass-ocr-dem-800.
      *              *-------------------------------------------------*
      *              * Importi                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Importo definitivo per la riga              *
      *                  *---------------------------------------------*
           if        rf-ocr-imp-rig       not  = zero
                     divide  w-cdc        into rf-ocr-imp-rig         .
      *                  *---------------------------------------------*
      *                  * Importo ausiliario per la riga              *
      *                  *---------------------------------------------*
           if        rf-ocr-iau-rig       not  = zero
                     divide  w-cdc        into rf-ocr-iau-rig         .
       exe-ass-ocr-dem-850.
      *              *-------------------------------------------------*
      *              * Flag di prezzo unitario                         *
      *              *-------------------------------------------------*
           move      "K"                  to   rf-ocr-flg-puq         .
       exe-ass-ocr-dem-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-ocr-dem-999.
       exe-ass-ocr-dem-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *-----------------------------------------------------------*
       exe-ass-orc-oct-000.
      *              *-------------------------------------------------*
      *              * Ottenimento record [oct]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      kk1-num-prt          to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se non trovato : uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-ass-orc-oct-900.
       exe-ass-orc-oct-400.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della valuta per la  *
      *                  * fatturazione                                *
      *                  *---------------------------------------------*
           if        rf-oct-sgl-vpf       =    "LIT"
                     go to exe-ass-orc-oct-420
           else if   rf-oct-sgl-vpf       =    "EUR"
                     go to exe-ass-orc-oct-430
           else if   rf-oct-sgl-vpf       =    "DEM"
                     go to exe-ass-orc-oct-440
           else      go to exe-ass-orc-oct-450.
       exe-ass-orc-oct-420.
      *                  *---------------------------------------------*
      *                  * Se Lire                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-oct-lit-000  thru exe-ass-oct-lit-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-oct-700.
       exe-ass-orc-oct-430.
      *                  *---------------------------------------------*
      *                  * Se Euro                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-oct-eur-000  thru exe-ass-oct-eur-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-oct-700.
       exe-ass-orc-oct-440.
      *                  *---------------------------------------------*
      *                  * Se Marchi                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine                              *
      *                      *-----------------------------------------*
           perform   exe-ass-oct-dem-000  thru exe-ass-oct-dem-999    .
      *                      *-----------------------------------------*
      *                      * A fasi comuni                           *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-oct-700.
       exe-ass-orc-oct-450.
      *                  *---------------------------------------------*
      *                  * Se Altro                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A rilascio                              *
      *                      *-----------------------------------------*
           go to     exe-ass-orc-oct-850.
       exe-ass-orc-oct-700.
      *              *-------------------------------------------------*
      *              * Fasi comuni                                     *
      *              *-------------------------------------------------*
       exe-ass-orc-oct-800.
      *              *-------------------------------------------------*
      *              * Scrittura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
      *                  *---------------------------------------------*
      *                  * Update record [oct]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-ass-orc-oct-850.
      *                  *---------------------------------------------*
      *                  * Rilascio record [oct]                       *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
       exe-ass-orc-oct-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-orc-oct-999.
       exe-ass-orc-oct-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *                                                           *
      *    * Subroutine di conversione testate in Lire                 *
      *    *-----------------------------------------------------------*
       exe-ass-oct-lit-000.
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-oct-sgl-vpf         .
           move      2                    to   rf-oct-dec-vpf         .
           move      "*"                  to   rf-oct-tdc-vpf         .
           move      000001,00000         to   rf-oct-cdc-vpf         .
      *              *-------------------------------------------------*
      *              * Ammontare della quota a forfait                 *
      *              *-------------------------------------------------*
           move      rf-oct-pag-qaf       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-pag-qaf         .
      *              *-------------------------------------------------*
      *              * Ammontare acconto                               *
      *              *-------------------------------------------------*
           move      rf-oct-pag-act       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-pag-act         .
       exe-ass-oct-lit-100.
           move      zero                 to   w-inx                  .
       exe-ass-oct-lit-200.
           add       1                    to   w-inx                  .
           if        w-inx                >    9
                     go to exe-ass-oct-lit-300.
      *              *-------------------------------------------------*
      *              * Totale                                          *
      *              *-------------------------------------------------*
           move      rf-oct-tot-rig (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-tot-rig (w-inx) .
           go to     exe-ass-oct-lit-200.
       exe-ass-oct-lit-300.
      *              *-------------------------------------------------*
      *              * Importo sconto in chiusura                      *
      *              *-------------------------------------------------*
           move      rf-oct-tot-scc       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-tot-scc         .
      *              *-------------------------------------------------*
      *              * Importo sconto pagamento                        *
      *              *-------------------------------------------------*
           move      rf-oct-tot-scp       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-tot-scp         .
       exe-ass-oct-lit-400.
           move      zero                 to   w-inx                  .
       exe-ass-oct-lit-500.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-ass-oct-lit-600.
      *              *-------------------------------------------------*
      *              * Spesa                                           *
      *              *-------------------------------------------------*
           move      rf-oct-spe-imp (w-inx)
                                          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-spe-imp (w-inx) .
           go to     exe-ass-oct-lit-500.
       exe-ass-oct-lit-600.
      *              *-------------------------------------------------*
      *              * Totale documento                                *
      *              *-------------------------------------------------*
           move      rf-oct-tot-doc       to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-oct-tot-doc         .
       exe-ass-oct-lit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-oct-lit-999.
       exe-ass-oct-lit-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *                                                           *
      *    * Subroutine di conversione testate in Euro                 *
      *    *-----------------------------------------------------------*
       exe-ass-oct-eur-000.
      *              *-------------------------------------------------*
      *              * Coefficiente di cambio                          *
      *              *-------------------------------------------------*
           move      000001,00000         to   rf-oct-cdc-vpf         .
      *              *-------------------------------------------------*
      *              * Codice listino                                  *
      *              *-------------------------------------------------*
           move      spaces               to   rf-oct-cod-lst         .
       exe-ass-oct-eur-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-oct-eur-999.
       exe-ass-oct-eur-999.
           exit.

      *    *===========================================================*
      *    * Assestamento ordini clienti inevasi                       *
      *    *                                                           *
      *    * Subroutine di conversione testate                         *
      *    *                                                           *
      *    * Subroutine di conversione testate in Marchi               *
      *    *-----------------------------------------------------------*
       exe-ass-oct-dem-000.
      *              *-------------------------------------------------*
      *              * Coefficiente fisso                              *
      *              *-------------------------------------------------*
           move      1,95583              to   w-cdc                  .
      *              *-------------------------------------------------*
      *              * Valuta per la fatturazione                      *
      *              *-------------------------------------------------*
           move      "EUR"                to   rf-oct-sgl-vpf         .
           move      2                    to   rf-oct-dec-vpf         .
           move      "*"                  to   rf-oct-tdc-vpf         .
           move      000001,00000         to   rf-oct-cdc-vpf         .
      *              *-------------------------------------------------*
      *              * Ammontare della quota a forfait                 *
      *              *-------------------------------------------------*
           if        rf-oct-pag-qaf       not  = zero
                     divide  w-cdc        into rf-oct-pag-qaf         .
      *              *-------------------------------------------------*
      *              * Ammontare acconto                               *
      *              *-------------------------------------------------*
           if        rf-oct-pag-act       not  = zero
                     divide  w-cdc        into rf-oct-pag-act         .
       exe-ass-oct-dem-100.
           move      zero                 to   w-inx                  .
       exe-ass-oct-dem-200.
           add       1                    to   w-inx                  .
           if        w-inx                >    9
                     go to exe-ass-oct-dem-300.
      *              *-------------------------------------------------*
      *              * Totale                                          *
      *              *-------------------------------------------------*
           if        rf-oct-tot-rig (w-inx)
                                          not  = zero
                     divide  w-cdc        into rf-oct-tot-rig (w-inx) .
           go to     exe-ass-oct-dem-200.
       exe-ass-oct-dem-300.
      *              *-------------------------------------------------*
      *              * Importo sconto in chiusura                      *
      *              *-------------------------------------------------*
           if        rf-oct-tot-scc       not  = zero
                     divide  w-cdc        into rf-oct-tot-scc         .
      *              *-------------------------------------------------*
      *              * Importo sconto pagamento                        *
      *              *-------------------------------------------------*
           if        rf-oct-tot-scp       not  = zero
                     divide  w-cdc        into rf-oct-tot-scp         .
       exe-ass-oct-dem-400.
           move      zero                 to   w-inx                  .
       exe-ass-oct-dem-500.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-ass-oct-dem-600.
      *              *-------------------------------------------------*
      *              * Spesa                                           *
      *              *-------------------------------------------------*
           if        rf-oct-spe-imp (w-inx)
                                          not  = zero
                     divide  w-cdc        into rf-oct-spe-imp (w-inx) .
           go to     exe-ass-oct-dem-500.
       exe-ass-oct-dem-600.
      *              *-------------------------------------------------*
      *              * Totale documento                                *
      *              *-------------------------------------------------*
           if        rf-oct-tot-doc       not  = zero
                     divide  w-cdc        into rf-oct-tot-doc         .
       exe-ass-oct-dem-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-ass-oct-dem-999.
       exe-ass-oct-dem-999.
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
      *    * Routine di determinazione quantita' secondaria            *
      *    *-----------------------------------------------------------*
       det-qta-a02-000.
      *              *-------------------------------------------------*
      *              * Se trasformazione per l'unita' di misura solo   *
      *              * per il prezzo                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' impostata in work di lavoro       *
      *                  *---------------------------------------------*
           move      w-det-qta-a02-qim    to   w-det-qta-a02-wpq      .
      *                  *---------------------------------------------*
      *                  * Moltiplicazione per coefficiente di divi-   *
      *                  * sione                                       *
      *                  *---------------------------------------------*
           multiply  w-det-qta-a02-cdu    by   w-det-qta-a02-wpq      .
       det-qta-a02-420.
      *                  *---------------------------------------------*
      *                  * Divisione per coefficiente di moltiplica-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del numero decima- *
      *                      * li secondo il fornitore                 *
      *                      *-----------------------------------------*
           go to     det-qta-a02-430
                     det-qta-a02-435
                     det-qta-a02-440
                     depending            on   w-det-qta-a02-ndu      .
       det-qta-a02-425.
      *                      *-----------------------------------------*
      *                      * Numero decimali : zero                  *
      *                      *-----------------------------------------*
           divide    w-det-qta-a02-cmu    into w-det-qta-a02-wpq
                                        giving w-det-qta-a02-wq0
                                                         rounded      .
           move      w-det-qta-a02-wq0    to   w-det-qta-a02-wpq      .
           go to     det-qta-a02-450.
       det-qta-a02-430.
      *                      *-----------------------------------------*
      *                      * Numero decimali : 1                     *
      *                      *-----------------------------------------*
           divide    w-det-qta-a02-cmu    into w-det-qta-a02-wpq
                                        giving w-det-qta-a02-wq1
                                                         rounded      .
           move      w-det-qta-a02-wq1    to   w-det-qta-a02-wpq      .
           go to     det-qta-a02-450.
       det-qta-a02-435.
      *                      *-----------------------------------------*
      *                      * Numero decimali : 2                     *
      *                      *-----------------------------------------*
           divide    w-det-qta-a02-cmu    into w-det-qta-a02-wpq
                                        giving w-det-qta-a02-wq2
                                                         rounded      .
           move      w-det-qta-a02-wq2    to   w-det-qta-a02-wpq      .
           go to     det-qta-a02-450.
       det-qta-a02-440.
      *                      *-----------------------------------------*
      *                      * Numero decimali : 3                     *
      *                      *-----------------------------------------*
           divide    w-det-qta-a02-cmu    into w-det-qta-a02-wpq
                                        giving w-det-qta-a02-wq3
                                                         rounded      .
           move      w-det-qta-a02-wq3    to   w-det-qta-a02-wpq      .
           go to     det-qta-a02-450.
       det-qta-a02-450.
      *                  *---------------------------------------------*
      *                  * Valore in output                            *
      *                  *---------------------------------------------*
           move      w-det-qta-a02-wpq    to   w-det-qta-a02-qts      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-qta-a02-999.
       det-qta-a02-999.
           exit.

      *    *===========================================================*
      *    * Routines per la conversione                               *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/prg/cpy/cnveur01.wks"                   .

