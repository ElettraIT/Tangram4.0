       Identification Division.
       Program-Id.                                 eurtrc01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    eur                 *
      *                                   Fase:    eurtrc01            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 07/09/00    *
      *                       Ultima revisione:    NdK del 03/01/02    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per EURO - Agavi                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Copia archivi                               *
      *                                                                *
      * {gpf}   [hbx]      File di confronto per l'esportazione        *
      * {gsc}   [hac]      Autorizzazioni al conferimento              *
      * {gsc}   [had]      Tabella classi di trasporto ADR             *
      * {gsc}   [hai]      Tipologie ISTAT autorizzate                 *
      * {gsc}   [har]      Relazioni di analisi : righe                *
      * {gsc}   [hat]      Relazioni di analisi : testate              *
      * {gsc}   [hau]      Archivio autorizzazioni                     *
      * {gsc}   [hax]      Relazioni di analisi : estensioni           *
      * {gsc}   [haz]      Archivio anagrafica azienda                 *
      * {gsc}   [hce]      Tabella codici CEE rifiuti                  *
      * {gsc}   [hcl]      Tabella classificazioni del rifiuto         *
      * {gsc}   [hcp]      Tabella classi di pericolosita'             *
      * {gsc}   [hcx]      Contratti di smaltimento : estensioni       *
      * {gsc}   [hdd]      Anagrafica Destinatari Diversi              *
      * {gsc}   [hdl]      Tabella dislocazioni                        *
      * {gsc}   [hdt]      Tipologie di magazzino e dislocazioni       *
      * {gsc}   [hgx]      Tabella codici ISTAT comuni                 *
      * {gsc}   [hit]      Tipologie ISTAT e tipologie di magazzino    *
      * {gsc}   [hmg]      Archivio codici di magazzino                *
      * {gsc}   [hmm]      Movimenti di magazzino                      *
      * {gsc}   [hni]      Nuovi codici di ISTAT dei processi prod.    *
      * {gsc}   [hnu]      Numerazioni per 'gsr'                       *
      * {gsc}   [hox]      Ordini di conferimento : estensioni         *
      * {gsc}   [hpa]      Tabella parametri certificati analisi       *
      * {gsc}   [hpd]      Anagrafica Produttori                       *
      * {gsc}   [hpr]      Produttori e tipologie ISTAT rifiuti        *
      * {gsc}   [hrc]      Catasto rifiuti : Classi                    *
      * {gsc}   [hre]      Codici residui                              *
      * {gsc}   [hrg]      Archivio registro carico-scarico            *
      * {gsc}   [hrs]      Catasto rifiuti : Sottoclassi               *
      * {gsc}   [hrt]      Catasto rifiuti : Tipologie                 *
      * {gsc}   [hti]      Tabella tipi imballo                        *
      * {gsc}   [htm]      Tabella tipi movimento                      *
      * {gsc}   [htp]      Anagrafica Trasportatori                    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    Conversione                                 *
      *                                                                *
      * {gsr} v [haf]      Addebiti per fatturazione                   *
      * {gsr} v [hcr]      Contratti di smaltimento : righe            *
      * {gsr} v [hct]      Contratti di smaltimento : testate          *
      * {gsr} v [hor]      Ordini di conferimento : righe              *
      * {gsr} v [hot]      Ordini di conferimento : testate            *
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
      *    * File Control [haf]                                        *
      *    *-----------------------------------------------------------*
           select  optional  haf   assign to disk           f-haf-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is haf-k01
                   alternate record key   is haf-k02
                   alternate record key   is haf-k03
                   alternate record key   is haf-k04
                   alternate record key   is haf-k05
                             file status  is                f-haf-sts .

      *    *===========================================================*
      *    * File Control [hcr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hcr   assign to disk           f-hcr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hcr-k01
                   alternate record key   is hcr-k02
                   alternate record key   is hcr-k03
                   alternate record key   is hcr-k04
                             file status  is                f-hcr-sts .

      *    *===========================================================*
      *    * File Control [hct]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hct   assign to disk           f-hct-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hct-k01
                   alternate record key   is hct-k02
                   alternate record key   is hct-k03
                   alternate record key   is hct-k04
                   alternate record key   is hct-k05
                   alternate record key   is hct-k06
                             file status  is                f-hct-sts .

      *    *===========================================================*
      *    * File Control [hor]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hor   assign to disk           f-hor-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hor-k01
                   alternate record key   is hor-k02
                   alternate record key   is hor-k03
                   alternate record key   is hor-k04
                             file status  is                f-hor-sts .

      *    *===========================================================*
      *    * File Control [hot]                                        *
      *    *-----------------------------------------------------------*
           select  optional  hot   assign to disk           f-hot-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is hot-k01
                   alternate record key   is hot-k02
                   alternate record key   is hot-k03
                   alternate record key   is hot-k04
                   alternate record key   is hot-k05
                   alternate record key   is hot-k06
                             file status  is                f-hot-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [haf]                                    *
      *    *-----------------------------------------------------------*
       fd  haf       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  haf-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  haf-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  haf-k01.
                   15  haf-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATREG                         *
      *            *---------------------------------------------------*
               10  haf-k02.
                   15  haf-dat-reg        pic  9(07)       comp-3     .
                   15  haf-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATSYS                         *
      *            *---------------------------------------------------*
               10  haf-k03.
                   15  haf-ide-dat        pic  9(07)       comp-3     .
                   15  haf-dat-reg-3      pic  9(07)       comp-3     .
                   15  haf-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : OCFPRT                         *
      *            *---------------------------------------------------*
               10  haf-k04.
                   15  haf-prt-ocf        pic  9(11)       comp-3     .
                   15  haf-dat-reg-4      pic  9(07)       comp-3     .
                   15  haf-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CLIPRT                         *
      *            *---------------------------------------------------*
               10  haf-k05.
                   15  haf-cod-cli        pic  9(07)       comp-3     .
                   15  haf-dat-reg-5      pic  9(07)       comp-3     .
                   15  haf-num-prt-5      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  haf-dat.
               10  haf-ide-ute            pic  x(08)                  .
               10  haf-ide-fas            pic  x(06)                  .
               10  haf-cod-dpz            pic  9(02)                  .
               10  haf-dpz-cli            pic  x(04)                  .
               10  haf-tip-add            pic  9(02)                  .
               10  haf-cod-add            pic  9(03)       comp-3     .
               10  haf-num-pro            pic  9(07)       comp-3     .
               10  haf-alf-pro            pic  x(14)                  .
               10  haf-des-add.
                   15  haf-des-rig occurs 10
                                          pic  x(40)                  .
               10  haf-cod-iva            pic  9(05)       comp-3     .
               10  haf-ctp-ven            pic  9(07)       comp-3     .
               10  haf-qta-add            pic s9(08)v9(03) comp-3     .
               10  haf-prz-add            pic  9(09)       comp-3     .
               10  haf-prt-fit            pic  9(09)       comp-3     .
               10  haf-prt-atz            pic  9(11)       comp-3     .
               10  haf-prt-mmm            pic  9(11)       comp-3     .
               10  haf-dat-cpt            pic  9(07)       comp-3     .
               10  haf-ndo-rif            pic  x(10)                  .
               10  haf-ddo-rif            pic  9(07)       comp-3     .
               10  haf-flg-pul            pic  x(01)                  .
               10  haf-flg-tda            pic  x(01)                  .
               10  haf-dec-prz            pic  9(01)                  .
               10  haf-alx-exp.
                   15  filler  occurs 199 pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hcr]                                    *
      *    *-----------------------------------------------------------*
       fd  hcr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hcr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hcr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PRTPRG                         *
      *            *---------------------------------------------------*
               10  hcr-k01.
                   15  hcr-num-prt        pic  9(11)       comp-3     .
                   15  hcr-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATREG                         *
      *            *---------------------------------------------------*
               10  hcr-k02.
                   15  hcr-dat-reg        pic  9(07)       comp-3     .
                   15  hcr-num-prt-2      pic  9(11)       comp-3     .
                   15  hcr-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROPRT                         *
      *            *---------------------------------------------------*
               10  hcr-k03.
                   15  hcr-num-pro        pic  9(07)       comp-3     .
                   15  hcr-dat-reg-3      pic  9(07)       comp-3     .
                   15  hcr-num-prt-3      pic  9(11)       comp-3     .
                   15  hcr-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNFPRT                         *
      *            *---------------------------------------------------*
               10  hcr-k04.
                   15  hcr-dat-cnf        pic  9(07)       comp-3     .
                   15  hcr-num-prt-4      pic  9(11)       comp-3     .
                   15  hcr-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hcr-dat.
               10  hcr-tip-mag            pic  9(02)                  .
               10  hcr-tip-rig            pic  x(05)                  .
               10  hcr-tmo-ctt            pic  x(05)                  .
               10  hcr-cod-dpz            pic  9(02)                  .
               10  hcr-cod-dsl            pic  x(07)                  .
               10  hcr-alf-pro            pic  x(14)                  .
               10  hcr-des-ext            pic  9(01)                  .
               10  hcr-des-rig            pic  x(40)                  .
               10  hcr-prz-uni            pic  9(09)v9(02) comp-3     .
               10  hcr-qsc-kgs            pic  9(08)v9(03) comp-3     .
               10  hcr-per-scc occurs 05  pic  9(02)v9(01) comp-3     .
               10  hcr-udm-pro            pic  x(03)                  .
               10  hcr-snx-ade            pic  x(01)                  .
               10  hcr-flg-ela.
                   15  hcr-flg-blo.
                       20  hcr-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hcr-flg-nbl.
                       20  hcr-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hcr-flg-pul            pic  x(01)                  .
               10  hcr-flg-tda            pic  x(01)                  .
               10  hcr-alx-exp.
                   15  filler occurs 200  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hct]                                    *
      *    *-----------------------------------------------------------*
       fd  hct       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hct-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hct-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hct-k01.
                   15  hct-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATREG                         *
      *            *---------------------------------------------------*
               10  hct-k02.
                   15  hct-dat-reg        pic  9(07)       comp-3     .
                   15  hct-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hct-k03.
                   15  hct-ide-dat        pic  9(07)       comp-3     .
                   15  hct-dat-reg-3      pic  9(07)       comp-3     .
                   15  hct-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CLIPRT                         *
      *            *---------------------------------------------------*
               10  hct-k04.
                   15  hct-cod-cli        pic  9(07)       comp-3     .
                   15  hct-dat-reg-4      pic  9(07)       comp-3     .
                   15  hct-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PROPRT                         *
      *            *---------------------------------------------------*
               10  hct-k05.
                   15  hct-tip-pro        pic  9(02)                  .
                   15  hct-cod-pro        pic  9(07)       comp-3     .
                   15  hct-dat-reg-5      pic  9(07)       comp-3     .
                   15  hct-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CFOPRT                         *
      *            *---------------------------------------------------*
               10  hct-k06.
                   15  hct-prt-cfo        pic  9(11)       comp-3     .
                   15  hct-num-prt-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hct-dat.
               10  hct-ide-ute            pic  x(08)                  .
               10  hct-ide-fas            pic  x(06)                  .
               10  hct-tmo-ctt            pic  x(05)                  .
               10  hct-cod-dpz            pic  9(02)                  .
               10  hct-dat-cfo            pic  9(07)       comp-3     .
               10  hct-dpz-pro            pic  x(04)                  .
               10  hct-dpz-cli            pic  x(04)                  .
               10  hct-cod-fop            pic  9(07)       comp-3     .
               10  hct-cod-abi            pic  9(05)       comp-3     .
               10  hct-cod-cab            pic  9(05)       comp-3     .
               10  hct-ccc-app            pic  x(12)                  .
               10  hct-cod-lst            pic  x(03)                  .
               10  hct-cod-age            pic  9(07)       comp-3     .
               10  hct-cat-pvg            pic  9(05)       comp-3     .
               10  hct-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  hct-amm-pvg            pic  9(09)       comp-3     .
               10  hct-tip-tra            pic  9(02)                  .
               10  hct-cod-tra            pic  9(07)       comp-3     .
               10  hct-dpz-tra            pic  x(04)                  .
               10  hct-dat-scd            pic  9(07)       comp-3     .
               10  hct-num-rda            pic  9(11)       comp-3     .
               10  hct-ndo-rif            pic  x(10)                  .
               10  hct-ddo-rif            pic  9(07)       comp-3     .
               10  hct-ndo-nsr            pic  x(10)                  .
               10  hct-ddo-nsr            pic  9(07)       comp-3     .
               10  hct-cct-rif            pic  x(05)                  .
               10  hct-des-rif.
                   15  hct-rig-rif occurs 3
                                          pic  x(40)                  .
               10  hct-ccl-rif            pic  9(02)                  .
               10  hct-num-pro            pic  9(07)       comp-3     .
               10  hct-alf-pro            pic  x(14)                  .
               10  hct-qta-kgg            pic  9(08)v9(03) comp-3     .
               10  hct-qta-kgs            pic  9(08)v9(03) comp-3     .
               10  hct-num-cnf            pic  9(05)       comp-3     .
               10  hct-qsc-kgs            pic  9(08)v9(03) comp-3     .
               10  hct-din-cnf            pic  9(07)       comp-3     .
               10  hct-frs-ngg            pic  9(03)       comp-3     .
               10  hct-crf-ana            pic  x(40)                  .
               10  hct-cod-dsl            pic  x(07)                  .
               10  hct-cst-add.
                   15  hct-trf-add occurs 6.
                       20  hct-ele-add    pic  9(11)       comp-3     .
                       20  hct-tip-add    pic  9(02)                  .
               10  hct-cod-ann            pic  9(03)       comp-3     .
               10  hct-ann-ctt.
                   15  hct-rig-ann occurs 3
                                          pic  x(40)                  .
               10  hct-flg-ela.
                   15  hct-flg-blo.
                       20  hct-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hct-flg-nbl.
                       20  hct-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hct-flg-pul            pic  x(01)                  .
               10  hct-flg-stp            pic  x(01)                  .
               10  hct-flg-tda            pic  x(01)                  .
               10  hct-flg-trt            pic  x(01)                  .
               10  hct-cod-cee            pic  x(14)                  .
               10  hct-cod-res            pic  x(14)                  .
               10  hct-cad-gio            pic  9(03)                  .
               10  hct-snx-sab            pic  x(01)                  .
               10  hct-alx-exp.
                   15  filler occurs 167  pic  x(01)                  .

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
      *            * Chiave numero 01 : PRTPRG                         *
      *            *---------------------------------------------------*
               10  hor-k01.
                   15  hor-num-prt        pic  9(11)       comp-3     .
                   15  hor-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATREG                         *
      *            *---------------------------------------------------*
               10  hor-k02.
                   15  hor-dat-reg        pic  9(07)       comp-3     .
                   15  hor-num-prt-2      pic  9(11)       comp-3     .
                   15  hor-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROPRT                         *
      *            *---------------------------------------------------*
               10  hor-k03.
                   15  hor-num-pro        pic  9(07)       comp-3     .
                   15  hor-dat-reg-3      pic  9(07)       comp-3     .
                   15  hor-num-prt-3      pic  9(11)       comp-3     .
                   15  hor-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNFPRT                         *
      *            *---------------------------------------------------*
               10  hor-k04.
                   15  hor-dat-cnf        pic  9(07)       comp-3     .
                   15  hor-num-prt-4      pic  9(11)       comp-3     .
                   15  hor-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hor-dat.
               10  hor-tip-mag            pic  9(02)                  .
               10  hor-tip-rig            pic  x(05)                  .
               10  hor-tmo-ctt            pic  x(05)                  .
               10  hor-cod-dpz            pic  9(02)                  .
               10  hor-cod-dsl            pic  x(07)                  .
               10  hor-alf-pro            pic  x(14)                  .
               10  hor-des-ext            pic  9(01)                  .
               10  hor-des-rig            pic  x(40)                  .
               10  hor-prz-uni            pic  9(09)v9(02) comp-3     .
               10  hor-qsc-kgs            pic  9(08)v9(03) comp-3     .
               10  hor-per-scc occurs 05  pic  9(02)v9(01) comp-3     .
               10  hor-udm-pro            pic  x(03)                  .
               10  hor-snx-ade            pic  x(01)                  .
               10  hor-flg-ela.
                   15  hor-flg-blo.
                       20  hor-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hor-flg-nbl.
                       20  hor-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hor-flg-pul            pic  x(01)                  .
               10  hor-flg-tda            pic  x(01)                  .
               10  hor-alx-exp.
                   15  filler occurs 200  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [hot]                                    *
      *    *-----------------------------------------------------------*
       fd  hot       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  hot-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  hot-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  hot-k01.
                   15  hot-num-prt        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATREG                         *
      *            *---------------------------------------------------*
               10  hot-k02.
                   15  hot-dat-reg        pic  9(07)       comp-3     .
                   15  hot-num-prt-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATSYS                         *
      *            *---------------------------------------------------*
               10  hot-k03.
                   15  hot-ide-dat        pic  9(07)       comp-3     .
                   15  hot-dat-reg-3      pic  9(07)       comp-3     .
                   15  hot-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CLIPRT                         *
      *            *---------------------------------------------------*
               10  hot-k04.
                   15  hot-cod-cli        pic  9(07)       comp-3     .
                   15  hot-dat-reg-4      pic  9(07)       comp-3     .
                   15  hot-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : PROPRT                         *
      *            *---------------------------------------------------*
               10  hot-k05.
                   15  hot-tip-pro        pic  9(02)                  .
                   15  hot-cod-pro        pic  9(07)       comp-3     .
                   15  hot-dat-reg-5      pic  9(07)       comp-3     .
                   15  hot-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CTTPRT                         *
      *            *---------------------------------------------------*
               10  hot-k06.
                   15  hot-prt-ctt        pic  9(11)       comp-3     .
                   15  hot-num-prt-6      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  hot-dat.
               10  hot-ide-ute            pic  x(08)                  .
               10  hot-ide-fas            pic  x(06)                  .
               10  hot-tmo-ctt            pic  x(05)                  .
               10  hot-cod-dpz            pic  9(02)                  .
               10  hot-dat-cfo            pic  9(07)       comp-3     .
               10  hot-dpz-pro            pic  x(04)                  .
               10  hot-dpz-cli            pic  x(04)                  .
               10  hot-cod-fop            pic  9(07)       comp-3     .
               10  hot-cod-abi            pic  9(05)       comp-3     .
               10  hot-cod-cab            pic  9(05)       comp-3     .
               10  hot-ccc-app            pic  x(12)                  .
               10  hot-cod-lst            pic  x(03)                  .
               10  hot-cod-age            pic  9(07)       comp-3     .
               10  hot-cat-pvg            pic  9(05)       comp-3     .
               10  hot-per-pvg occurs 03  pic  9(02)v9(01) comp-3     .
               10  hot-amm-pvg            pic  9(09)       comp-3     .
               10  hot-tip-tra            pic  9(02)                  .
               10  hot-cod-tra            pic  9(07)       comp-3     .
               10  hot-dpz-tra            pic  x(04)                  .
               10  hot-dat-scd            pic  9(07)       comp-3     .
               10  hot-num-rda            pic  9(11)       comp-3     .
               10  hot-ndo-rif            pic  x(10)                  .
               10  hot-ddo-rif            pic  9(07)       comp-3     .
               10  hot-ndo-nsr            pic  x(10)                  .
               10  hot-ddo-nsr            pic  9(07)       comp-3     .
               10  hot-cct-rif            pic  x(05)                  .
               10  hot-des-rif.
                   15  hot-rig-rif occurs 3
                                          pic  x(40)                  .
               10  hot-ccl-rif            pic  9(02)                  .
               10  hot-num-pro            pic  9(07)       comp-3     .
               10  hot-alf-pro            pic  x(14)                  .
               10  hot-qta-kgg            pic  9(08)v9(03) comp-3     .
               10  hot-qta-kgs            pic  9(08)v9(03) comp-3     .
               10  hot-num-cnf            pic  9(05)       comp-3     .
               10  hot-qsc-kgs            pic  9(08)v9(03) comp-3     .
               10  hot-din-cnf            pic  9(07)       comp-3     .
               10  hot-frs-ngg            pic  9(03)       comp-3     .
               10  hot-crf-ana            pic  x(40)                  .
               10  hot-cod-dsl            pic  x(07)                  .
               10  hot-cst-add.
                   15  hot-trf-add occurs 6.
                       20  hot-ele-add    pic  9(11)       comp-3     .
                       20  hot-tip-add    pic  9(02)                  .
               10  hot-cod-ann            pic  9(03)       comp-3     .
               10  hot-ann-ctt.
                   15  hot-rig-ann occurs 3
                                          pic  x(40)                  .
               10  hot-flg-ela.
                   15  hot-flg-blo.
                       20  hot-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  hot-flg-nbl.
                       20  hot-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  hot-flg-pul            pic  x(01)                  .
               10  hot-flg-stp            pic  x(01)                  .
               10  hot-flg-tda            pic  x(01)                  .
               10  hot-flg-trt            pic  x(01)                  .
               10  hot-cod-cee            pic  x(14)                  .
               10  hot-cod-res            pic  x(14)                  .
               10  hot-cad-gio            pic  9(03)                  .
               10  hot-snx-sab            pic  x(01)                  .
               10  hot-alx-exp.
                   15  filler occurs 167  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [haf]                                       *
      *    *-----------------------------------------------------------*
       01  f-haf.
           05  f-haf-nam                  pic  x(04)                  .
           05  f-haf-pat                  pic  x(40)                  .
           05  f-haf-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hcr]                                       *
      *    *-----------------------------------------------------------*
       01  f-hcr.
           05  f-hcr-nam                  pic  x(04)                  .
           05  f-hcr-pat                  pic  x(40)                  .
           05  f-hcr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hct]                                       *
      *    *-----------------------------------------------------------*
       01  f-hct.
           05  f-hct-nam                  pic  x(04)                  .
           05  f-hct-pat                  pic  x(40)                  .
           05  f-hct-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hor]                                       *
      *    *-----------------------------------------------------------*
       01  f-hor.
           05  f-hor-nam                  pic  x(04)                  .
           05  f-hor-pat                  pic  x(40)                  .
           05  f-hor-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * File area per [hot]                                       *
      *    *-----------------------------------------------------------*
       01  f-hot.
           05  f-hot-nam                  pic  x(04)                  .
           05  f-hot-pat                  pic  x(40)                  .
           05  f-hot-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [haf]                                                 *
      *        *-------------------------------------------------------*
           copy      "trc/gsr/fls/rec/rfhaf"                          .
      *        *-------------------------------------------------------*
      *        * [hcr]                                                 *
      *        *-------------------------------------------------------*
           copy      "trc/gsr/fls/rec/rfhcr"                          .
      *        *-------------------------------------------------------*
      *        * [hct]                                                 *
      *        *-------------------------------------------------------*
           copy      "trc/gsr/fls/rec/rfhct"                          .
      *        *-------------------------------------------------------*
      *        * [hor]                                                 *
      *        *-------------------------------------------------------*
           copy      "trc/gsr/fls/rec/rfhor"                          .
      *        *-------------------------------------------------------*
      *        * [hot]                                                 *
      *        *-------------------------------------------------------*
           copy      "trc/gsr/fls/rec/rfhot"                          .

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
                     "eurtrc"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "eurtrc01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "      CONVERSIONI PER EURO - AGAVI      "       .

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
      *        *-------------------------------------------------------*
      *        * Work per messaggi errore                              *
      *        *-------------------------------------------------------*
           05  w-ide-dat                  pic  x(08)                  .
           05  w-ide-saa                  pic  x(03)                  .
           05  w-ide-mes                  pic  x(02)                  .
           05  w-ide-arc                  pic  x(07)                  .
           05  w-ide-mag                  pic  x(07)                  .
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

      *    *===========================================================*
      *    * Work area per tabella archivi                             *
      *    *-----------------------------------------------------------*
       01  w-arc.
           05  w-arc-ele.
               10  w-arc-ele-max          pic  9(03) value 034        .
               10  w-arc-ele-tbl.
                   15  filler             pic  x(55) value
             "[hbx]      File di confronto per l'esportazione        ".
                   15  filler             pic  x(55) value
             "[hac]      Autorizzazioni al conferimento              ".
                   15  filler             pic  x(55) value
             "[had]      Tabella classi di trasporto ADR             ".
                   15  filler             pic  x(55) value
             "[hai]      Tipologie ISTAT autorizzate                 ".
                   15  filler             pic  x(55) value
             "[har]      Relazioni di analisi : righe                ".
                   15  filler             pic  x(55) value
             "[hat]      Relazioni di analisi : testate              ".
                   15  filler             pic  x(55) value
             "[hau]      Archivio autorizzazioni                     ".
                   15  filler             pic  x(55) value
             "[hax]      Relazioni di analisi : estensioni           ".
                   15  filler             pic  x(55) value
             "[haz]      Archivio anagrafica azienda                 ".
                   15  filler             pic  x(55) value
             "[hce]      Tabella codici CEE rifiuti                  ".
                   15  filler             pic  x(55) value
             "[hcl]      Tabella classificazioni del rifiuto         ".
                   15  filler             pic  x(55) value
             "[hcp]      Tabella classi di pericolosita'             ".
                   15  filler             pic  x(55) value
             "[hcx]      Contratti di smaltimento : estensioni       ".
                   15  filler             pic  x(55) value
             "[hdd]      Anagrafica Destinatari Diversi              ".
                   15  filler             pic  x(55) value
             "[hdl]      Tabella dislocazioni                        ".
                   15  filler             pic  x(55) value
             "[hdt]      Tipologie di magazzino e dislocazioni       ".
                   15  filler             pic  x(55) value
             "[hgx]      Tabella codici ISTAT comuni                 ".
                   15  filler             pic  x(55) value
             "[hit]      Tipologie ISTAT e tipologie di magazzino    ".
                   15  filler             pic  x(55) value
             "[hmg]      Archivio codici di magazzino                ".
                   15  filler             pic  x(55) value
             "[hmm]      Movimenti di magazzino                      ".
                   15  filler             pic  x(55) value
             "[hni]      Nuovi codici di ISTAT dei processi prod.    ".
                   15  filler             pic  x(55) value
             "[hnu]      Numerazioni per 'gsr'                       ".
                   15  filler             pic  x(55) value
             "[hox]      Ordini di conferimento : estensioni         ".
                   15  filler             pic  x(55) value
             "[hpa]      Tabella parametri certificati analisi       ".
                   15  filler             pic  x(55) value
             "[hpd]      Anagrafica Produttori                       ".
                   15  filler             pic  x(55) value
             "[hpr]      Produttori e tipologie ISTAT rifiuti        ".
                   15  filler             pic  x(55) value
             "[hrc]      Catasto rifiuti : Classi                    ".
                   15  filler             pic  x(55) value
             "[hre]      Codici residui                              ".
                   15  filler             pic  x(55) value
             "[hrg]      Archivio registro carico-scarico            ".
                   15  filler             pic  x(55) value
             "[hrs]      Catasto rifiuti : Sottoclassi               ".
                   15  filler             pic  x(55) value
             "[hrt]      Catasto rifiuti : Tipologie                 ".
                   15  filler             pic  x(55) value
             "[hti]      Tabella tipi imballo                        ".
                   15  filler             pic  x(55) value
             "[htm]      Tabella tipi movimento                      ".
                   15  filler             pic  x(55) value
             "[htp]      Anagrafica Trasportatori                    ".
               10  w-arc-ele-tbr redefines
                   w-arc-ele-tbl.
                   15  w-arc-ele-ele occurs 034.
                       20  filler         pic  x(01)                  .
                       20  w-arc-ele-nam  pic  x(03)                  .
                       20  filler         pic  x(05)                  .
                       20  w-arc-ele-des  pic  x(46)                  .

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
      *              * Conversione [haf]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-haf-000      thru exe-cnv-haf-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hcr]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hcr-000      thru exe-cnv-hcr-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hct]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hct-000      thru exe-cnv-hct-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hor]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hor-000      thru exe-cnv-hor-999        .
      *              *-------------------------------------------------*
      *              * Conversione [hot]                               *
      *              *-------------------------------------------------*
           perform   exe-cnv-hot-000      thru exe-cnv-hot-999        .
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
           move      02                   to   v-dec                  .
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
      *    * Conversione [haf]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-haf-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "haf "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "trc/gsr/fls/ioc/obj/iofhaf"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-haf-999.
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
       exe-cnv-haf-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-haf                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-haf-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-haf-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    haf                                       .
       exe-cnv-haf-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   haf-k01                .
           start     haf    key not less
                            haf-k01
                            invalid key
                            go to exe-cnv-haf-800.
       exe-cnv-haf-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      haf    next
                            with no lock
                            at end
                            go to exe-cnv-haf-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-haf-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-haf-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-haf                 .
       exe-cnv-haf-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-haf                 .
           move      haf-ide-dat          to   rf-haf-ide-dat         .
           move      haf-ide-ute          to   rf-haf-ide-ute         .
           move      haf-ide-fas          to   rf-haf-ide-fas         .
           move      haf-cod-dpz          to   rf-haf-cod-dpz         .
           move      haf-num-prt          to   rf-haf-num-prt         .
           move      haf-dat-reg          to   rf-haf-dat-reg         .
           move      haf-prt-ocf          to   rf-haf-prt-ocf         .
           move      haf-cod-cli          to   rf-haf-cod-cli         .
           move      haf-dpz-cli          to   rf-haf-dpz-cli         .
           move      haf-tip-add          to   rf-haf-tip-add         .
           move      haf-cod-add          to   rf-haf-cod-add         .
           move      haf-num-pro          to   rf-haf-num-pro         .
           move      haf-alf-pro          to   rf-haf-alf-pro         .
           move      haf-des-add          to   rf-haf-des-add         .
           move      haf-cod-iva          to   rf-haf-cod-iva         .
           move      haf-ctp-ven          to   rf-haf-ctp-ven         .
           move      haf-qta-add          to   rf-haf-qta-add         .
           move      haf-prz-add          to   rf-haf-prz-add         .
           move      haf-prt-fit          to   rf-haf-prt-fit         .
           move      haf-prt-atz          to   rf-haf-prt-atz         .
           move      haf-prt-mmm          to   rf-haf-prt-mmm         .
           move      haf-dat-cpt          to   rf-haf-dat-cpt         .
           move      haf-ndo-rif          to   rf-haf-ndo-rif         .
           move      haf-ddo-rif          to   rf-haf-ddo-rif         .
           move      haf-flg-pul          to   rf-haf-flg-pul         .
           move      haf-flg-tda          to   rf-haf-flg-tda         .
           move      haf-dec-prz          to   rf-haf-dec-prz         .
           move      haf-alx-exp          to   rf-haf-alx-exp         .
       exe-cnv-haf-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario di addebito                 *
      *                  *---------------------------------------------*
           move      haf-prz-add          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-haf-prz-add         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-haf-msg-000
                                          thru exe-cnv-haf-msg-999    .
       exe-cnv-haf-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-haf                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-haf-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-haf-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-haf-250.
       exe-cnv-haf-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-haf                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     haf                                              .
       exe-cnv-haf-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-haf-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-haf-999.
       exe-cnv-haf-999.
           exit.

      *    *===========================================================*
      *    * Conversione [haf]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-haf-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      haf-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-haf-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hcr]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hcr-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hcr "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "trc/gsr/fls/ioc/obj/iofhcr"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hcr-999.
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
       exe-cnv-hcr-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hcr                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hcr-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hcr-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hcr                                       .
       exe-cnv-hcr-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hcr-k01                .
           start     hcr    key not less
                            hcr-k01
                            invalid key
                            go to exe-cnv-hcr-800.
       exe-cnv-hcr-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hcr    next
                            with no lock
                            at end
                            go to exe-cnv-hcr-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hcr-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hcr-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hcr                 .
       exe-cnv-hcr-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hcr                 .
           move      hcr-dat-reg          to   rf-hcr-dat-reg         .
           move      hcr-num-prt          to   rf-hcr-num-prt         .
           move      hcr-num-prg          to   rf-hcr-num-prg         .
           move      hcr-tip-mag          to   rf-hcr-tip-mag         .
           move      hcr-tip-rig          to   rf-hcr-tip-rig         .
           move      hcr-tmo-ctt          to   rf-hcr-tmo-ctt         .
           move      hcr-cod-dpz          to   rf-hcr-cod-dpz         .
           move      hcr-cod-dsl          to   rf-hcr-cod-dsl         .
           move      hcr-num-pro          to   rf-hcr-num-pro         .
           move      hcr-alf-pro          to   rf-hcr-alf-pro         .
           move      hcr-des-ext          to   rf-hcr-des-ext         .
           move      hcr-des-rig          to   rf-hcr-des-rig         .
           move      hcr-prz-uni          to   rf-hcr-prz-uni         .
           move      hcr-qsc-kgs          to   rf-hcr-qsc-kgs         .
           move      hcr-dat-cnf          to   rf-hcr-dat-cnf         .
           move      hcr-per-scc (1)      to   rf-hcr-per-scc (1)     .
           move      hcr-per-scc (2)      to   rf-hcr-per-scc (2)     .
           move      hcr-per-scc (3)      to   rf-hcr-per-scc (3)     .
           move      hcr-per-scc (4)      to   rf-hcr-per-scc (4)     .
           move      hcr-per-scc (5)      to   rf-hcr-per-scc (5)     .
           move      hcr-udm-pro          to   rf-hcr-udm-pro         .
           move      hcr-snx-ade          to   rf-hcr-snx-ade         .
           move      hcr-flg-ela          to   rf-hcr-flg-ela         .
           move      hcr-flg-pul          to   rf-hcr-flg-pul         .
           move      hcr-flg-tda          to   rf-hcr-flg-tda         .
           move      hcr-alx-exp          to   rf-hcr-alx-exp         .
       exe-cnv-hcr-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario di addebito                 *
      *                  *---------------------------------------------*
           move      hcr-prz-uni          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hcr-prz-uni         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hcr-msg-000
                                          thru exe-cnv-hcr-msg-999    .
       exe-cnv-hcr-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hcr                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hcr-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hcr-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hcr-250.
       exe-cnv-hcr-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hcr                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hcr                                              .
       exe-cnv-hcr-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hcr-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hcr-999.
       exe-cnv-hcr-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hcr]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hcr-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hcr-num-prt          to   v-num                  .
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
           move      hcr-num-prg          to   v-num                  .
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
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hcr-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hct]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hct-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hct "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "trc/gsr/fls/ioc/obj/iofhct"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hct-999.
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
       exe-cnv-hct-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hct-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hct-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hct                                       .
       exe-cnv-hct-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hct-k01                .
           start     hct    key not less
                            hct-k01
                            invalid key
                            go to exe-cnv-hct-800.
       exe-cnv-hct-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hct    next
                            with no lock
                            at end
                            go to exe-cnv-hct-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hct-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hct-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
       exe-cnv-hct-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hct                 .
           move      hct-ide-dat          to   rf-hct-ide-dat         .
           move      hct-ide-ute          to   rf-hct-ide-ute         .
           move      hct-ide-fas          to   rf-hct-ide-fas         .
           move      hct-tmo-ctt          to   rf-hct-tmo-ctt         .
           move      hct-num-prt          to   rf-hct-num-prt         .
           move      hct-cod-dpz          to   rf-hct-cod-dpz         .
           move      hct-dat-reg          to   rf-hct-dat-reg         .
           move      hct-prt-cfo          to   rf-hct-prt-cfo         .
           move      hct-dat-cfo          to   rf-hct-dat-cfo         .
           move      hct-cod-cli          to   rf-hct-cod-cli         .
           move      hct-dpz-cli          to   rf-hct-dpz-cli         .
           move      hct-cod-fop          to   rf-hct-cod-fop         .
           move      hct-cod-abi          to   rf-hct-cod-abi         .
           move      hct-cod-cab          to   rf-hct-cod-cab         .
           move      hct-ccc-app          to   rf-hct-ccc-app         .
           move      hct-cod-lst          to   rf-hct-cod-lst         .
           move      hct-cod-age          to   rf-hct-cod-age         .
           move      hct-cat-pvg          to   rf-hct-cat-pvg         .
           move      hct-per-pvg (1)      to   rf-hct-per-pvg (1)     .
           move      hct-per-pvg (2)      to   rf-hct-per-pvg (2)     .
           move      hct-per-pvg (3)      to   rf-hct-per-pvg (3)     .
           move      hct-amm-pvg          to   rf-hct-amm-pvg         .
           move      hct-tip-pro          to   rf-hct-tip-pro         .
           move      hct-cod-pro          to   rf-hct-cod-pro         .
           move      hct-dpz-pro          to   rf-hct-dpz-pro         .
           move      hct-tip-tra          to   rf-hct-tip-tra         .
           move      hct-cod-tra          to   rf-hct-cod-tra         .
           move      hct-dpz-tra          to   rf-hct-dpz-tra         .
           move      hct-dat-scd          to   rf-hct-dat-scd         .
           move      hct-num-rda          to   rf-hct-num-rda         .
           move      hct-ndo-rif          to   rf-hct-ndo-rif         .
           move      hct-ddo-rif          to   rf-hct-ddo-rif         .
           move      hct-ndo-nsr          to   rf-hct-ndo-nsr         .
           move      hct-ddo-nsr          to   rf-hct-ddo-nsr         .
           move      hct-cct-rif          to   rf-hct-cct-rif         .
           move      hct-des-rif          to   rf-hct-des-rif         .
           move      hct-ccl-rif          to   rf-hct-ccl-rif         .
           move      hct-num-pro          to   rf-hct-num-pro         .
           move      hct-alf-pro          to   rf-hct-alf-pro         .
           move      hct-qta-kgg          to   rf-hct-qta-kgg         .
           move      hct-qta-kgs          to   rf-hct-qta-kgs         .
           move      hct-num-cnf          to   rf-hct-num-cnf         .
           move      hct-qsc-kgs          to   rf-hct-qsc-kgs         .
           move      hct-din-cnf          to   rf-hct-din-cnf         .
           move      hct-frs-ngg          to   rf-hct-frs-ngg         .
           move      hct-crf-ana          to   rf-hct-crf-ana         .
           move      hct-cod-dsl          to   rf-hct-cod-dsl         .
           move      hct-ele-add (1)      to   rf-hct-ele-add (1)     .
           move      hct-ele-add (2)      to   rf-hct-ele-add (2)     .
           move      hct-ele-add (3)      to   rf-hct-ele-add (3)     .
           move      hct-ele-add (4)      to   rf-hct-ele-add (4)     .
           move      hct-ele-add (5)      to   rf-hct-ele-add (5)     .
           move      hct-ele-add (6)      to   rf-hct-ele-add (6)     .
           move      hct-tip-add (1)      to   rf-hct-tip-add (1)     .
           move      hct-tip-add (2)      to   rf-hct-tip-add (2)     .
           move      hct-tip-add (3)      to   rf-hct-tip-add (3)     .
           move      hct-tip-add (4)      to   rf-hct-tip-add (4)     .
           move      hct-tip-add (5)      to   rf-hct-tip-add (5)     .
           move      hct-tip-add (6)      to   rf-hct-tip-add (6)     .
           move      hct-cod-ann          to   rf-hct-cod-ann         .
           move      hct-ann-ctt          to   rf-hct-ann-ctt         .
           move      hct-flg-ela          to   rf-hct-flg-ela         .
           move      hct-flg-pul          to   rf-hct-flg-pul         .
           move      hct-flg-stp          to   rf-hct-flg-stp         .
           move      hct-flg-tda          to   rf-hct-flg-tda         .
           move      hct-flg-trt          to   rf-hct-flg-trt         .
           move      hct-cod-cee          to   rf-hct-cod-cee         .
           move      hct-cod-res          to   rf-hct-cod-res         .
           move      hct-cad-gio          to   rf-hct-cad-gio         .
           move      hct-snx-sab          to   rf-hct-snx-sab         .
           move      hct-alx-exp          to   rf-hct-alx-exp         .
       exe-cnv-hct-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Castelletto addebiti                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-cnv-hct-520.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-cnv-hct-700.
      *                  *---------------------------------------------*
      *                  * Test speciale per cliente 263 e per ordini  *
      *                  * successivi al 31/08/01                      *
      *                  *---------------------------------------------*
           if        hct-cod-cli          not  = 0000263
                     go to exe-cnv-hct-560.
           if        hct-dat-reg          not  > 1010831
                     go to exe-cnv-hct-560.
           go to     exe-cnv-hct-580.
       exe-cnv-hct-560.
      *                  *---------------------------------------------*
      *                  * Valore di addebito                          *
      *                  *---------------------------------------------*
           if        hct-ele-add (w-inx)  =    zero
                     move  zero           to   rf-hct-ele-add (w-inx)
                     go to exe-cnv-hct-580.
           move      hct-ele-add (w-inx)  to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hct-ele-add (w-inx) .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hct-msg-000
                                          thru exe-cnv-hct-msg-999    .
       exe-cnv-hct-580.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-cnv-hct-520.
       exe-cnv-hct-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hct-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hct-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hct-250.
       exe-cnv-hct-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hct                                              .
       exe-cnv-hct-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hct-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hct-999.
       exe-cnv-hct-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hct]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hct-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hct-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hct-msg-999.
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
           move      "trc/gsr/fls/ioc/obj/iofhor"
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
       exe-cnv-hor-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hor-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hor-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hor                                       .
       exe-cnv-hor-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hor-k01                .
           start     hor    key not less
                            hor-k01
                            invalid key
                            go to exe-cnv-hor-800.
       exe-cnv-hor-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
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
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hor-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
       exe-cnv-hor-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hor                 .
           move      hor-dat-reg          to   rf-hor-dat-reg         .
           move      hor-num-prt          to   rf-hor-num-prt         .
           move      hor-num-prg          to   rf-hor-num-prg         .
           move      hor-tip-mag          to   rf-hor-tip-mag         .
           move      hor-tip-rig          to   rf-hor-tip-rig         .
           move      hor-tmo-ctt          to   rf-hor-tmo-ctt         .
           move      hor-cod-dpz          to   rf-hor-cod-dpz         .
           move      hor-cod-dsl          to   rf-hor-cod-dsl         .
           move      hor-num-pro          to   rf-hor-num-pro         .
           move      hor-alf-pro          to   rf-hor-alf-pro         .
           move      hor-des-ext          to   rf-hor-des-ext         .
           move      hor-des-rig          to   rf-hor-des-rig         .
           move      hor-prz-uni          to   rf-hor-prz-uni         .
           move      hor-qsc-kgs          to   rf-hor-qsc-kgs         .
           move      hor-dat-cnf          to   rf-hor-dat-cnf         .
           move      hor-per-scc (1)      to   rf-hor-per-scc (1)     .
           move      hor-per-scc (2)      to   rf-hor-per-scc (2)     .
           move      hor-per-scc (3)      to   rf-hor-per-scc (3)     .
           move      hor-per-scc (4)      to   rf-hor-per-scc (4)     .
           move      hor-per-scc (5)      to   rf-hor-per-scc (5)     .
           move      hor-udm-pro          to   rf-hor-udm-pro         .
           move      hor-snx-ade          to   rf-hor-snx-ade         .
           move      hor-flg-ela          to   rf-hor-flg-ela         .
           move      hor-flg-pul          to   rf-hor-flg-pul         .
           move      hor-flg-tda          to   rf-hor-flg-tda         .
           move      hor-alx-exp          to   rf-hor-alx-exp         .
       exe-cnv-hor-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prezzo unitario di addebito                 *
      *                  *---------------------------------------------*
           move      hor-prz-uni          to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hor-prz-uni         .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hor-msg-000
                                          thru exe-cnv-hor-msg-999    .
       exe-cnv-hor-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
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
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hor                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hor                                              .
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
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hor-999.
       exe-cnv-hor-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hor]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hor-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hor-num-prt          to   v-num                  .
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
           move      hor-num-prg          to   v-num                  .
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
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hor-msg-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hot]                                         *
      *    *-----------------------------------------------------------*
       exe-cnv-hot-000.
      *              *-------------------------------------------------*
      *              * Preparazione file area generica                 *
      *              *-------------------------------------------------*
           move      "hot "               to   f-xxx-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname modulo oggetto            *
      *              *-------------------------------------------------*
           move      "trc/gsr/fls/ioc/obj/iofhot"
                                          to   f-xxx-obj              .
      *              *-------------------------------------------------*
      *              * Richiesta di conferma, se manuale               *
      *              *-------------------------------------------------*
           perform   ric-cnf-man-000      thru ric-cnf-man-999        .
      *              *-------------------------------------------------*
      *              * Se non confermato : ad uscita                   *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S"
                     go to exe-cnv-hot-999.
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
       exe-cnv-hot-100.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hot                 .
      *              *-------------------------------------------------*
      *              * Composizione pathname file originale            *
      *              *-------------------------------------------------*
           move      spaces               to   f-hot-pat              .
           string    "/abd/azi/"
                                delimited by size
                     rr-cod-azi delimited by spaces
                     "/"        delimited by size
                     f-xxx-nam  delimited by spaces
                                          into f-hot-pat              .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           open      i-o    hot                                       .
       exe-cnv-hot-200.
      *              *-------------------------------------------------*
      *              * Start su [old]                                  *
      *              *-------------------------------------------------*
           move      low-values           to   hot-k01                .
           start     hot    key not less
                            hot-k01
                            invalid key
                            go to exe-cnv-hot-800.
       exe-cnv-hot-250.
      *              *-------------------------------------------------*
      *              * Next su [old]                                   *
      *              *-------------------------------------------------*
           read      hot    next
                            with no lock
                            at end
                            go to exe-cnv-hot-800.
      *              *-------------------------------------------------*
      *              * Incremento numero records letti                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-000      thru inc-rec-let-999        .
       exe-cnv-hot-300.
      *              *-------------------------------------------------*
      *              * Test se conversione da effettuare               *
      *              *-------------------------------------------------*
       exe-cnv-hot-350.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare [new]               *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hot                 .
       exe-cnv-hot-400.
      *              *-------------------------------------------------*
      *              * Composizione [new]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripresa record                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-hot                 .
           move      hot-ide-dat          to   rf-hot-ide-dat         .
           move      hot-ide-ute          to   rf-hot-ide-ute         .
           move      hot-ide-fas          to   rf-hot-ide-fas         .
           move      hot-tmo-ctt          to   rf-hot-tmo-ctt         .
           move      hot-num-prt          to   rf-hot-num-prt         .
           move      hot-cod-dpz          to   rf-hot-cod-dpz         .
           move      hot-dat-reg          to   rf-hot-dat-reg         .
           move      hot-prt-ctt          to   rf-hot-prt-ctt         .
           move      hot-dat-cfo          to   rf-hot-dat-cfo         .
           move      hot-cod-cli          to   rf-hot-cod-cli         .
           move      hot-dpz-cli          to   rf-hot-dpz-cli         .
           move      hot-cod-fop          to   rf-hot-cod-fop         .
           move      hot-cod-abi          to   rf-hot-cod-abi         .
           move      hot-cod-cab          to   rf-hot-cod-cab         .
           move      hot-ccc-app          to   rf-hot-ccc-app         .
           move      hot-cod-lst          to   rf-hot-cod-lst         .
           move      hot-cod-age          to   rf-hot-cod-age         .
           move      hot-cat-pvg          to   rf-hot-cat-pvg         .
           move      hot-per-pvg (1)      to   rf-hot-per-pvg (1)     .
           move      hot-per-pvg (2)      to   rf-hot-per-pvg (2)     .
           move      hot-per-pvg (3)      to   rf-hot-per-pvg (3)     .
           move      hot-amm-pvg          to   rf-hot-amm-pvg         .
           move      hot-tip-pro          to   rf-hot-tip-pro         .
           move      hot-cod-pro          to   rf-hot-cod-pro         .
           move      hot-dpz-pro          to   rf-hot-dpz-pro         .
           move      hot-tip-tra          to   rf-hot-tip-tra         .
           move      hot-cod-tra          to   rf-hot-cod-tra         .
           move      hot-dpz-tra          to   rf-hot-dpz-tra         .
           move      hot-dat-scd          to   rf-hot-dat-scd         .
           move      hot-num-rda          to   rf-hot-num-rda         .
           move      hot-ndo-rif          to   rf-hot-ndo-rif         .
           move      hot-ddo-rif          to   rf-hot-ddo-rif         .
           move      hot-ndo-nsr          to   rf-hot-ndo-nsr         .
           move      hot-ddo-nsr          to   rf-hot-ddo-nsr         .
           move      hot-cct-rif          to   rf-hot-cct-rif         .
           move      hot-des-rif          to   rf-hot-des-rif         .
           move      hot-ccl-rif          to   rf-hot-ccl-rif         .
           move      hot-num-pro          to   rf-hot-num-pro         .
           move      hot-alf-pro          to   rf-hot-alf-pro         .
           move      hot-qta-kgg          to   rf-hot-qta-kgg         .
           move      hot-qta-kgs          to   rf-hot-qta-kgs         .
           move      hot-num-cnf          to   rf-hot-num-cnf         .
           move      hot-qsc-kgs          to   rf-hot-qsc-kgs         .
           move      hot-din-cnf          to   rf-hot-din-cnf         .
           move      hot-frs-ngg          to   rf-hot-frs-ngg         .
           move      hot-crf-ana          to   rf-hot-crf-ana         .
           move      hot-cod-dsl          to   rf-hot-cod-dsl         .
           move      hot-ele-add (1)      to   rf-hot-ele-add (1)     .
           move      hot-ele-add (2)      to   rf-hot-ele-add (2)     .
           move      hot-ele-add (3)      to   rf-hot-ele-add (3)     .
           move      hot-ele-add (4)      to   rf-hot-ele-add (4)     .
           move      hot-ele-add (5)      to   rf-hot-ele-add (5)     .
           move      hot-ele-add (6)      to   rf-hot-ele-add (6)     .
           move      hot-tip-add (1)      to   rf-hot-tip-add (1)     .
           move      hot-tip-add (2)      to   rf-hot-tip-add (2)     .
           move      hot-tip-add (3)      to   rf-hot-tip-add (3)     .
           move      hot-tip-add (4)      to   rf-hot-tip-add (4)     .
           move      hot-tip-add (5)      to   rf-hot-tip-add (5)     .
           move      hot-tip-add (6)      to   rf-hot-tip-add (6)     .
           move      hot-cod-ann          to   rf-hot-cod-ann         .
           move      hot-ann-ctt          to   rf-hot-ann-ctt         .
           move      hot-flg-ela          to   rf-hot-flg-ela         .
           move      hot-flg-pul          to   rf-hot-flg-pul         .
           move      hot-flg-stp          to   rf-hot-flg-stp         .
           move      hot-flg-tda          to   rf-hot-flg-tda         .
           move      hot-flg-trt          to   rf-hot-flg-trt         .
           move      hot-cod-cee          to   rf-hot-cod-cee         .
           move      hot-cod-res          to   rf-hot-cod-res         .
           move      hot-cad-gio          to   rf-hot-cad-gio         .
           move      hot-snx-sab          to   rf-hot-snx-sab         .
           move      hot-alx-exp          to   rf-hot-alx-exp         .
       exe-cnv-hot-500.
      *              *-------------------------------------------------*
      *              * Conversioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Castelletto addebiti                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       exe-cnv-hot-520.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to exe-cnv-hot-700.
      *                  *---------------------------------------------*
      *                  * Test speciale per cliente 263 e per ordini  *
      *                  * successivi al 31/08/01                      *
      *                  *---------------------------------------------*
           if        hot-cod-cli          not  = 0000263
                     go to exe-cnv-hot-560.
           if        hot-dat-reg          not  > 1010831
                     go to exe-cnv-hot-560.
           go to     exe-cnv-hot-580.
       exe-cnv-hot-560.
      *                  *---------------------------------------------*
      *                  * Valore di addebito                          *
      *                  *---------------------------------------------*
           if        hot-ele-add (w-inx)  =    zero
                     move  zero           to   rf-hot-ele-add (w-inx)
                     go to exe-cnv-hot-580.
           move      hot-ele-add (w-inx)  to   w-cal-imp-eur-imi      .
           move      zero                 to   w-cal-imp-eur-dec      .
           perform   cal-imp-eur-000      thru cal-imp-eur-999        .
           move      w-cal-imp-eur-ime    to   rf-hot-ele-add (w-inx) .
      *
           if        w-cal-imp-eur-flg    not  = spaces
                     perform exe-cnv-hot-msg-000
                                          thru exe-cnv-hot-msg-999    .
       exe-cnv-hot-580.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-cnv-hot-520.
       exe-cnv-hot-700.
      *              *-------------------------------------------------*
      *              * Scrittura [new]                                 *
      *              *-------------------------------------------------*
           move      f-xxx-ope            to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hot                 .
      *              *-------------------------------------------------*
      *              * Se errore : no incremento records scritti       *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cnv-hot-750.
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-cnv-hot-750.
      *              *-------------------------------------------------*
      *              * Riciclo a Next                                  *
      *              *-------------------------------------------------*
           go to     exe-cnv-hot-250.
       exe-cnv-hot-800.
      *              *-------------------------------------------------*
      *              * [new]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      f-xxx-obj            to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hot                 .
      *              *-------------------------------------------------*
      *              * [old]                                           *
      *              *-------------------------------------------------*
           close     hot                                              .
       exe-cnv-hot-850.
      *              *-------------------------------------------------*
      *              * Visualizzazione nr records letti e scritti      *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
       exe-cnv-hot-900.
      *              *-------------------------------------------------*
      *              * Scrittura rullino messaggi                      *
      *              *-------------------------------------------------*
           perform   wrt-rou-msg-000      thru wrt-rou-msg-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cnv-hot-999.
       exe-cnv-hot-999.
           exit.

      *    *===========================================================*
      *    * Conversione [hot]                                         *
      *    *                                                           *
      *    * Messaggi di errore                                        *
      *    *-----------------------------------------------------------*
       exe-cnv-hot-msg-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing protocollo                          *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      hot-num-prt          to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-ide-prt              .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] ID "    delimited by   size
                     w-ide-prt  delimited by   spaces
                     " - Inferiore a 20 Lire !"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-cnv-hot-msg-999.
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
      *    * Routines per la conversione                               *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/prg/cpy/cnveur01.wks"                   .

