       Identification Division.
       Program-Id.                                 loadvar            .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    loadvar             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/01/00    *
      *                       Ultima revisione:    NdK del 10/04/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:    Prove di accesso tramite Flash 5.0         *
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
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-des-key        pic  x(40)                  .
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFPRO                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-alf-pro        pic  x(14)                  .
                   15  fil-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNPRO                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-syn-pro        pic  x(13)                  .
                   15  fil-num-pro-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cla-pro        pic  9(05)       comp-3     .
                   15  fil-gru-pro        pic  9(05)       comp-3     .
                   15  fil-sgr-pro        pic  9(05)       comp-3     .
                   15  fil-des-key-6      pic  x(40)                  .
                   15  fil-num-pro-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cla-pro-7      pic  9(05)       comp-3     .
                   15  fil-gru-pro-7      pic  9(05)       comp-3     .
                   15  fil-sgr-pro-7      pic  9(05)       comp-3     .
                   15  fil-alf-pro-7      pic  x(14)                  .
                   15  fil-num-pro-7      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : KLBPRO                         *
      *            *---------------------------------------------------*
               10  fil-k08.
                   15  fil-klb-pro        pic  x(13)                  .
                   15  fil-num-pro-8      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-inf-gen.
                   15  fil-ide-ute        pic  x(08)                  .
                   15  fil-ide-fas        pic  x(06)                  .
                   15  fil-des-pro        pic  x(40)                  .
                   15  fil-des-pdx        pic  9(01)                  .
                   15  fil-tip-pro        pic  9(02)                  .
                   15  fil-tip-acp        pic  9(02)                  .
                   15  fil-not-g01        pic  x(40)                  .
               10  fil-inf-fis.
                   15  fil-tip-cfz        pic  9(02)                  .
                   15  fil-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  fil-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  fil-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  fil-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  fil-dim-pro.
                       20  fil-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  fil-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  fil-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  fil-pcl-fis        pic  x(10)                  .
                   15  fil-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  fil-coe-div        pic  9(04)v9(03) comp-3     .
                   15  fil-spc-lib        pic  x(20)                  .
               10  fil-inf-fat.
                   15  fil-cod-iva        pic  9(05)       comp-3     .
                   15  fil-ctp-ven        pic  9(07)       comp-3     .
                   15  fil-umi-ven        pic  x(03)                  .
                   15  fil-dec-qta        pic  9(01)                  .
                   15  fil-sgl-vlt        pic  x(03)                  .
                   15  fil-dec-vlt        pic  9(01)                  .
                   15  fil-dec-prz        pic  9(01)                  .
                   15  fil-prz-lst        pic  9(09)       comp-3     .
                   15  fil-lot-ven        pic  9(06)v9(03) comp-3     .
                   15  fil-epz-rgf        pic  9(01)                  .
                   15  fil-snx-2qt        pic  9(01)                  .
                   15  fil-dec-2qt        pic  9(01)                  .
                   15  fil-snx-3qt        pic  9(01)                  .
                   15  fil-dec-3qt        pic  9(01)                  .
                   15  fil-snx-2pz        pic  9(01)                  .
                   15  fil-tip-vve        pic  x(03)                  .
               10  fil-inf-cdv.
                   15  fil-cat-scr        pic  9(05)       comp-3     .
                   15  fil-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  fil-inf-gag.
                   15  fil-cat-pvg        pic  9(05)       comp-3     .
                   15  fil-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
                   15  fil-amm-pvg        pic  9(09)       comp-3     .
               10  fil-inf-lst.
                   15  fil-snx-lst        pic  9(01)                  .
                   15  fil-epz-lst.
                       20  fil-epz-ele occurs 20
                                          pic  x(01)                  .
                   15  fil-pag-lst        pic  x(10)                  .
                   15  fil-rfl-lst        pic  x(12)                  .
                   15  fil-tmc-lst        pic  9(03)       comp-3     .
                   15  fil-daa-lst        pic  9(01)                  .
                   15  fil-aut-lst        pic  x(03)                  .
               10  fil-inf-pcs.
                   15  fil-cod-s01        pic  9(05)       comp-3     .
                   15  fil-cod-s02        pic  9(05)       comp-3     .
                   15  fil-cod-s03        pic  9(05)       comp-3     .
                   15  fil-dat-icm        pic  9(07)       comp-3     .
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)       comp-3     .
                   15  fil-sta-tuc        pic  9(07)       comp-3     .
                   15  fil-sta-tux        pic  9(02)                  .
               10  fil-inf-mkt.
                   15  fil-cld-imp        pic  x(01)                  .
                   15  fil-pre-ctn        pic  9(02)                  .
                   15  fil-gra-ico        pic  9(02)                  .
                   15  fil-pcl-ccz        pic  9(02)                  .
                   15  fil-cod-mkt        pic  9(05)       comp-3     .
                   15  fil-ind-mkt        pic  x(10)                  .
               10  fil-inf-bdg.
                   15  fil-cla-bdg        pic  9(05)       comp-3     .
               10  fil-inf-bol.
                   15  fil-for-blo        pic  9(02)                  .
                   15  fil-dor-blo        pic  9(07)       comp-3     .
                   15  fil-fco-blo        pic  9(02)                  .
                   15  fil-dco-blo        pic  9(07)       comp-3     .
               10  fil-inf-iic.
                   15  fil-cdn-cdm        pic  9(08)                  .
               10  fil-inf-aps.
                   15  fil-alx-exp.
                       20  filler occurs 80
                                          pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*
      
      *    *===========================================================*
      *    * File area per [fil]                                       *
      *    *-----------------------------------------------------------*
       01  f-fil.
           05  f-fil-nam                  pic  x(04)                  .
           05  f-fil-pat                  pic  x(40)                  .
           05  f-fil-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area per parametri di 'chaining' dal chiamante            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Variabile di environment V_ALTER_HOME                 *
      *        *-------------------------------------------------------*
       77  parametro                      pic  x(200)                 .

      ******************************************************************
       Procedure Division             chaining parametro              .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Open [dcp]                                      *
      *              *-------------------------------------------------*
           move      "/abd/azi/prv/dcp"
                                          to   f-fil-pat              .
           open      i-o    fil                                       .
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      1                    to   fil-num-pro            .
           move      "PROVA"              to   fil-alf-pro            .
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
           move      parametro            to   fil-des-pro            .
      *              *-------------------------------------------------*
      *              * Forced put record                               *
      *              *-------------------------------------------------*
           rewrite   fil-rec invalid key
                            go to main-900.
       main-900.
      *              *-------------------------------------------------*
      *              * Close [dcp]                                     *
      *              *-------------------------------------------------*
           close     fil                                              .
       main-999.
           exit      program.
           stop run.

