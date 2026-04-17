       Identification Division.
       Program-Id.                                 iofdcc             .
      *================================================================*
      *                                                                *
      *                  Input-Output File dcc                         *
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
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-cli        pic  9(07)       comp-3     .
                   15  fil-dpz-cli        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-cod-cli-2      pic  9(07)       comp-3     .
                   15  fil-dpz-cli-2      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-rag-key        pic  x(40)                  .
                   15  fil-cod-cli-3      pic  9(07)       comp-3     .
                   15  fil-dpz-cli-3      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-inf-gen.
                   15  fil-ide-ute        pic  x(08)                  .
                   15  fil-ide-fas        pic  x(06)                  .
                   15  fil-rag-soc        pic  x(40)                  .
                   15  fil-via-dcc        pic  x(40)                  .
                   15  fil-loc-dcc        pic  x(40)                  .
                   15  fil-cod-naz        pic  x(03)                  .
                   15  fil-cod-cmn        pic  9(05)       comp-3     .
                   15  fil-cod-fzn        pic  9(03)       comp-3     .
                   15  fil-cod-lct        pic  9(03)       comp-3     .
                   15  fil-rs1-doc        pic  x(40)                  .
                   15  fil-rs2-doc        pic  x(40)                  .
                   15  fil-not-g01        pic  x(40)                  .
                   15  fil-not-g02        pic  x(40)                  .
                   15  fil-not-g03        pic  x(40)                  .
                   15  fil-num-tel        pic  x(20)                  .
                   15  fil-tel-alt        pic  x(20)                  .
                   15  fil-nom-int        pic  x(30)                  .
                   15  fil-int-alt        pic  x(30)                  .
                   15  fil-num-fax        pic  x(20)                  .
                   15  fil-num-tlx        pic  x(20)                  .
                   15  fil-num-ptp        pic  x(20)                  .
                   15  fil-num-ptf        pic  x(20)                  .
                   15  fil-tel-mdm        pic  x(20)                  .
                   15  fil-idn-ema        pic  x(20)                  .
                   15  fil-inl-dcm        pic  9(02)                  .
                   15  fil-inl-pgt        pic  9(02)                  .
                   15  fil-noi-fnt        pic  x(15)                  .
                   15  fil-mod-sft        pic  9(02)                  .
               10  fil-inf-fat.
                   15  fil-tas-ivc        pic  9(02)                  .
                   15  fil-ctp-ven        pic  9(07)       comp-3     .
                   15  fil-tdp-plc        pic  9(02)                  .
                   15  fil-per-fat        pic  9(02)                  .
                   15  fil-rag-bft        pic  9(02)                  .
                   15  fil-epz-pes        pic  9(02)                  .
                   15  fil-cod-vlt        pic  x(03)                  .
                   15  fil-mom-acv        pic  9(02)                  .
                   15  fil-snx-rlv        pic  x(01)                  .
                   15  fil-mom-alv        pic  9(02)                  .
                   15  fil-cod-lng        pic  x(03)                  .
                   15  fil-tip-frn        pic  9(02)                  .
                   15  fil-arc-plf        pic  9(07)       comp-3     .
                   15  fil-dpz-plf        pic  x(04)                  .
                   15  fil-tip-ftz        pic  9(02)                  .
               10  fil-inf-cdv.
                   15  fil-cod-lst        pic  x(03)                  .
                   15  fil-cat-scr        pic  9(05)       comp-3     .
                   15  fil-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  fil-cat-scc        pic  9(05)       comp-3     .
                   15  fil-per-scc        pic  9(02)v9(01) comp-3     .
                   15  fil-add-spe occurs 06.
                       20  fil-snm-spe    pic  9(02)                  .
                       20  fil-per-spe    pic  9(02)v9(01) comp-3     .
                       20  fil-imp-spe    pic  9(09)       comp-3     .
                   15  fil-vde-fat occurs 06.
                       20  fil-vde-cod    pic  x(03)                  .
               10  fil-inf-cdp.
                   15  fil-cod-fop        pic  9(07)       comp-3     .
                   15  fil-tip-esm        pic  9(02)                  .
                   15  fil-ggg-alt        pic  9(02)                  .
                   15  fil-mmm-e01        pic  9(02)                  .
                   15  fil-mmm-e02        pic  9(02)                  .
                   15  fil-add-spi        pic  x(03)                  .
                   15  fil-add-spb        pic  x(03)                  .
                   15  fil-cod-abi        pic  9(05)       comp-3     .
                   15  fil-cod-cab        pic  9(05)       comp-3     .
                   15  fil-ccc-app        pic  x(12)                  .
                   15  fil-cod-cin        pic  x(01)                  .
                   15  fil-nos-ban        pic  x(10)                  .
                   15  fil-nos-bpe        pic  x(10)                  .
                   15  fil-nos-ccp        pic  x(10)                  .
                   15  fil-ipr-iel        pic  9(02)                  .
               10  fil-inf-gag.
                   15  fil-cod-age        pic  9(07)       comp-3     .
                   15  fil-cat-pvg        pic  9(05)       comp-3     .
                   15  fil-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
               10  fil-inf-pcs.
                   15  fil-cod-zon        pic  9(05)       comp-3     .
                   15  fil-cod-cat        pic  9(05)       comp-3     .
                   15  fil-cod-stt        pic  9(05)       comp-3     .
                   15  fil-dat-aqz        pic  9(07)       comp-3     .
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)       comp-3     .
                   15  fil-sta-tuc        pic  9(07)       comp-3     .
                   15  fil-sta-tux        pic  9(02)                  .
               10  fil-inf-mkt.
                   15  fil-cld-imp        pic  x(01)                  .
                   15  fil-tra-pco        pic  9(02)                  .
                   15  fil-gra-ico        pic  9(02)                  .
                   15  fil-pcl-ccz        pic  9(02)                  .
                   15  fil-pre-ctn        pic  9(02)                  .
                   15  fil-org-ctt        pic  9(02)                  .
                   15  fil-cod-mkt        pic  9(05)       comp-3     .
                   15  fil-ind-mkt        pic  x(10)                  .
               10  fil-inf-bdg.
                   15  fil-cla-bdg        pic  9(05)       comp-3     .
               10  fil-inf-bol.
                   15  fil-for-blo        pic  9(02)                  .
                   15  fil-dor-blo        pic  9(07)       comp-3     .
                   15  fil-fco-blo        pic  9(02)                  .
                   15  fil-dco-blo        pic  9(07)       comp-3     .
                   15  fil-dtt-acu        pic  9(02)                  .
                   15  fil-cod-vet        pic  9(07)       comp-3     .
                   15  fil-cod-vt2        pic  9(07)       comp-3     .
                   15  fil-cod-vt3        pic  9(07)       comp-3     .
                   15  fil-dst-klm        pic  9(04)v9(01) comp-3     .
                   15  fil-dti-cpf        pic  9(07)       comp-3     .
                   15  fil-dtf-cpf        pic  9(07)       comp-3     .
                   15  fil-gdl-nls        pic  x(07)                  .
                   15  fil-oam-oin        pic  9(04)                  .
                   15  fil-oam-ofi        pic  9(04)                  .
                   15  fil-oap-oin        pic  9(04)                  .
                   15  fil-oap-ofi        pic  9(04)                  .
                   15  fil-abn-vtt        pic  x(12)                  .
               10  fil-inf-aps.
                   15  fil-idn-em2        pic  x(20)                  .
                   15  fil-cod-cdv        pic  9(03)                  .
                   15  fil-cin-eur        pic  x(02)                  .
                   15  fil-cop-pcl        pic  x(03)                  .
                   15  fil-cop-tdo        pic  x(01)                  .
                   15  fil-tip-mci        pic  x(01)                  .
                   15  fil-alx-exp.
                       20  filler occurs 38
                                          pic  x(01)                  .

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
      *            * Chiave numero 01 : CODCLI                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-cli        pic  9(07)       comp-3     .
                   15  pul-dpz-cli        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-cod-cli-2      pic  9(07)       comp-3     .
                   15  pul-dpz-cli-2      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-rag-key        pic  x(40)                  .
                   15  pul-cod-cli-3      pic  9(07)       comp-3     .
                   15  pul-dpz-cli-3      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-inf-gen.
                   15  pul-ide-ute        pic  x(08)                  .
                   15  pul-ide-fas        pic  x(06)                  .
                   15  pul-rag-soc        pic  x(40)                  .
                   15  pul-via-dcc        pic  x(40)                  .
                   15  pul-loc-dcc        pic  x(40)                  .
                   15  pul-cod-naz        pic  x(03)                  .
                   15  pul-cod-cmn        pic  9(05)       comp-3     .
                   15  pul-cod-fzn        pic  9(03)       comp-3     .
                   15  pul-cod-lct        pic  9(03)       comp-3     .
                   15  pul-rs1-doc        pic  x(40)                  .
                   15  pul-rs2-doc        pic  x(40)                  .
                   15  pul-not-g01        pic  x(40)                  .
                   15  pul-not-g02        pic  x(40)                  .
                   15  pul-not-g03        pic  x(40)                  .
                   15  pul-num-tel        pic  x(20)                  .
                   15  pul-tel-alt        pic  x(20)                  .
                   15  pul-nom-int        pic  x(30)                  .
                   15  pul-int-alt        pic  x(30)                  .
                   15  pul-num-fax        pic  x(20)                  .
                   15  pul-num-tlx        pic  x(20)                  .
                   15  pul-num-ptp        pic  x(20)                  .
                   15  pul-num-ptf        pic  x(20)                  .
                   15  pul-tel-mdm        pic  x(20)                  .
                   15  pul-idn-ema        pic  x(20)                  .
                   15  pul-inl-dcm        pic  9(02)                  .
                   15  pul-inl-pgt        pic  9(02)                  .
                   15  pul-noi-fnt        pic  x(15)                  .
                   15  pul-mod-sft        pic  9(02)                  .
               10  pul-inf-fat.
                   15  pul-tas-ivc        pic  9(02)                  .
                   15  pul-ctp-ven        pic  9(07)       comp-3     .
                   15  pul-tdp-plc        pic  9(02)                  .
                   15  pul-per-fat        pic  9(02)                  .
                   15  pul-rag-bft        pic  9(02)                  .
                   15  pul-epz-pes        pic  9(02)                  .
                   15  pul-cod-vlt        pic  x(03)                  .
                   15  pul-mom-acv        pic  9(02)                  .
                   15  pul-snx-rlv        pic  x(01)                  .
                   15  pul-mom-alv        pic  9(02)                  .
                   15  pul-cod-lng        pic  x(03)                  .
                   15  pul-tip-frn        pic  9(02)                  .
                   15  pul-arc-plf        pic  9(07)       comp-3     .
                   15  pul-dpz-plf        pic  x(04)                  .
                   15  pul-tip-ftz        pic  9(02)                  .
               10  pul-inf-cdv.
                   15  pul-cod-lst        pic  x(03)                  .
                   15  pul-cat-scr        pic  9(05)       comp-3     .
                   15  pul-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
                   15  pul-cat-scc        pic  9(05)       comp-3     .
                   15  pul-per-scc        pic  9(02)v9(01) comp-3     .
                   15  pul-add-spe occurs 06.
                       20  pul-snm-spe    pic  9(02)                  .
                       20  pul-per-spe    pic  9(02)v9(01) comp-3     .
                       20  pul-imp-spe    pic  9(09)       comp-3     .
                   15  pul-vde-fat occurs 06.
                       20  pul-vde-cod    pic  x(03)                  .
               10  pul-inf-cdp.
                   15  pul-cod-fop        pic  9(07)       comp-3     .
                   15  pul-tip-esm        pic  9(02)                  .
                   15  pul-ggg-alt        pic  9(02)                  .
                   15  pul-mmm-e01        pic  9(02)                  .
                   15  pul-mmm-e02        pic  9(02)                  .
                   15  pul-add-spi        pic  x(03)                  .
                   15  pul-add-spb        pic  x(03)                  .
                   15  pul-cod-abi        pic  9(05)       comp-3     .
                   15  pul-cod-cab        pic  9(05)       comp-3     .
                   15  pul-ccc-app        pic  x(12)                  .
                   15  pul-cod-cin        pic  x(01)                  .
                   15  pul-nos-ban        pic  x(10)                  .
                   15  pul-nos-bpe        pic  x(10)                  .
                   15  pul-nos-ccp        pic  x(10)                  .
                   15  pul-ipr-iel        pic  9(02)                  .
               10  pul-inf-gag.
                   15  pul-cod-age        pic  9(07)       comp-3     .
                   15  pul-cat-pvg        pic  9(05)       comp-3     .
                   15  pul-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
               10  pul-inf-pcs.
                   15  pul-cod-zon        pic  9(05)       comp-3     .
                   15  pul-cod-cat        pic  9(05)       comp-3     .
                   15  pul-cod-stt        pic  9(05)       comp-3     .
                   15  pul-dat-aqz        pic  9(07)       comp-3     .
                   15  pul-sta-tus        pic  9(02)                  .
                   15  pul-sta-tud        pic  9(07)       comp-3     .
                   15  pul-sta-tuc        pic  9(07)       comp-3     .
                   15  pul-sta-tux        pic  9(02)                  .
               10  pul-inf-mkt.
                   15  pul-cld-imp        pic  x(01)                  .
                   15  pul-tra-pco        pic  9(02)                  .
                   15  pul-gra-ico        pic  9(02)                  .
                   15  pul-pcl-ccz        pic  9(02)                  .
                   15  pul-pre-ctn        pic  9(02)                  .
                   15  pul-org-ctt        pic  9(02)                  .
                   15  pul-cod-mkt        pic  9(05)       comp-3     .
                   15  pul-ind-mkt        pic  x(10)                  .
               10  pul-inf-bdg.
                   15  pul-cla-bdg        pic  9(05)       comp-3     .
               10  pul-inf-bol.
                   15  pul-for-blo        pic  9(02)                  .
                   15  pul-dor-blo        pic  9(07)       comp-3     .
                   15  pul-fco-blo        pic  9(02)                  .
                   15  pul-dco-blo        pic  9(07)       comp-3     .
                   15  pul-dtt-acu        pic  9(02)                  .
                   15  pul-cod-vet        pic  9(07)       comp-3     .
                   15  pul-cod-vt2        pic  9(07)       comp-3     .
                   15  pul-cod-vt3        pic  9(07)       comp-3     .
                   15  pul-dst-klm        pic  9(04)v9(01) comp-3     .
                   15  pul-dti-cpf        pic  9(07)       comp-3     .
                   15  pul-dtf-cpf        pic  9(07)       comp-3     .
                   15  pul-gdl-nls        pic  x(07)                  .
                   15  pul-oam-oin        pic  9(04)                  .
                   15  pul-oam-ofi        pic  9(04)                  .
                   15  pul-oap-oin        pic  9(04)                  .
                   15  pul-oap-ofi        pic  9(04)                  .
                   15  pul-abn-vtt        pic  x(12)                  .
               10  pul-inf-aps.
                   15  pul-idn-em2        pic  x(20)                  .
                   15  pul-cod-cdv        pic  9(03)                  .
                   15  pul-cin-eur        pic  x(02)                  .
                   15  pul-cop-pcl        pic  x(03)                  .
                   15  pul-cop-tdo        pic  x(01)                  .
                   15  pul-tip-mci        pic  x(01)                  .
                   15  pul-alx-exp.
                       20  filler occurs 38
                                          pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sigla del File                                        *
      *        *-------------------------------------------------------*
           02  i-ide-sdf                  pic  x(04) value
                     "dcc "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcc/fls/ioc/obj/iofdcc              "       .

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
      *    * Area Lunghezza record in bytes ed Elenco chiavi previste  *
      *    *-----------------------------------------------------------*
       01  k.
      *        *-------------------------------------------------------*
      *        * Numero chiavi di accesso                              *
      *        *-------------------------------------------------------*
           05  k-ctr                      pic  9(02) value 3          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODCLI"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RAGKEY"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    3      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [dcc]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

      ******************************************************************
       Procedure Division                using f rf-dcc               .
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
           move      e-not-fnd            to   f-sts                 .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-dcc                 .
           move      zero                 to   rf-dcc-ide-dat         .
           move      spaces               to   rf-dcc-ide-ute         .
           move      spaces               to   rf-dcc-ide-fas         .
           move      zero                 to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      spaces               to   rf-dcc-rag-key         .
           move      spaces               to   rf-dcc-rag-soc         .
           move      spaces               to   rf-dcc-via-dcc         .
           move      spaces               to   rf-dcc-loc-dcc         .
           move      spaces               to   rf-dcc-cod-naz         .
           move      zero                 to   rf-dcc-cod-cmn         .
           move      zero                 to   rf-dcc-cod-fzn         .
           move      zero                 to   rf-dcc-cod-lct         .
           move      spaces               to   rf-dcc-rs1-doc         .
           move      spaces               to   rf-dcc-rs2-doc         .
           move      spaces               to   rf-dcc-not-g01         .
           move      spaces               to   rf-dcc-not-g02         .
           move      spaces               to   rf-dcc-not-g03         .
           move      spaces               to   rf-dcc-num-tel         .
           move      spaces               to   rf-dcc-tel-alt         .
           move      spaces               to   rf-dcc-nom-int         .
           move      spaces               to   rf-dcc-int-alt         .
           move      spaces               to   rf-dcc-num-fax         .
           move      spaces               to   rf-dcc-num-tlx         .
           move      spaces               to   rf-dcc-num-ptp         .
           move      spaces               to   rf-dcc-num-ptf         .
           move      spaces               to   rf-dcc-tel-mdm         .
           move      spaces               to   rf-dcc-idn-ema         .
           move      zero                 to   rf-dcc-inl-dcm         .
           move      zero                 to   rf-dcc-inl-pgt         .
           move      spaces               to   rf-dcc-noi-fnt         .
           move      zero                 to   rf-dcc-mod-sft         .
           move      zero                 to   rf-dcc-tas-ivc         .
           move      zero                 to   rf-dcc-ctp-ven         .
           move      zero                 to   rf-dcc-tdp-plc         .
           move      zero                 to   rf-dcc-per-fat         .
           move      zero                 to   rf-dcc-rag-bft         .
           move      zero                 to   rf-dcc-epz-pes         .
           move      spaces               to   rf-dcc-cod-vlt         .
           move      zero                 to   rf-dcc-mom-acv         .
           move      spaces               to   rf-dcc-snx-rlv         .
           move      zero                 to   rf-dcc-mom-alv         .
           move      spaces               to   rf-dcc-cod-lng         .
           move      zero                 to   rf-dcc-tip-frn         .
           move      zero                 to   rf-dcc-arc-plf         .
           move      spaces               to   rf-dcc-dpz-plf         .
           move      zero                 to   rf-dcc-tip-ftz         .
           move      spaces               to   rf-dcc-cod-lst         .
           move      zero                 to   rf-dcc-cat-scr         .
           move      zero                 to   rf-dcc-per-scr (1)     .
           move      zero                 to   rf-dcc-per-scr (2)     .
           move      zero                 to   rf-dcc-per-scr (3)     .
           move      zero                 to   rf-dcc-per-scr (4)     .
           move      zero                 to   rf-dcc-per-scr (5)     .
           move      zero                 to   rf-dcc-cat-scc         .
           move      zero                 to   rf-dcc-per-scc         .
           move      zero                 to   rf-dcc-snm-spe (1)     .
           move      zero                 to   rf-dcc-snm-spe (2)     .
           move      zero                 to   rf-dcc-snm-spe (3)     .
           move      zero                 to   rf-dcc-snm-spe (4)     .
           move      zero                 to   rf-dcc-snm-spe (5)     .
           move      zero                 to   rf-dcc-snm-spe (6)     .
           move      zero                 to   rf-dcc-per-spe (1)     .
           move      zero                 to   rf-dcc-per-spe (2)     .
           move      zero                 to   rf-dcc-per-spe (3)     .
           move      zero                 to   rf-dcc-per-spe (4)     .
           move      zero                 to   rf-dcc-per-spe (5)     .
           move      zero                 to   rf-dcc-per-spe (6)     .
           move      zero                 to   rf-dcc-imp-spe (1)     .
           move      zero                 to   rf-dcc-imp-spe (2)     .
           move      zero                 to   rf-dcc-imp-spe (3)     .
           move      zero                 to   rf-dcc-imp-spe (4)     .
           move      zero                 to   rf-dcc-imp-spe (5)     .
           move      zero                 to   rf-dcc-imp-spe (6)     .
           move      spaces               to   rf-dcc-vde-cod (1)     .
           move      spaces               to   rf-dcc-vde-cod (2)     .
           move      spaces               to   rf-dcc-vde-cod (3)     .
           move      spaces               to   rf-dcc-vde-cod (4)     .
           move      spaces               to   rf-dcc-vde-cod (5)     .
           move      spaces               to   rf-dcc-vde-cod (6)     .
           move      zero                 to   rf-dcc-cod-fop         .
           move      zero                 to   rf-dcc-tip-esm         .
           move      zero                 to   rf-dcc-ggg-alt         .
           move      zero                 to   rf-dcc-mmm-e01         .
           move      zero                 to   rf-dcc-mmm-e02         .
           move      spaces               to   rf-dcc-add-spi         .
           move      spaces               to   rf-dcc-add-spb         .
           move      zero                 to   rf-dcc-cod-abi         .
           move      zero                 to   rf-dcc-cod-cab         .
           move      spaces               to   rf-dcc-ccc-app         .
           move      spaces               to   rf-dcc-cod-cin         .
           move      spaces               to   rf-dcc-nos-ban         .
           move      spaces               to   rf-dcc-nos-bpe         .
           move      spaces               to   rf-dcc-nos-ccp         .
           move      zero                 to   rf-dcc-ipr-iel         .
           move      zero                 to   rf-dcc-cod-age         .
           move      zero                 to   rf-dcc-cat-pvg         .
           move      zero                 to   rf-dcc-per-pvg (1)     .
           move      zero                 to   rf-dcc-per-pvg (2)     .
           move      zero                 to   rf-dcc-per-pvg (3)     .
           move      zero                 to   rf-dcc-cod-zon         .
           move      zero                 to   rf-dcc-cod-cat         .
           move      zero                 to   rf-dcc-cod-stt         .
           move      zero                 to   rf-dcc-dat-aqz         .
           move      zero                 to   rf-dcc-sta-tus         .
           move      zero                 to   rf-dcc-sta-tud         .
           move      zero                 to   rf-dcc-sta-tuc         .
           move      zero                 to   rf-dcc-sta-tux         .
           move      spaces               to   rf-dcc-cld-imp         .
           move      zero                 to   rf-dcc-tra-pco         .
           move      zero                 to   rf-dcc-gra-ico         .
           move      zero                 to   rf-dcc-pcl-ccz         .
           move      zero                 to   rf-dcc-pre-ctn         .
           move      zero                 to   rf-dcc-org-ctt         .
           move      zero                 to   rf-dcc-cod-mkt         .
           move      spaces               to   rf-dcc-ind-mkt         .
           move      zero                 to   rf-dcc-cla-bdg         .
           move      zero                 to   rf-dcc-for-blo         .
           move      zero                 to   rf-dcc-dor-blo         .
           move      zero                 to   rf-dcc-fco-blo         .
           move      zero                 to   rf-dcc-dco-blo         .
           move      zero                 to   rf-dcc-dtt-acu         .
           move      zero                 to   rf-dcc-cod-vet         .
           move      zero                 to   rf-dcc-cod-vt2         .
           move      zero                 to   rf-dcc-cod-vt3         .
           move      spaces               to   rf-dcc-abn-vtt         .
           move      zero                 to   rf-dcc-dst-klm         .
           move      zero                 to   rf-dcc-dti-cpf         .
           move      zero                 to   rf-dcc-dtf-cpf         .
           move      spaces               to   rf-dcc-gdl-nls         .
           move      zero                 to   rf-dcc-oam-oin         .
           move      zero                 to   rf-dcc-oam-ofi         .
           move      zero                 to   rf-dcc-oap-oin         .
           move      zero                 to   rf-dcc-oap-ofi         .
           move      spaces               to   rf-dcc-idn-em2         .
           move      zero                 to   rf-dcc-cod-cdv         .
           move      spaces               to   rf-dcc-cin-eur         .
           move      spaces               to   rf-dcc-cop-pcl         .
           move      spaces               to   rf-dcc-cop-tdo         .
           move      spaces               to   rf-dcc-tip-mci         .
           move      spaces               to   rf-dcc-alx-exp         .
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
           move      zero                 to   z-key                  .
           move      rf-dcc-ide-ute       to   fil-ide-ute            .
           move      rf-dcc-ide-fas       to   fil-ide-fas            .
           move      rf-dcc-rag-soc       to   fil-rag-soc            .
           move      rf-dcc-via-dcc       to   fil-via-dcc            .
           move      rf-dcc-loc-dcc       to   fil-loc-dcc            .
           move      rf-dcc-cod-naz       to   fil-cod-naz            .
           move      rf-dcc-cod-cmn       to   fil-cod-cmn            .
           move      rf-dcc-cod-fzn       to   fil-cod-fzn            .
           move      rf-dcc-cod-lct       to   fil-cod-lct            .
           move      rf-dcc-rs1-doc       to   fil-rs1-doc            .
           move      rf-dcc-rs2-doc       to   fil-rs2-doc            .
           move      rf-dcc-not-g01       to   fil-not-g01            .
           move      rf-dcc-not-g02       to   fil-not-g02            .
           move      rf-dcc-not-g03       to   fil-not-g03            .
           move      rf-dcc-num-tel       to   fil-num-tel            .
           move      rf-dcc-tel-alt       to   fil-tel-alt            .
           move      rf-dcc-nom-int       to   fil-nom-int            .
           move      rf-dcc-int-alt       to   fil-int-alt            .
           move      rf-dcc-num-fax       to   fil-num-fax            .
           move      rf-dcc-num-tlx       to   fil-num-tlx            .
           move      rf-dcc-num-ptp       to   fil-num-ptp            .
           move      rf-dcc-num-ptf       to   fil-num-ptf            .
           move      rf-dcc-tel-mdm       to   fil-tel-mdm            .
           move      rf-dcc-idn-ema       to   fil-idn-ema            .
           move      rf-dcc-inl-dcm       to   fil-inl-dcm            .
           move      rf-dcc-inl-pgt       to   fil-inl-pgt            .
           move      rf-dcc-noi-fnt       to   fil-noi-fnt            .
           move      rf-dcc-mod-sft       to   fil-mod-sft            .
           move      rf-dcc-tas-ivc       to   fil-tas-ivc            .
           move      rf-dcc-ctp-ven       to   fil-ctp-ven            .
           move      rf-dcc-tdp-plc       to   fil-tdp-plc            .
           move      rf-dcc-per-fat       to   fil-per-fat            .
           move      rf-dcc-rag-bft       to   fil-rag-bft            .
           move      rf-dcc-epz-pes       to   fil-epz-pes            .
           move      rf-dcc-cod-vlt       to   fil-cod-vlt            .
           move      rf-dcc-mom-acv       to   fil-mom-acv            .
           move      rf-dcc-snx-rlv       to   fil-snx-rlv            .
           move      rf-dcc-mom-alv       to   fil-mom-alv            .
           move      rf-dcc-cod-lng       to   fil-cod-lng            .
           move      rf-dcc-tip-frn       to   fil-tip-frn            .
           move      rf-dcc-arc-plf       to   fil-arc-plf            .
           move      rf-dcc-dpz-plf       to   fil-dpz-plf            .
           move      rf-dcc-tip-ftz       to   fil-tip-ftz            .
           move      rf-dcc-cod-lst       to   fil-cod-lst            .
           move      rf-dcc-cat-scr       to   fil-cat-scr            .
           move      rf-dcc-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-dcc-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-dcc-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-dcc-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-dcc-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-dcc-cat-scc       to   fil-cat-scc            .
           move      rf-dcc-per-scc       to   fil-per-scc            .
           move      rf-dcc-snm-spe (1)   to   fil-snm-spe (1)        .
           move      rf-dcc-snm-spe (2)   to   fil-snm-spe (2)        .
           move      rf-dcc-snm-spe (3)   to   fil-snm-spe (3)        .
           move      rf-dcc-snm-spe (4)   to   fil-snm-spe (4)        .
           move      rf-dcc-snm-spe (5)   to   fil-snm-spe (5)        .
           move      rf-dcc-snm-spe (6)   to   fil-snm-spe (6)        .
           move      rf-dcc-per-spe (1)   to   fil-per-spe (1)        .
           move      rf-dcc-per-spe (2)   to   fil-per-spe (2)        .
           move      rf-dcc-per-spe (3)   to   fil-per-spe (3)        .
           move      rf-dcc-per-spe (4)   to   fil-per-spe (4)        .
           move      rf-dcc-per-spe (5)   to   fil-per-spe (5)        .
           move      rf-dcc-per-spe (6)   to   fil-per-spe (6)        .
           move      rf-dcc-imp-spe (1)   to   fil-imp-spe (1)        .
           move      rf-dcc-imp-spe (2)   to   fil-imp-spe (2)        .
           move      rf-dcc-imp-spe (3)   to   fil-imp-spe (3)        .
           move      rf-dcc-imp-spe (4)   to   fil-imp-spe (4)        .
           move      rf-dcc-imp-spe (5)   to   fil-imp-spe (5)        .
           move      rf-dcc-imp-spe (6)   to   fil-imp-spe (6)        .
           move      rf-dcc-vde-cod (1)   to   fil-vde-cod (1)        .
           move      rf-dcc-vde-cod (2)   to   fil-vde-cod (2)        .
           move      rf-dcc-vde-cod (3)   to   fil-vde-cod (3)        .
           move      rf-dcc-vde-cod (4)   to   fil-vde-cod (4)        .
           move      rf-dcc-vde-cod (5)   to   fil-vde-cod (5)        .
           move      rf-dcc-vde-cod (6)   to   fil-vde-cod (6)        .
           move      rf-dcc-cod-fop       to   fil-cod-fop            .
           move      rf-dcc-tip-esm       to   fil-tip-esm            .
           move      rf-dcc-ggg-alt       to   fil-ggg-alt            .
           move      rf-dcc-mmm-e01       to   fil-mmm-e01            .
           move      rf-dcc-mmm-e02       to   fil-mmm-e02            .
           move      rf-dcc-add-spi       to   fil-add-spi            .
           move      rf-dcc-add-spb       to   fil-add-spb            .
           move      rf-dcc-cod-abi       to   fil-cod-abi            .
           move      rf-dcc-cod-cab       to   fil-cod-cab            .
           move      rf-dcc-ccc-app       to   fil-ccc-app            .
           move      rf-dcc-cod-cin       to   fil-cod-cin            .
           move      rf-dcc-nos-ban       to   fil-nos-ban            .
           move      rf-dcc-nos-bpe       to   fil-nos-bpe            .
           move      rf-dcc-nos-ccp       to   fil-nos-ccp            .
           move      rf-dcc-ipr-iel       to   fil-ipr-iel            .
           move      rf-dcc-cod-age       to   fil-cod-age            .
           move      rf-dcc-cat-pvg       to   fil-cat-pvg            .
           move      rf-dcc-per-pvg (1)   to   fil-per-pvg (1)        .
           move      rf-dcc-per-pvg (2)   to   fil-per-pvg (2)        .
           move      rf-dcc-per-pvg (3)   to   fil-per-pvg (3)        .
           move      rf-dcc-cod-zon       to   fil-cod-zon            .
           move      rf-dcc-cod-cat       to   fil-cod-cat            .
           move      rf-dcc-cod-stt       to   fil-cod-stt            .
           move      rf-dcc-dat-aqz       to   fil-dat-aqz            .
           move      rf-dcc-sta-tus       to   fil-sta-tus            .
           move      rf-dcc-sta-tud       to   fil-sta-tud            .
           move      rf-dcc-sta-tuc       to   fil-sta-tuc            .
           move      rf-dcc-sta-tux       to   fil-sta-tux            .
           move      rf-dcc-cld-imp       to   fil-cld-imp            .
           move      rf-dcc-tra-pco       to   fil-tra-pco            .
           move      rf-dcc-gra-ico       to   fil-gra-ico            .
           move      rf-dcc-pcl-ccz       to   fil-pcl-ccz            .
           move      rf-dcc-pre-ctn       to   fil-pre-ctn            .
           move      rf-dcc-org-ctt       to   fil-org-ctt            .
           move      rf-dcc-cod-mkt       to   fil-cod-mkt            .
           move      rf-dcc-ind-mkt       to   fil-ind-mkt            .
           move      rf-dcc-cla-bdg       to   fil-cla-bdg            .
           move      rf-dcc-for-blo       to   fil-for-blo            .
           move      rf-dcc-dor-blo       to   fil-dor-blo            .
           move      rf-dcc-fco-blo       to   fil-fco-blo            .
           move      rf-dcc-dco-blo       to   fil-dco-blo            .
           move      rf-dcc-dtt-acu       to   fil-dtt-acu            .
           move      rf-dcc-cod-vet       to   fil-cod-vet            .
           move      rf-dcc-cod-vt2       to   fil-cod-vt2            .
           move      rf-dcc-cod-vt3       to   fil-cod-vt3            .
           move      rf-dcc-abn-vtt       to   fil-abn-vtt            .
           move      rf-dcc-dst-klm       to   fil-dst-klm            .
           move      rf-dcc-dti-cpf       to   fil-dti-cpf            .
           move      rf-dcc-dtf-cpf       to   fil-dtf-cpf            .
           move      rf-dcc-gdl-nls       to   fil-gdl-nls            .
           move      rf-dcc-oam-oin       to   fil-oam-oin            .
           move      rf-dcc-oam-ofi       to   fil-oam-ofi            .
           move      rf-dcc-oap-oin       to   fil-oap-oin            .
           move      rf-dcc-oap-ofi       to   fil-oap-ofi            .
           move      rf-dcc-idn-em2       to   fil-idn-em2            .
           move      rf-dcc-cod-cdv       to   fil-cod-cdv            .
           move      rf-dcc-cin-eur       to   fil-cin-eur            .
           move      rf-dcc-cop-pcl       to   fil-cop-pcl            .
           move      rf-dcc-cop-tdo       to   fil-cop-tdo            .
           move      rf-dcc-tip-mci       to   fil-tip-mci            .
           move      rf-dcc-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-dcc-cod-cli       to   fil-cod-cli            .
           move      rf-dcc-dpz-cli       to   fil-dpz-cli            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-dcc-ide-dat       to   fil-ide-dat            .
           move      rf-dcc-cod-cli       to   fil-cod-cli-2          .
           move      rf-dcc-dpz-cli       to   fil-dpz-cli-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-dcc-rag-key       to   fil-rag-key            .
           move      rf-dcc-cod-cli       to   fil-cod-cli-3          .
           move      rf-dcc-dpz-cli       to   fil-dpz-cli-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-dcc                 .
           move      fil-ide-dat          to   rf-dcc-ide-dat         .
           move      fil-ide-ute          to   rf-dcc-ide-ute         .
           move      fil-ide-fas          to   rf-dcc-ide-fas         .
           move      fil-cod-cli          to   rf-dcc-cod-cli         .
           move      fil-dpz-cli          to   rf-dcc-dpz-cli         .
           move      fil-rag-key          to   rf-dcc-rag-key         .
           move      fil-rag-soc          to   rf-dcc-rag-soc         .
           move      fil-via-dcc          to   rf-dcc-via-dcc         .
           move      fil-loc-dcc          to   rf-dcc-loc-dcc         .
           move      fil-cod-naz          to   rf-dcc-cod-naz         .
           move      fil-cod-cmn          to   rf-dcc-cod-cmn         .
           move      fil-cod-fzn          to   rf-dcc-cod-fzn         .
           move      fil-cod-lct          to   rf-dcc-cod-lct         .
           move      fil-rs1-doc          to   rf-dcc-rs1-doc         .
           move      fil-rs2-doc          to   rf-dcc-rs2-doc         .
           move      fil-not-g01          to   rf-dcc-not-g01         .
           move      fil-not-g02          to   rf-dcc-not-g02         .
           move      fil-not-g03          to   rf-dcc-not-g03         .
           move      fil-num-tel          to   rf-dcc-num-tel         .
           move      fil-tel-alt          to   rf-dcc-tel-alt         .
           move      fil-nom-int          to   rf-dcc-nom-int         .
           move      fil-int-alt          to   rf-dcc-int-alt         .
           move      fil-num-fax          to   rf-dcc-num-fax         .
           move      fil-num-tlx          to   rf-dcc-num-tlx         .
           move      fil-num-ptp          to   rf-dcc-num-ptp         .
           move      fil-num-ptf          to   rf-dcc-num-ptf         .
           move      fil-tel-mdm          to   rf-dcc-tel-mdm         .
           move      fil-idn-ema          to   rf-dcc-idn-ema         .
           move      fil-inl-dcm          to   rf-dcc-inl-dcm         .
           move      fil-inl-pgt          to   rf-dcc-inl-pgt         .
           move      fil-noi-fnt          to   rf-dcc-noi-fnt         .
           move      fil-mod-sft          to   rf-dcc-mod-sft         .
           move      fil-tas-ivc          to   rf-dcc-tas-ivc         .
           move      fil-ctp-ven          to   rf-dcc-ctp-ven         .
           move      fil-tdp-plc          to   rf-dcc-tdp-plc         .
           move      fil-per-fat          to   rf-dcc-per-fat         .
           move      fil-rag-bft          to   rf-dcc-rag-bft         .
           move      fil-epz-pes          to   rf-dcc-epz-pes         .
           move      fil-cod-vlt          to   rf-dcc-cod-vlt         .
           move      fil-mom-acv          to   rf-dcc-mom-acv         .
           move      fil-snx-rlv          to   rf-dcc-snx-rlv         .
           move      fil-mom-alv          to   rf-dcc-mom-alv         .
           move      fil-cod-lng          to   rf-dcc-cod-lng         .
           move      fil-tip-frn          to   rf-dcc-tip-frn         .
           move      fil-arc-plf          to   rf-dcc-arc-plf         .
           move      fil-dpz-plf          to   rf-dcc-dpz-plf         .
           move      fil-tip-ftz          to   rf-dcc-tip-ftz         .
           move      fil-cod-lst          to   rf-dcc-cod-lst         .
           move      fil-cat-scr          to   rf-dcc-cat-scr         .
           move      fil-per-scr (1)      to   rf-dcc-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-dcc-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-dcc-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-dcc-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-dcc-per-scr (5)     .
           move      fil-cat-scc          to   rf-dcc-cat-scc         .
           move      fil-per-scc          to   rf-dcc-per-scc         .
           move      fil-snm-spe (1)      to   rf-dcc-snm-spe (1)     .
           move      fil-snm-spe (2)      to   rf-dcc-snm-spe (2)     .
           move      fil-snm-spe (3)      to   rf-dcc-snm-spe (3)     .
           move      fil-snm-spe (4)      to   rf-dcc-snm-spe (4)     .
           move      fil-snm-spe (5)      to   rf-dcc-snm-spe (5)     .
           move      fil-snm-spe (6)      to   rf-dcc-snm-spe (6)     .
           move      fil-per-spe (1)      to   rf-dcc-per-spe (1)     .
           move      fil-per-spe (2)      to   rf-dcc-per-spe (2)     .
           move      fil-per-spe (3)      to   rf-dcc-per-spe (3)     .
           move      fil-per-spe (4)      to   rf-dcc-per-spe (4)     .
           move      fil-per-spe (5)      to   rf-dcc-per-spe (5)     .
           move      fil-per-spe (6)      to   rf-dcc-per-spe (6)     .
           move      fil-imp-spe (1)      to   rf-dcc-imp-spe (1)     .
           move      fil-imp-spe (2)      to   rf-dcc-imp-spe (2)     .
           move      fil-imp-spe (3)      to   rf-dcc-imp-spe (3)     .
           move      fil-imp-spe (4)      to   rf-dcc-imp-spe (4)     .
           move      fil-imp-spe (5)      to   rf-dcc-imp-spe (5)     .
           move      fil-imp-spe (6)      to   rf-dcc-imp-spe (6)     .
           move      fil-vde-cod (1)      to   rf-dcc-vde-cod (1)     .
           move      fil-vde-cod (2)      to   rf-dcc-vde-cod (2)     .
           move      fil-vde-cod (3)      to   rf-dcc-vde-cod (3)     .
           move      fil-vde-cod (4)      to   rf-dcc-vde-cod (4)     .
           move      fil-vde-cod (5)      to   rf-dcc-vde-cod (5)     .
           move      fil-vde-cod (6)      to   rf-dcc-vde-cod (6)     .
           move      fil-cod-fop          to   rf-dcc-cod-fop         .
           move      fil-tip-esm          to   rf-dcc-tip-esm         .
           move      fil-ggg-alt          to   rf-dcc-ggg-alt         .
           move      fil-mmm-e01          to   rf-dcc-mmm-e01         .
           move      fil-mmm-e02          to   rf-dcc-mmm-e02         .
           move      fil-add-spi          to   rf-dcc-add-spi         .
           move      fil-add-spb          to   rf-dcc-add-spb         .
           move      fil-cod-abi          to   rf-dcc-cod-abi         .
           move      fil-cod-cab          to   rf-dcc-cod-cab         .
           move      fil-ccc-app          to   rf-dcc-ccc-app         .
           move      fil-cod-cin          to   rf-dcc-cod-cin         .
           move      fil-nos-ban          to   rf-dcc-nos-ban         .
           move      fil-nos-bpe          to   rf-dcc-nos-bpe         .
           move      fil-nos-ccp          to   rf-dcc-nos-ccp         .
           move      fil-ipr-iel          to   rf-dcc-ipr-iel         .
           move      fil-cod-age          to   rf-dcc-cod-age         .
           move      fil-cat-pvg          to   rf-dcc-cat-pvg         .
           move      fil-per-pvg (1)      to   rf-dcc-per-pvg (1)     .
           move      fil-per-pvg (2)      to   rf-dcc-per-pvg (2)     .
           move      fil-per-pvg (3)      to   rf-dcc-per-pvg (3)     .
           move      fil-cod-zon          to   rf-dcc-cod-zon         .
           move      fil-cod-cat          to   rf-dcc-cod-cat         .
           move      fil-cod-stt          to   rf-dcc-cod-stt         .
           move      fil-dat-aqz          to   rf-dcc-dat-aqz         .
           move      fil-sta-tus          to   rf-dcc-sta-tus         .
           move      fil-sta-tud          to   rf-dcc-sta-tud         .
           move      fil-sta-tuc          to   rf-dcc-sta-tuc         .
           move      fil-sta-tux          to   rf-dcc-sta-tux         .
           move      fil-cld-imp          to   rf-dcc-cld-imp         .
           move      fil-tra-pco          to   rf-dcc-tra-pco         .
           move      fil-gra-ico          to   rf-dcc-gra-ico         .
           move      fil-pcl-ccz          to   rf-dcc-pcl-ccz         .
           move      fil-pre-ctn          to   rf-dcc-pre-ctn         .
           move      fil-org-ctt          to   rf-dcc-org-ctt         .
           move      fil-cod-mkt          to   rf-dcc-cod-mkt         .
           move      fil-ind-mkt          to   rf-dcc-ind-mkt         .
           move      fil-cla-bdg          to   rf-dcc-cla-bdg         .
           move      fil-for-blo          to   rf-dcc-for-blo         .
           move      fil-dor-blo          to   rf-dcc-dor-blo         .
           move      fil-fco-blo          to   rf-dcc-fco-blo         .
           move      fil-dco-blo          to   rf-dcc-dco-blo         .
           move      fil-dtt-acu          to   rf-dcc-dtt-acu         .
           move      fil-cod-vet          to   rf-dcc-cod-vet         .
           move      fil-cod-vt2          to   rf-dcc-cod-vt2         .
           move      fil-cod-vt3          to   rf-dcc-cod-vt3         .
           move      fil-abn-vtt          to   rf-dcc-abn-vtt         .
           move      fil-dst-klm          to   rf-dcc-dst-klm         .
           move      fil-dti-cpf          to   rf-dcc-dti-cpf         .
           move      fil-dtf-cpf          to   rf-dcc-dtf-cpf         .
           move      fil-gdl-nls          to   rf-dcc-gdl-nls         .
           move      fil-oam-oin          to   rf-dcc-oam-oin         .
           move      fil-oam-ofi          to   rf-dcc-oam-ofi         .
           move      fil-oap-oin          to   rf-dcc-oap-oin         .
           move      fil-oap-ofi          to   rf-dcc-oap-ofi         .
           move      fil-idn-em2          to   rf-dcc-idn-em2         .
           move      fil-cod-cdv          to   rf-dcc-cod-cdv         .
           move      fil-cin-eur          to   rf-dcc-cin-eur         .
           move      fil-cop-pcl          to   rf-dcc-cop-pcl         .
           move      fil-cop-tdo          to   rf-dcc-cop-tdo         .
           move      fil-tip-mci          to   rf-dcc-tip-mci         .
           move      fil-alx-exp          to   rf-dcc-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-dcc               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-dcc
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

