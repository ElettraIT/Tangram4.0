       Identification Division.
       Program-Id.                                 iofdcp             .
      *================================================================*
      *                                                                *
      *                  Input-Output File dcp                         *
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
                   alternate record key   is fil-k08
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
                   alternate record key   is pul-k08
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
                   15  fil-lot-ven        pic  9(10)v9(03) comp-3     .
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
               10  fil-spf-pro.
                   15  fil-add-pro        pic  9(03)                  .
               10  fil-aaq-pro.
                   15  fil-dcf-pfz        pic  9(07)                  .
                   15  fil-cod-pdt        pic  9(07)                  .
                   15  fil-cop-sfn        pic  x(20)                  .
               10  fil-inf-aps.
                   15  fil-dat-sti        pic  9(07)                  .
                   15  fil-dat-stf        pic  9(07)                  .
                   15  fil-alx-exp.
                       20  filler occurs 29
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
      *            * Chiave numero 01 : NUMPRO                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-pro        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-pro-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-des-key        pic  x(40)                  .
                   15  pul-num-pro-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFPRO                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-alf-pro        pic  x(14)                  .
                   15  pul-num-pro-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNPRO                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-syn-pro        pic  x(13)                  .
                   15  pul-num-pro-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cla-pro        pic  9(05)       comp-3     .
                   15  pul-gru-pro        pic  9(05)       comp-3     .
                   15  pul-sgr-pro        pic  9(05)       comp-3     .
                   15  pul-des-key-6      pic  x(40)                  .
                   15  pul-num-pro-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-cla-pro-7      pic  9(05)       comp-3     .
                   15  pul-gru-pro-7      pic  9(05)       comp-3     .
                   15  pul-sgr-pro-7      pic  9(05)       comp-3     .
                   15  pul-alf-pro-7      pic  x(14)                  .
                   15  pul-num-pro-7      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : KLBPRO                         *
      *            *---------------------------------------------------*
               10  pul-k08.
                   15  pul-klb-pro        pic  x(13)                  .
                   15  pul-num-pro-8      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-inf-gen.
                   15  pul-ide-ute        pic  x(08)                  .
                   15  pul-ide-fas        pic  x(06)                  .
                   15  pul-des-pro        pic  x(40)                  .
                   15  pul-des-pdx        pic  9(01)                  .
                   15  pul-tip-pro        pic  9(02)                  .
                   15  pul-tip-acp        pic  9(02)                  .
                   15  pul-not-g01        pic  x(40)                  .
               10  pul-inf-fis.
                   15  pul-tip-cfz        pic  9(02)                  .
                   15  pul-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  pul-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  pul-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  pul-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  pul-dim-pro.
                       20  pul-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  pul-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  pul-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  pul-pcl-fis        pic  x(10)                  .
                   15  pul-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  pul-coe-div        pic  9(04)v9(03) comp-3     .
                   15  pul-spc-lib        pic  x(20)                  .
               10  pul-inf-fat.
                   15  pul-cod-iva        pic  9(05)       comp-3     .
                   15  pul-ctp-ven        pic  9(07)       comp-3     .
                   15  pul-umi-ven        pic  x(03)                  .
                   15  pul-dec-qta        pic  9(01)                  .
                   15  pul-sgl-vlt        pic  x(03)                  .
                   15  pul-dec-vlt        pic  9(01)                  .
                   15  pul-dec-prz        pic  9(01)                  .
                   15  pul-prz-lst        pic  9(09)       comp-3     .
                   15  pul-lot-ven        pic  9(10)v9(03) comp-3     .
                   15  pul-epz-rgf        pic  9(01)                  .
                   15  pul-snx-2qt        pic  9(01)                  .
                   15  pul-dec-2qt        pic  9(01)                  .
                   15  pul-snx-3qt        pic  9(01)                  .
                   15  pul-dec-3qt        pic  9(01)                  .
                   15  pul-snx-2pz        pic  9(01)                  .
                   15  pul-tip-vve        pic  x(03)                  .
               10  pul-inf-cdv.
                   15  pul-cat-scr        pic  9(05)       comp-3     .
                   15  pul-per-scr occurs 05
                                          pic  9(02)v9(01) comp-3     .
               10  pul-inf-gag.
                   15  pul-cat-pvg        pic  9(05)       comp-3     .
                   15  pul-per-pvg  occurs 03
                                          pic  9(02)v9(01) comp-3     .
                   15  pul-amm-pvg        pic  9(09)       comp-3     .
               10  pul-inf-lst.
                   15  pul-snx-lst        pic  9(01)                  .
                   15  pul-epz-lst.
                       20  pul-epz-ele occurs 20
                                          pic  x(01)                  .
                   15  pul-pag-lst        pic  x(10)                  .
                   15  pul-rfl-lst        pic  x(12)                  .
                   15  pul-tmc-lst        pic  9(03)       comp-3     .
                   15  pul-daa-lst        pic  9(01)                  .
                   15  pul-aut-lst        pic  x(03)                  .
               10  pul-inf-pcs.
                   15  pul-cod-s01        pic  9(05)       comp-3     .
                   15  pul-cod-s02        pic  9(05)       comp-3     .
                   15  pul-cod-s03        pic  9(05)       comp-3     .
                   15  pul-dat-icm        pic  9(07)       comp-3     .
                   15  pul-sta-tus        pic  9(02)                  .
                   15  pul-sta-tud        pic  9(07)       comp-3     .
                   15  pul-sta-tuc        pic  9(07)       comp-3     .
                   15  pul-sta-tux        pic  9(02)                  .
               10  pul-inf-mkt.
                   15  pul-cld-imp        pic  x(01)                  .
                   15  pul-pre-ctn        pic  9(02)                  .
                   15  pul-gra-ico        pic  9(02)                  .
                   15  pul-pcl-ccz        pic  9(02)                  .
                   15  pul-cod-mkt        pic  9(05)       comp-3     .
                   15  pul-ind-mkt        pic  x(10)                  .
               10  pul-inf-bdg.
                   15  pul-cla-bdg        pic  9(05)       comp-3     .
               10  pul-inf-bol.
                   15  pul-for-blo        pic  9(02)                  .
                   15  pul-dor-blo        pic  9(07)       comp-3     .
                   15  pul-fco-blo        pic  9(02)                  .
                   15  pul-dco-blo        pic  9(07)       comp-3     .
               10  pul-inf-iic.
                   15  pul-cdn-cdm        pic  9(08)                  .
               10  pul-spf-pro.
                   15  pul-add-pro        pic  9(03)                  .
               10  pul-aaq-pro.
                   15  pul-dcf-pfz        pic  9(07)                  .
                   15  pul-cod-pdt        pic  9(07)                  .
                   15  pul-cop-sfn        pic  x(20)                  .
               10  pul-inf-aps.
                   15  pul-dat-sti        pic  9(07)                  .
                   15  pul-dat-stf        pic  9(07)                  .
                   15  pul-alx-exp.
                       20  filler occurs 29
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
                     "dcp "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcp/fls/ioc/obj/iofdcp              "       .

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
           05  k-ctr                      pic  9(02) value 8          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMPRO"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DESKEY"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ALFPRO"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "SYNPRO"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CGSDES"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CGSALF"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 8                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "KLBPRO"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    8      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [pro]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .

      ******************************************************************
       Procedure Division                using f rf-dcp               .
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
                     str-800
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
       str-800.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 8                       *
      *              *-------------------------------------------------*
           go to     str-810
                     str-820
                     str-830
                     depending            on   z-tco                  .
       str-810.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-820.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k08
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-830.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 8           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k08
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
                     rea-800
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
       rea-800.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 8                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-810.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k08
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-810.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k08
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
           move      spaces               to   rf-dcp                 .
           move      zero                 to   rf-dcp-ide-dat         .
           move      spaces               to   rf-dcp-ide-ute         .
           move      spaces               to   rf-dcp-ide-fas         .
           move      zero                 to   rf-dcp-num-pro         .
           move      spaces               to   rf-dcp-alf-pro         .
           move      spaces               to   rf-dcp-syn-pro         .
           move      spaces               to   rf-dcp-klb-pro         .
           move      spaces               to   rf-dcp-des-key         .
           move      spaces               to   rf-dcp-des-pro         .
           move      zero                 to   rf-dcp-des-pdx         .
           move      zero                 to   rf-dcp-cla-pro         .
           move      zero                 to   rf-dcp-gru-pro         .
           move      zero                 to   rf-dcp-sgr-pro         .
           move      zero                 to   rf-dcp-tip-pro         .
           move      zero                 to   rf-dcp-tip-acp         .
           move      spaces               to   rf-dcp-not-g01         .
           move      zero                 to   rf-dcp-tip-cfz         .
           move      zero                 to   rf-dcp-qta-cfz         .
           move      zero                 to   rf-dcp-pes-uni         .
           move      zero                 to   rf-dcp-pes-tar         .
           move      zero                 to   rf-dcp-vol-uni         .
           move      zero                 to   rf-dcp-dim-lar         .
           move      zero                 to   rf-dcp-dim-alt         .
           move      zero                 to   rf-dcp-dim-prf         .
           move      spaces               to   rf-dcp-pcl-fis         .
           move      zero                 to   rf-dcp-coe-mol         .
           move      zero                 to   rf-dcp-coe-div         .
           move      spaces               to   rf-dcp-spc-lib         .
           move      zero                 to   rf-dcp-cod-iva         .
           move      zero                 to   rf-dcp-ctp-ven         .
           move      spaces               to   rf-dcp-umi-ven         .
           move      zero                 to   rf-dcp-dec-qta         .
           move      spaces               to   rf-dcp-sgl-vlt         .
           move      zero                 to   rf-dcp-dec-vlt         .
           move      zero                 to   rf-dcp-dec-prz         .
           move      zero                 to   rf-dcp-prz-lst         .
           move      zero                 to   rf-dcp-lot-ven         .
           move      zero                 to   rf-dcp-epz-rgf         .
           move      zero                 to   rf-dcp-snx-2qt         .
           move      zero                 to   rf-dcp-dec-2qt         .
           move      zero                 to   rf-dcp-snx-3qt         .
           move      zero                 to   rf-dcp-dec-3qt         .
           move      zero                 to   rf-dcp-snx-2pz         .
           move      spaces               to   rf-dcp-tip-vve         .
           move      zero                 to   rf-dcp-cat-scr         .
           move      zero                 to   rf-dcp-per-scr (1)     .
           move      zero                 to   rf-dcp-per-scr (2)     .
           move      zero                 to   rf-dcp-per-scr (3)     .
           move      zero                 to   rf-dcp-per-scr (4)     .
           move      zero                 to   rf-dcp-per-scr (5)     .
           move      zero                 to   rf-dcp-cat-pvg         .
           move      zero                 to   rf-dcp-per-pvg (1)     .
           move      zero                 to   rf-dcp-per-pvg (2)     .
           move      zero                 to   rf-dcp-per-pvg (3)     .
           move      zero                 to   rf-dcp-amm-pvg         .
           move      zero                 to   rf-dcp-snx-lst         .
           move      spaces               to   rf-dcp-epz-lst         .
           move      spaces               to   rf-dcp-pag-lst         .
           move      spaces               to   rf-dcp-rfl-lst         .
           move      zero                 to   rf-dcp-tmc-lst         .
           move      zero                 to   rf-dcp-daa-lst         .
           move      spaces               to   rf-dcp-aut-lst         .
           move      zero                 to   rf-dcp-cod-s01         .
           move      zero                 to   rf-dcp-cod-s02         .
           move      zero                 to   rf-dcp-cod-s03         .
           move      zero                 to   rf-dcp-dat-icm         .
           move      zero                 to   rf-dcp-sta-tus         .
           move      zero                 to   rf-dcp-sta-tud         .
           move      zero                 to   rf-dcp-sta-tuc         .
           move      zero                 to   rf-dcp-sta-tux         .
           move      spaces               to   rf-dcp-cld-imp         .
           move      zero                 to   rf-dcp-pre-ctn         .
           move      zero                 to   rf-dcp-gra-ico         .
           move      zero                 to   rf-dcp-pcl-ccz         .
           move      zero                 to   rf-dcp-cod-mkt         .
           move      spaces               to   rf-dcp-ind-mkt         .
           move      zero                 to   rf-dcp-cla-bdg         .
           move      zero                 to   rf-dcp-for-blo         .
           move      zero                 to   rf-dcp-dor-blo         .
           move      zero                 to   rf-dcp-fco-blo         .
           move      zero                 to   rf-dcp-dco-blo         .
           move      zero                 to   rf-dcp-cdn-cdm         .
           move      zero                 to   rf-dcp-add-pro         .
           move      zero                 to   rf-dcp-dcf-pfz         .
           move      zero                 to   rf-dcp-cod-pdt         .
           move      spaces               to   rf-dcp-cop-sfn         .
           move      zero                 to   rf-dcp-dat-sti         .
           move      zero                 to   rf-dcp-dat-stf         .
           move      spaces               to   rf-dcp-alx-exp         .
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
           move      rf-dcp-ide-ute       to   fil-ide-ute            .
           move      rf-dcp-ide-fas       to   fil-ide-fas            .
           move      rf-dcp-des-pro       to   fil-des-pro            .
           move      rf-dcp-des-pdx       to   fil-des-pdx            .
           move      rf-dcp-tip-pro       to   fil-tip-pro            .
           move      rf-dcp-tip-acp       to   fil-tip-acp            .
           move      rf-dcp-not-g01       to   fil-not-g01            .
           move      rf-dcp-tip-cfz       to   fil-tip-cfz            .
           move      rf-dcp-qta-cfz       to   fil-qta-cfz            .
           move      rf-dcp-pes-uni       to   fil-pes-uni            .
           move      rf-dcp-pes-tar       to   fil-pes-tar            .
           move      rf-dcp-vol-uni       to   fil-vol-uni            .
           move      rf-dcp-dim-lar       to   fil-dim-lar            .
           move      rf-dcp-dim-alt       to   fil-dim-alt            .
           move      rf-dcp-dim-prf       to   fil-dim-prf            .
           move      rf-dcp-pcl-fis       to   fil-pcl-fis            .
           move      rf-dcp-coe-mol       to   fil-coe-mol            .
           move      rf-dcp-coe-div       to   fil-coe-div            .
           move      rf-dcp-spc-lib       to   fil-spc-lib            .
           move      rf-dcp-cod-iva       to   fil-cod-iva            .
           move      rf-dcp-ctp-ven       to   fil-ctp-ven            .
           move      rf-dcp-umi-ven       to   fil-umi-ven            .
           move      rf-dcp-dec-qta       to   fil-dec-qta            .
           move      rf-dcp-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-dcp-dec-vlt       to   fil-dec-vlt            .
           move      rf-dcp-dec-prz       to   fil-dec-prz            .
           move      rf-dcp-prz-lst       to   fil-prz-lst            .
           move      rf-dcp-lot-ven       to   fil-lot-ven            .
           move      rf-dcp-epz-rgf       to   fil-epz-rgf            .
           move      rf-dcp-snx-2qt       to   fil-snx-2qt            .
           move      rf-dcp-dec-2qt       to   fil-dec-2qt            .
           move      rf-dcp-snx-3qt       to   fil-snx-3qt            .
           move      rf-dcp-dec-3qt       to   fil-dec-3qt            .
           move      rf-dcp-snx-2pz       to   fil-snx-2pz            .
           move      rf-dcp-tip-vve       to   fil-tip-vve            .
           move      rf-dcp-cat-scr       to   fil-cat-scr            .
           move      rf-dcp-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-dcp-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-dcp-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-dcp-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-dcp-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-dcp-cat-pvg       to   fil-cat-pvg            .
           move      rf-dcp-per-pvg (1)   to   fil-per-pvg (1)        .
           move      rf-dcp-per-pvg (2)   to   fil-per-pvg (2)        .
           move      rf-dcp-per-pvg (3)   to   fil-per-pvg (3)        .
           move      rf-dcp-amm-pvg       to   fil-amm-pvg            .
           move      rf-dcp-snx-lst       to   fil-snx-lst            .
           move      rf-dcp-epz-lst       to   fil-epz-lst            .
           move      rf-dcp-pag-lst       to   fil-pag-lst            .
           move      rf-dcp-rfl-lst       to   fil-rfl-lst            .
           move      rf-dcp-tmc-lst       to   fil-tmc-lst            .
           move      rf-dcp-daa-lst       to   fil-daa-lst            .
           move      rf-dcp-aut-lst       to   fil-aut-lst            .
           move      rf-dcp-cod-s01       to   fil-cod-s01            .
           move      rf-dcp-cod-s02       to   fil-cod-s02            .
           move      rf-dcp-cod-s03       to   fil-cod-s03            .
           move      rf-dcp-dat-icm       to   fil-dat-icm            .
           move      rf-dcp-sta-tus       to   fil-sta-tus            .
           move      rf-dcp-sta-tud       to   fil-sta-tud            .
           move      rf-dcp-sta-tuc       to   fil-sta-tuc            .
           move      rf-dcp-sta-tux       to   fil-sta-tux            .
           move      rf-dcp-cld-imp       to   fil-cld-imp            .
           move      rf-dcp-pre-ctn       to   fil-pre-ctn            .
           move      rf-dcp-gra-ico       to   fil-gra-ico            .
           move      rf-dcp-pcl-ccz       to   fil-pcl-ccz            .
           move      rf-dcp-cod-mkt       to   fil-cod-mkt            .
           move      rf-dcp-ind-mkt       to   fil-ind-mkt            .
           move      rf-dcp-cla-bdg       to   fil-cla-bdg            .
           move      rf-dcp-for-blo       to   fil-for-blo            .
           move      rf-dcp-dor-blo       to   fil-dor-blo            .
           move      rf-dcp-fco-blo       to   fil-fco-blo            .
           move      rf-dcp-dco-blo       to   fil-dco-blo            .
           move      rf-dcp-cdn-cdm       to   fil-cdn-cdm            .
           move      rf-dcp-add-pro       to   fil-add-pro            .
           move      rf-dcp-dcf-pfz       to   fil-dcf-pfz            .
           move      rf-dcp-cod-pdt       to   fil-cod-pdt            .
           move      rf-dcp-cop-sfn       to   fil-cop-sfn            .
           move      rf-dcp-dat-sti       to   fil-dat-sti            .
           move      rf-dcp-dat-stf       to   fil-dat-stf            .
           move      rf-dcp-alx-exp       to   fil-alx-exp            .
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
                     cmp-key-fis-800
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-dcp-num-pro       to   fil-num-pro            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-dcp-ide-dat       to   fil-ide-dat            .
           move      rf-dcp-num-pro       to   fil-num-pro-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-dcp-des-key       to   fil-des-key            .
           move      rf-dcp-num-pro       to   fil-num-pro-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-dcp-alf-pro       to   fil-alf-pro            .
           move      rf-dcp-num-pro       to   fil-num-pro-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-dcp-syn-pro       to   fil-syn-pro            .
           move      rf-dcp-num-pro       to   fil-num-pro-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-dcp-cla-pro       to   fil-cla-pro            .
           move      rf-dcp-gru-pro       to   fil-gru-pro            .
           move      rf-dcp-sgr-pro       to   fil-sgr-pro            .
           move      rf-dcp-des-key       to   fil-des-key-6          .
           move      rf-dcp-num-pro       to   fil-num-pro-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-dcp-cla-pro       to   fil-cla-pro-7          .
           move      rf-dcp-gru-pro       to   fil-gru-pro-7          .
           move      rf-dcp-sgr-pro       to   fil-sgr-pro-7          .
           move      rf-dcp-alf-pro       to   fil-alf-pro-7          .
           move      rf-dcp-num-pro       to   fil-num-pro-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-800.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 8                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k08                .
           move      rf-dcp-klb-pro       to   fil-klb-pro            .
           move      rf-dcp-num-pro       to   fil-num-pro-8          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-dcp                 .
           move      fil-ide-dat          to   rf-dcp-ide-dat         .
           move      fil-ide-ute          to   rf-dcp-ide-ute         .
           move      fil-ide-fas          to   rf-dcp-ide-fas         .
           move      fil-num-pro          to   rf-dcp-num-pro         .
           move      fil-alf-pro          to   rf-dcp-alf-pro         .
           move      fil-syn-pro          to   rf-dcp-syn-pro         .
           move      fil-klb-pro          to   rf-dcp-klb-pro         .
           move      fil-des-key          to   rf-dcp-des-key         .
           move      fil-des-pro          to   rf-dcp-des-pro         .
           move      fil-des-pdx          to   rf-dcp-des-pdx         .
           move      fil-cla-pro          to   rf-dcp-cla-pro         .
           move      fil-gru-pro          to   rf-dcp-gru-pro         .
           move      fil-sgr-pro          to   rf-dcp-sgr-pro         .
           move      fil-tip-pro          to   rf-dcp-tip-pro         .
           move      fil-tip-acp          to   rf-dcp-tip-acp         .
           move      fil-not-g01          to   rf-dcp-not-g01         .
           move      fil-tip-cfz          to   rf-dcp-tip-cfz         .
           move      fil-qta-cfz          to   rf-dcp-qta-cfz         .
           move      fil-pes-uni          to   rf-dcp-pes-uni         .
           move      fil-pes-tar          to   rf-dcp-pes-tar         .
           move      fil-vol-uni          to   rf-dcp-vol-uni         .
           move      fil-dim-lar          to   rf-dcp-dim-lar         .
           move      fil-dim-alt          to   rf-dcp-dim-alt         .
           move      fil-dim-prf          to   rf-dcp-dim-prf         .
           move      fil-pcl-fis          to   rf-dcp-pcl-fis         .
           move      fil-coe-mol          to   rf-dcp-coe-mol         .
           move      fil-coe-div          to   rf-dcp-coe-div         .
           move      fil-spc-lib          to   rf-dcp-spc-lib         .
           move      fil-cod-iva          to   rf-dcp-cod-iva         .
           move      fil-ctp-ven          to   rf-dcp-ctp-ven         .
           move      fil-umi-ven          to   rf-dcp-umi-ven         .
           move      fil-dec-qta          to   rf-dcp-dec-qta         .
           move      fil-sgl-vlt          to   rf-dcp-sgl-vlt         .
           move      fil-dec-vlt          to   rf-dcp-dec-vlt         .
           move      fil-dec-prz          to   rf-dcp-dec-prz         .
           move      fil-prz-lst          to   rf-dcp-prz-lst         .
           move      fil-lot-ven          to   rf-dcp-lot-ven         .
           move      fil-epz-rgf          to   rf-dcp-epz-rgf         .
           move      fil-snx-2qt          to   rf-dcp-snx-2qt         .
           move      fil-dec-2qt          to   rf-dcp-dec-2qt         .
           move      fil-snx-3qt          to   rf-dcp-snx-3qt         .
           move      fil-dec-3qt          to   rf-dcp-dec-3qt         .
           move      fil-snx-2pz          to   rf-dcp-snx-2pz         .
           move      fil-tip-vve          to   rf-dcp-tip-vve         .
           move      fil-cat-scr          to   rf-dcp-cat-scr         .
           move      fil-per-scr (1)      to   rf-dcp-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-dcp-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-dcp-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-dcp-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-dcp-per-scr (5)     .
           move      fil-cat-pvg          to   rf-dcp-cat-pvg         .
           move      fil-per-pvg (1)      to   rf-dcp-per-pvg (1)     .
           move      fil-per-pvg (2)      to   rf-dcp-per-pvg (2)     .
           move      fil-per-pvg (3)      to   rf-dcp-per-pvg (3)     .
           move      fil-amm-pvg          to   rf-dcp-amm-pvg         .
           move      fil-snx-lst          to   rf-dcp-snx-lst         .
           move      fil-epz-lst          to   rf-dcp-epz-lst         .
           move      fil-pag-lst          to   rf-dcp-pag-lst         .
           move      fil-rfl-lst          to   rf-dcp-rfl-lst         .
           move      fil-tmc-lst          to   rf-dcp-tmc-lst         .
           move      fil-daa-lst          to   rf-dcp-daa-lst         .
           move      fil-aut-lst          to   rf-dcp-aut-lst         .
           move      fil-cod-s01          to   rf-dcp-cod-s01         .
           move      fil-cod-s02          to   rf-dcp-cod-s02         .
           move      fil-cod-s03          to   rf-dcp-cod-s03         .
           move      fil-dat-icm          to   rf-dcp-dat-icm         .
           move      fil-sta-tus          to   rf-dcp-sta-tus         .
           move      fil-sta-tud          to   rf-dcp-sta-tud         .
           move      fil-sta-tuc          to   rf-dcp-sta-tuc         .
           move      fil-sta-tux          to   rf-dcp-sta-tux         .
           move      fil-cld-imp          to   rf-dcp-cld-imp         .
           move      fil-pre-ctn          to   rf-dcp-pre-ctn         .
           move      fil-gra-ico          to   rf-dcp-gra-ico         .
           move      fil-pcl-ccz          to   rf-dcp-pcl-ccz         .
           move      fil-cod-mkt          to   rf-dcp-cod-mkt         .
           move      fil-ind-mkt          to   rf-dcp-ind-mkt         .
           move      fil-cla-bdg          to   rf-dcp-cla-bdg         .
           move      fil-for-blo          to   rf-dcp-for-blo         .
           move      fil-dor-blo          to   rf-dcp-dor-blo         .
           move      fil-fco-blo          to   rf-dcp-fco-blo         .
           move      fil-dco-blo          to   rf-dcp-dco-blo         .
           move      fil-cdn-cdm          to   rf-dcp-cdn-cdm         .
           move      fil-add-pro          to   rf-dcp-add-pro         .
           move      fil-dcf-pfz          to   rf-dcp-dcf-pfz         .
           move      fil-cod-pdt          to   rf-dcp-cod-pdt         .
           move      fil-cop-sfn          to   rf-dcp-cop-sfn         .
           move      fil-dat-sti          to   rf-dcp-dat-sti         .
           move      fil-dat-stf          to   rf-dcp-dat-stf         .
           move      fil-alx-exp          to   rf-dcp-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-dcp               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-dcp
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

