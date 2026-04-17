       Identification Division.
       Program-Id.                                 iofvpt             .
      *================================================================*
      *                                                                *
      *                  Input-Output File vdp                         *
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
                   alternate record key   is fil-k09
                   alternate record key   is fil-k10
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
                   alternate record key   is pul-k09
                   alternate record key   is pul-k10
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
                   15  fil-tmo-vdp        pic  x(05)                  .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-scl-ann        pic  9(03)       comp-3     .
                   15  fil-cod-dpz-4      pic  9(02)                  .
                   15  fil-sgl-num        pic  x(03)                  .
                   15  fil-num-doc-4      pic  9(11)       comp-3     .
                   15  fil-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : TMONUM                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dpz-5      pic  9(02)                  .
                   15  fil-tmo-vdp-5      pic  x(05)                  .
                   15  fil-num-doc-5      pic  9(11)       comp-3     .
                   15  fil-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZDSZ                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cod-dpz-6      pic  9(02)                  .
                   15  fil-dsz-prd        pic  9(02)                  .
                   15  fil-dsz-cod        pic  9(07)       comp-3     .
                   15  fil-dat-doc-6      pic  9(07)       comp-3     .
                   15  fil-num-doc-6      pic  9(11)       comp-3     .
                   15  fil-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZESZ                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cod-dpz-7      pic  9(02)                  .
                   15  fil-esz-prd        pic  9(02)                  .
                   15  fil-esz-cod        pic  9(07)       comp-3     .
                   15  fil-dat-doc-7      pic  9(07)       comp-3     .
                   15  fil-num-doc-7      pic  9(11)       comp-3     .
                   15  fil-num-prt-7      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : TMODAT                         *
      *            *---------------------------------------------------*
               10  fil-k08.
                   15  fil-cod-dpz-8      pic  9(02)                  .
                   15  fil-tmo-vdp-8      pic  x(05)                  .
                   15  fil-dat-doc-8      pic  9(07)       comp-3     .
                   15  fil-num-doc-8      pic  9(11)       comp-3     .
                   15  fil-num-prt-8      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  fil-k09.
                   15  fil-cod-dpz-9      pic  9(02)                  .
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-mag        pic  9(07)       comp-3     .
                   15  fil-dat-doc-9      pic  9(07)       comp-3     .
                   15  fil-num-doc-9      pic  9(11)       comp-3     .
                   15  fil-num-prt-9      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 10 : RIFCDP                         *
      *            *---------------------------------------------------*
               10  fil-k10.
                   15  fil-prt-cdp        pic  9(11)       comp-3     .
                   15  fil-num-prt-10     pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-pmg-car            pic  9(11)       comp-3     .
               10  fil-pmg-sca occurs 10  pic  9(11)       comp-3     .
               10  fil-pmg-cad            pic  9(11)       comp-3     .
               10  fil-tmo-cdp            pic  x(05)                  .
               10  fil-dat-cdp            pic  9(07)       comp-3     .
               10  fil-num-cdp            pic  9(11)       comp-3     .
               10  fil-dsz-dpz            pic  x(04)                  .
               10  fil-esz-dpz            pic  x(04)                  .
               10  fil-esz-dat            pic  9(07)       comp-3     .
               10  fil-esz-num            pic  x(10)                  .
               10  fil-com-int.
                   15  fil-com-rig occurs 03
                                          pic  x(40)                  .
               10  fil-voc-des occurs 06  pic  x(03)                  .
               10  fil-qas-vde occurs 06  pic s9(06)v9(03) comp-3     .
               10  fil-alf-mag            pic  x(14)                  .
               10  fil-sgl-vrn            pic  x(14)                  .
               10  fil-umi-prd            pic  x(03)                  .
               10  fil-dec-qta            pic  9(01)                  .
               10  fil-qta-pdt            pic s9(06)v9(03) comp-3     .
               10  fil-cod-dsl            pic  x(07)                  .
               10  fil-snx-2qt            pic  9(01)                  .
               10  fil-dec-2qt            pic  9(01)                  .
               10  fil-qta-a02            pic s9(06)v9(03) comp-3     .
               10  fil-snx-3qt            pic  9(01)                  .
               10  fil-dec-3qt            pic  9(01)                  .
               10  fil-qta-a03            pic s9(06)v9(03) comp-3     .
               10  fil-cdp-fzs            pic  x(01)                  .
               10  fil-ctr-stp            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
                   15  pul-tmo-vdp        pic  x(05)                  .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : CNTDEN                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-scl-ann        pic  9(03)       comp-3     .
                   15  pul-cod-dpz-4      pic  9(02)                  .
                   15  pul-sgl-num        pic  x(03)                  .
                   15  pul-num-doc-4      pic  9(11)       comp-3     .
                   15  pul-num-prt-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : TMONUM                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dpz-5      pic  9(02)                  .
                   15  pul-tmo-vdp-5      pic  x(05)                  .
                   15  pul-num-doc-5      pic  9(11)       comp-3     .
                   15  pul-num-prt-5      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : DPZDSZ                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cod-dpz-6      pic  9(02)                  .
                   15  pul-dsz-prd        pic  9(02)                  .
                   15  pul-dsz-cod        pic  9(07)       comp-3     .
                   15  pul-dat-doc-6      pic  9(07)       comp-3     .
                   15  pul-num-doc-6      pic  9(11)       comp-3     .
                   15  pul-num-prt-6      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : DPZESZ                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-cod-dpz-7      pic  9(02)                  .
                   15  pul-esz-prd        pic  9(02)                  .
                   15  pul-esz-cod        pic  9(07)       comp-3     .
                   15  pul-dat-doc-7      pic  9(07)       comp-3     .
                   15  pul-num-doc-7      pic  9(11)       comp-3     .
                   15  pul-num-prt-7      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 08 : TMODAT                         *
      *            *---------------------------------------------------*
               10  pul-k08.
                   15  pul-cod-dpz-8      pic  9(02)                  .
                   15  pul-tmo-vdp-8      pic  x(05)                  .
                   15  pul-dat-doc-8      pic  9(07)       comp-3     .
                   15  pul-num-doc-8      pic  9(11)       comp-3     .
                   15  pul-num-prt-8      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 09 : MAGDAT                         *
      *            *---------------------------------------------------*
               10  pul-k09.
                   15  pul-cod-dpz-9      pic  9(02)                  .
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-mag        pic  9(07)       comp-3     .
                   15  pul-dat-doc-9      pic  9(07)       comp-3     .
                   15  pul-num-doc-9      pic  9(11)       comp-3     .
                   15  pul-num-prt-9      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 10 : RIFCDP                         *
      *            *---------------------------------------------------*
               10  pul-k10.
                   15  pul-prt-cdp        pic  9(11)       comp-3     .
                   15  pul-num-prt-10     pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-pmg-car            pic  9(11)       comp-3     .
               10  pul-pmg-sca occurs 10  pic  9(11)       comp-3     .
               10  pul-pmg-cad            pic  9(11)       comp-3     .
               10  pul-tmo-cdp            pic  x(05)                  .
               10  pul-dat-cdp            pic  9(07)       comp-3     .
               10  pul-num-cdp            pic  9(11)       comp-3     .
               10  pul-dsz-dpz            pic  x(04)                  .
               10  pul-esz-dpz            pic  x(04)                  .
               10  pul-esz-dat            pic  9(07)       comp-3     .
               10  pul-esz-num            pic  x(10)                  .
               10  pul-com-int.
                   15  pul-com-rig occurs 03
                                          pic  x(40)                  .
               10  pul-voc-des occurs 06  pic  x(03)                  .
               10  pul-qas-vde occurs 06  pic s9(06)v9(03) comp-3     .
               10  pul-alf-mag            pic  x(14)                  .
               10  pul-sgl-vrn            pic  x(14)                  .
               10  pul-umi-prd            pic  x(03)                  .
               10  pul-dec-qta            pic  9(01)                  .
               10  pul-qta-pdt            pic s9(06)v9(03) comp-3     .
               10  pul-cod-dsl            pic  x(07)                  .
               10  pul-snx-2qt            pic  9(01)                  .
               10  pul-dec-2qt            pic  9(01)                  .
               10  pul-qta-a02            pic s9(06)v9(03) comp-3     .
               10  pul-snx-3qt            pic  9(01)                  .
               10  pul-dec-3qt            pic  9(01)                  .
               10  pul-qta-a03            pic s9(06)v9(03) comp-3     .
               10  pul-cdp-fzs            pic  x(01)                  .
               10  pul-ctr-stp            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 80   pic  x(01)                  .

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
                     "vpt "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/vdp/fls/ioc/obj/iofvpt              "       .

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
           05  k-ctr                      pic  9(02) value 10         .
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
                            "TMONUM    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZDSZ    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 7                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DPZESZ    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 8                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "TMODAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 9                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "MAGDAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 10                             *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "RIFCDP    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    10     pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [vpt]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfvpt"                          .

      ******************************************************************
       Procedure Division                using f rf-vpt               .
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
                     str-900
                     str-950
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
       str-900.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 9                       *
      *              *-------------------------------------------------*
           go to     str-910
                     str-920
                     str-930
                     depending            on   z-tco                  .
       str-910.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 9           *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k09
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-920.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 9           *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k09
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-930.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 9           *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k09
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-950.
      *              *-------------------------------------------------*
      *              * Start per chiave indice 10                      *
      *              *-------------------------------------------------*
           go to     str-955
                     str-960
                     str-965
                     depending            on   z-tco                  .
       str-955.
      *                     *------------------------------------------*
      *                     * Start not less chiave indice 10          *
      *                     *------------------------------------------*
           start     fil    key not less
                            fil-k10
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-960.
      *                     *------------------------------------------*
      *                     * Start greater  chiave indice 10          *
      *                     *------------------------------------------*
           start     fil    key greater
                            fil-k10
                            invalid key
                            go to   str-990.
           go to     str-980.
       str-965.
      *                     *------------------------------------------*
      *                     * Start not >    chiave indice 10          *
      *                     *------------------------------------------*
           start     fil    key not greater
                            fil-k10
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
                     rea-900
                     rea-950
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
       rea-900.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 9                        *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-910.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k09
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-910.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k09
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-950.
      *              *-------------------------------------------------*
      *              * Read per chiave indice 10                       *
      *              *-------------------------------------------------*
           if        z-lok                =    1
                     go to rea-960.
      *                     *------------------------------------------*
      *                     * Read no lock                             *
      *                     *------------------------------------------*
           read      fil    with no lock
                            key  is fil-k10
                            invalid key
                            go to   rea-990.
           go to     rea-980.
       rea-960.
      *                     *------------------------------------------*
      *                     * Read with lock                           *
      *                     *------------------------------------------*
           read      fil    key  is fil-k10
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
           move      spaces               to   rf-vpt                 .
           move      zero                 to   rf-vpt-ide-dat         .
           move      spaces               to   rf-vpt-ide-ute         .
           move      spaces               to   rf-vpt-ide-fas         .
           move      zero                 to   rf-vpt-num-prt         .
           move      spaces               to   rf-vpt-tmo-vdp         .
           move      zero                 to   rf-vpt-pmg-car         .
           move      zero                 to   rf-vpt-pmg-sca (01)    .
           move      zero                 to   rf-vpt-pmg-sca (02)    .
           move      zero                 to   rf-vpt-pmg-sca (03)    .
           move      zero                 to   rf-vpt-pmg-sca (04)    .
           move      zero                 to   rf-vpt-pmg-sca (05)    .
           move      zero                 to   rf-vpt-pmg-sca (06)    .
           move      zero                 to   rf-vpt-pmg-sca (07)    .
           move      zero                 to   rf-vpt-pmg-sca (08)    .
           move      zero                 to   rf-vpt-pmg-sca (09)    .
           move      zero                 to   rf-vpt-pmg-sca (10)    .
           move      zero                 to   rf-vpt-pmg-cad         .
           move      zero                 to   rf-vpt-cod-dpz         .
           move      zero                 to   rf-vpt-dat-doc         .
           move      zero                 to   rf-vpt-num-doc         .
           move      zero                 to   rf-vpt-scl-ann         .
           move      spaces               to   rf-vpt-sgl-num         .
           move      spaces               to   rf-vpt-tmo-cdp         .
           move      zero                 to   rf-vpt-dat-cdp         .
           move      zero                 to   rf-vpt-num-cdp         .
           move      zero                 to   rf-vpt-prt-cdp         .
           move      zero                 to   rf-vpt-dsz-prd         .
           move      zero                 to   rf-vpt-dsz-cod         .
           move      spaces               to   rf-vpt-dsz-dpz         .
           move      zero                 to   rf-vpt-esz-prd         .
           move      zero                 to   rf-vpt-esz-cod         .
           move      spaces               to   rf-vpt-esz-dpz         .
           move      zero                 to   rf-vpt-esz-dat         .
           move      spaces               to   rf-vpt-esz-num         .
           move      spaces               to   rf-vpt-com-int         .
           move      spaces               to   rf-vpt-voc-des (1)     .
           move      spaces               to   rf-vpt-voc-des (2)     .
           move      spaces               to   rf-vpt-voc-des (3)     .
           move      spaces               to   rf-vpt-voc-des (4)     .
           move      spaces               to   rf-vpt-voc-des (5)     .
           move      spaces               to   rf-vpt-voc-des (6)     .
           move      zero                 to   rf-vpt-qas-vde (1)     .
           move      zero                 to   rf-vpt-qas-vde (2)     .
           move      zero                 to   rf-vpt-qas-vde (3)     .
           move      zero                 to   rf-vpt-qas-vde (4)     .
           move      zero                 to   rf-vpt-qas-vde (5)     .
           move      zero                 to   rf-vpt-qas-vde (6)     .
           move      zero                 to   rf-vpt-tip-mag         .
           move      zero                 to   rf-vpt-num-mag         .
           move      spaces               to   rf-vpt-alf-mag         .
           move      spaces               to   rf-vpt-sgl-vrn         .
           move      spaces               to   rf-vpt-umi-prd         .
           move      zero                 to   rf-vpt-dec-qta         .
           move      zero                 to   rf-vpt-qta-pdt         .
           move      spaces               to   rf-vpt-cod-dsl         .
           move      zero                 to   rf-vpt-snx-2qt         .
           move      zero                 to   rf-vpt-dec-2qt         .
           move      zero                 to   rf-vpt-qta-a02         .
           move      zero                 to   rf-vpt-snx-3qt         .
           move      zero                 to   rf-vpt-dec-3qt         .
           move      zero                 to   rf-vpt-qta-a03         .
           move      spaces               to   rf-vpt-cdp-fzs         .
           move      zero                 to   rf-vpt-ctr-stp         .
           move      spaces               to   rf-vpt-flg-ela         .
           move      spaces               to   rf-vpt-flg-pul         .
           move      spaces               to   rf-vpt-alx-exp         .
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
           move      rf-vpt-ide-ute       to   fil-ide-ute            .
           move      rf-vpt-ide-fas       to   fil-ide-fas            .
           move      rf-vpt-pmg-car       to   fil-pmg-car            .
           move      rf-vpt-pmg-sca (01)  to   fil-pmg-sca (01)       .
           move      rf-vpt-pmg-sca (02)  to   fil-pmg-sca (02)       .
           move      rf-vpt-pmg-sca (03)  to   fil-pmg-sca (03)       .
           move      rf-vpt-pmg-sca (04)  to   fil-pmg-sca (04)       .
           move      rf-vpt-pmg-sca (05)  to   fil-pmg-sca (05)       .
           move      rf-vpt-pmg-sca (06)  to   fil-pmg-sca (06)       .
           move      rf-vpt-pmg-sca (07)  to   fil-pmg-sca (07)       .
           move      rf-vpt-pmg-sca (08)  to   fil-pmg-sca (08)       .
           move      rf-vpt-pmg-sca (09)  to   fil-pmg-sca (09)       .
           move      rf-vpt-pmg-sca (10)  to   fil-pmg-sca (10)       .
           move      rf-vpt-pmg-cad       to   fil-pmg-cad            .
           move      rf-vpt-tmo-cdp       to   fil-tmo-cdp            .
           move      rf-vpt-dat-cdp       to   fil-dat-cdp            .
           move      rf-vpt-num-cdp       to   fil-num-cdp            .
           move      rf-vpt-prt-cdp       to   fil-prt-cdp            .
           move      rf-vpt-dsz-dpz       to   fil-dsz-dpz            .
           move      rf-vpt-esz-dpz       to   fil-esz-dpz            .
           move      rf-vpt-esz-dat       to   fil-esz-dat            .
           move      rf-vpt-esz-num       to   fil-esz-num            .
           move      rf-vpt-com-int       to   fil-com-int            .
           move      rf-vpt-voc-des (1)   to   fil-voc-des (1)        .
           move      rf-vpt-voc-des (2)   to   fil-voc-des (2)        .
           move      rf-vpt-voc-des (3)   to   fil-voc-des (3)        .
           move      rf-vpt-voc-des (4)   to   fil-voc-des (4)        .
           move      rf-vpt-voc-des (5)   to   fil-voc-des (5)        .
           move      rf-vpt-voc-des (6)   to   fil-voc-des (6)        .
           move      rf-vpt-qas-vde (1)   to   fil-qas-vde (1)        .
           move      rf-vpt-qas-vde (2)   to   fil-qas-vde (2)        .
           move      rf-vpt-qas-vde (3)   to   fil-qas-vde (3)        .
           move      rf-vpt-qas-vde (4)   to   fil-qas-vde (4)        .
           move      rf-vpt-qas-vde (5)   to   fil-qas-vde (5)        .
           move      rf-vpt-qas-vde (6)   to   fil-qas-vde (6)        .
           move      rf-vpt-alf-mag       to   fil-alf-mag            .
           move      rf-vpt-sgl-vrn       to   fil-sgl-vrn            .
           move      rf-vpt-umi-prd       to   fil-umi-prd            .
           move      rf-vpt-dec-qta       to   fil-dec-qta            .
           move      rf-vpt-qta-pdt       to   fil-qta-pdt            .
           move      rf-vpt-cod-dsl       to   fil-cod-dsl            .
           move      rf-vpt-snx-2qt       to   fil-snx-2qt            .
           move      rf-vpt-dec-2qt       to   fil-dec-2qt            .
           move      rf-vpt-qta-a02       to   fil-qta-a02            .
           move      rf-vpt-snx-3qt       to   fil-snx-3qt            .
           move      rf-vpt-dec-3qt       to   fil-dec-3qt            .
           move      rf-vpt-qta-a03       to   fil-qta-a03            .
           move      rf-vpt-cdp-fzs       to   fil-cdp-fzs            .
           move      rf-vpt-ctr-stp       to   fil-ctr-stp            .
           move      rf-vpt-flg-ela       to   fil-flg-ela            .
           move      rf-vpt-flg-pul       to   fil-flg-pul            .
           move      rf-vpt-alx-exp       to   fil-alx-exp            .
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
                     cmp-key-fis-900
                     cmp-key-fis-950
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-vpt-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-vpt-ide-dat       to   fil-ide-dat            .
           move      rf-vpt-dat-doc       to   fil-dat-doc            .
           move      rf-vpt-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-vpt-dat-doc       to   fil-dat-doc-3          .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz            .
           move      rf-vpt-num-doc       to   fil-num-doc            .
           move      rf-vpt-tmo-vdp       to   fil-tmo-vdp            .
           move      rf-vpt-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-vpt-scl-ann       to   fil-scl-ann            .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz-4          .
           move      rf-vpt-sgl-num       to   fil-sgl-num            .
           move      rf-vpt-num-doc       to   fil-num-doc-4          .
           move      rf-vpt-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz-5          .
           move      rf-vpt-tmo-vdp       to   fil-tmo-vdp-5          .
           move      rf-vpt-num-doc       to   fil-num-doc-5          .
           move      rf-vpt-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz-6          .
           move      rf-vpt-dsz-prd       to   fil-dsz-prd            .
           move      rf-vpt-dsz-cod       to   fil-dsz-cod            .
           move      rf-vpt-dat-doc       to   fil-dat-doc-6          .
           move      rf-vpt-num-doc       to   fil-num-doc-6          .
           move      rf-vpt-num-prt       to   fil-num-prt-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz-7          .
           move      rf-vpt-esz-prd       to   fil-esz-prd            .
           move      rf-vpt-esz-cod       to   fil-esz-cod            .
           move      rf-vpt-dat-doc       to   fil-dat-doc-7          .
           move      rf-vpt-num-doc       to   fil-num-doc-7          .
           move      rf-vpt-num-prt       to   fil-num-prt-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-800.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 8                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k08                .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz-8          .
           move      rf-vpt-tmo-vdp       to   fil-tmo-vdp-8          .
           move      rf-vpt-dat-doc       to   fil-dat-doc-8          .
           move      rf-vpt-num-doc       to   fil-num-doc-8          .
           move      rf-vpt-num-prt       to   fil-num-prt-8          .
           go to     cmp-key-fis-999.
       cmp-key-fis-900.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 9                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k09                .
           move      rf-vpt-cod-dpz       to   fil-cod-dpz-9          .
           move      rf-vpt-tip-mag       to   fil-tip-mag            .
           move      rf-vpt-num-mag       to   fil-num-mag            .
           move      rf-vpt-dat-doc       to   fil-dat-doc-9          .
           move      rf-vpt-num-doc       to   fil-num-doc-9          .
           move      rf-vpt-num-prt       to   fil-num-prt-9          .
           go to     cmp-key-fis-999.
       cmp-key-fis-950.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 10                   *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k10                .
           move      rf-vpt-prt-cdp       to   fil-prt-cdp            .
           move      rf-vpt-num-prt       to   fil-num-prt-10         .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-vpt                 .
           move      fil-ide-dat          to   rf-vpt-ide-dat         .
           move      fil-ide-ute          to   rf-vpt-ide-ute         .
           move      fil-ide-fas          to   rf-vpt-ide-fas         .
           move      fil-num-prt          to   rf-vpt-num-prt         .
           move      fil-tmo-vdp          to   rf-vpt-tmo-vdp         .
           move      fil-pmg-car          to   rf-vpt-pmg-car         .
           move      fil-pmg-sca (01)     to   rf-vpt-pmg-sca (01)    .
           move      fil-pmg-sca (02)     to   rf-vpt-pmg-sca (02)    .
           move      fil-pmg-sca (03)     to   rf-vpt-pmg-sca (03)    .
           move      fil-pmg-sca (04)     to   rf-vpt-pmg-sca (04)    .
           move      fil-pmg-sca (05)     to   rf-vpt-pmg-sca (05)    .
           move      fil-pmg-sca (06)     to   rf-vpt-pmg-sca (06)    .
           move      fil-pmg-sca (07)     to   rf-vpt-pmg-sca (07)    .
           move      fil-pmg-sca (08)     to   rf-vpt-pmg-sca (08)    .
           move      fil-pmg-sca (09)     to   rf-vpt-pmg-sca (09)    .
           move      fil-pmg-sca (10)     to   rf-vpt-pmg-sca (10)    .
           move      fil-pmg-cad          to   rf-vpt-pmg-cad         .
           move      fil-cod-dpz          to   rf-vpt-cod-dpz         .
           move      fil-dat-doc          to   rf-vpt-dat-doc         .
           move      fil-num-doc          to   rf-vpt-num-doc         .
           move      fil-scl-ann          to   rf-vpt-scl-ann         .
           move      fil-sgl-num          to   rf-vpt-sgl-num         .
           move      fil-tmo-cdp          to   rf-vpt-tmo-cdp         .
           move      fil-dat-cdp          to   rf-vpt-dat-cdp         .
           move      fil-num-cdp          to   rf-vpt-num-cdp         .
           move      fil-prt-cdp          to   rf-vpt-prt-cdp         .
           move      fil-dsz-prd          to   rf-vpt-dsz-prd         .
           move      fil-dsz-cod          to   rf-vpt-dsz-cod         .
           move      fil-dsz-dpz          to   rf-vpt-dsz-dpz         .
           move      fil-esz-prd          to   rf-vpt-esz-prd         .
           move      fil-esz-cod          to   rf-vpt-esz-cod         .
           move      fil-esz-dpz          to   rf-vpt-esz-dpz         .
           move      fil-esz-dat          to   rf-vpt-esz-dat         .
           move      fil-esz-num          to   rf-vpt-esz-num         .
           move      fil-com-int          to   rf-vpt-com-int         .
           move      fil-voc-des (1)      to   rf-vpt-voc-des (1)     .
           move      fil-voc-des (2)      to   rf-vpt-voc-des (2)     .
           move      fil-voc-des (3)      to   rf-vpt-voc-des (3)     .
           move      fil-voc-des (4)      to   rf-vpt-voc-des (4)     .
           move      fil-voc-des (5)      to   rf-vpt-voc-des (5)     .
           move      fil-voc-des (6)      to   rf-vpt-voc-des (6)     .
           move      fil-qas-vde (1)      to   rf-vpt-qas-vde (1)     .
           move      fil-qas-vde (2)      to   rf-vpt-qas-vde (2)     .
           move      fil-qas-vde (3)      to   rf-vpt-qas-vde (3)     .
           move      fil-qas-vde (4)      to   rf-vpt-qas-vde (4)     .
           move      fil-qas-vde (5)      to   rf-vpt-qas-vde (5)     .
           move      fil-qas-vde (6)      to   rf-vpt-qas-vde (6)     .
           move      fil-tip-mag          to   rf-vpt-tip-mag         .
           move      fil-num-mag          to   rf-vpt-num-mag         .
           move      fil-alf-mag          to   rf-vpt-alf-mag         .
           move      fil-sgl-vrn          to   rf-vpt-sgl-vrn         .
           move      fil-umi-prd          to   rf-vpt-umi-prd         .
           move      fil-dec-qta          to   rf-vpt-dec-qta         .
           move      fil-qta-pdt          to   rf-vpt-qta-pdt         .
           move      fil-cod-dsl          to   rf-vpt-cod-dsl         .
           move      fil-snx-2qt          to   rf-vpt-snx-2qt         .
           move      fil-dec-2qt          to   rf-vpt-dec-2qt         .
           move      fil-qta-a02          to   rf-vpt-qta-a02         .
           move      fil-snx-3qt          to   rf-vpt-snx-3qt         .
           move      fil-dec-3qt          to   rf-vpt-dec-3qt         .
           move      fil-qta-a03          to   rf-vpt-qta-a03         .
           move      fil-cdp-fzs          to   rf-vpt-cdp-fzs         .
           move      fil-ctr-stp          to   rf-vpt-ctr-stp         .
           move      fil-flg-ela          to   rf-vpt-flg-ela         .
           move      fil-flg-pul          to   rf-vpt-flg-pul         .
           move      fil-alx-exp          to   rf-vpt-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-vpt               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-vpt
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
