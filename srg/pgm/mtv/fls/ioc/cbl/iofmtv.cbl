       Identification Division.
       Program-Id.                                 iofmtv             .
      *================================================================*
      *                                                                *
      *                  Input-Output File mtv                         *
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
      *            * Chiave numero 01 : NUMMTV                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-mtv        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-mtv-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-des-key        pic  x(40)                  .
                   15  fil-num-mtv-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFMTV                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-alf-mtv        pic  x(14)                  .
                   15  fil-num-mtv-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNMTV                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-syn-mtv        pic  x(13)                  .
                   15  fil-num-mtv-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cla-mtv        pic  9(05)       comp-3     .
                   15  fil-gru-mtv        pic  9(05)       comp-3     .
                   15  fil-sgr-mtv        pic  9(05)       comp-3     .
                   15  fil-des-key-6      pic  x(40)                  .
                   15  fil-num-mtv-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  fil-k07.
                   15  fil-cla-mtv-7      pic  9(05)       comp-3     .
                   15  fil-gru-mtv-7      pic  9(05)       comp-3     .
                   15  fil-sgr-mtv-7      pic  9(05)       comp-3     .
                   15  fil-alf-mtv-7      pic  x(14)                  .
                   15  fil-num-mtv-7      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-inf-gen.
                   15  fil-ide-ute        pic  x(08)                  .
                   15  fil-ide-fas        pic  x(06)                  .
                   15  fil-des-mtv        pic  x(40)                  .
                   15  fil-tip-mtv        pic  9(02)                  .
               10  fil-inf-tdg.
                   15  fil-snx-mag        pic  x(01)                  .
                   15  fil-snx-mac        pic  x(01)                  .
               10  fil-inf-fis.
                   15  fil-tip-cfz        pic  9(02)                  .
                   15  fil-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  fil-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  fil-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  fil-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  fil-dim-mtv.
                       20  fil-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  fil-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  fil-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  fil-pcl-fis        pic  x(10)                  .
                   15  fil-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  fil-coe-div        pic  9(04)v9(03) comp-3     .
               10  fil-inf-gst.
                   15  fil-umi-gst        pic  x(03)                  .
                   15  fil-dec-qta        pic  9(01)                  .
                   15  fil-snx-2qt        pic  9(01)                  .
                   15  fil-dec-2qt        pic  9(01)                  .
                   15  fil-snx-3qt        pic  9(01)                  .
                   15  fil-dec-3qt        pic  9(01)                  .
                   15  fil-tip-vpr        pic  x(03)                  .
               10  fil-inf-pcs.
                   15  fil-cod-s01        pic  9(05)       comp-3     .
                   15  fil-cod-s02        pic  9(05)       comp-3     .
                   15  fil-cod-s03        pic  9(05)       comp-3     .
               10  fil-inf-bdg.
                   15  fil-cla-bdg        pic  9(05)       comp-3     .
               10  fil-inf-aps.
                   15  fil-alx-exp.
                       20  filler occurs 80
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
      *            * Chiave numero 01 : NUMMTV                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-mtv        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-mtv-2      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-des-key        pic  x(40)                  .
                   15  pul-num-mtv-3      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ALFMTV                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-alf-mtv        pic  x(14)                  .
                   15  pul-num-mtv-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : SYNMTV                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-syn-mtv        pic  x(13)                  .
                   15  pul-num-mtv-5      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : CGSDES                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cla-mtv        pic  9(05)       comp-3     .
                   15  pul-gru-mtv        pic  9(05)       comp-3     .
                   15  pul-sgr-mtv        pic  9(05)       comp-3     .
                   15  pul-des-key-6      pic  x(40)                  .
                   15  pul-num-mtv-6      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 07 : CGSALF                         *
      *            *---------------------------------------------------*
               10  pul-k07.
                   15  pul-cla-mtv-7      pic  9(05)       comp-3     .
                   15  pul-gru-mtv-7      pic  9(05)       comp-3     .
                   15  pul-sgr-mtv-7      pic  9(05)       comp-3     .
                   15  pul-alf-mtv-7      pic  x(14)                  .
                   15  pul-num-mtv-7      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-inf-gen.
                   15  pul-ide-ute        pic  x(08)                  .
                   15  pul-ide-fas        pic  x(06)                  .
                   15  pul-des-mtv        pic  x(40)                  .
                   15  pul-tip-mtv        pic  9(02)                  .
               10  pul-inf-tdg.
                   15  pul-snx-mag        pic  x(01)                  .
                   15  pul-snx-mac        pic  x(01)                  .
               10  pul-inf-fis.
                   15  pul-tip-cfz        pic  9(02)                  .
                   15  pul-qta-cfz        pic  9(06)v9(03) comp-3     .
                   15  pul-pes-uni        pic  9(06)v9(03) comp-3     .
                   15  pul-pes-tar        pic  9(06)v9(03) comp-3     .
                   15  pul-vol-uni        pic  9(06)v9(03) comp-3     .
                   15  pul-dim-mtv.
                       20  pul-dim-lar    pic  9(06)v9(03) comp-3     .
                       20  pul-dim-alt    pic  9(06)v9(03) comp-3     .
                       20  pul-dim-prf    pic  9(06)v9(03) comp-3     .
                   15  pul-pcl-fis        pic  x(10)                  .
                   15  pul-coe-mol        pic  9(04)v9(03) comp-3     .
                   15  pul-coe-div        pic  9(04)v9(03) comp-3     .
               10  pul-inf-gst.
                   15  pul-umi-gst        pic  x(03)                  .
                   15  pul-dec-qta        pic  9(01)                  .
                   15  pul-snx-2qt        pic  9(01)                  .
                   15  pul-dec-2qt        pic  9(01)                  .
                   15  pul-snx-3qt        pic  9(01)                  .
                   15  pul-dec-3qt        pic  9(01)                  .
                   15  pul-tip-vpr        pic  x(03)                  .
               10  pul-inf-pcs.
                   15  pul-cod-s01        pic  9(05)       comp-3     .
                   15  pul-cod-s02        pic  9(05)       comp-3     .
                   15  pul-cod-s03        pic  9(05)       comp-3     .
               10  pul-inf-bdg.
                   15  pul-cla-bdg        pic  9(05)       comp-3     .
               10  pul-inf-aps.
                   15  pul-alx-exp.
                       20  filler occurs 80
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
                     "mtv "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/mtv/fls/ioc/obj/iofmtv              "       .

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
           05  k-ctr                      pic  9(02) value 7          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMMTV"                                  .
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
                            "ALFMTV"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "SYNMTV"                                  .
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
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    7      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [mtv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .

      ******************************************************************
       Procedure Division                using f rf-mtv               .
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
           move      spaces               to   rf-mtv                 .
           move      zero                 to   rf-mtv-ide-dat         .
           move      spaces               to   rf-mtv-ide-ute         .
           move      spaces               to   rf-mtv-ide-fas         .
           move      zero                 to   rf-mtv-num-mtv         .
           move      spaces               to   rf-mtv-alf-mtv         .
           move      spaces               to   rf-mtv-syn-mtv         .
           move      spaces               to   rf-mtv-des-key         .
           move      spaces               to   rf-mtv-des-mtv         .
           move      zero                 to   rf-mtv-cla-mtv         .
           move      zero                 to   rf-mtv-gru-mtv         .
           move      zero                 to   rf-mtv-sgr-mtv         .
           move      zero                 to   rf-mtv-tip-mtv         .
           move      spaces               to   rf-mtv-snx-mag         .
           move      spaces               to   rf-mtv-snx-mac         .
           move      zero                 to   rf-mtv-tip-cfz         .
           move      zero                 to   rf-mtv-qta-cfz         .
           move      zero                 to   rf-mtv-pes-uni         .
           move      zero                 to   rf-mtv-pes-tar         .
           move      zero                 to   rf-mtv-vol-uni         .
           move      zero                 to   rf-mtv-dim-lar         .
           move      zero                 to   rf-mtv-dim-alt         .
           move      zero                 to   rf-mtv-dim-prf         .
           move      spaces               to   rf-mtv-pcl-fis         .
           move      zero                 to   rf-mtv-coe-mol         .
           move      zero                 to   rf-mtv-coe-div         .
           move      spaces               to   rf-mtv-umi-gst         .
           move      zero                 to   rf-mtv-dec-qta         .
           move      zero                 to   rf-mtv-snx-2qt         .
           move      zero                 to   rf-mtv-dec-2qt         .
           move      zero                 to   rf-mtv-snx-3qt         .
           move      zero                 to   rf-mtv-dec-3qt         .
           move      spaces               to   rf-mtv-tip-vpr         .
           move      zero                 to   rf-mtv-cod-s01         .
           move      zero                 to   rf-mtv-cod-s02         .
           move      zero                 to   rf-mtv-cod-s03         .
           move      zero                 to   rf-mtv-cla-bdg         .
           move      spaces               to   rf-mtv-alx-exp         .
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
           move      rf-mtv-ide-ute       to   fil-ide-ute            .
           move      rf-mtv-ide-fas       to   fil-ide-fas            .
           move      rf-mtv-des-mtv       to   fil-des-mtv            .
           move      rf-mtv-tip-mtv       to   fil-tip-mtv            .
           move      rf-mtv-snx-mag       to   fil-snx-mag            .
           move      rf-mtv-snx-mac       to   fil-snx-mac            .
           move      rf-mtv-tip-cfz       to   fil-tip-cfz            .
           move      rf-mtv-qta-cfz       to   fil-qta-cfz            .
           move      rf-mtv-pes-uni       to   fil-pes-uni            .
           move      rf-mtv-pes-tar       to   fil-pes-tar            .
           move      rf-mtv-vol-uni       to   fil-vol-uni            .
           move      rf-mtv-dim-lar       to   fil-dim-lar            .
           move      rf-mtv-dim-alt       to   fil-dim-alt            .
           move      rf-mtv-dim-prf       to   fil-dim-prf            .
           move      rf-mtv-pcl-fis       to   fil-pcl-fis            .
           move      rf-mtv-coe-mol       to   fil-coe-mol            .
           move      rf-mtv-coe-div       to   fil-coe-div            .
           move      rf-mtv-umi-gst       to   fil-umi-gst            .
           move      rf-mtv-dec-qta       to   fil-dec-qta            .
           move      rf-mtv-snx-2qt       to   fil-snx-2qt            .
           move      rf-mtv-dec-2qt       to   fil-dec-2qt            .
           move      rf-mtv-snx-3qt       to   fil-snx-3qt            .
           move      rf-mtv-dec-3qt       to   fil-dec-3qt            .
           move      rf-mtv-tip-vpr       to   fil-tip-vpr            .
           move      rf-mtv-cod-s01       to   fil-cod-s01            .
           move      rf-mtv-cod-s02       to   fil-cod-s02            .
           move      rf-mtv-cod-s03       to   fil-cod-s03            .
           move      rf-mtv-cla-bdg       to   fil-cla-bdg            .
           move      rf-mtv-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-mtv-num-mtv       to   fil-num-mtv            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-mtv-ide-dat       to   fil-ide-dat            .
           move      rf-mtv-num-mtv       to   fil-num-mtv-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-mtv-des-key       to   fil-des-key            .
           move      rf-mtv-num-mtv       to   fil-num-mtv-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-mtv-alf-mtv       to   fil-alf-mtv            .
           move      rf-mtv-num-mtv       to   fil-num-mtv-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-mtv-syn-mtv       to   fil-syn-mtv            .
           move      rf-mtv-num-mtv       to   fil-num-mtv-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-mtv-cla-mtv       to   fil-cla-mtv            .
           move      rf-mtv-gru-mtv       to   fil-gru-mtv            .
           move      rf-mtv-sgr-mtv       to   fil-sgr-mtv            .
           move      rf-mtv-des-key       to   fil-des-key-6          .
           move      rf-mtv-num-mtv       to   fil-num-mtv-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-700.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 7                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k07                .
           move      rf-mtv-cla-mtv       to   fil-cla-mtv-7          .
           move      rf-mtv-gru-mtv       to   fil-gru-mtv-7          .
           move      rf-mtv-sgr-mtv       to   fil-sgr-mtv-7          .
           move      rf-mtv-alf-mtv       to   fil-alf-mtv-7          .
           move      rf-mtv-num-mtv       to   fil-num-mtv-7          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-mtv                 .
           move      fil-ide-dat          to   rf-mtv-ide-dat         .
           move      fil-ide-ute          to   rf-mtv-ide-ute         .
           move      fil-ide-fas          to   rf-mtv-ide-fas         .
           move      fil-num-mtv          to   rf-mtv-num-mtv         .
           move      fil-alf-mtv          to   rf-mtv-alf-mtv         .
           move      fil-syn-mtv          to   rf-mtv-syn-mtv         .
           move      fil-des-key          to   rf-mtv-des-key         .
           move      fil-des-mtv          to   rf-mtv-des-mtv         .
           move      fil-cla-mtv          to   rf-mtv-cla-mtv         .
           move      fil-gru-mtv          to   rf-mtv-gru-mtv         .
           move      fil-sgr-mtv          to   rf-mtv-sgr-mtv         .
           move      fil-tip-mtv          to   rf-mtv-tip-mtv         .
           move      fil-snx-mag          to   rf-mtv-snx-mag         .
           move      fil-snx-mac          to   rf-mtv-snx-mac         .
           move      fil-tip-cfz          to   rf-mtv-tip-cfz         .
           move      fil-qta-cfz          to   rf-mtv-qta-cfz         .
           move      fil-pes-uni          to   rf-mtv-pes-uni         .
           move      fil-pes-tar          to   rf-mtv-pes-tar         .
           move      fil-vol-uni          to   rf-mtv-vol-uni         .
           move      fil-dim-lar          to   rf-mtv-dim-lar         .
           move      fil-dim-alt          to   rf-mtv-dim-alt         .
           move      fil-dim-prf          to   rf-mtv-dim-prf         .
           move      fil-pcl-fis          to   rf-mtv-pcl-fis         .
           move      fil-coe-mol          to   rf-mtv-coe-mol         .
           move      fil-coe-div          to   rf-mtv-coe-div         .
           move      fil-umi-gst          to   rf-mtv-umi-gst         .
           move      fil-dec-qta          to   rf-mtv-dec-qta         .
           move      fil-snx-2qt          to   rf-mtv-snx-2qt         .
           move      fil-dec-2qt          to   rf-mtv-dec-2qt         .
           move      fil-snx-3qt          to   rf-mtv-snx-3qt         .
           move      fil-dec-3qt          to   rf-mtv-dec-3qt         .
           move      fil-tip-vpr          to   rf-mtv-tip-vpr         .
           move      fil-cod-s01          to   rf-mtv-cod-s01         .
           move      fil-cod-s02          to   rf-mtv-cod-s02         .
           move      fil-cod-s03          to   rf-mtv-cod-s03         .
           move      fil-cla-bdg          to   rf-mtv-cla-bdg         .
           move      fil-alx-exp          to   rf-mtv-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-mtv               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-mtv
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

