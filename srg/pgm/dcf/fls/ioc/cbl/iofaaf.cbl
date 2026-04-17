       Identification Division.
       Program-Id.                                 iofaaf             .
      *================================================================*
      *                                                                *
      *                  Input-Output File aaf                         *
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
                   alternate record key   is pul-k06
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
      *            * Chiave numero 01 : PRFNFM                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-mag        pic  9(02)                  .
                   15  fil-num-pro        pic  9(07)       comp-3     .
                   15  fil-cod-dcf        pic  9(07)       comp-3     .
                   15  fil-fda-pif        pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-tip-mag-2      pic  9(02)                  .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
                   15  fil-cod-dcf-2      pic  9(07)       comp-3     .
                   15  fil-fda-pif-2      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNPRFM                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-dcf-3      pic  9(07)       comp-3     .
                   15  fil-tip-mag-3      pic  9(02)                  .
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
                   15  fil-fda-pif-3      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PRFMFN                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-tip-mag-4      pic  9(02)                  .
                   15  fil-num-pro-4      pic  9(07)       comp-3     .
                   15  fil-fda-pif-4      pic  x(14)                  .
                   15  fil-cod-dcf-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DCFCOPSFN                      *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-dcf-5      pic  9(07)       comp-3     .
                   15  fil-cop-sfn        pic  x(14)                  .
                   15  fil-tip-mag-5      pic  9(02)                  .
                   15  fil-num-pro-5      pic  9(07)       comp-3     .
                   15  fil-fda-pif-5      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : COPSFN                         *
      *            *---------------------------------------------------*
               10  fil-k06.
                   15  fil-cop-sfn-6      pic  x(14)                  .
                   15  fil-tip-mag-6      pic  9(02)                  .
                   15  fil-cod-dcf-6      pic  9(07)       comp-3     .
                   15  fil-num-pro-6      pic  9(07)       comp-3     .
                   15  fil-fda-pif-6      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dep-sfn            pic  x(40)                  .
               10  fil-xdp-sfn            pic  9(01)                  .
               10  fil-snx-tum            pic  x(01)                  .
               10  fil-umf-tum            pic  x(03)                  .
               10  fil-nde-tum            pic  9(01)                  .
               10  fil-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  fil-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  fil-dpz-dcf            pic  x(04)                  .
               10  fil-ann-not            pic  x(40)                  .
               10  fil-tmp-cns            pic  9(03)       comp-3     .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-dec-prz            pic  9(01)                  .
               10  fil-tip-pza            pic  9(02)                  .
               10  fil-lot-acq            pic  9(10)v9(03) comp-3     .
               10  fil-tap-pes            pic  9(02)                  .
               10  fil-uda-pes            pic  9(07)       comp-3     .
               10  fil-lgv-vlt            pic  x(03)                  .
               10  fil-lgv-dcv            pic  9(01)                  .
               10  fil-lgv-tdc            pic  x(01)                  .
               10  fil-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  fil-lgv-pdt            pic  9(01)v9(02) comp-3     .
               10  fil-tbl-pes.
                   15  fil-ele-pes occurs 06.
                       20  fil-qta-pes    pic  9(06)v9(03) comp-3     .
                       20  fil-prz-pes    pic  9(09)       comp-3     .
                       20  fil-csr-pes    pic  9(05)       comp-3     .
                       20  fil-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  fil-per-mpa            pic  9(02)v9(01)            .
               10  fil-rif-lst            pic  x(20)                  .
               10  fil-per-ric            pic  9(06)v9(03)            .
               10  fil-alx-exp.
                   15  filler  occurs 11  pic  x(01)                  .

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
      *            * Chiave numero 01 : PRFNFM                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-mag        pic  9(02)                  .
                   15  pul-num-pro        pic  9(07)       comp-3     .
                   15  pul-cod-dcf        pic  9(07)       comp-3     .
                   15  pul-fda-pif        pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-tip-mag-2      pic  9(02)                  .
                   15  pul-num-pro-2      pic  9(07)       comp-3     .
                   15  pul-cod-dcf-2      pic  9(07)       comp-3     .
                   15  pul-fda-pif-2      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNPRFM                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-dcf-3      pic  9(07)       comp-3     .
                   15  pul-tip-mag-3      pic  9(02)                  .
                   15  pul-num-pro-3      pic  9(07)       comp-3     .
                   15  pul-fda-pif-3      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : PRFMFN                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-tip-mag-4      pic  9(02)                  .
                   15  pul-num-pro-4      pic  9(07)       comp-3     .
                   15  pul-fda-pif-4      pic  x(14)                  .
                   15  pul-cod-dcf-4      pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : DCFCOPSFN                      *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-dcf-5      pic  9(07)       comp-3     .
                   15  pul-cop-sfn        pic  x(14)                  .
                   15  pul-tip-mag-5      pic  9(02)                  .
                   15  pul-num-pro-5      pic  9(07)       comp-3     .
                   15  pul-fda-pif-5      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 06 : COPSFN                         *
      *            *---------------------------------------------------*
               10  pul-k06.
                   15  pul-cop-sfn-6      pic  x(14)                  .
                   15  pul-tip-mag-6      pic  9(02)                  .
                   15  pul-cod-dcf-6      pic  9(07)       comp-3     .
                   15  pul-num-pro-6      pic  9(07)       comp-3     .
                   15  pul-fda-pif-6      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dep-sfn            pic  x(40)                  .
               10  pul-xdp-sfn            pic  9(01)                  .
               10  pul-snx-tum            pic  x(01)                  .
               10  pul-umf-tum            pic  x(03)                  .
               10  pul-nde-tum            pic  9(01)                  .
               10  pul-cmo-tum            pic  9(06)v9(03) comp-3     .
               10  pul-cdi-tum            pic  9(06)v9(03) comp-3     .
               10  pul-dpz-dcf            pic  x(04)                  .
               10  pul-ann-not            pic  x(40)                  .
               10  pul-tmp-cns            pic  9(03)       comp-3     .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-dec-prz            pic  9(01)                  .
               10  pul-tip-pza            pic  9(02)                  .
               10  pul-lot-acq            pic  9(10)v9(03) comp-3     .
               10  pul-tap-pes            pic  9(02)                  .
               10  pul-uda-pes            pic  9(07)       comp-3     .
               10  pul-lgv-vlt            pic  x(03)                  .
               10  pul-lgv-dcv            pic  9(01)                  .
               10  pul-lgv-tdc            pic  x(01)                  .
               10  pul-lgv-cdc            pic  9(06)v9(05) comp-3     .
               10  pul-lgv-pdt            pic  9(01)v9(02) comp-3     .
               10  pul-tbl-pes.
                   15  pul-ele-pes occurs 06.
                       20  pul-qta-pes    pic  9(06)v9(03) comp-3     .
                       20  pul-prz-pes    pic  9(09)       comp-3     .
                       20  pul-csr-pes    pic  9(05)       comp-3     .
                       20  pul-psr-pes occurs 05
                                          pic  9(02)v9(01)            .
               10  pul-per-mpa            pic  9(02)v9(01)            .
               10  pul-rif-lst            pic  x(20)                  .
               10  pul-per-ric            pic  9(06)v9(03)            .
               10  pul-alx-exp.
                   15  filler  occurs 11  pic  x(01)                  .

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
                     "aaf "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcf/fls/ioc/obj/iofaaf              "       .

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
           05  k-ctr                      pic  9(02) value 6          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PRFNFM"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "FNPRFM"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PRFMFN"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DCFCOPSFN"                               .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 6                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "COPSFN"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    6      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [aaf]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .

      ******************************************************************
       Procedure Division                using f rf-aaf               .
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
           move      spaces               to   rf-aaf                 .
           move      zero                 to   rf-aaf-ide-dat         .
           move      spaces               to   rf-aaf-ide-ute         .
           move      spaces               to   rf-aaf-ide-fas         .
           move      zero                 to   rf-aaf-tip-mag         .
           move      zero                 to   rf-aaf-num-pro         .
           move      zero                 to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      spaces               to   rf-aaf-cop-sfn         .
           move      spaces               to   rf-aaf-dep-sfn         .
           move      zero                 to   rf-aaf-xdp-sfn         .
           move      spaces               to   rf-aaf-snx-tum         .
           move      spaces               to   rf-aaf-umf-tum         .
           move      zero                 to   rf-aaf-nde-tum         .
           move      zero                 to   rf-aaf-cmo-tum         .
           move      zero                 to   rf-aaf-cdi-tum         .
           move      spaces               to   rf-aaf-dpz-dcf         .
           move      spaces               to   rf-aaf-ann-not         .
           move      zero                 to   rf-aaf-tmp-cns         .
           move      spaces               to   rf-aaf-sgl-vlt         .
           move      zero                 to   rf-aaf-dec-vlt         .
           move      zero                 to   rf-aaf-dec-prz         .
           move      zero                 to   rf-aaf-tip-pza         .
           move      zero                 to   rf-aaf-lot-acq         .
           move      zero                 to   rf-aaf-tap-pes         .
           move      zero                 to   rf-aaf-uda-pes         .
           move      spaces               to   rf-aaf-lgv-vlt         .
           move      zero                 to   rf-aaf-lgv-dcv         .
           move      spaces               to   rf-aaf-lgv-tdc         .
           move      zero                 to   rf-aaf-lgv-cdc         .
           move      zero                 to   rf-aaf-lgv-pdt         .
           move      zero                 to   rf-aaf-qta-pes (1)     .
           move      zero                 to   rf-aaf-qta-pes (2)     .
           move      zero                 to   rf-aaf-qta-pes (3)     .
           move      zero                 to   rf-aaf-qta-pes (4)     .
           move      zero                 to   rf-aaf-qta-pes (5)     .
           move      zero                 to   rf-aaf-qta-pes (6)     .
           move      zero                 to   rf-aaf-prz-pes (1)     .
           move      zero                 to   rf-aaf-prz-pes (2)     .
           move      zero                 to   rf-aaf-prz-pes (3)     .
           move      zero                 to   rf-aaf-prz-pes (4)     .
           move      zero                 to   rf-aaf-prz-pes (5)     .
           move      zero                 to   rf-aaf-prz-pes (6)     .
           move      zero                 to   rf-aaf-csr-pes (1)     .
           move      zero                 to   rf-aaf-csr-pes (2)     .
           move      zero                 to   rf-aaf-csr-pes (3)     .
           move      zero                 to   rf-aaf-csr-pes (4)     .
           move      zero                 to   rf-aaf-csr-pes (5)     .
           move      zero                 to   rf-aaf-csr-pes (6)     .
           move      zero                 to   rf-aaf-psr-pes (1, 1)  .
           move      zero                 to   rf-aaf-psr-pes (1, 2)  .
           move      zero                 to   rf-aaf-psr-pes (1, 3)  .
           move      zero                 to   rf-aaf-psr-pes (1, 4)  .
           move      zero                 to   rf-aaf-psr-pes (1, 5)  .
           move      zero                 to   rf-aaf-psr-pes (2, 1)  .
           move      zero                 to   rf-aaf-psr-pes (2, 2)  .
           move      zero                 to   rf-aaf-psr-pes (2, 3)  .
           move      zero                 to   rf-aaf-psr-pes (2, 4)  .
           move      zero                 to   rf-aaf-psr-pes (2, 5)  .
           move      zero                 to   rf-aaf-psr-pes (3, 1)  .
           move      zero                 to   rf-aaf-psr-pes (3, 2)  .
           move      zero                 to   rf-aaf-psr-pes (3, 3)  .
           move      zero                 to   rf-aaf-psr-pes (3, 4)  .
           move      zero                 to   rf-aaf-psr-pes (3, 5)  .
           move      zero                 to   rf-aaf-psr-pes (4, 1)  .
           move      zero                 to   rf-aaf-psr-pes (4, 2)  .
           move      zero                 to   rf-aaf-psr-pes (4, 3)  .
           move      zero                 to   rf-aaf-psr-pes (4, 4)  .
           move      zero                 to   rf-aaf-psr-pes (4, 5)  .
           move      zero                 to   rf-aaf-psr-pes (5, 1)  .
           move      zero                 to   rf-aaf-psr-pes (5, 2)  .
           move      zero                 to   rf-aaf-psr-pes (5, 3)  .
           move      zero                 to   rf-aaf-psr-pes (5, 4)  .
           move      zero                 to   rf-aaf-psr-pes (5, 5)  .
           move      zero                 to   rf-aaf-psr-pes (6, 1)  .
           move      zero                 to   rf-aaf-psr-pes (6, 2)  .
           move      zero                 to   rf-aaf-psr-pes (6, 3)  .
           move      zero                 to   rf-aaf-psr-pes (6, 4)  .
           move      zero                 to   rf-aaf-psr-pes (6, 5)  .
           move      zero                 to   rf-aaf-per-mpa         .
           move      spaces               to   rf-aaf-rif-lst         .
           move      zero                 to   rf-aaf-per-ric         .
           move      spaces               to   rf-aaf-alx-exp         .
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
           move      rf-aaf-ide-ute       to   fil-ide-ute            .
           move      rf-aaf-ide-fas       to   fil-ide-fas            .
           move      rf-aaf-dep-sfn       to   fil-dep-sfn            .
           move      rf-aaf-xdp-sfn       to   fil-xdp-sfn            .
           move      rf-aaf-snx-tum       to   fil-snx-tum            .
           move      rf-aaf-umf-tum       to   fil-umf-tum            .
           move      rf-aaf-nde-tum       to   fil-nde-tum            .
           move      rf-aaf-cmo-tum       to   fil-cmo-tum            .
           move      rf-aaf-cdi-tum       to   fil-cdi-tum            .
           move      rf-aaf-dpz-dcf       to   fil-dpz-dcf            .
           move      rf-aaf-ann-not       to   fil-ann-not            .
           move      rf-aaf-tmp-cns       to   fil-tmp-cns            .
           move      rf-aaf-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-aaf-dec-vlt       to   fil-dec-vlt            .
           move      rf-aaf-dec-prz       to   fil-dec-prz            .
           move      rf-aaf-tip-pza       to   fil-tip-pza            .
           move      rf-aaf-lot-acq       to   fil-lot-acq            .
           move      rf-aaf-tap-pes       to   fil-tap-pes            .
           move      rf-aaf-uda-pes       to   fil-uda-pes            .
           move      rf-aaf-lgv-vlt       to   fil-lgv-vlt            .
           move      rf-aaf-lgv-dcv       to   fil-lgv-dcv            .
           move      rf-aaf-lgv-tdc       to   fil-lgv-tdc            .
           move      rf-aaf-lgv-cdc       to   fil-lgv-cdc            .
           move      rf-aaf-lgv-pdt       to   fil-lgv-pdt            .
           move      rf-aaf-qta-pes (1)   to   fil-qta-pes (1)        .
           move      rf-aaf-qta-pes (2)   to   fil-qta-pes (2)        .
           move      rf-aaf-qta-pes (3)   to   fil-qta-pes (3)        .
           move      rf-aaf-qta-pes (4)   to   fil-qta-pes (4)        .
           move      rf-aaf-qta-pes (5)   to   fil-qta-pes (5)        .
           move      rf-aaf-qta-pes (6)   to   fil-qta-pes (6)        .
           move      rf-aaf-prz-pes (1)   to   fil-prz-pes (1)        .
           move      rf-aaf-prz-pes (2)   to   fil-prz-pes (2)        .
           move      rf-aaf-prz-pes (3)   to   fil-prz-pes (3)        .
           move      rf-aaf-prz-pes (4)   to   fil-prz-pes (4)        .
           move      rf-aaf-prz-pes (5)   to   fil-prz-pes (5)        .
           move      rf-aaf-prz-pes (6)   to   fil-prz-pes (6)        .
           move      rf-aaf-csr-pes (1)   to   fil-csr-pes (1)        .
           move      rf-aaf-csr-pes (2)   to   fil-csr-pes (2)        .
           move      rf-aaf-csr-pes (3)   to   fil-csr-pes (3)        .
           move      rf-aaf-csr-pes (4)   to   fil-csr-pes (4)        .
           move      rf-aaf-csr-pes (5)   to   fil-csr-pes (5)        .
           move      rf-aaf-csr-pes (6)   to   fil-csr-pes (6)        .
           move      rf-aaf-psr-pes (1, 1)
                                          to   fil-psr-pes (1, 1)     .
           move      rf-aaf-psr-pes (1, 2)
                                          to   fil-psr-pes (1, 2)     .
           move      rf-aaf-psr-pes (1, 3)
                                          to   fil-psr-pes (1, 3)     .
           move      rf-aaf-psr-pes (1, 4)
                                          to   fil-psr-pes (1, 4)     .
           move      rf-aaf-psr-pes (1, 5)
                                          to   fil-psr-pes (1, 5)     .
           move      rf-aaf-psr-pes (2, 1)
                                          to   fil-psr-pes (2, 1)     .
           move      rf-aaf-psr-pes (2, 2)
                                          to   fil-psr-pes (2, 2)     .
           move      rf-aaf-psr-pes (2, 3)
                                          to   fil-psr-pes (2, 3)     .
           move      rf-aaf-psr-pes (2, 4)
                                          to   fil-psr-pes (2, 4)     .
           move      rf-aaf-psr-pes (2, 5)
                                          to   fil-psr-pes (2, 5)     .
           move      rf-aaf-psr-pes (3, 1)
                                          to   fil-psr-pes (3, 1)     .
           move      rf-aaf-psr-pes (3, 2)
                                          to   fil-psr-pes (3, 2)     .
           move      rf-aaf-psr-pes (3, 3)
                                          to   fil-psr-pes (3, 3)     .
           move      rf-aaf-psr-pes (3, 4)
                                          to   fil-psr-pes (3, 4)     .
           move      rf-aaf-psr-pes (3, 5)
                                          to   fil-psr-pes (3, 5)     .
           move      rf-aaf-psr-pes (4, 1)
                                          to   fil-psr-pes (4, 1)     .
           move      rf-aaf-psr-pes (4, 2)
                                          to   fil-psr-pes (4, 2)     .
           move      rf-aaf-psr-pes (4, 3)
                                          to   fil-psr-pes (4, 3)     .
           move      rf-aaf-psr-pes (4, 4)
                                          to   fil-psr-pes (4, 4)     .
           move      rf-aaf-psr-pes (4, 5)
                                          to   fil-psr-pes (4, 5)     .
           move      rf-aaf-psr-pes (5, 1)
                                          to   fil-psr-pes (5, 1)     .
           move      rf-aaf-psr-pes (5, 2)
                                          to   fil-psr-pes (5, 2)     .
           move      rf-aaf-psr-pes (5, 3)
                                          to   fil-psr-pes (5, 3)     .
           move      rf-aaf-psr-pes (5, 4)
                                          to   fil-psr-pes (5, 4)     .
           move      rf-aaf-psr-pes (5, 5)
                                          to   fil-psr-pes (5, 5)     .
           move      rf-aaf-psr-pes (6, 1)
                                          to   fil-psr-pes (6, 1)     .
           move      rf-aaf-psr-pes (6, 2)
                                          to   fil-psr-pes (6, 2)     .
           move      rf-aaf-psr-pes (6, 3)
                                          to   fil-psr-pes (6, 3)     .
           move      rf-aaf-psr-pes (6, 4)
                                          to   fil-psr-pes (6, 4)     .
           move      rf-aaf-psr-pes (6, 5)
                                          to   fil-psr-pes (6, 5)     .
           move      rf-aaf-per-mpa       to   fil-per-mpa            .
           move      rf-aaf-rif-lst       to   fil-rif-lst            .
           move      rf-aaf-per-ric       to   fil-per-ric            .
           move      rf-aaf-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-aaf-tip-mag       to   fil-tip-mag            .
           move      rf-aaf-num-pro       to   fil-num-pro            .
           move      rf-aaf-cod-dcf       to   fil-cod-dcf            .
           move      rf-aaf-fda-pif       to   fil-fda-pif            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-aaf-ide-dat       to   fil-ide-dat            .
           move      rf-aaf-tip-mag       to   fil-tip-mag-2          .
           move      rf-aaf-num-pro       to   fil-num-pro-2          .
           move      rf-aaf-cod-dcf       to   fil-cod-dcf-2          .
           move      rf-aaf-fda-pif       to   fil-fda-pif-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-aaf-cod-dcf       to   fil-cod-dcf-3          .
           move      rf-aaf-tip-mag       to   fil-tip-mag-3          .
           move      rf-aaf-num-pro       to   fil-num-pro-3          .
           move      rf-aaf-fda-pif       to   fil-fda-pif-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-aaf-tip-mag       to   fil-tip-mag-4          .
           move      rf-aaf-num-pro       to   fil-num-pro-4          .
           move      rf-aaf-fda-pif       to   fil-fda-pif-4          .
           move      rf-aaf-cod-dcf       to   fil-cod-dcf-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-aaf-cod-dcf       to   fil-cod-dcf-5          .
           move      rf-aaf-cop-sfn       to   fil-cop-sfn            .
           move      rf-aaf-tip-mag       to   fil-tip-mag-5          .
           move      rf-aaf-num-pro       to   fil-num-pro-5          .
           move      rf-aaf-fda-pif       to   fil-fda-pif-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-600.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 6                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k06                .
           move      rf-aaf-cop-sfn       to   fil-cop-sfn-6          .
           move      rf-aaf-tip-mag       to   fil-tip-mag-6          .
           move      rf-aaf-cod-dcf       to   fil-cod-dcf-6          .
           move      rf-aaf-num-pro       to   fil-num-pro-6          .
           move      rf-aaf-fda-pif       to   fil-fda-pif-6          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-aaf                 .
           move      fil-ide-dat          to   rf-aaf-ide-dat         .
           move      fil-ide-ute          to   rf-aaf-ide-ute         .
           move      fil-ide-fas          to   rf-aaf-ide-fas         .
           move      fil-tip-mag          to   rf-aaf-tip-mag         .
           move      fil-num-pro          to   rf-aaf-num-pro         .
           move      fil-cod-dcf          to   rf-aaf-cod-dcf         .
           move      fil-fda-pif          to   rf-aaf-fda-pif         .
           move      fil-cop-sfn          to   rf-aaf-cop-sfn         .
           move      fil-dep-sfn          to   rf-aaf-dep-sfn         .
           move      fil-xdp-sfn          to   rf-aaf-xdp-sfn         .
           move      fil-snx-tum          to   rf-aaf-snx-tum         .
           move      fil-umf-tum          to   rf-aaf-umf-tum         .
           move      fil-nde-tum          to   rf-aaf-nde-tum         .
           move      fil-cmo-tum          to   rf-aaf-cmo-tum         .
           move      fil-cdi-tum          to   rf-aaf-cdi-tum         .
           move      fil-dpz-dcf          to   rf-aaf-dpz-dcf         .
           move      fil-ann-not          to   rf-aaf-ann-not         .
           move      fil-tmp-cns          to   rf-aaf-tmp-cns         .
           move      fil-sgl-vlt          to   rf-aaf-sgl-vlt         .
           move      fil-dec-vlt          to   rf-aaf-dec-vlt         .
           move      fil-dec-prz          to   rf-aaf-dec-prz         .
           move      fil-tip-pza          to   rf-aaf-tip-pza         .
           move      fil-lot-acq          to   rf-aaf-lot-acq         .
           move      fil-tap-pes          to   rf-aaf-tap-pes         .
           move      fil-uda-pes          to   rf-aaf-uda-pes         .
           move      fil-lgv-vlt          to   rf-aaf-lgv-vlt         .
           move      fil-lgv-dcv          to   rf-aaf-lgv-dcv         .
           move      fil-lgv-tdc          to   rf-aaf-lgv-tdc         .
           move      fil-lgv-cdc          to   rf-aaf-lgv-cdc         .
           move      fil-lgv-pdt          to   rf-aaf-lgv-pdt         .
           move      fil-qta-pes (1)      to   rf-aaf-qta-pes (1)     .
           move      fil-qta-pes (2)      to   rf-aaf-qta-pes (2)     .
           move      fil-qta-pes (3)      to   rf-aaf-qta-pes (3)     .
           move      fil-qta-pes (4)      to   rf-aaf-qta-pes (4)     .
           move      fil-qta-pes (5)      to   rf-aaf-qta-pes (5)     .
           move      fil-qta-pes (6)      to   rf-aaf-qta-pes (6)     .
           move      fil-prz-pes (1)      to   rf-aaf-prz-pes (1)     .
           move      fil-prz-pes (2)      to   rf-aaf-prz-pes (2)     .
           move      fil-prz-pes (3)      to   rf-aaf-prz-pes (3)     .
           move      fil-prz-pes (4)      to   rf-aaf-prz-pes (4)     .
           move      fil-prz-pes (5)      to   rf-aaf-prz-pes (5)     .
           move      fil-prz-pes (6)      to   rf-aaf-prz-pes (6)     .
           move      fil-csr-pes (1)      to   rf-aaf-csr-pes (1)     .
           move      fil-csr-pes (2)      to   rf-aaf-csr-pes (2)     .
           move      fil-csr-pes (3)      to   rf-aaf-csr-pes (3)     .
           move      fil-csr-pes (4)      to   rf-aaf-csr-pes (4)     .
           move      fil-csr-pes (5)      to   rf-aaf-csr-pes (5)     .
           move      fil-csr-pes (6)      to   rf-aaf-csr-pes (6)     .
           move      fil-psr-pes (1, 1)   to   rf-aaf-psr-pes (1, 1)  .
           move      fil-psr-pes (1, 2)   to   rf-aaf-psr-pes (1, 2)  .
           move      fil-psr-pes (1, 3)   to   rf-aaf-psr-pes (1, 3)  .
           move      fil-psr-pes (1, 4)   to   rf-aaf-psr-pes (1, 4)  .
           move      fil-psr-pes (1, 5)   to   rf-aaf-psr-pes (1, 5)  .
           move      fil-psr-pes (2, 1)   to   rf-aaf-psr-pes (2, 1)  .
           move      fil-psr-pes (2, 2)   to   rf-aaf-psr-pes (2, 2)  .
           move      fil-psr-pes (2, 3)   to   rf-aaf-psr-pes (2, 3)  .
           move      fil-psr-pes (2, 4)   to   rf-aaf-psr-pes (2, 4)  .
           move      fil-psr-pes (2, 5)   to   rf-aaf-psr-pes (2, 5)  .
           move      fil-psr-pes (3, 1)   to   rf-aaf-psr-pes (3, 1)  .
           move      fil-psr-pes (3, 2)   to   rf-aaf-psr-pes (3, 2)  .
           move      fil-psr-pes (3, 3)   to   rf-aaf-psr-pes (3, 3)  .
           move      fil-psr-pes (3, 4)   to   rf-aaf-psr-pes (3, 4)  .
           move      fil-psr-pes (3, 5)   to   rf-aaf-psr-pes (3, 5)  .
           move      fil-psr-pes (4, 1)   to   rf-aaf-psr-pes (4, 1)  .
           move      fil-psr-pes (4, 2)   to   rf-aaf-psr-pes (4, 2)  .
           move      fil-psr-pes (4, 3)   to   rf-aaf-psr-pes (4, 3)  .
           move      fil-psr-pes (4, 4)   to   rf-aaf-psr-pes (4, 4)  .
           move      fil-psr-pes (4, 5)   to   rf-aaf-psr-pes (4, 5)  .
           move      fil-psr-pes (5, 1)   to   rf-aaf-psr-pes (5, 1)  .
           move      fil-psr-pes (5, 2)   to   rf-aaf-psr-pes (5, 2)  .
           move      fil-psr-pes (5, 3)   to   rf-aaf-psr-pes (5, 3)  .
           move      fil-psr-pes (5, 4)   to   rf-aaf-psr-pes (5, 4)  .
           move      fil-psr-pes (5, 5)   to   rf-aaf-psr-pes (5, 5)  .
           move      fil-psr-pes (6, 1)   to   rf-aaf-psr-pes (6, 1)  .
           move      fil-psr-pes (6, 2)   to   rf-aaf-psr-pes (6, 2)  .
           move      fil-psr-pes (6, 3)   to   rf-aaf-psr-pes (6, 3)  .
           move      fil-psr-pes (6, 4)   to   rf-aaf-psr-pes (6, 4)  .
           move      fil-psr-pes (6, 5)   to   rf-aaf-psr-pes (6, 5)  .
           move      fil-per-mpa          to   rf-aaf-per-mpa         .
           move      fil-rif-lst          to   rf-aaf-rif-lst         .
           move      fil-per-ric          to   rf-aaf-per-ric         .
           move      fil-alx-exp          to   rf-aaf-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-aaf               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-aaf
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

