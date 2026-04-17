       Identification Division.
       Program-Id.                                 iofsff             .
      *================================================================*
      *                                                                *
      *                  Input-Output File sff                         *
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
      *            * Chiave numero 01 : NUMFTF                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-ftf        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-ftf-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNTDTR                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-fnt        pic  9(07)       comp-3     .
                   15  fil-dtr-ftf        pic  9(07)       comp-3     .
                   15  fil-num-ftf-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-ddo-ftf        pic  9(07)       comp-3     .
                   15  fil-ndo-ftf        pic  x(10)                  .
                   15  fil-num-ftf-4      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-tip-ftf            pic  9(02)                  .
               10  fil-tpv-ftf            pic  9(02)                  .
               10  fil-dpz-fnt            pic  x(04)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-tdc-vlt            pic  x(01)                  .
               10  fil-cdc-ftf            pic  9(06)v9(05) comp-3     .
               10  fil-iiv-ftf            pic s9(11)       comp-3     .
               10  fil-imp-ftf            pic s9(11)       comp-3     .
               10  fil-inl-pgf            pic  9(02)                  .
               10  fil-ctp-acq            pic  9(07)       comp-3     .
               10  fil-cod-fop            pic  9(07)       comp-3     .
               10  fil-nsb-ftf            pic  x(10)                  .
               10  fil-abi-ftf            pic  9(05)       comp-3     .
               10  fil-cab-ftf            pic  9(05)       comp-3     .
               10  fil-ccc-ftf            pic  x(12)                  .
               10  fil-ccp-ftf            pic  x(12)                  .
               10  fil-bef-ftf            pic  9(05)       comp-3     .
               10  fil-pag-dsm            pic  9(07)       comp-3     .
               10  fil-pag-qaf            pic  9(09)       comp-3     .
               10  fil-pag-act            pic  9(09)       comp-3     .
               10  fil-des-cau.
                   15  fil-rig-cau occurs 04
                                          pic  x(40)                  .
               10  fil-num-giv            pic  9(02)                  .
               10  fil-npt-iva            pic  9(07)       comp-3     .
               10  fil-iva-cst.
                   15  fil-iva-rig occurs 06.
                       20  fil-iva-cod    pic  9(05)       comp-3     .
                       20  fil-iva-ibv    pic s9(11)       comp-3     .
                       20  fil-iva-ibl    pic s9(11)       comp-3     .
                       20  fil-iva-imp    pic s9(11)       comp-3     .
                   15  fil-iva-tdo        pic s9(11)       comp-3     .
               10  fil-drg-cge            pic  9(07)       comp-3     .
               10  fil-npr-cge            pic  9(07)       comp-3     .
               10  fil-nrg-cge            pic  9(02)                  .
               10  fil-npr-scf            pic  9(11)       comp-3     .
               10  fil-nrg-scf            pic  9(02)                  .
               10  fil-prt-ffo            pic  9(11)       comp-3     .
               10  fil-fcs-uno            pic  9(02)                  .
               10  fil-fcs-due            pic  9(02)                  .
               10  fil-fcs-tre            pic  9(02)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-flg-pul            pic  x(01)                  .
               10  fil-snx-rda            pic  x(01)                  .
               10  fil-prt-rda            pic  9(09)                  .
               10  fil-flg-rfp            pic  x(01)                  .
               10  fil-flg-sdi            pic  x(01)                  .
               10  fil-dri-mgd            pic  9(07)                  .
               10  fil-nri-mgd            pic  x(10)                  .
               10  fil-flg-mos            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 170 pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMFTF                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-ftf        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-ftf-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNTDTR                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-fnt        pic  9(07)       comp-3     .
                   15  pul-dtr-ftf        pic  9(07)       comp-3     .
                   15  pul-num-ftf-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-ddo-ftf        pic  9(07)       comp-3     .
                   15  pul-ndo-ftf        pic  x(10)                  .
                   15  pul-num-ftf-4      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-tip-ftf            pic  9(02)                  .
               10  pul-tpv-ftf            pic  9(02)                  .
               10  pul-dpz-fnt            pic  x(04)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-tdc-vlt            pic  x(01)                  .
               10  pul-cdc-ftf            pic  9(06)v9(05) comp-3     .
               10  pul-iiv-ftf            pic s9(11)       comp-3     .
               10  pul-imp-ftf            pic s9(11)       comp-3     .
               10  pul-inl-pgf            pic  9(02)                  .
               10  pul-ctp-acq            pic  9(07)       comp-3     .
               10  pul-cod-fop            pic  9(07)       comp-3     .
               10  pul-nsb-ftf            pic  x(10)                  .
               10  pul-abi-ftf            pic  9(05)       comp-3     .
               10  pul-cab-ftf            pic  9(05)       comp-3     .
               10  pul-ccc-ftf            pic  x(12)                  .
               10  pul-ccp-ftf            pic  x(12)                  .
               10  pul-bef-ftf            pic  9(05)       comp-3     .
               10  pul-pag-dsm            pic  9(07)       comp-3     .
               10  pul-pag-qaf            pic  9(09)       comp-3     .
               10  pul-pag-act            pic  9(09)       comp-3     .
               10  pul-des-cau.
                   15  pul-rig-cau occurs 04
                                          pic  x(40)                  .
               10  pul-num-giv            pic  9(02)                  .
               10  pul-npt-iva            pic  9(07)       comp-3     .
               10  pul-iva-cst.
                   15  pul-iva-rig occurs 06.
                       20  pul-iva-cod    pic  9(05)       comp-3     .
                       20  pul-iva-ibv    pic s9(11)       comp-3     .
                       20  pul-iva-ibl    pic s9(11)       comp-3     .
                       20  pul-iva-imp    pic s9(11)       comp-3     .
                   15  pul-iva-tdo        pic s9(11)       comp-3     .
               10  pul-drg-cge            pic  9(07)       comp-3     .
               10  pul-npr-cge            pic  9(07)       comp-3     .
               10  pul-nrg-cge            pic  9(02)                  .
               10  pul-npr-scf            pic  9(11)       comp-3     .
               10  pul-nrg-scf            pic  9(02)                  .
               10  pul-prt-ffo            pic  9(11)       comp-3     .
               10  pul-fcs-uno            pic  9(02)                  .
               10  pul-fcs-due            pic  9(02)                  .
               10  pul-fcs-tre            pic  9(02)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-flg-pul            pic  x(01)                  .
               10  pul-snx-rda            pic  x(01)                  .
               10  pul-prt-rda            pic  9(09)                  .
               10  pul-flg-rfp            pic  x(01)                  .
               10  pul-flg-sdi            pic  x(01)                  .
               10  pul-dri-mgd            pic  9(07)                  .
               10  pul-nri-mgd            pic  x(10)                  .
               10  pul-flg-mos            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 170 pic  x(01)                  .

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
                     "sff "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/scf/fls/ioc/obj/iofsff              "       .

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
           05  k-ctr                      pic  9(02) value 4          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMFTF    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "FNTDTR    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "IDEDOC    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    4      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  w-c01                      pic  9(02)                  .
           05  w-c02                      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [sff]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsff"                          .

      ******************************************************************
       Procedure Division                using f rf-sff               .
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
           move      spaces               to   rf-sff                 .
           move      zero                 to   rf-sff-ide-dat         .
           move      spaces               to   rf-sff-ide-ute         .
           move      spaces               to   rf-sff-ide-fas         .
           move      zero                 to   rf-sff-dtr-ftf         .
           move      zero                 to   rf-sff-num-ftf         .
           move      zero                 to   rf-sff-tip-ftf         .
           move      zero                 to   rf-sff-tpv-ftf         .
           move      zero                 to   rf-sff-cod-fnt         .
           move      spaces               to   rf-sff-dpz-fnt         .
           move      zero                 to   rf-sff-ddo-ftf         .
           move      spaces               to   rf-sff-ndo-ftf         .
           move      spaces               to   rf-sff-sgl-vlt         .
           move      zero                 to   rf-sff-dec-vlt         .
           move      spaces               to   rf-sff-tdc-vlt         .
           move      zero                 to   rf-sff-cdc-ftf         .
           move      zero                 to   rf-sff-iiv-ftf         .
           move      zero                 to   rf-sff-imp-ftf         .
           move      zero                 to   rf-sff-inl-pgf         .
           move      zero                 to   rf-sff-ctp-acq         .
           move      zero                 to   rf-sff-cod-fop         .
           move      spaces               to   rf-sff-nsb-ftf         .
           move      zero                 to   rf-sff-abi-ftf         .
           move      zero                 to   rf-sff-cab-ftf         .
           move      spaces               to   rf-sff-ccc-ftf         .
           move      spaces               to   rf-sff-ccp-ftf         .
           move      zero                 to   rf-sff-bef-ftf         .
           move      zero                 to   rf-sff-pag-dsm         .
           move      zero                 to   rf-sff-pag-qaf         .
           move      zero                 to   rf-sff-pag-act         .
           move      spaces               to   rf-sff-des-cau         .
           move      zero                 to   rf-sff-num-giv         .
           move      zero                 to   rf-sff-npt-iva         .
           move      zero                 to   w-c01                  .
       nor-rec-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to nor-rec-log-120.
           move      zero                 to   rf-sff-iva-cod (w-c01) .
           move      zero                 to   rf-sff-iva-ibv (w-c01) .
           move      zero                 to   rf-sff-iva-ibl (w-c01) .
           move      zero                 to   rf-sff-iva-imp (w-c01) .
           go to     nor-rec-log-100.
       nor-rec-log-120.
           move      zero                 to   rf-sff-iva-tdo         .
           move      zero                 to   rf-sff-drg-cge         .
           move      zero                 to   rf-sff-npr-cge         .
           move      zero                 to   rf-sff-nrg-cge         .
           move      zero                 to   rf-sff-npr-scf         .
           move      zero                 to   rf-sff-nrg-scf         .
           move      zero                 to   rf-sff-prt-ffo         .
           move      zero                 to   rf-sff-fcs-uno         .
           move      zero                 to   rf-sff-fcs-due         .
           move      zero                 to   rf-sff-fcs-tre         .
           move      spaces               to   rf-sff-flg-ela         .
           move      spaces               to   rf-sff-flg-pul         .
           move      spaces               to   rf-sff-snx-rda         .
           move      zero                 to   rf-sff-prt-rda         .
           move      spaces               to   rf-sff-flg-rfp         .
           move      spaces               to   rf-sff-flg-sdi         .
           move      zero                 to   rf-sff-dri-mgd         .
           move      spaces               to   rf-sff-nri-mgd         .
           move      spaces               to   rf-sff-flg-mos         .
           move      spaces               to   rf-sff-alx-exp         .
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
           move      rf-sff-ide-ute       to   fil-ide-ute            .
           move      rf-sff-ide-fas       to   fil-ide-fas            .
           move      rf-sff-tip-ftf       to   fil-tip-ftf            .
           move      rf-sff-tpv-ftf       to   fil-tpv-ftf            .
           move      rf-sff-dpz-fnt       to   fil-dpz-fnt            .
           move      rf-sff-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-sff-dec-vlt       to   fil-dec-vlt            .
           move      rf-sff-tdc-vlt       to   fil-tdc-vlt            .
           move      rf-sff-cdc-ftf       to   fil-cdc-ftf            .
           move      rf-sff-iiv-ftf       to   fil-iiv-ftf            .
           move      rf-sff-imp-ftf       to   fil-imp-ftf            .
           move      rf-sff-inl-pgf       to   fil-inl-pgf            .
           move      rf-sff-ctp-acq       to   fil-ctp-acq            .
           move      rf-sff-cod-fop       to   fil-cod-fop            .
           move      rf-sff-nsb-ftf       to   fil-nsb-ftf            .
           move      rf-sff-abi-ftf       to   fil-abi-ftf            .
           move      rf-sff-cab-ftf       to   fil-cab-ftf            .
           move      rf-sff-ccc-ftf       to   fil-ccc-ftf            .
           move      rf-sff-ccp-ftf       to   fil-ccp-ftf            .
           move      rf-sff-bef-ftf       to   fil-bef-ftf            .
           move      rf-sff-pag-dsm       to   fil-pag-dsm            .
           move      rf-sff-pag-qaf       to   fil-pag-qaf            .
           move      rf-sff-pag-act       to   fil-pag-act            .
           move      rf-sff-des-cau       to   fil-des-cau            .
           move      rf-sff-num-giv       to   fil-num-giv            .
           move      rf-sff-npt-iva       to   fil-npt-iva            .
           move      zero                 to   w-c01                  .
       cmp-log-fis-120.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to cmp-log-fis-140.
           move      rf-sff-iva-cod (w-c01)
                                          to   fil-iva-cod (w-c01)    .
           move      rf-sff-iva-ibv (w-c01)
                                          to   fil-iva-ibv (w-c01)    .
           move      rf-sff-iva-ibl (w-c01)
                                          to   fil-iva-ibl (w-c01)    .
           move      rf-sff-iva-imp (w-c01)
                                          to   fil-iva-imp (w-c01)    .
           go to     cmp-log-fis-120.
       cmp-log-fis-140.
           move      rf-sff-iva-tdo       to   fil-iva-tdo            .
           move      rf-sff-drg-cge       to   fil-drg-cge            .
           move      rf-sff-npr-cge       to   fil-npr-cge            .
           move      rf-sff-nrg-cge       to   fil-nrg-cge            .
           move      rf-sff-npr-scf       to   fil-npr-scf            .
           move      rf-sff-nrg-scf       to   fil-nrg-scf            .
           move      rf-sff-prt-ffo       to   fil-prt-ffo            .
           move      rf-sff-fcs-uno       to   fil-fcs-uno            .
           move      rf-sff-fcs-due       to   fil-fcs-due            .
           move      rf-sff-fcs-tre       to   fil-fcs-tre            .
           move      rf-sff-flg-ela       to   fil-flg-ela            .
           move      rf-sff-flg-pul       to   fil-flg-pul            .
           move      rf-sff-snx-rda       to   fil-snx-rda            .
           move      rf-sff-prt-rda       to   fil-prt-rda            .
           move      rf-sff-flg-rfp       to   fil-flg-rfp            .
           move      rf-sff-flg-sdi       to   fil-flg-sdi            .
           move      rf-sff-dri-mgd       to   fil-dri-mgd            .
           move      rf-sff-nri-mgd       to   fil-nri-mgd            .
           move      rf-sff-flg-mos       to   fil-flg-mos            .
           move      rf-sff-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-sff-num-ftf       to   fil-num-ftf            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-sff-ide-dat       to   fil-ide-dat            .
           move      rf-sff-num-ftf       to   fil-num-ftf-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-sff-cod-fnt       to   fil-cod-fnt            .
           move      rf-sff-dtr-ftf       to   fil-dtr-ftf            .
           move      rf-sff-num-ftf       to   fil-num-ftf-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-sff-ddo-ftf       to   fil-ddo-ftf            .
           move      rf-sff-ndo-ftf       to   fil-ndo-ftf            .
           move      rf-sff-num-ftf       to   fil-num-ftf-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-sff                 .
           move      fil-ide-dat          to   rf-sff-ide-dat         .
           move      fil-ide-ute          to   rf-sff-ide-ute         .
           move      fil-ide-fas          to   rf-sff-ide-fas         .
           move      fil-dtr-ftf          to   rf-sff-dtr-ftf         .
           move      fil-num-ftf          to   rf-sff-num-ftf         .
           move      fil-tip-ftf          to   rf-sff-tip-ftf         .
           move      fil-tpv-ftf          to   rf-sff-tpv-ftf         .
           move      fil-cod-fnt          to   rf-sff-cod-fnt         .
           move      fil-dpz-fnt          to   rf-sff-dpz-fnt         .
           move      fil-ddo-ftf          to   rf-sff-ddo-ftf         .
           move      fil-ndo-ftf          to   rf-sff-ndo-ftf         .
           move      fil-sgl-vlt          to   rf-sff-sgl-vlt         .
           move      fil-dec-vlt          to   rf-sff-dec-vlt         .
           move      fil-tdc-vlt          to   rf-sff-tdc-vlt         .
           move      fil-cdc-ftf          to   rf-sff-cdc-ftf         .
           move      fil-iiv-ftf          to   rf-sff-iiv-ftf         .
           move      fil-imp-ftf          to   rf-sff-imp-ftf         .
           move      fil-inl-pgf          to   rf-sff-inl-pgf         .
           move      fil-ctp-acq          to   rf-sff-ctp-acq         .
           move      fil-cod-fop          to   rf-sff-cod-fop         .
           move      fil-nsb-ftf          to   rf-sff-nsb-ftf         .
           move      fil-abi-ftf          to   rf-sff-abi-ftf         .
           move      fil-cab-ftf          to   rf-sff-cab-ftf         .
           move      fil-ccc-ftf          to   rf-sff-ccc-ftf         .
           move      fil-ccp-ftf          to   rf-sff-ccp-ftf         .
           move      fil-bef-ftf          to   rf-sff-bef-ftf         .
           move      fil-pag-dsm          to   rf-sff-pag-dsm         .
           move      fil-pag-qaf          to   rf-sff-pag-qaf         .
           move      fil-pag-act          to   rf-sff-pag-act         .
           move      fil-des-cau          to   rf-sff-des-cau         .
           move      fil-num-giv          to   rf-sff-num-giv         .
           move      fil-npt-iva          to   rf-sff-npt-iva         .
           move      zero                 to   w-c01                  .
       dec-fis-log-100.
           add       1                    to   w-c01                  .
           if        w-c01                >    6
                     go to dec-fis-log-120.
           move      fil-iva-cod (w-c01)  to   rf-sff-iva-cod (w-c01) .
           move      fil-iva-ibv (w-c01)  to   rf-sff-iva-ibv (w-c01) .
           move      fil-iva-ibl (w-c01)  to   rf-sff-iva-ibl (w-c01) .
           move      fil-iva-imp (w-c01)  to   rf-sff-iva-imp (w-c01) .
           go to     dec-fis-log-100.
       dec-fis-log-120.
           move      fil-iva-tdo          to   rf-sff-iva-tdo         .
           move      fil-drg-cge          to   rf-sff-drg-cge         .
           move      fil-npr-cge          to   rf-sff-npr-cge         .
           move      fil-nrg-cge          to   rf-sff-nrg-cge         .
           move      fil-npr-scf          to   rf-sff-npr-scf         .
           move      fil-nrg-scf          to   rf-sff-nrg-scf         .
           move      fil-prt-ffo          to   rf-sff-prt-ffo         .
           move      fil-fcs-uno          to   rf-sff-fcs-uno         .
           move      fil-fcs-due          to   rf-sff-fcs-due         .
           move      fil-fcs-tre          to   rf-sff-fcs-tre         .
           move      fil-flg-ela          to   rf-sff-flg-ela         .
           move      fil-flg-pul          to   rf-sff-flg-pul         .
           move      fil-snx-rda          to   rf-sff-snx-rda         .
           move      fil-prt-rda          to   rf-sff-prt-rda         .
           move      fil-flg-rfp          to   rf-sff-flg-rfp         .
           move      fil-flg-sdi          to   rf-sff-flg-sdi         .
           move      fil-dri-mgd          to   rf-sff-dri-mgd         .
           move      fil-nri-mgd          to   rf-sff-nri-mgd         .
           move      fil-flg-mos          to   rf-sff-flg-mos         .
           move      fil-alx-exp          to   rf-sff-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-sff               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-sff
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
