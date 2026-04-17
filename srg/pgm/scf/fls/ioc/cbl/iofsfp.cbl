       Identification Division.
       Program-Id.                                 iofsfp             .
      *================================================================*
      *                                                                *
      *                  Input-Output File sfp                         *
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
      *            * Chiave numero 01 : NUMPGF                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-pgf        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-pgf-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNTPGF                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-cod-fnt        pic  9(07)       comp-3     .
                   15  fil-num-pgf-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ADPPGF                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-num-adp        pic  9(11)       comp-3     .
                   15  fil-num-pgf-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CBPADP                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cbp-pgf        pic  x(10)                  .
                   15  fil-num-adp-5      pic  9(11)       comp-3     .
                   15  fil-num-pgf-5      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dtr-pgf            pic  9(07)       comp-3     .
               10  fil-drc-pgf            pic  9(07)       comp-3     .
               10  fil-npc-pgf            pic  9(07)       comp-3     .
               10  fil-mod-pgf            pic  9(02)                  .
               10  fil-tvl-pgf            pic  9(02)                  .
               10  fil-dpz-fnt            pic  x(04)                  .
               10  fil-inl-pgf            pic  9(02)                  .
               10  fil-tip-pgf            pic  9(02)                  .
               10  fil-abi-pgf            pic  9(05)       comp-3     .
               10  fil-cab-pgf            pic  9(05)       comp-3     .
               10  fil-bef-pgf            pic  9(05)       comp-3     .
               10  fil-ccc-pgf            pic  x(12)                  .
               10  fil-sgl-vlt            pic  x(03)                  .
               10  fil-dec-vlt            pic  9(01)                  .
               10  fil-tdc-vlt            pic  x(01)                  .
               10  fil-cdc-pgf            pic  9(06)v9(05) comp-3     .
               10  fil-iiv-pgf            pic s9(11)       comp-3     .
               10  fil-imp-pgf            pic s9(11)       comp-3     .
               10  fil-spe-pgf            pic s9(11)       comp-3     .
               10  fil-ddo-pgf            pic  9(07)       comp-3     .
               10  fil-ndo-pgf            pic  x(10)                  .
               10  fil-aos-pgf            pic  9(02)                  .
               10  fil-rns-pgf            pic  9(02)                  .
               10  fil-nns-pgf            pic  9(11)       comp-3     .
               10  fil-tns-pgf            pic  9(02)                  .
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
               10  fil-dvf-bfc            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler  occurs 13  pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMPGF                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-pgf        pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-pgf-2      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : FNTPGF                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-cod-fnt        pic  9(07)       comp-3     .
                   15  pul-num-pgf-3      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : ADPPGF                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-num-adp        pic  9(11)       comp-3     .
                   15  pul-num-pgf-4      pic  9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : CBPADP                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cbp-pgf        pic  x(10)                  .
                   15  pul-num-adp-5      pic  9(11)       comp-3     .
                   15  pul-num-pgf-5      pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dtr-pgf            pic  9(07)       comp-3     .
               10  pul-drc-pgf            pic  9(07)       comp-3     .
               10  pul-npc-pgf            pic  9(07)       comp-3     .
               10  pul-mod-pgf            pic  9(02)                  .
               10  pul-tvl-pgf            pic  9(02)                  .
               10  pul-dpz-fnt            pic  x(04)                  .
               10  pul-inl-pgf            pic  9(02)                  .
               10  pul-tip-pgf            pic  9(02)                  .
               10  pul-abi-pgf            pic  9(05)       comp-3     .
               10  pul-cab-pgf            pic  9(05)       comp-3     .
               10  pul-bef-pgf            pic  9(05)       comp-3     .
               10  pul-ccc-pgf            pic  x(12)                  .
               10  pul-sgl-vlt            pic  x(03)                  .
               10  pul-dec-vlt            pic  9(01)                  .
               10  pul-tdc-vlt            pic  x(01)                  .
               10  pul-cdc-pgf            pic  9(06)v9(05) comp-3     .
               10  pul-iiv-pgf            pic s9(11)       comp-3     .
               10  pul-imp-pgf            pic s9(11)       comp-3     .
               10  pul-spe-pgf            pic s9(11)       comp-3     .
               10  pul-ddo-pgf            pic  9(07)       comp-3     .
               10  pul-ndo-pgf            pic  x(10)                  .
               10  pul-aos-pgf            pic  9(02)                  .
               10  pul-rns-pgf            pic  9(02)                  .
               10  pul-nns-pgf            pic  9(11)       comp-3     .
               10  pul-tns-pgf            pic  9(02)                  .
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
               10  pul-dvf-bfc            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler  occurs 13  pic  x(01)                  .

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
                     "sfp "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/scf/fls/ioc/obj/iofsfp              "       .

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
           05  k-ctr                      pic  9(02) value 5          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMPGF    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "FNTPGF    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ADPPGF    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CBPADP    "                              .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    5      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [sfp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .

      ******************************************************************
       Procedure Division                using f rf-sfp               .
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
           move      spaces               to   rf-sfp                 .
           move      zero                 to   rf-sfp-ide-dat         .
           move      spaces               to   rf-sfp-ide-ute         .
           move      spaces               to   rf-sfp-ide-fas         .
           move      zero                 to   rf-sfp-dtr-pgf         .
           move      zero                 to   rf-sfp-drc-pgf         .
           move      zero                 to   rf-sfp-npc-pgf         .
           move      zero                 to   rf-sfp-num-pgf         .
           move      zero                 to   rf-sfp-mod-pgf         .
           move      zero                 to   rf-sfp-tvl-pgf         .
           move      zero                 to   rf-sfp-cod-fnt         .
           move      spaces               to   rf-sfp-dpz-fnt         .
           move      zero                 to   rf-sfp-inl-pgf         .
           move      spaces               to   rf-sfp-cbp-pgf         .
           move      zero                 to   rf-sfp-tip-pgf         .
           move      zero                 to   rf-sfp-abi-pgf         .
           move      zero                 to   rf-sfp-cab-pgf         .
           move      zero                 to   rf-sfp-bef-pgf         .
           move      spaces               to   rf-sfp-ccc-pgf         .
           move      spaces               to   rf-sfp-sgl-vlt         .
           move      zero                 to   rf-sfp-dec-vlt         .
           move      spaces               to   rf-sfp-tdc-vlt         .
           move      zero                 to   rf-sfp-cdc-pgf         .
           move      zero                 to   rf-sfp-iiv-pgf         .
           move      zero                 to   rf-sfp-imp-pgf         .
           move      zero                 to   rf-sfp-spe-pgf         .
           move      zero                 to   rf-sfp-ddo-pgf         .
           move      spaces               to   rf-sfp-ndo-pgf         .
           move      zero                 to   rf-sfp-dvf-bfc         .
           move      zero                 to   rf-sfp-aos-pgf         .
           move      zero                 to   rf-sfp-rns-pgf         .
           move      zero                 to   rf-sfp-nns-pgf         .
           move      zero                 to   rf-sfp-tns-pgf         .
           move      zero                 to   rf-sfp-num-adp         .
           move      zero                 to   rf-sfp-fcs-uno         .
           move      zero                 to   rf-sfp-fcs-due         .
           move      zero                 to   rf-sfp-fcs-tre         .
           move      spaces               to   rf-sfp-flg-ela         .
           move      spaces               to   rf-sfp-flg-pul         .
           move      spaces               to   rf-sfp-alx-exp         .
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
           move      rf-sfp-ide-ute       to   fil-ide-ute            .
           move      rf-sfp-ide-fas       to   fil-ide-fas            .
           move      rf-sfp-dtr-pgf       to   fil-dtr-pgf            .
           move      rf-sfp-drc-pgf       to   fil-drc-pgf            .
           move      rf-sfp-npc-pgf       to   fil-npc-pgf            .
           move      rf-sfp-mod-pgf       to   fil-mod-pgf            .
           move      rf-sfp-tvl-pgf       to   fil-tvl-pgf            .
           move      rf-sfp-dpz-fnt       to   fil-dpz-fnt            .
           move      rf-sfp-inl-pgf       to   fil-inl-pgf            .
           move      rf-sfp-tip-pgf       to   fil-tip-pgf            .
           move      rf-sfp-abi-pgf       to   fil-abi-pgf            .
           move      rf-sfp-cab-pgf       to   fil-cab-pgf            .
           move      rf-sfp-bef-pgf       to   fil-bef-pgf            .
           move      rf-sfp-ccc-pgf       to   fil-ccc-pgf            .
           move      rf-sfp-sgl-vlt       to   fil-sgl-vlt            .
           move      rf-sfp-dec-vlt       to   fil-dec-vlt            .
           move      rf-sfp-tdc-vlt       to   fil-tdc-vlt            .
           move      rf-sfp-cdc-pgf       to   fil-cdc-pgf            .
           move      rf-sfp-iiv-pgf       to   fil-iiv-pgf            .
           move      rf-sfp-imp-pgf       to   fil-imp-pgf            .
           move      rf-sfp-spe-pgf       to   fil-spe-pgf            .
           move      rf-sfp-ddo-pgf       to   fil-ddo-pgf            .
           move      rf-sfp-ndo-pgf       to   fil-ndo-pgf            .
           move      rf-sfp-dvf-bfc       to   fil-dvf-bfc            .
           move      rf-sfp-aos-pgf       to   fil-aos-pgf            .
           move      rf-sfp-rns-pgf       to   fil-rns-pgf            .
           move      rf-sfp-nns-pgf       to   fil-nns-pgf            .
           move      rf-sfp-tns-pgf       to   fil-tns-pgf            .
           move      rf-sfp-fcs-uno       to   fil-fcs-uno            .
           move      rf-sfp-fcs-due       to   fil-fcs-due            .
           move      rf-sfp-fcs-tre       to   fil-fcs-tre            .
           move      rf-sfp-flg-ela       to   fil-flg-ela            .
           move      rf-sfp-flg-pul       to   fil-flg-pul            .
           move      rf-sfp-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-sfp-num-pgf       to   fil-num-pgf            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-sfp-ide-dat       to   fil-ide-dat            .
           move      rf-sfp-num-pgf       to   fil-num-pgf-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-sfp-cod-fnt       to   fil-cod-fnt            .
           move      rf-sfp-num-pgf       to   fil-num-pgf-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-sfp-num-adp       to   fil-num-adp            .
           move      rf-sfp-num-pgf       to   fil-num-pgf-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-sfp-cbp-pgf       to   fil-cbp-pgf            .
           move      rf-sfp-num-adp       to   fil-num-adp-5          .
           move      rf-sfp-num-pgf       to   fil-num-pgf-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-sfp                 .
           move      fil-ide-dat          to   rf-sfp-ide-dat         .
           move      fil-ide-ute          to   rf-sfp-ide-ute         .
           move      fil-ide-fas          to   rf-sfp-ide-fas         .
           move      fil-dtr-pgf          to   rf-sfp-dtr-pgf         .
           move      fil-drc-pgf          to   rf-sfp-drc-pgf         .
           move      fil-npc-pgf          to   rf-sfp-npc-pgf         .
           move      fil-num-pgf          to   rf-sfp-num-pgf         .
           move      fil-mod-pgf          to   rf-sfp-mod-pgf         .
           move      fil-tvl-pgf          to   rf-sfp-tvl-pgf         .
           move      fil-cod-fnt          to   rf-sfp-cod-fnt         .
           move      fil-dpz-fnt          to   rf-sfp-dpz-fnt         .
           move      fil-inl-pgf          to   rf-sfp-inl-pgf         .
           move      fil-cbp-pgf          to   rf-sfp-cbp-pgf         .
           move      fil-tip-pgf          to   rf-sfp-tip-pgf         .
           move      fil-abi-pgf          to   rf-sfp-abi-pgf         .
           move      fil-cab-pgf          to   rf-sfp-cab-pgf         .
           move      fil-bef-pgf          to   rf-sfp-bef-pgf         .
           move      fil-ccc-pgf          to   rf-sfp-ccc-pgf         .
           move      fil-sgl-vlt          to   rf-sfp-sgl-vlt         .
           move      fil-dec-vlt          to   rf-sfp-dec-vlt         .
           move      fil-tdc-vlt          to   rf-sfp-tdc-vlt         .
           move      fil-cdc-pgf          to   rf-sfp-cdc-pgf         .
           move      fil-iiv-pgf          to   rf-sfp-iiv-pgf         .
           move      fil-imp-pgf          to   rf-sfp-imp-pgf         .
           move      fil-spe-pgf          to   rf-sfp-spe-pgf         .
           move      fil-ddo-pgf          to   rf-sfp-ddo-pgf         .
           move      fil-ndo-pgf          to   rf-sfp-ndo-pgf         .
           move      fil-dvf-bfc          to   rf-sfp-dvf-bfc         .
           move      fil-aos-pgf          to   rf-sfp-aos-pgf         .
           move      fil-rns-pgf          to   rf-sfp-rns-pgf         .
           move      fil-nns-pgf          to   rf-sfp-nns-pgf         .
           move      fil-tns-pgf          to   rf-sfp-tns-pgf         .
           move      fil-num-adp          to   rf-sfp-num-adp         .
           move      fil-fcs-uno          to   rf-sfp-fcs-uno         .
           move      fil-fcs-due          to   rf-sfp-fcs-due         .
           move      fil-fcs-tre          to   rf-sfp-fcs-tre         .
           move      fil-flg-ela          to   rf-sfp-flg-ela         .
           move      fil-flg-pul          to   rf-sfp-flg-pul         .
           move      fil-alx-exp          to   rf-sfp-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-sfp               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-sfp
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.
