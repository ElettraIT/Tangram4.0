       Identification Division.
       Program-Id.                                 iofdcf             .
      *================================================================*
      *                                                                *
      *                  Input-Output File dcf                         *
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
      *            * Chiave numero 01 : CODFNT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-fnt        pic  9(07)       comp-3     .
                   15  fil-dpz-fnt        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-cod-fnt-2      pic  9(07)       comp-3     .
                   15  fil-dpz-fnt-2      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-rag-key        pic  x(40)                  .
                   15  fil-cod-fnt-3      pic  9(07)       comp-3     .
                   15  fil-dpz-fnt-3      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-inf-gen.
                   15  fil-ide-ute        pic  x(08)                  .
                   15  fil-ide-fas        pic  x(06)                  .
                   15  fil-rag-soc        pic  x(40)                  .
                   15  fil-via-dcf        pic  x(40)                  .
                   15  fil-loc-dcf        pic  x(40)                  .
                   15  fil-cod-naz        pic  x(03)                  .
                   15  fil-cod-cmn        pic  9(05)       comp-3     .
                   15  fil-cod-fzn        pic  9(03)       comp-3     .
                   15  fil-cod-lct        pic  9(03)       comp-3     .
                   15  fil-rs1-doc        pic  x(40)                  .
                   15  fil-rs2-doc        pic  x(40)                  .
                   15  fil-num-tel        pic  x(20)                  .
                   15  fil-num-fax        pic  x(20)                  .
                   15  fil-num-tlx        pic  x(20)                  .
                   15  fil-nom-int        pic  x(30)                  .
                   15  fil-inl-dcm        pic  9(02)                  .
                   15  fil-inl-pgt        pic  9(02)                  .
                   15  fil-noi-cli        pic  x(15)                  .
               10  fil-inf-fat.
                   15  fil-tas-ivf        pic  9(02)                  .
                   15  fil-ctp-acq        pic  9(07)       comp-3     .
                   15  fil-per-fat        pic  9(02)                  .
                   15  fil-rag-bft        pic  9(02)                  .
                   15  fil-cod-vlt        pic  x(03)                  .
                   15  fil-cod-lng        pic  x(03)                  .
               10  fil-inf-cda.
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
                   15  fil-nos-ban        pic  x(10)                  .
                   15  fil-cod-abi        pic  9(05)       comp-3     .
                   15  fil-cod-cab        pic  9(05)       comp-3     .
                   15  fil-ccc-app        pic  x(12)                  .
                   15  fil-ccp-app        pic  x(12)                  .
                   15  fil-ipr-iel        pic  9(02)                  .
               10  fil-inf-aqt.
                   15  fil-cod-aqt        pic  9(07)       comp-3     .
                   15  fil-cdp-aqt        pic  9(05)       comp-3     .
                   15  fil-pdp-aqt  occurs 03
                                          pic  9(02)v9(01) comp-3     .
               10  fil-inf-pcs.
                   15  fil-cod-zon        pic  9(05)       comp-3     .
                   15  fil-cod-cat        pic  9(05)       comp-3     .
                   15  fil-cod-stt        pic  9(05)       comp-3     .
               10  fil-inf-bdg.
                   15  fil-cla-bdg        pic  9(05)       comp-3     .
               10  fil-inf-orf.
                   15  fil-for-blo        pic  9(02)                  .
                   15  fil-eml-int        pic  x(40)                  .
               10  fil-inf-aps.
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)                  .
                   15  fil-sta-tuc        pic  9(07)                  .
                   15  fil-sta-tux        pic  9(02)                  .
                   15  fil-cin-ita        pic  x(01)                  .
                   15  fil-cin-eur        pic  x(02)                  .
                   15  fil-tip-mci        pic  x(01)                  .
                   15  fil-alx-exp.
                           20  filler occurs 18
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
      *            * Chiave numero 01 : CODFNT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-fnt        pic  9(07)       comp-3     .
                   15  pul-dpz-fnt        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-cod-fnt-2      pic  9(07)       comp-3     .
                   15  pul-dpz-fnt-2      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : RAGKEY                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-rag-key        pic  x(40)                  .
                   15  pul-cod-fnt-3      pic  9(07)       comp-3     .
                   15  pul-dpz-fnt-3      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-inf-gen.
                   15  pul-ide-ute        pic  x(08)                  .
                   15  pul-ide-fas        pic  x(06)                  .
                   15  pul-rag-soc        pic  x(40)                  .
                   15  pul-via-dcf        pic  x(40)                  .
                   15  pul-loc-dcf        pic  x(40)                  .
                   15  pul-cod-naz        pic  x(03)                  .
                   15  pul-cod-cmn        pic  9(05)       comp-3     .
                   15  pul-cod-fzn        pic  9(03)       comp-3     .
                   15  pul-cod-lct        pic  9(03)       comp-3     .
                   15  pul-rs1-doc        pic  x(40)                  .
                   15  pul-rs2-doc        pic  x(40)                  .
                   15  pul-num-tel        pic  x(20)                  .
                   15  pul-num-fax        pic  x(20)                  .
                   15  pul-num-tlx        pic  x(20)                  .
                   15  pul-nom-int        pic  x(30)                  .
                   15  pul-inl-dcm        pic  9(02)                  .
                   15  pul-inl-pgt        pic  9(02)                  .
                   15  pul-noi-cli        pic  x(15)                  .
               10  pul-inf-fat.
                   15  pul-tas-ivf        pic  9(02)                  .
                   15  pul-ctp-acq        pic  9(07)       comp-3     .
                   15  pul-per-fat        pic  9(02)                  .
                   15  pul-rag-bft        pic  9(02)                  .
                   15  pul-cod-vlt        pic  x(03)                  .
                   15  pul-cod-lng        pic  x(03)                  .
               10  pul-inf-cda.
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
                   15  pul-nos-ban        pic  x(10)                  .
                   15  pul-cod-abi        pic  9(05)       comp-3     .
                   15  pul-cod-cab        pic  9(05)       comp-3     .
                   15  pul-ccc-app        pic  x(12)                  .
                   15  pul-ccp-app        pic  x(12)                  .
                   15  pul-ipr-iel        pic  9(02)                  .
               10  pul-inf-aqt.
                   15  pul-cod-aqt        pic  9(07)       comp-3     .
                   15  pul-cdp-aqt        pic  9(05)       comp-3     .
                   15  pul-pdp-aqt  occurs 03
                                          pic  9(02)v9(01) comp-3     .
               10  pul-inf-pcs.
                   15  pul-cod-zon        pic  9(05)       comp-3     .
                   15  pul-cod-cat        pic  9(05)       comp-3     .
                   15  pul-cod-stt        pic  9(05)       comp-3     .
               10  pul-inf-bdg.
                   15  pul-cla-bdg        pic  9(05)       comp-3     .
               10  pul-inf-orf.
                   15  pul-for-blo        pic  9(02)                  .
                   15  pul-eml-int        pic  x(40)                  .
               10  pul-inf-aps.
                   15  pul-sta-tus        pic  9(02)                  .
                   15  pul-sta-tud        pic  9(07)                  .
                   15  pul-sta-tuc        pic  9(07)                  .
                   15  pul-sta-tux        pic  9(02)                  .
                   15  pul-cin-ita        pic  x(01)                  .
                   15  pul-cin-eur        pic  x(02)                  .
                   15  pul-tip-mci        pic  x(01)                  .
                   15  pul-alx-exp.
                           20  filler occurs 18
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
                     "dcf "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/dcf/fls/ioc/obj/iofdcf              "       .

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
                            "CODFNT"                                  .
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
      *    * Record logico file [dcf]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .

      ******************************************************************
       Procedure Division                using f rf-dcf               .
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
           move      spaces               to   rf-dcf                 .
           move      zero                 to   rf-dcf-ide-dat         .
           move      spaces               to   rf-dcf-ide-ute         .
           move      spaces               to   rf-dcf-ide-fas         .
           move      zero                 to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      spaces               to   rf-dcf-rag-key         .
           move      spaces               to   rf-dcf-rag-soc         .
           move      spaces               to   rf-dcf-via-dcf         .
           move      spaces               to   rf-dcf-loc-dcf         .
           move      spaces               to   rf-dcf-cod-naz         .
           move      zero                 to   rf-dcf-cod-cmn         .
           move      zero                 to   rf-dcf-cod-fzn         .
           move      zero                 to   rf-dcf-cod-lct         .
           move      spaces               to   rf-dcf-rs1-doc         .
           move      spaces               to   rf-dcf-rs2-doc         .
           move      spaces               to   rf-dcf-num-tel         .
           move      spaces               to   rf-dcf-num-fax         .
           move      spaces               to   rf-dcf-num-tlx         .
           move      spaces               to   rf-dcf-nom-int         .
           move      zero                 to   rf-dcf-inl-dcm         .
           move      zero                 to   rf-dcf-inl-pgt         .
           move      spaces               to   rf-dcf-noi-cli         .
           move      zero                 to   rf-dcf-tas-ivf         .
           move      zero                 to   rf-dcf-ctp-acq         .
           move      zero                 to   rf-dcf-per-fat         .
           move      zero                 to   rf-dcf-rag-bft         .
           move      spaces               to   rf-dcf-cod-vlt         .
           move      spaces               to   rf-dcf-cod-lng         .
           move      spaces               to   rf-dcf-cod-lst         .
           move      zero                 to   rf-dcf-cat-scr         .
           move      zero                 to   rf-dcf-per-scr (1)     .
           move      zero                 to   rf-dcf-per-scr (2)     .
           move      zero                 to   rf-dcf-per-scr (3)     .
           move      zero                 to   rf-dcf-per-scr (4)     .
           move      zero                 to   rf-dcf-per-scr (5)     .
           move      zero                 to   rf-dcf-cat-scc         .
           move      zero                 to   rf-dcf-per-scc         .
           move      zero                 to   rf-dcf-snm-spe (1)     .
           move      zero                 to   rf-dcf-snm-spe (2)     .
           move      zero                 to   rf-dcf-snm-spe (3)     .
           move      zero                 to   rf-dcf-snm-spe (4)     .
           move      zero                 to   rf-dcf-snm-spe (5)     .
           move      zero                 to   rf-dcf-snm-spe (6)     .
           move      zero                 to   rf-dcf-per-spe (1)     .
           move      zero                 to   rf-dcf-per-spe (2)     .
           move      zero                 to   rf-dcf-per-spe (3)     .
           move      zero                 to   rf-dcf-per-spe (4)     .
           move      zero                 to   rf-dcf-per-spe (5)     .
           move      zero                 to   rf-dcf-per-spe (6)     .
           move      zero                 to   rf-dcf-imp-spe (1)     .
           move      zero                 to   rf-dcf-imp-spe (2)     .
           move      zero                 to   rf-dcf-imp-spe (3)     .
           move      zero                 to   rf-dcf-imp-spe (4)     .
           move      zero                 to   rf-dcf-imp-spe (5)     .
           move      zero                 to   rf-dcf-imp-spe (6)     .
           move      spaces               to   rf-dcf-vde-cod (1)     .
           move      spaces               to   rf-dcf-vde-cod (2)     .
           move      spaces               to   rf-dcf-vde-cod (3)     .
           move      spaces               to   rf-dcf-vde-cod (4)     .
           move      spaces               to   rf-dcf-vde-cod (5)     .
           move      spaces               to   rf-dcf-vde-cod (6)     .
           move      zero                 to   rf-dcf-cod-fop         .
           move      zero                 to   rf-dcf-tip-esm         .
           move      zero                 to   rf-dcf-ggg-alt         .
           move      zero                 to   rf-dcf-mmm-e01         .
           move      zero                 to   rf-dcf-mmm-e02         .
           move      spaces               to   rf-dcf-add-spi         .
           move      spaces               to   rf-dcf-add-spb         .
           move      spaces               to   rf-dcf-nos-ban         .
           move      zero                 to   rf-dcf-cod-abi         .
           move      zero                 to   rf-dcf-cod-cab         .
           move      spaces               to   rf-dcf-ccc-app         .
           move      spaces               to   rf-dcf-ccp-app         .
           move      zero                 to   rf-dcf-ipr-iel         .
           move      zero                 to   rf-dcf-cod-aqt         .
           move      zero                 to   rf-dcf-cdp-aqt         .
           move      zero                 to   rf-dcf-pdp-aqt (1)     .
           move      zero                 to   rf-dcf-pdp-aqt (2)     .
           move      zero                 to   rf-dcf-pdp-aqt (3)     .
           move      zero                 to   rf-dcf-cod-zon         .
           move      zero                 to   rf-dcf-cod-cat         .
           move      zero                 to   rf-dcf-cod-stt         .
           move      zero                 to   rf-dcf-cla-bdg         .
           move      zero                 to   rf-dcf-for-blo         .
           move      spaces               to   rf-dcf-eml-int         .
           move      zero                 to   rf-dcf-sta-tus         .
           move      zero                 to   rf-dcf-sta-tud         .
           move      zero                 to   rf-dcf-sta-tuc         .
           move      zero                 to   rf-dcf-sta-tux         .
           move      spaces               to   rf-dcf-cin-ita         .
           move      spaces               to   rf-dcf-cin-eur         .
           move      spaces               to   rf-dcf-tip-mci         .
           move      spaces               to   rf-dcf-alx-exp         .
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
           move      rf-dcf-ide-ute       to   fil-ide-ute            .
           move      rf-dcf-ide-fas       to   fil-ide-fas            .
           move      rf-dcf-rag-soc       to   fil-rag-soc            .
           move      rf-dcf-via-dcf       to   fil-via-dcf            .
           move      rf-dcf-loc-dcf       to   fil-loc-dcf            .
           move      rf-dcf-cod-naz       to   fil-cod-naz            .
           move      rf-dcf-cod-cmn       to   fil-cod-cmn            .
           move      rf-dcf-cod-fzn       to   fil-cod-fzn            .
           move      rf-dcf-cod-lct       to   fil-cod-lct            .
           move      rf-dcf-rs1-doc       to   fil-rs1-doc            .
           move      rf-dcf-rs2-doc       to   fil-rs2-doc            .
           move      rf-dcf-num-tel       to   fil-num-tel            .
           move      rf-dcf-num-fax       to   fil-num-fax            .
           move      rf-dcf-num-tlx       to   fil-num-tlx            .
           move      rf-dcf-nom-int       to   fil-nom-int            .
           move      rf-dcf-inl-dcm       to   fil-inl-dcm            .
           move      rf-dcf-inl-pgt       to   fil-inl-pgt            .
           move      rf-dcf-noi-cli       to   fil-noi-cli            .
           move      rf-dcf-tas-ivf       to   fil-tas-ivf            .
           move      rf-dcf-ctp-acq       to   fil-ctp-acq            .
           move      rf-dcf-per-fat       to   fil-per-fat            .
           move      rf-dcf-rag-bft       to   fil-rag-bft            .
           move      rf-dcf-cod-vlt       to   fil-cod-vlt            .
           move      rf-dcf-cod-lng       to   fil-cod-lng            .
           move      rf-dcf-cod-lst       to   fil-cod-lst            .
           move      rf-dcf-cat-scr       to   fil-cat-scr            .
           move      rf-dcf-per-scr (1)   to   fil-per-scr (1)        .
           move      rf-dcf-per-scr (2)   to   fil-per-scr (2)        .
           move      rf-dcf-per-scr (3)   to   fil-per-scr (3)        .
           move      rf-dcf-per-scr (4)   to   fil-per-scr (4)        .
           move      rf-dcf-per-scr (5)   to   fil-per-scr (5)        .
           move      rf-dcf-cat-scc       to   fil-cat-scc            .
           move      rf-dcf-per-scc       to   fil-per-scc            .
           move      rf-dcf-snm-spe (1)   to   fil-snm-spe (1)        .
           move      rf-dcf-snm-spe (2)   to   fil-snm-spe (2)        .
           move      rf-dcf-snm-spe (3)   to   fil-snm-spe (3)        .
           move      rf-dcf-snm-spe (4)   to   fil-snm-spe (4)        .
           move      rf-dcf-snm-spe (5)   to   fil-snm-spe (5)        .
           move      rf-dcf-snm-spe (6)   to   fil-snm-spe (6)        .
           move      rf-dcf-per-spe (1)   to   fil-per-spe (1)        .
           move      rf-dcf-per-spe (2)   to   fil-per-spe (2)        .
           move      rf-dcf-per-spe (3)   to   fil-per-spe (3)        .
           move      rf-dcf-per-spe (4)   to   fil-per-spe (4)        .
           move      rf-dcf-per-spe (5)   to   fil-per-spe (5)        .
           move      rf-dcf-per-spe (6)   to   fil-per-spe (6)        .
           move      rf-dcf-imp-spe (1)   to   fil-imp-spe (1)        .
           move      rf-dcf-imp-spe (2)   to   fil-imp-spe (2)        .
           move      rf-dcf-imp-spe (3)   to   fil-imp-spe (3)        .
           move      rf-dcf-imp-spe (4)   to   fil-imp-spe (4)        .
           move      rf-dcf-imp-spe (5)   to   fil-imp-spe (5)        .
           move      rf-dcf-imp-spe (6)   to   fil-imp-spe (6)        .
           move      rf-dcf-vde-cod (1)   to   fil-vde-cod (1)        .
           move      rf-dcf-vde-cod (2)   to   fil-vde-cod (2)        .
           move      rf-dcf-vde-cod (3)   to   fil-vde-cod (3)        .
           move      rf-dcf-vde-cod (4)   to   fil-vde-cod (4)        .
           move      rf-dcf-vde-cod (5)   to   fil-vde-cod (5)        .
           move      rf-dcf-vde-cod (6)   to   fil-vde-cod (6)        .
           move      rf-dcf-cod-fop       to   fil-cod-fop            .
           move      rf-dcf-tip-esm       to   fil-tip-esm            .
           move      rf-dcf-ggg-alt       to   fil-ggg-alt            .
           move      rf-dcf-mmm-e01       to   fil-mmm-e01            .
           move      rf-dcf-mmm-e02       to   fil-mmm-e02            .
           move      rf-dcf-add-spi       to   fil-add-spi            .
           move      rf-dcf-add-spb       to   fil-add-spb            .
           move      rf-dcf-nos-ban       to   fil-nos-ban            .
           move      rf-dcf-cod-abi       to   fil-cod-abi            .
           move      rf-dcf-cod-cab       to   fil-cod-cab            .
           move      rf-dcf-ccc-app       to   fil-ccc-app            .
           move      rf-dcf-ccp-app       to   fil-ccp-app            .
           move      rf-dcf-ipr-iel       to   fil-ipr-iel            .
           move      rf-dcf-cod-aqt       to   fil-cod-aqt            .
           move      rf-dcf-cdp-aqt       to   fil-cdp-aqt            .
           move      rf-dcf-pdp-aqt (1)   to   fil-pdp-aqt (1)        .
           move      rf-dcf-pdp-aqt (2)   to   fil-pdp-aqt (2)        .
           move      rf-dcf-pdp-aqt (3)   to   fil-pdp-aqt (3)        .
           move      rf-dcf-cod-zon       to   fil-cod-zon            .
           move      rf-dcf-cod-cat       to   fil-cod-cat            .
           move      rf-dcf-cod-stt       to   fil-cod-stt            .
           move      rf-dcf-cla-bdg       to   fil-cla-bdg            .
           move      rf-dcf-for-blo       to   fil-for-blo            .
           move      rf-dcf-eml-int       to   fil-eml-int            .
           move      rf-dcf-sta-tus       to   fil-sta-tus            .
           move      rf-dcf-sta-tud       to   fil-sta-tud            .
           move      rf-dcf-sta-tuc       to   fil-sta-tuc            .
           move      rf-dcf-sta-tux       to   fil-sta-tux            .
           move      rf-dcf-cin-ita       to   fil-cin-ita            .
           move      rf-dcf-cin-eur       to   fil-cin-eur            .
           move      rf-dcf-tip-mci       to   fil-tip-mci            .
           move      rf-dcf-alx-exp       to   fil-alx-exp            .
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
           move      rf-dcf-cod-fnt       to   fil-cod-fnt            .
           move      rf-dcf-dpz-fnt       to   fil-dpz-fnt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-dcf-ide-dat       to   fil-ide-dat            .
           move      rf-dcf-cod-fnt       to   fil-cod-fnt-2          .
           move      rf-dcf-dpz-fnt       to   fil-dpz-fnt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-dcf-rag-key       to   fil-rag-key            .
           move      rf-dcf-cod-fnt       to   fil-cod-fnt-3          .
           move      rf-dcf-dpz-fnt       to   fil-dpz-fnt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-dcf                 .
           move      fil-ide-dat          to   rf-dcf-ide-dat         .
           move      fil-ide-ute          to   rf-dcf-ide-ute         .
           move      fil-ide-fas          to   rf-dcf-ide-fas         .
           move      fil-cod-fnt          to   rf-dcf-cod-fnt         .
           move      fil-dpz-fnt          to   rf-dcf-dpz-fnt         .
           move      fil-rag-key          to   rf-dcf-rag-key         .
           move      fil-rag-soc          to   rf-dcf-rag-soc         .
           move      fil-via-dcf          to   rf-dcf-via-dcf         .
           move      fil-loc-dcf          to   rf-dcf-loc-dcf         .
           move      fil-cod-naz          to   rf-dcf-cod-naz         .
           move      fil-cod-cmn          to   rf-dcf-cod-cmn         .
           move      fil-cod-fzn          to   rf-dcf-cod-fzn         .
           move      fil-cod-lct          to   rf-dcf-cod-lct         .
           move      fil-rs1-doc          to   rf-dcf-rs1-doc         .
           move      fil-rs2-doc          to   rf-dcf-rs2-doc         .
           move      fil-num-tel          to   rf-dcf-num-tel         .
           move      fil-num-fax          to   rf-dcf-num-fax         .
           move      fil-num-tlx          to   rf-dcf-num-tlx         .
           move      fil-nom-int          to   rf-dcf-nom-int         .
           move      fil-inl-dcm          to   rf-dcf-inl-dcm         .
           move      fil-inl-pgt          to   rf-dcf-inl-pgt         .
           move      fil-noi-cli          to   rf-dcf-noi-cli         .
           move      fil-tas-ivf          to   rf-dcf-tas-ivf         .
           move      fil-ctp-acq          to   rf-dcf-ctp-acq         .
           move      fil-per-fat          to   rf-dcf-per-fat         .
           move      fil-rag-bft          to   rf-dcf-rag-bft         .
           move      fil-cod-vlt          to   rf-dcf-cod-vlt         .
           move      fil-cod-lng          to   rf-dcf-cod-lng         .
           move      fil-cod-lst          to   rf-dcf-cod-lst         .
           move      fil-cat-scr          to   rf-dcf-cat-scr         .
           move      fil-per-scr (1)      to   rf-dcf-per-scr (1)     .
           move      fil-per-scr (2)      to   rf-dcf-per-scr (2)     .
           move      fil-per-scr (3)      to   rf-dcf-per-scr (3)     .
           move      fil-per-scr (4)      to   rf-dcf-per-scr (4)     .
           move      fil-per-scr (5)      to   rf-dcf-per-scr (5)     .
           move      fil-cat-scc          to   rf-dcf-cat-scc         .
           move      fil-per-scc          to   rf-dcf-per-scc         .
           move      fil-snm-spe (1)      to   rf-dcf-snm-spe (1)     .
           move      fil-snm-spe (2)      to   rf-dcf-snm-spe (2)     .
           move      fil-snm-spe (3)      to   rf-dcf-snm-spe (3)     .
           move      fil-snm-spe (4)      to   rf-dcf-snm-spe (4)     .
           move      fil-snm-spe (5)      to   rf-dcf-snm-spe (5)     .
           move      fil-snm-spe (6)      to   rf-dcf-snm-spe (6)     .
           move      fil-per-spe (1)      to   rf-dcf-per-spe (1)     .
           move      fil-per-spe (2)      to   rf-dcf-per-spe (2)     .
           move      fil-per-spe (3)      to   rf-dcf-per-spe (3)     .
           move      fil-per-spe (4)      to   rf-dcf-per-spe (4)     .
           move      fil-per-spe (5)      to   rf-dcf-per-spe (5)     .
           move      fil-per-spe (6)      to   rf-dcf-per-spe (6)     .
           move      fil-imp-spe (1)      to   rf-dcf-imp-spe (1)     .
           move      fil-imp-spe (2)      to   rf-dcf-imp-spe (2)     .
           move      fil-imp-spe (3)      to   rf-dcf-imp-spe (3)     .
           move      fil-imp-spe (4)      to   rf-dcf-imp-spe (4)     .
           move      fil-imp-spe (5)      to   rf-dcf-imp-spe (5)     .
           move      fil-imp-spe (6)      to   rf-dcf-imp-spe (6)     .
           move      fil-vde-cod (1)      to   rf-dcf-vde-cod (1)     .
           move      fil-vde-cod (2)      to   rf-dcf-vde-cod (2)     .
           move      fil-vde-cod (3)      to   rf-dcf-vde-cod (3)     .
           move      fil-vde-cod (4)      to   rf-dcf-vde-cod (4)     .
           move      fil-vde-cod (5)      to   rf-dcf-vde-cod (5)     .
           move      fil-vde-cod (6)      to   rf-dcf-vde-cod (6)     .
           move      fil-cod-fop          to   rf-dcf-cod-fop         .
           move      fil-tip-esm          to   rf-dcf-tip-esm         .
           move      fil-ggg-alt          to   rf-dcf-ggg-alt         .
           move      fil-mmm-e01          to   rf-dcf-mmm-e01         .
           move      fil-mmm-e02          to   rf-dcf-mmm-e02         .
           move      fil-add-spi          to   rf-dcf-add-spi         .
           move      fil-add-spb          to   rf-dcf-add-spb         .
           move      fil-nos-ban          to   rf-dcf-nos-ban         .
           move      fil-cod-abi          to   rf-dcf-cod-abi         .
           move      fil-cod-cab          to   rf-dcf-cod-cab         .
           move      fil-ccc-app          to   rf-dcf-ccc-app         .
           move      fil-ccp-app          to   rf-dcf-ccp-app         .
           move      fil-ipr-iel          to   rf-dcf-ipr-iel         .
           move      fil-cod-aqt          to   rf-dcf-cod-aqt         .
           move      fil-cdp-aqt          to   rf-dcf-cdp-aqt         .
           move      fil-pdp-aqt (1)      to   rf-dcf-pdp-aqt (1)     .
           move      fil-pdp-aqt (2)      to   rf-dcf-pdp-aqt (2)     .
           move      fil-pdp-aqt (3)      to   rf-dcf-pdp-aqt (3)     .
           move      fil-cod-zon          to   rf-dcf-cod-zon         .
           move      fil-cod-cat          to   rf-dcf-cod-cat         .
           move      fil-cod-stt          to   rf-dcf-cod-stt         .
           move      fil-cla-bdg          to   rf-dcf-cla-bdg         .
           move      fil-for-blo          to   rf-dcf-for-blo         .
           move      fil-eml-int          to   rf-dcf-eml-int         .
           move      fil-sta-tus          to   rf-dcf-sta-tus         .
           move      fil-sta-tud          to   rf-dcf-sta-tud         .
           move      fil-sta-tuc          to   rf-dcf-sta-tuc         .
           move      fil-sta-tux          to   rf-dcf-sta-tux         .
           move      fil-cin-ita          to   rf-dcf-cin-ita         .
           move      fil-cin-eur          to   rf-dcf-cin-eur         .
           move      fil-tip-mci          to   rf-dcf-tip-mci         .
           move      fil-alx-exp          to   rf-dcf-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-dcf               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-dcf
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

