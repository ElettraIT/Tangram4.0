       Identification Division.
       Program-Id.                                 iofhfa             .
      *================================================================*
      *                                                                *
      *                  Input-Output File hfa                         *
      *                                                                *
      *              ___ MAI USATO ricavato da 'ram' di 'rda' ___      *
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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-dat-reg        pic  9(07)       comp-3     .
                   15  fil-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-dat-doc        pic  9(07)       comp-3     .
                   15  fil-num-doc        pic  x(10)                  .
                   15  fil-cod-tmo        pic  x(05)                  .
                   15  fil-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : FNTDAT                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-cod-fnt        pic  9(07)       comp-3     .
                   15  fil-dat-doc-4      pic  9(07)       comp-3     .
                   15  fil-cod-tmo-4      pic  x(05)                  .
                   15  fil-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : FNTPAG                         *
      *            *---------------------------------------------------*
               10  fil-k05.
                   15  fil-cod-fnt-5      pic  9(07)       comp-3     .
                   15  fil-dat-pag        pic  9(07)       comp-3     .
                   15  fil-dat-doc-5      pic  9(07)       comp-3     .
                   15  fil-cod-tmo-5      pic  x(05)                  .
                   15  fil-num-prt-5      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-dpz-fnt            pic  x(04)                  .
               10  fil-snx-prf            pic  9(02)                  .
               10  fil-snx-tpv            pic  9(02)                  .
               10  fil-cod-cau            pic  9(05)       comp-3     .
               10  fil-per-cau            pic  9(03)v9(02) comp-3     .
               10  fil-pim-cau            pic  9(03)v9(02) comp-3     .
               10  fil-cod-tcb            pic  9(05)       comp-3     .
               10  fil-per-tcb            pic  9(03)v9(02) comp-3     .
               10  fil-pim-tcb            pic  9(03)v9(02) comp-3     .
               10  fil-cod-trb            pic  9(05)       comp-3     .
               10  fil-per-trb            pic  9(03)v9(02) comp-3     .
               10  fil-pim-trb            pic  9(03)v9(02) comp-3     .
               10  fil-tot-ibl            pic s9(11)       comp-3     .
               10  fil-tot-irt            pic s9(11)       comp-3     .
               10  fil-tot-rmb            pic s9(11)       comp-3     .
               10  fil-tot-pvn            pic s9(11)       comp-3     .
               10  fil-dat-vrs            pic  9(07)       comp-3     .
               10  fil-tip-vrs            pic  9(02)                  .
               10  fil-num-vrs            pic  x(10)                  .
               10  fil-ann-mov.
                   15  fil-rig-ann occurs 3
                                          pic  x(40)                  .
               10  fil-flg-ela.
                   15  fil-flg-blo.
                       20  fil-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  fil-flg-nbl.
                       20  fil-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs 200  pic  x(01)                  .

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
                   15  pul-num-prt        pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-dat-reg        pic  9(07)       comp-3     .
                   15  pul-num-prt-2      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : IDEDOC                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-dat-doc        pic  9(07)       comp-3     .
                   15  pul-num-doc        pic  x(10)                  .
                   15  pul-cod-tmo        pic  x(05)                  .
                   15  pul-num-prt-3      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : FNTDAT                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-cod-fnt        pic  9(07)       comp-3     .
                   15  pul-dat-doc-4      pic  9(07)       comp-3     .
                   15  pul-cod-tmo-4      pic  x(05)                  .
                   15  pul-num-prt-4      pic  9(09)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 05 : FNTPAG                         *
      *            *---------------------------------------------------*
               10  pul-k05.
                   15  pul-cod-fnt-5      pic  9(07)       comp-3     .
                   15  pul-dat-pag        pic  9(07)       comp-3     .
                   15  pul-dat-doc-5      pic  9(07)       comp-3     .
                   15  pul-cod-tmo-5      pic  x(05)                  .
                   15  pul-num-prt-5      pic  9(09)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-dpz-fnt            pic  x(04)                  .
               10  pul-snx-prf            pic  9(02)                  .
               10  pul-snx-tpv            pic  9(02)                  .
               10  pul-cod-cau            pic  9(05)       comp-3     .
               10  pul-per-cau            pic  9(03)v9(02) comp-3     .
               10  pul-pim-cau            pic  9(03)v9(02) comp-3     .
               10  pul-cod-tcb            pic  9(05)       comp-3     .
               10  pul-per-tcb            pic  9(03)v9(02) comp-3     .
               10  pul-pim-tcb            pic  9(03)v9(02) comp-3     .
               10  pul-cod-trb            pic  9(05)       comp-3     .
               10  pul-per-trb            pic  9(03)v9(02) comp-3     .
               10  pul-pim-trb            pic  9(03)v9(02) comp-3     .
               10  pul-tot-ibl            pic s9(11)       comp-3     .
               10  pul-tot-irt            pic s9(11)       comp-3     .
               10  pul-tot-rmb            pic s9(11)       comp-3     .
               10  pul-tot-pvn            pic s9(11)       comp-3     .
               10  pul-dat-vrs            pic  9(07)       comp-3     .
               10  pul-tip-vrs            pic  9(02)                  .
               10  pul-num-vrs            pic  x(10)                  .
               10  pul-ann-mov.
                   15  pul-rig-ann occurs 3
                                          pic  x(40)                  .
               10  pul-flg-ela.
                   15  pul-flg-blo.
                       20  pul-flg-blx occurs 07
                                          pic  x(01)                  .
                   15  pul-flg-nbl.
                       20  pul-flg-nbx occurs 03
                                          pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs 200  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sigla del file                                        *
      *        *-------------------------------------------------------*
           02  i-ide-sdf                  pic  x(04) value
                     "hfa "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "nob/aut/fls/ioc/obj/iofhfa              "       .

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
      *    * Area per elenco chiavi previste                           *
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
                            "FNTDAT    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 5                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "FNTPAG    "                              .
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
      *    * Record logico file [hfa]                                  *
      *    *-----------------------------------------------------------*
           copy      "nob/aut/fls/rec/rfhfa"                          .

      ******************************************************************
       Procedure Division                using f rf-hfa               .
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
           move      e-not-fnd            to   f-sts                  .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-hfa                 .
           move      zero                 to   rf-hfa-ide-dat         .
           move      spaces               to   rf-hfa-ide-ute         .
           move      spaces               to   rf-hfa-ide-fas         .
           move      zero                 to   rf-hfa-num-prt         .
           move      zero                 to   rf-hfa-dat-reg         .
           move      spaces               to   rf-hfa-cod-tmo         .
           move      zero                 to   rf-hfa-dat-doc         .
           move      spaces               to   rf-hfa-num-doc         .
           move      zero                 to   rf-hfa-cod-fnt         .
           move      spaces               to   rf-hfa-dpz-fnt         .
           move      zero                 to   rf-hfa-snx-prf         .
           move      zero                 to   rf-hfa-snx-tpv         .
           move      zero                 to   rf-hfa-cod-cau         .
           move      zero                 to   rf-hfa-per-cau         .
           move      zero                 to   rf-hfa-pim-cau         .
           move      zero                 to   rf-hfa-cod-tcb         .
           move      zero                 to   rf-hfa-per-tcb         .
           move      zero                 to   rf-hfa-pim-tcb         .
           move      zero                 to   rf-hfa-cod-trb         .
           move      zero                 to   rf-hfa-per-trb         .
           move      zero                 to   rf-hfa-pim-trb         .
           move      zero                 to   rf-hfa-tot-ibl         .
           move      zero                 to   rf-hfa-tot-irt         .
           move      zero                 to   rf-hfa-tot-rmb         .
           move      zero                 to   rf-hfa-tot-pvn         .
           move      zero                 to   rf-hfa-dat-pag         .
           move      zero                 to   rf-hfa-dat-vrs         .
           move      zero                 to   rf-hfa-tip-vrs         .
           move      spaces               to   rf-hfa-num-vrs         .
           move      spaces               to   rf-hfa-ann-mov         .
           move      spaces               to   rf-hfa-flg-ela         .
           move      spaces               to   rf-hfa-alx-exp         .
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
           move      rf-hfa-ide-ute       to   fil-ide-ute            .
           move      rf-hfa-ide-fas       to   fil-ide-fas            .
           move      rf-hfa-dpz-fnt       to   fil-dpz-fnt            .
           move      rf-hfa-snx-prf       to   fil-snx-prf            .
           move      rf-hfa-snx-tpv       to   fil-snx-tpv            .
           move      rf-hfa-cod-cau       to   fil-cod-cau            .
           move      rf-hfa-per-cau       to   fil-per-cau            .
           move      rf-hfa-pim-cau       to   fil-pim-cau            .
           move      rf-hfa-cod-tcb       to   fil-cod-tcb            .
           move      rf-hfa-per-tcb       to   fil-per-tcb            .
           move      rf-hfa-pim-tcb       to   fil-pim-tcb            .
           move      rf-hfa-cod-trb       to   fil-cod-trb            .
           move      rf-hfa-per-trb       to   fil-per-trb            .
           move      rf-hfa-pim-trb       to   fil-pim-trb            .
           move      rf-hfa-tot-ibl       to   fil-tot-ibl            .
           move      rf-hfa-tot-irt       to   fil-tot-irt            .
           move      rf-hfa-tot-rmb       to   fil-tot-rmb            .
           move      rf-hfa-tot-pvn       to   fil-tot-pvn            .
           move      rf-hfa-dat-vrs       to   fil-dat-vrs            .
           move      rf-hfa-tip-vrs       to   fil-tip-vrs            .
           move      rf-hfa-num-vrs       to   fil-num-vrs            .
           move      rf-hfa-ann-mov       to   fil-ann-mov            .
           move      rf-hfa-flg-ela       to   fil-flg-ela            .
           move      rf-hfa-alx-exp       to   fil-alx-exp            .
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
           move      rf-hfa-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-hfa-ide-dat       to   fil-ide-dat            .
           move      rf-hfa-dat-reg       to   fil-dat-reg            .
           move      rf-hfa-num-prt       to   fil-num-prt-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-hfa-dat-doc       to   fil-dat-doc            .
           move      rf-hfa-num-doc       to   fil-num-doc            .
           move      rf-hfa-cod-tmo       to   fil-cod-tmo            .
           move      rf-hfa-num-prt       to   fil-num-prt-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-hfa-cod-fnt       to   fil-cod-fnt            .
           move      rf-hfa-dat-doc       to   fil-dat-doc-4          .
           move      rf-hfa-cod-tmo       to   fil-cod-tmo-4          .
           move      rf-hfa-num-prt       to   fil-num-prt-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 5                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k05                .
           move      rf-hfa-cod-fnt       to   fil-cod-fnt-5          .
           move      rf-hfa-dat-pag       to   fil-dat-pag            .
           move      rf-hfa-dat-doc       to   fil-dat-doc-5          .
           move      rf-hfa-cod-tmo       to   fil-cod-tmo-5          .
           move      rf-hfa-num-prt       to   fil-num-prt-5          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-hfa                 .
           move      fil-ide-dat          to   rf-hfa-ide-dat         .
           move      fil-ide-ute          to   rf-hfa-ide-ute         .
           move      fil-ide-fas          to   rf-hfa-ide-fas         .
           move      fil-num-prt          to   rf-hfa-num-prt         .
           move      fil-dat-reg          to   rf-hfa-dat-reg         .
           move      fil-cod-tmo          to   rf-hfa-cod-tmo         .
           move      fil-dat-doc          to   rf-hfa-dat-doc         .
           move      fil-num-doc          to   rf-hfa-num-doc         .
           move      fil-cod-fnt          to   rf-hfa-cod-fnt         .
           move      fil-dpz-fnt          to   rf-hfa-dpz-fnt         .
           move      fil-snx-prf          to   rf-hfa-snx-prf         .
           move      fil-snx-tpv          to   rf-hfa-snx-tpv         .
           move      fil-cod-cau          to   rf-hfa-cod-cau         .
           move      fil-per-cau          to   rf-hfa-per-cau         .
           move      fil-pim-cau          to   rf-hfa-pim-cau         .
           move      fil-cod-tcb          to   rf-hfa-cod-tcb         .
           move      fil-per-tcb          to   rf-hfa-per-tcb         .
           move      fil-pim-tcb          to   rf-hfa-pim-tcb         .
           move      fil-cod-trb          to   rf-hfa-cod-trb         .
           move      fil-per-trb          to   rf-hfa-per-trb         .
           move      fil-pim-trb          to   rf-hfa-pim-trb         .
           move      fil-tot-ibl          to   rf-hfa-tot-ibl         .
           move      fil-tot-irt          to   rf-hfa-tot-irt         .
           move      fil-tot-rmb          to   rf-hfa-tot-rmb         .
           move      fil-tot-pvn          to   rf-hfa-tot-pvn         .
           move      fil-dat-pag          to   rf-hfa-dat-pag         .
           move      fil-dat-vrs          to   rf-hfa-dat-vrs         .
           move      fil-tip-vrs          to   rf-hfa-tip-vrs         .
           move      fil-num-vrs          to   rf-hfa-num-vrs         .
           move      fil-ann-mov          to   rf-hfa-ann-mov         .
           move      fil-flg-ela          to   rf-hfa-flg-ela         .
           move      fil-alx-exp          to   rf-hfa-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-hfa               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-hfa
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

