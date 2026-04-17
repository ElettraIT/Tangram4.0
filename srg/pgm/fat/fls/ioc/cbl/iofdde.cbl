       Identification Division.
       Program-Id.                                 iofdde             .
      *================================================================*
      *                                                                *
      *                  Input-Output File dde                         *
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
      *            * Chiave numero 01 : NUMDST                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-dst        pic  9(11)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-num-dst-2      pic  9(11)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : ARCDST                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-arc        pic  x(01)                  .
                   15  fil-cod-arc        pic  9(07)       comp-3     .
                   15  fil-dat-reg        pic  9(07)       comp-3     .
                   15  fil-num-dst-3      pic  9(11)       comp-3     .
                   15  fil-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DOCDST                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-prt-doc        pic  9(11)       comp-3     .
                   15  fil-num-dst-4      pic  9(11)       comp-3     .
                   15  fil-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-tip-dst            pic  9(02)                  .
               10  fil-dpz-arc            pic  x(04)                  .
               10  fil-tip-ids            pic  9(02)                  .
               10  fil-eml-ind            pic  x(80)                  .
               10  fil-eml-mit            pic  x(80)                  .
               10  fil-eml-all            pic  x(80)                  .
               10  fil-tip-doc            pic  9(02)                  .
               10  fil-tmo-doc            pic  x(05)                  .
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-doc            pic  x(20)                  .
               10  fil-imp-doc            pic s9(13)       comp-3     .
               10  fil-flg-apr            pic  x(01)                  .
               10  fil-dat-inv            pic  9(07)       comp-3     .
               10  fil-ora-inv            pic  9(04)                  .
               10  fil-cod-vet            pic  9(07)       comp-3     .
               10  fil-aff-spd            pic  9(09)       comp-3     .
               10  fil-pes-spd            pic  9(06)v9(03) comp-3     .
               10  fil-flg-ain            pic  x(01)                  .
               10  fil-tip-con            pic  x(03)                  .
               10  fil-flg-esi            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 195 pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMDST                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-dst        pic  9(11)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-num-dst-2      pic  9(11)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : ARCDST                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-arc        pic  x(01)                  .
                   15  pul-cod-arc        pic  9(07)       comp-3     .
                   15  pul-dat-reg        pic  9(07)       comp-3     .
                   15  pul-num-dst-3      pic  9(11)       comp-3     .
                   15  pul-num-prg-3      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : DOCDST                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-prt-doc        pic  9(11)       comp-3     .
                   15  pul-num-dst-4      pic  9(11)       comp-3     .
                   15  pul-num-prg-4      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-tip-dst            pic  9(02)                  .
               10  pul-dpz-arc            pic  x(04)                  .
               10  pul-tip-ids            pic  9(02)                  .
               10  pul-eml-ind            pic  x(80)                  .
               10  pul-eml-mit            pic  x(80)                  .
               10  pul-eml-all            pic  x(80)                  .
               10  pul-tip-doc            pic  9(02)                  .
               10  pul-tmo-doc            pic  x(05)                  .
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-doc            pic  x(20)                  .
               10  pul-imp-doc            pic s9(13)       comp-3     .
               10  pul-flg-apr            pic  x(01)                  .
               10  pul-dat-inv            pic  9(07)       comp-3     .
               10  pul-ora-inv            pic  9(04)                  .
               10  pul-cod-vet            pic  9(07)       comp-3     .
               10  pul-aff-spd            pic  9(09)       comp-3     .
               10  pul-pes-spd            pic  9(06)v9(03) comp-3     .
               10  pul-flg-ain            pic  x(01)                  .
               10  pul-tip-con            pic  x(03)                  .
               10  pul-flg-esi            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 195 pic  x(01)                  .

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
                     "dde "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/fat/fls/ioc/obj/iofdde              "       .

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
                            "NUMDST"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "ARCDST"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DOCDST"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    4      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [dde]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfdde"                          .

      ******************************************************************
       Procedure Division                using f rf-dde               .
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
           move      spaces               to   rf-dde                 .
           move      zero                 to   rf-dde-ide-dat         .
           move      spaces               to   rf-dde-ide-ute         .
           move      spaces               to   rf-dde-ide-fas         .
           move      zero                 to   rf-dde-num-dst         .
           move      zero                 to   rf-dde-num-prg         .
           move      zero                 to   rf-dde-dat-reg         .
           move      zero                 to   rf-dde-tip-dst         .
           move      spaces               to   rf-dde-tip-arc         .
           move      zero                 to   rf-dde-cod-arc         .
           move      spaces               to   rf-dde-dpz-arc         .
           move      zero                 to   rf-dde-tip-ids         .
           move      spaces               to   rf-dde-eml-mit         .
           move      spaces               to   rf-dde-eml-ind         .
           move      spaces               to   rf-dde-eml-all         .
           move      zero                 to   rf-dde-tip-doc         .
           move      spaces               to   rf-dde-tmo-doc         .
           move      zero                 to   rf-dde-dat-doc         .
           move      spaces               to   rf-dde-num-doc         .
           move      zero                 to   rf-dde-prt-doc         .
           move      zero                 to   rf-dde-imp-doc         .
           move      spaces               to   rf-dde-flg-apr         .
           move      zero                 to   rf-dde-dat-inv         .
           move      zero                 to   rf-dde-ora-inv         .
           move      zero                 to   rf-dde-cod-vet         .
           move      zero                 to   rf-dde-aff-spd         .
           move      zero                 to   rf-dde-pes-spd         .
           move      spaces               to   rf-dde-flg-ain         .
           move      spaces               to   rf-dde-tip-con         .
           move      spaces               to   rf-dde-flg-esi         .
           move      spaces               to   rf-dde-alx-exp         .
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
           move      rf-dde-ide-ute       to   fil-ide-ute            .
           move      rf-dde-ide-fas       to   fil-ide-fas            .
           move      rf-dde-tip-dst       to   fil-tip-dst            .
           move      rf-dde-dpz-arc       to   fil-dpz-arc            .
           move      rf-dde-tip-ids       to   fil-tip-ids            .
           move      rf-dde-eml-mit       to   fil-eml-mit            .
           move      rf-dde-eml-ind       to   fil-eml-ind            .
           move      rf-dde-eml-all       to   fil-eml-all            .
           move      rf-dde-tip-doc       to   fil-tip-doc            .
           move      rf-dde-tmo-doc       to   fil-tmo-doc            .
           move      rf-dde-dat-doc       to   fil-dat-doc            .
           move      rf-dde-num-doc       to   fil-num-doc            .
           move      rf-dde-imp-doc       to   fil-imp-doc            .
           move      rf-dde-flg-apr       to   fil-flg-apr            .
           move      rf-dde-dat-inv       to   fil-dat-inv            .
           move      rf-dde-ora-inv       to   fil-ora-inv            .
           move      rf-dde-cod-vet       to   fil-cod-vet            .
           move      rf-dde-aff-spd       to   fil-aff-spd            .
           move      rf-dde-pes-spd       to   fil-pes-spd            .
           move      rf-dde-flg-ain       to   fil-flg-ain            .
           move      rf-dde-tip-con       to   fil-tip-con            .
           move      rf-dde-flg-esi       to   fil-flg-esi            .
           move      rf-dde-alx-exp       to   fil-alx-exp            .
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
           move      rf-dde-num-dst       to   fil-num-dst            .
           move      rf-dde-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-dde-ide-dat       to   fil-ide-dat            .
           move      rf-dde-num-dst       to   fil-num-dst-2          .
           move      rf-dde-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-dde-tip-arc       to   fil-tip-arc            .
           move      rf-dde-cod-arc       to   fil-cod-arc            .
           move      rf-dde-dat-reg       to   fil-dat-reg            .
           move      rf-dde-num-dst       to   fil-num-dst-3          .
           move      rf-dde-num-prg       to   fil-num-prg-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-dde-prt-doc       to   fil-prt-doc            .
           move      rf-dde-num-dst       to   fil-num-dst-4          .
           move      rf-dde-num-prg       to   fil-num-prg-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-dde                 .
           move      fil-ide-dat          to   rf-dde-ide-dat         .
           move      fil-ide-ute          to   rf-dde-ide-ute         .
           move      fil-ide-fas          to   rf-dde-ide-fas         .
           move      fil-num-dst          to   rf-dde-num-dst         .
           move      fil-num-prg          to   rf-dde-num-prg         .
           move      fil-dat-reg          to   rf-dde-dat-reg         .
           move      fil-tip-dst          to   rf-dde-tip-dst         .
           move      fil-tip-arc          to   rf-dde-tip-arc         .
           move      fil-cod-arc          to   rf-dde-cod-arc         .
           move      fil-dpz-arc          to   rf-dde-dpz-arc         .
           move      fil-tip-ids          to   rf-dde-tip-ids         .
           move      fil-eml-mit          to   rf-dde-eml-mit         .
           move      fil-eml-ind          to   rf-dde-eml-ind         .
           move      fil-eml-all          to   rf-dde-eml-all         .
           move      fil-tip-doc          to   rf-dde-tip-doc         .
           move      fil-tmo-doc          to   rf-dde-tmo-doc         .
           move      fil-dat-doc          to   rf-dde-dat-doc         .
           move      fil-num-doc          to   rf-dde-num-doc         .
           move      fil-prt-doc          to   rf-dde-prt-doc         .
           move      fil-imp-doc          to   rf-dde-imp-doc         .
           move      fil-flg-apr          to   rf-dde-flg-apr         .
           move      fil-dat-inv          to   rf-dde-dat-inv         .
           move      fil-ora-inv          to   rf-dde-ora-inv         .
           move      fil-cod-vet          to   rf-dde-cod-vet         .
           move      fil-aff-spd          to   rf-dde-aff-spd         .
           move      fil-pes-spd          to   rf-dde-pes-spd         .
           move      fil-flg-ain          to   rf-dde-flg-ain         .
           move      fil-tip-con          to   rf-dde-tip-con         .
           move      fil-flg-esi          to   rf-dde-flg-esi         .
           move      fil-alx-exp          to   rf-dde-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-dde               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.
      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-dde
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

