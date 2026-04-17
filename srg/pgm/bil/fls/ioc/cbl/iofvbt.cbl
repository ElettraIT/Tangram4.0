       Identification Division.
       Program-Id.                                 iofvbt             .
      *================================================================*
      *                                                                *
      *                  Input-Output File vbt                         *
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
      *            * Chiave numero 01 : CODVDB                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-bil        pic  x(05)                  .
                   15  fil-vdb-lv1        pic  9(05)       comp-3     .
                   15  fil-vdb-lv2        pic  9(05)       comp-3     .
                   15  fil-vdb-lv3        pic  9(05)       comp-3     .
                   15  fil-vdb-lv4        pic  9(05)       comp-3     .
                   15  fil-vdb-lv5        pic  9(05)       comp-3     .
                   15  fil-vdb-lv6        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-ide-dat        pic  9(07)       comp-3     .
                   15  fil-tip-bil-2      pic  x(05)                  .
                   15  fil-vdb-lv1-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv2-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv3-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv4-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv5-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv6-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : VDBMNE                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-tip-bil-3      pic  x(05)                  .
                   15  fil-vdb-lv1-3      pic  9(05)       comp-3     .
                   15  fil-vdb-lv2-3      pic  9(05)       comp-3     .
                   15  fil-vdb-lv3-3      pic  9(05)       comp-3     .
                   15  fil-vdb-lv4-3      pic  9(05)       comp-3     .
                   15  fil-vdb-lv5-3      pic  9(05)       comp-3     .
                   15  fil-vdb-lv6-3      pic  9(05)       comp-3     .
                   15  fil-mne-vdb        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : LIVVDB                         *
      *            *---------------------------------------------------*
               10  fil-k04.
                   15  fil-tip-bil-4      pic  x(05)                  .
                   15  fil-liv-vdb        pic  9(02)                  .
                   15  fil-vdb-lv1-4      pic  9(05)       comp-3     .
                   15  fil-vdb-lv2-4      pic  9(05)       comp-3     .
                   15  fil-vdb-lv3-4      pic  9(05)       comp-3     .
                   15  fil-vdb-lv4-4      pic  9(05)       comp-3     .
                   15  fil-vdb-lv5-4      pic  9(05)       comp-3     .
                   15  fil-vdb-lv6-4      pic  9(05)       comp-3     .
                   15  fil-mne-vdb-4      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-des-vdb.
                   15  fil-rde-vdb occurs 04
                                          pic  x(50)                  .
               10  fil-tip-vdb            pic  9(02)                  .
               10  fil-snx-isg            pic  x(01)                  .
               10  fil-snx-tot            pic  x(01)                  .
               10  fil-dic-tot            pic  x(50)                  .
               10  fil-spz-sep            pic  9(02)                  .
               10  fil-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .
               10  fil-alx-spc.
                   15  filler  occurs 40  pic  x(01)                  .

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
      *            * Chiave numero 01 : CODVDB                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-bil        pic  x(05)                  .
                   15  pul-vdb-lv1        pic  9(05)       comp-3     .
                   15  pul-vdb-lv2        pic  9(05)       comp-3     .
                   15  pul-vdb-lv3        pic  9(05)       comp-3     .
                   15  pul-vdb-lv4        pic  9(05)       comp-3     .
                   15  pul-vdb-lv5        pic  9(05)       comp-3     .
                   15  pul-vdb-lv6        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATSYS                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-ide-dat        pic  9(07)       comp-3     .
                   15  pul-tip-bil-2      pic  x(05)                  .
                   15  pul-vdb-lv1-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv2-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv3-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv4-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv5-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv6-2      pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : VDBMNE                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-tip-bil-3      pic  x(05)                  .
                   15  pul-vdb-lv1-3      pic  9(05)       comp-3     .
                   15  pul-vdb-lv2-3      pic  9(05)       comp-3     .
                   15  pul-vdb-lv3-3      pic  9(05)       comp-3     .
                   15  pul-vdb-lv4-3      pic  9(05)       comp-3     .
                   15  pul-vdb-lv5-3      pic  9(05)       comp-3     .
                   15  pul-vdb-lv6-3      pic  9(05)       comp-3     .
                   15  pul-mne-vdb        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 04 : LIVVDB                         *
      *            *---------------------------------------------------*
               10  pul-k04.
                   15  pul-tip-bil-4      pic  x(05)                  .
                   15  pul-liv-vdb        pic  9(02)                  .
                   15  pul-vdb-lv1-4      pic  9(05)       comp-3     .
                   15  pul-vdb-lv2-4      pic  9(05)       comp-3     .
                   15  pul-vdb-lv3-4      pic  9(05)       comp-3     .
                   15  pul-vdb-lv4-4      pic  9(05)       comp-3     .
                   15  pul-vdb-lv5-4      pic  9(05)       comp-3     .
                   15  pul-vdb-lv6-4      pic  9(05)       comp-3     .
                   15  pul-mne-vdb-4      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-des-vdb.
                   15  pul-rde-vdb occurs 04
                                          pic  x(50)                  .
               10  pul-tip-vdb            pic  9(02)                  .
               10  pul-snx-isg            pic  x(01)                  .
               10  pul-snx-tot            pic  x(01)                  .
               10  pul-dic-tot            pic  x(50)                  .
               10  pul-spz-sep            pic  9(02)                  .
               10  pul-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .
               10  pul-alx-spc.
                   15  filler  occurs 40  pic  x(01)                  .

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
                     "vbt "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bil/fls/ioc/obj/iofvbt              "       .

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
                            "CODVDB"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATSYS"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "VDBMNE"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 4                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "LIVVDB"                                  .
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
      *    * Record logico file [vbt]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbt"                          .

      ******************************************************************
       Procedure Division                using f rf-vbt               .
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
           move      spaces               to   rf-vbt                 .
           move      zero                 to   rf-vbt-ide-dat         .
           move      spaces               to   rf-vbt-ide-ute         .
           move      spaces               to   rf-vbt-ide-fas         .
           move      spaces               to   rf-vbt-tip-bil         .
           move      zero                 to   rf-vbt-vdb-lv1         .
           move      zero                 to   rf-vbt-vdb-lv2         .
           move      zero                 to   rf-vbt-vdb-lv3         .
           move      zero                 to   rf-vbt-vdb-lv4         .
           move      zero                 to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      zero                 to   rf-vbt-liv-vdb         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      spaces               to   rf-vbt-des-vdb         .
           move      zero                 to   rf-vbt-tip-vdb         .
           move      spaces               to   rf-vbt-snx-isg         .
           move      spaces               to   rf-vbt-snx-tot         .
           move      spaces               to   rf-vbt-dic-tot         .
           move      zero                 to   rf-vbt-spz-sep         .
           move      spaces               to   rf-vbt-alx-exp         .
           move      spaces               to   rf-vbt-alx-spc         .
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
           move      rf-vbt-ide-ute       to   fil-ide-ute            .
           move      rf-vbt-ide-fas       to   fil-ide-fas            .
           move      rf-vbt-des-vdb       to   fil-des-vdb            .
           move      rf-vbt-tip-vdb       to   fil-tip-vdb            .
           move      rf-vbt-snx-isg       to   fil-snx-isg            .
           move      rf-vbt-snx-tot       to   fil-snx-tot            .
           move      rf-vbt-dic-tot       to   fil-dic-tot            .
           move      rf-vbt-spz-sep       to   fil-spz-sep            .
           move      rf-vbt-alx-exp       to   fil-alx-exp            .
           move      rf-vbt-alx-spc       to   fil-alx-spc            .
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
           move      rf-vbt-tip-bil       to   fil-tip-bil            .
           move      rf-vbt-vdb-lv1       to   fil-vdb-lv1            .
           move      rf-vbt-vdb-lv2       to   fil-vdb-lv2            .
           move      rf-vbt-vdb-lv3       to   fil-vdb-lv3            .
           move      rf-vbt-vdb-lv4       to   fil-vdb-lv4            .
           move      rf-vbt-vdb-lv5       to   fil-vdb-lv5            .
           move      rf-vbt-vdb-lv6       to   fil-vdb-lv6            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-vbt-ide-dat       to   fil-ide-dat            .
           move      rf-vbt-tip-bil       to   fil-tip-bil-2          .
           move      rf-vbt-vdb-lv1       to   fil-vdb-lv1-2          .
           move      rf-vbt-vdb-lv2       to   fil-vdb-lv2-2          .
           move      rf-vbt-vdb-lv3       to   fil-vdb-lv3-2          .
           move      rf-vbt-vdb-lv4       to   fil-vdb-lv4-2          .
           move      rf-vbt-vdb-lv5       to   fil-vdb-lv5-2          .
           move      rf-vbt-vdb-lv6       to   fil-vdb-lv6-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-vbt-tip-bil       to   fil-tip-bil-3          .
           move      rf-vbt-vdb-lv1       to   fil-vdb-lv1-3          .
           move      rf-vbt-vdb-lv2       to   fil-vdb-lv2-3          .
           move      rf-vbt-vdb-lv3       to   fil-vdb-lv3-3          .
           move      rf-vbt-vdb-lv4       to   fil-vdb-lv4-3          .
           move      rf-vbt-vdb-lv5       to   fil-vdb-lv5-3          .
           move      rf-vbt-vdb-lv6       to   fil-vdb-lv6-3          .
           move      rf-vbt-mne-vdb       to   fil-mne-vdb            .
           go to     cmp-key-fis-999.
       cmp-key-fis-400.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 4                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k04                .
           move      rf-vbt-tip-bil       to   fil-tip-bil-4          .
           move      rf-vbt-liv-vdb       to   fil-liv-vdb            .
           move      rf-vbt-vdb-lv1       to   fil-vdb-lv1-4          .
           move      rf-vbt-vdb-lv2       to   fil-vdb-lv2-4          .
           move      rf-vbt-vdb-lv3       to   fil-vdb-lv3-4          .
           move      rf-vbt-vdb-lv4       to   fil-vdb-lv4-4          .
           move      rf-vbt-vdb-lv5       to   fil-vdb-lv5-4          .
           move      rf-vbt-vdb-lv6       to   fil-vdb-lv6-4          .
           move      rf-vbt-mne-vdb       to   fil-mne-vdb-4          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-vbt                 .
           move      fil-ide-dat          to   rf-vbt-ide-dat         .
           move      fil-ide-ute          to   rf-vbt-ide-ute         .
           move      fil-ide-fas          to   rf-vbt-ide-fas         .
           move      fil-tip-bil          to   rf-vbt-tip-bil         .
           move      fil-vdb-lv1          to   rf-vbt-vdb-lv1         .
           move      fil-vdb-lv2          to   rf-vbt-vdb-lv2         .
           move      fil-vdb-lv3          to   rf-vbt-vdb-lv3         .
           move      fil-vdb-lv4          to   rf-vbt-vdb-lv4         .
           move      fil-vdb-lv5          to   rf-vbt-vdb-lv5         .
           move      fil-vdb-lv6          to   rf-vbt-vdb-lv6         .
           move      fil-liv-vdb          to   rf-vbt-liv-vdb         .
           move      fil-mne-vdb          to   rf-vbt-mne-vdb         .
           move      fil-des-vdb          to   rf-vbt-des-vdb         .
           move      fil-tip-vdb          to   rf-vbt-tip-vdb         .
           move      fil-snx-isg          to   rf-vbt-snx-isg         .
           move      fil-snx-tot          to   rf-vbt-snx-tot         .
           move      fil-dic-tot          to   rf-vbt-dic-tot         .
           move      fil-spz-sep          to   rf-vbt-spz-sep         .
           move      fil-alx-exp          to   rf-vbt-alx-exp         .
           move      fil-alx-spc          to   rf-vbt-alx-spc         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-vbt               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-vbt
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

