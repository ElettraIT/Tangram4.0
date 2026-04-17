       Identification Division.
       Program-Id.                                 iofhbc             .
      *================================================================*
      *                                                                *
      *                  Input-Output File hbc                         *
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
      *            * Chiave numero 01 : PRTPRO                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-prt        pic  9(11)       comp-3     .
                   15  fil-num-pro        pic  9(07)       comp-3     .
                   15  fil-num-spl        pic  9(03)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRTBNC                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-num-prt-2      pic  9(11)       comp-3     .
                   15  fil-sgl-bnc        pic  x(20)                  .
                   15  fil-num-pro-2      pic  9(07)       comp-3     .
                   15  fil-num-spl-2      pic  9(03)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROPRT                         *
      *            *---------------------------------------------------*
               10  fil-k03.
                   15  fil-num-pro-3      pic  9(07)       comp-3     .
                   15  fil-num-prt-3      pic  9(11)       comp-3     .
                   15  fil-num-spl-3      pic  9(03)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-dat            pic  9(07)       comp-3     .
               10  fil-ide-ute            pic  x(08)                  .
               10  fil-ide-fas            pic  x(06)                  .
               10  fil-qta-spl            pic s9(10)v9(03) comp-3     .
               10  fil-dat-bcd occurs 06.
                   15  fil-sez-bcd        pic  x(80)                  .
               10  fil-ann-spl.
                   15  filler occurs 256  pic  x(01)                  .
               10  fil-pth-img            pic  x(80)                  .
               10  fil-flg-del            pic  x(01)                  .
               10  fil-num-bnc            pic  9(03)                  .
               10  fil-prt-ods            pic  9(11)                  .
               10  fil-alx-exp.
                   15  filler occurs 256  pic  x(01)                  .

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
      *            * Chiave numero 01 : PRTPRO                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-prt        pic  9(11)       comp-3     .
                   15  pul-num-pro        pic  9(07)       comp-3     .
                   15  pul-num-spl        pic  9(03)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : PRTBNC                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-num-prt-2      pic  9(11)       comp-3     .
                   15  pul-sgl-bnc        pic  x(20)                  .
                   15  pul-num-pro-2      pic  9(07)       comp-3     .
                   15  pul-num-spl-2      pic  9(03)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : PROPRT                         *
      *            *---------------------------------------------------*
               10  pul-k03.
                   15  pul-num-pro-3      pic  9(07)       comp-3     .
                   15  pul-num-prt-3      pic  9(11)       comp-3     .
                   15  pul-num-spl-3      pic  9(03)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-dat            pic  9(07)       comp-3     .
               10  pul-ide-ute            pic  x(08)                  .
               10  pul-ide-fas            pic  x(06)                  .
               10  pul-qta-spl            pic s9(10)v9(03) comp-3     .
               10  pul-dat-bcd occurs 06.
                   15  pul-sez-bcd        pic  x(80)                  .
               10  pul-ann-spl.
                   15  filler occurs 256  pic  x(01)                  .
               10  pul-pth-img            pic  x(80)                  .
               10  pul-flg-del            pic  x(01)                  .
               10  pul-num-bnc            pic  9(03)                  .
               10  pul-prt-ods            pic  9(11)                  .
               10  pul-alx-exp.
                   15  filler occurs 256  pic  x(01)                  .

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
                     "hbc "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "sir/agb/fls/ioc/obj/iofhbc              "       .

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
                            "PRTPRO"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PRTBNC    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 3                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "PROPRT    "                              .
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
      *    * Record logico file [hbc]                                  *
      *    *-----------------------------------------------------------*
           copy      "sir/agb/fls/rec/rfhbc"                          .

      ******************************************************************
       Procedure Division                using f rf-hbc               .
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
           move      spaces               to   rf-hbc                 .
           move      zero                 to   rf-hbc-ide-dat         .
           move      spaces               to   rf-hbc-ide-ute         .
           move      spaces               to   rf-hbc-ide-fas         .
           move      zero                 to   rf-hbc-num-prt         .
           move      zero                 to   rf-hbc-num-pro         .
           move      zero                 to   rf-hbc-num-spl         .
           move      zero                 to   rf-hbc-qta-spl         .
      *
           move      spaces               to   rf-hbc-sez-bcd (1)     .
           move      spaces               to   rf-hbc-sez-bcd (2)     .
           move      spaces               to   rf-hbc-sez-bcd (3)     .
           move      spaces               to   rf-hbc-sez-bcd (4)     .
           move      spaces               to   rf-hbc-sez-bcd (5)     .
           move      spaces               to   rf-hbc-sez-bcd (6)     .
      *
           move      spaces               to   rf-hbc-ann-spl         .
           move      spaces               to   rf-hbc-pth-img         .
           move      spaces               to   rf-hbc-flg-del         .
           move      zero                 to   rf-hbc-num-bnc         .
           move      spaces               to   rf-hbc-sgl-bnc         .
           move      zero                 to   rf-hbc-prt-ods         .
           move      spaces               to   rf-hbc-alx-exp         .
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
           move      rf-hbc-ide-dat       to   fil-ide-dat            .
           move      rf-hbc-ide-ute       to   fil-ide-ute            .
           move      rf-hbc-ide-fas       to   fil-ide-fas            .
           move      rf-hbc-qta-spl       to   fil-qta-spl            .
      *
           move      rf-hbc-sez-bcd (1)   to   fil-sez-bcd (1)        .
           move      rf-hbc-sez-bcd (2)   to   fil-sez-bcd (2)        .
           move      rf-hbc-sez-bcd (3)   to   fil-sez-bcd (3)        .
           move      rf-hbc-sez-bcd (4)   to   fil-sez-bcd (4)        .
           move      rf-hbc-sez-bcd (5)   to   fil-sez-bcd (5)        .
           move      rf-hbc-sez-bcd (6)   to   fil-sez-bcd (6)        .
      *
           move      rf-hbc-ann-spl       to   fil-ann-spl            .
           move      rf-hbc-pth-img       to   fil-pth-img            .
           move      rf-hbc-flg-del       to   fil-flg-del            .
           move      rf-hbc-num-bnc       to   fil-num-bnc            .
           move      rf-hbc-prt-ods       to   fil-prt-ods            .
           move      rf-hbc-alx-exp       to   fil-alx-exp            .
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
           move      rf-hbc-num-prt       to   fil-num-prt            .
           move      rf-hbc-num-pro       to   fil-num-pro            .
           move      rf-hbc-num-spl       to   fil-num-spl            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-hbc-num-prt       to   fil-num-prt-2          .
           move      rf-hbc-sgl-bnc       to   fil-sgl-bnc            .
           move      rf-hbc-num-pro       to   fil-num-pro-2          .
           move      rf-hbc-num-spl       to   fil-num-spl-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-300.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 3                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k03                .
           move      rf-hbc-num-pro       to   fil-num-pro-3          .
           move      rf-hbc-num-prt       to   fil-num-prt-3          .
           move      rf-hbc-num-spl       to   fil-num-spl-3          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-hbc                 .
           move      fil-ide-dat          to   rf-hbc-ide-dat         .
           move      fil-ide-ute          to   rf-hbc-ide-ute         .
           move      fil-ide-fas          to   rf-hbc-ide-fas         .
           move      fil-num-prt          to   rf-hbc-num-prt         .
           move      fil-num-pro          to   rf-hbc-num-pro         .
           move      fil-num-spl          to   rf-hbc-num-spl         .
           move      fil-qta-spl          to   rf-hbc-qta-spl         .
      *
           move      fil-sez-bcd (1)      to   rf-hbc-sez-bcd (1)     .
           move      fil-sez-bcd (2)      to   rf-hbc-sez-bcd (2)     .
           move      fil-sez-bcd (3)      to   rf-hbc-sez-bcd (3)     .
           move      fil-sez-bcd (4)      to   rf-hbc-sez-bcd (4)     .
           move      fil-sez-bcd (5)      to   rf-hbc-sez-bcd (5)     .
           move      fil-sez-bcd (6)      to   rf-hbc-sez-bcd (6)     .
      *
           move      fil-ann-spl          to   rf-hbc-ann-spl         .
           move      fil-pth-img          to   rf-hbc-pth-img         .
           move      fil-flg-del          to   rf-hbc-flg-del         .
           move      fil-num-bnc          to   rf-hbc-num-bnc         .
           move      fil-sgl-bnc          to   rf-hbc-sgl-bnc         .
           move      fil-prt-ods          to   rf-hbc-prt-ods         .
           move      fil-alx-exp          to   rf-hbc-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-hbc               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-hbc
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

