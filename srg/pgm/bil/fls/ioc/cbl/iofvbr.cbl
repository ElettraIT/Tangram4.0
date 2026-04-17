       Identification Division.
       Program-Id.                                 iofvbr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File vbr                         *
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
                             file status  is                f-fil-sts .

      *    *===========================================================*
      *    * File Control [pul]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pul   assign to disk           f-pul-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pul-k01
                   alternate record key   is pul-k02
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
                   15  fil-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : STCVDB                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-tip-bil-2      pic  x(05)                  .
                   15  fil-tip-elm        pic  x(01)                  .
                   15  fil-cod-stc        pic  9(07)       comp-3     .
                   15  fil-vdb-lv1-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv2-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv3-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv4-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv5-2      pic  9(05)       comp-3     .
                   15  fil-vdb-lv6-2      pic  9(05)       comp-3     .
                   15  fil-num-prg-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tip-sld            pic  x(01)                  .
               10  fil-tip-ope            pic  x(01)                  .
               10  fil-vrs-lv1            pic  9(05)       comp-3     .
               10  fil-vrs-lv2            pic  9(05)       comp-3     .
               10  fil-vrs-lv3            pic  9(05)       comp-3     .
               10  fil-vrs-lv4            pic  9(05)       comp-3     .
               10  fil-vrs-lv5            pic  9(05)       comp-3     .
               10  fil-vrs-lv6            pic  9(05)       comp-3     .
               10  fil-not-elm            pic  x(40)                  .
               10  fil-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .
               10  fil-alx-spc.
                   15  filler  occurs 20  pic  x(01)                  .

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
                   15  pul-num-prg        pic  9(05)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : STCVDB                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-tip-bil-2      pic  x(05)                  .
                   15  pul-tip-elm        pic  x(01)                  .
                   15  pul-cod-stc        pic  9(07)       comp-3     .
                   15  pul-vdb-lv1-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv2-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv3-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv4-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv5-2      pic  9(05)       comp-3     .
                   15  pul-vdb-lv6-2      pic  9(05)       comp-3     .
                   15  pul-num-prg-2      pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tip-sld            pic  x(01)                  .
               10  pul-tip-ope            pic  x(01)                  .
               10  pul-vrs-lv1            pic  9(05)       comp-3     .
               10  pul-vrs-lv2            pic  9(05)       comp-3     .
               10  pul-vrs-lv3            pic  9(05)       comp-3     .
               10  pul-vrs-lv4            pic  9(05)       comp-3     .
               10  pul-vrs-lv5            pic  9(05)       comp-3     .
               10  pul-vrs-lv6            pic  9(05)       comp-3     .
               10  pul-not-elm            pic  x(40)                  .
               10  pul-alx-exp.
                   15  filler  occurs 20  pic  x(01)                  .
               10  pul-alx-spc.
                   15  filler  occurs 20  pic  x(01)                  .

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
                     "vbr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bil/fls/ioc/obj/iofvbr              "       .

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
           05  k-ctr                      pic  9(02) value 2          .
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
                            "STCVDB"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    2      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [vbr]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbr"                          .

      ******************************************************************
       Procedure Division                using f rf-vbr               .
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
           move      spaces               to   rf-vbr                 .
           move      spaces               to   rf-vbr-tip-bil         .
           move      zero                 to   rf-vbr-vdb-lv1         .
           move      zero                 to   rf-vbr-vdb-lv2         .
           move      zero                 to   rf-vbr-vdb-lv3         .
           move      zero                 to   rf-vbr-vdb-lv4         .
           move      zero                 to   rf-vbr-vdb-lv5         .
           move      zero                 to   rf-vbr-vdb-lv6         .
           move      zero                 to   rf-vbr-num-prg         .
           move      spaces               to   rf-vbr-tip-elm         .
           move      spaces               to   rf-vbr-tip-sld         .
           move      spaces               to   rf-vbr-tip-ope         .
           move      zero                 to   rf-vbr-vrs-lv1         .
           move      zero                 to   rf-vbr-vrs-lv2         .
           move      zero                 to   rf-vbr-vrs-lv3         .
           move      zero                 to   rf-vbr-vrs-lv4         .
           move      zero                 to   rf-vbr-vrs-lv5         .
           move      zero                 to   rf-vbr-vrs-lv6         .
           move      zero                 to   rf-vbr-cod-stc         .
           move      spaces               to   rf-vbr-not-elm         .
           move      spaces               to   rf-vbr-alx-exp         .
           move      spaces               to   rf-vbr-alx-spc         .
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
           move      rf-vbr-tip-sld       to   fil-tip-sld            .
           move      rf-vbr-tip-ope       to   fil-tip-ope            .
           move      rf-vbr-vrs-lv1       to   fil-vrs-lv1            .
           move      rf-vbr-vrs-lv2       to   fil-vrs-lv2            .
           move      rf-vbr-vrs-lv3       to   fil-vrs-lv3            .
           move      rf-vbr-vrs-lv4       to   fil-vrs-lv4            .
           move      rf-vbr-vrs-lv5       to   fil-vrs-lv5            .
           move      rf-vbr-vrs-lv6       to   fil-vrs-lv6            .
           move      rf-vbr-not-elm       to   fil-not-elm            .
           move      rf-vbr-alx-exp       to   fil-alx-exp            .
           move      rf-vbr-alx-spc       to   fil-alx-spc            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-vbr-tip-bil       to   fil-tip-bil            .
           move      rf-vbr-vdb-lv1       to   fil-vdb-lv1            .
           move      rf-vbr-vdb-lv2       to   fil-vdb-lv2            .
           move      rf-vbr-vdb-lv3       to   fil-vdb-lv3            .
           move      rf-vbr-vdb-lv4       to   fil-vdb-lv4            .
           move      rf-vbr-vdb-lv5       to   fil-vdb-lv5            .
           move      rf-vbr-vdb-lv6       to   fil-vdb-lv6            .
           move      rf-vbr-num-prg       to   fil-num-prg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-vbr-tip-bil       to   fil-tip-bil-2          .
           move      rf-vbr-tip-elm       to   fil-tip-elm            .
           move      rf-vbr-cod-stc       to   fil-cod-stc            .
           move      rf-vbr-vdb-lv1       to   fil-vdb-lv1-2          .
           move      rf-vbr-vdb-lv2       to   fil-vdb-lv2-2          .
           move      rf-vbr-vdb-lv3       to   fil-vdb-lv3-2          .
           move      rf-vbr-vdb-lv4       to   fil-vdb-lv4-2          .
           move      rf-vbr-vdb-lv5       to   fil-vdb-lv5-2          .
           move      rf-vbr-vdb-lv6       to   fil-vdb-lv6-2          .
           move      rf-vbr-num-prg       to   fil-num-prg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-vbr                 .
           move      fil-tip-bil          to   rf-vbr-tip-bil         .
           move      fil-vdb-lv1          to   rf-vbr-vdb-lv1         .
           move      fil-vdb-lv2          to   rf-vbr-vdb-lv2         .
           move      fil-vdb-lv3          to   rf-vbr-vdb-lv3         .
           move      fil-vdb-lv4          to   rf-vbr-vdb-lv4         .
           move      fil-vdb-lv5          to   rf-vbr-vdb-lv5         .
           move      fil-vdb-lv6          to   rf-vbr-vdb-lv6         .
           move      fil-num-prg          to   rf-vbr-num-prg         .
           move      fil-tip-elm          to   rf-vbr-tip-elm         .
           move      fil-tip-sld          to   rf-vbr-tip-sld         .
           move      fil-tip-ope          to   rf-vbr-tip-ope         .
           move      fil-vrs-lv1          to   rf-vbr-vrs-lv1         .
           move      fil-vrs-lv2          to   rf-vbr-vrs-lv2         .
           move      fil-vrs-lv3          to   rf-vbr-vrs-lv3         .
           move      fil-vrs-lv4          to   rf-vbr-vrs-lv4         .
           move      fil-vrs-lv5          to   rf-vbr-vrs-lv5         .
           move      fil-vrs-lv6          to   rf-vbr-vrs-lv6         .
           move      fil-cod-stc          to   rf-vbr-cod-stc         .
           move      fil-not-elm          to   rf-vbr-not-elm         .
           move      fil-alx-exp          to   rf-vbr-alx-exp         .
           move      fil-alx-spc          to   rf-vbr-alx-spc         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-vbr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-vbr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

