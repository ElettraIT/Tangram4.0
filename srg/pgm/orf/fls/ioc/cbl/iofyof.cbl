       Identification Division.
       Program-Id.                                 iofyof             .
      *================================================================*
      *                                                                *
      *                  Input-Output File yof                         *
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
      *            * Chiave numero 01 : CODTOF                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-tof        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-des-key        pic  x(30)                  .
                   15  fil-cod-tof-2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-tof            pic  x(30)                  .
               10  fil-pwd-tof            pic  x(08)                  .
               10  fil-vld-dpz            pic  9(02)                  .
               10  fil-cod-dpz            pic  9(02)                  .
               10  fil-org-doc            pic  9(02)                  .
               10  fil-prv-doc            pic  9(02)                  .
               10  fil-sgl-num            pic  x(03)                  .
               10  fil-des-stp            pic  x(25)                  .
               10  fil-snx-prz            pic  9(02)                  .
               10  fil-snx-sco            pic  9(02)                  .
               10  fil-snx-dtc            pic  9(02)                  .
               10  fil-def-tpr            pic  x(05)                  .
               10  fil-snx-sto            pic  x(01)                  .
               10  fil-snx-stv            pic  x(01)                  .
               10  fil-sta-tus            pic  9(02)                  .
               10  fil-sta-tud            pic  9(07)       comp-3     .
               10  fil-sta-tuc            pic  x(05)                  .
               10  fil-sta-tux            pic  9(02)                  .
               10  fil-tip-ord            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler  occurs 79  pic  x(01)                  .

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
      *            * Chiave numero 01 : CODTOF                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-tof        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-des-key        pic  x(30)                  .
                   15  pul-cod-tof-2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-tof            pic  x(30)                  .
               10  pul-pwd-tof            pic  x(08)                  .
               10  pul-vld-dpz            pic  9(02)                  .
               10  pul-cod-dpz            pic  9(02)                  .
               10  pul-org-doc            pic  9(02)                  .
               10  pul-prv-doc            pic  9(02)                  .
               10  pul-sgl-num            pic  x(03)                  .
               10  pul-des-stp            pic  x(25)                  .
               10  pul-snx-prz            pic  9(02)                  .
               10  pul-snx-sco            pic  9(02)                  .
               10  pul-snx-dtc            pic  9(02)                  .
               10  pul-def-tpr            pic  x(05)                  .
               10  pul-snx-sto            pic  x(01)                  .
               10  pul-snx-stv            pic  x(01)                  .
               10  pul-sta-tus            pic  9(02)                  .
               10  pul-sta-tud            pic  9(07)       comp-3     .
               10  pul-sta-tuc            pic  x(05)                  .
               10  pul-sta-tux            pic  9(02)                  .
               10  pul-tip-ord            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler  occurs 79  pic  x(01)                  .

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
                     "yof "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/orf/fls/ioc/obj/iofyof              "       .

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
           05  k-ctr                      pic  9(02) value 2          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "CODTOF    "                              .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DESKEY    "                              .
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
      *    * Record logico file [yof]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyof"                          .

      ******************************************************************
       Procedure Division                using f rf-yof               .
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
           move      e-not-fnd            to   f-sts                  .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-yof                 .
           move      spaces               to   rf-yof-cod-tof         .
           move      spaces               to   rf-yof-des-key         .
           move      spaces               to   rf-yof-des-tof         .
           move      spaces               to   rf-yof-pwd-tof         .
           move      zero                 to   rf-yof-vld-dpz         .
           move      zero                 to   rf-yof-cod-dpz         .
           move      zero                 to   rf-yof-org-doc         .
           move      zero                 to   rf-yof-prv-doc         .
           move      spaces               to   rf-yof-sgl-num         .
           move      spaces               to   rf-yof-des-stp         .
           move      zero                 to   rf-yof-snx-prz         .
           move      zero                 to   rf-yof-snx-sco         .
           move      zero                 to   rf-yof-snx-dtc         .
           move      spaces               to   rf-yof-def-tpr         .
           move      spaces               to   rf-yof-snx-sto         .
           move      spaces               to   rf-yof-snx-stv         .
           move      zero                 to   rf-yof-sta-tus         .
           move      zero                 to   rf-yof-sta-tud         .
           move      spaces               to   rf-yof-sta-tuc         .
           move      zero                 to   rf-yof-sta-tux         .
           move      spaces               to   rf-yof-tip-ord         .
           move      spaces               to   rf-yof-alx-exp         .
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
           move      rf-yof-des-tof       to   fil-des-tof            .
           move      rf-yof-pwd-tof       to   fil-pwd-tof            .
           move      rf-yof-vld-dpz       to   fil-vld-dpz            .
           move      rf-yof-cod-dpz       to   fil-cod-dpz            .
           move      rf-yof-org-doc       to   fil-org-doc            .
           move      rf-yof-prv-doc       to   fil-prv-doc            .
           move      rf-yof-sgl-num       to   fil-sgl-num            .
           move      rf-yof-des-stp       to   fil-des-stp            .
           move      rf-yof-snx-prz       to   fil-snx-prz            .
           move      rf-yof-snx-sco       to   fil-snx-sco            .
           move      rf-yof-snx-dtc       to   fil-snx-dtc            .
           move      rf-yof-def-tpr       to   fil-def-tpr            .
           move      rf-yof-snx-sto       to   fil-snx-sto            .
           move      rf-yof-snx-stv       to   fil-snx-stv            .
           move      rf-yof-sta-tus       to   fil-sta-tus            .
           move      rf-yof-sta-tud       to   fil-sta-tud            .
           move      rf-yof-sta-tuc       to   fil-sta-tuc            .
           move      rf-yof-sta-tux       to   fil-sta-tux            .
           move      rf-yof-tip-ord       to   fil-tip-ord            .
           move      rf-yof-alx-exp       to   fil-alx-exp            .
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
           move      rf-yof-cod-tof       to   fil-cod-tof            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-yof-des-key       to   fil-des-key            .
           move      rf-yof-cod-tof       to   fil-cod-tof-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-yof                 .
           move      fil-cod-tof          to   rf-yof-cod-tof         .
           move      fil-des-key          to   rf-yof-des-key         .
           move      fil-des-tof          to   rf-yof-des-tof         .
           move      fil-pwd-tof          to   rf-yof-pwd-tof         .
           move      fil-vld-dpz          to   rf-yof-vld-dpz         .
           move      fil-cod-dpz          to   rf-yof-cod-dpz         .
           move      fil-org-doc          to   rf-yof-org-doc         .
           move      fil-prv-doc          to   rf-yof-prv-doc         .
           move      fil-sgl-num          to   rf-yof-sgl-num         .
           move      fil-des-stp          to   rf-yof-des-stp         .
           move      fil-snx-prz          to   rf-yof-snx-prz         .
           move      fil-snx-sco          to   rf-yof-snx-sco         .
           move      fil-snx-dtc          to   rf-yof-snx-dtc         .
           move      fil-def-tpr          to   rf-yof-def-tpr         .
           move      fil-snx-sto          to   rf-yof-snx-sto         .
           move      fil-snx-stv          to   rf-yof-snx-stv         .
           move      fil-sta-tus          to   rf-yof-sta-tus         .
           move      fil-sta-tud          to   rf-yof-sta-tud         .
           move      fil-sta-tuc          to   rf-yof-sta-tuc         .
           move      fil-sta-tux          to   rf-yof-sta-tux         .
           move      fil-tip-ord          to   rf-yof-tip-ord         .
           move      fil-alx-exp          to   rf-yof-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-yof               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-yof
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

