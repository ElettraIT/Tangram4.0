       Identification Division.
       Program-Id.                                 iofybf             .
      *================================================================*
      *                                                                *
      *                  Input-Output File ybf                         *
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
      *            * Chiave numero 01 : CODTMB                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-tmb        pic  x(05)                  .
                   15  fil-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-des-key        pic  x(30)                  .
                   15  fil-cod-tmb-2      pic  x(05)                  .
                   15  fil-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-dti-gen.
                   15  fil-des-tmb        pic  x(30)                  .
                   15  fil-pwd-tmb        pic  x(08)                  .
                   15  fil-int-ftr        pic  9(02)                  .
                   15  fil-tmo-ftr        pic  x(05)                  .
                   15  fil-cau-mag        pic  9(05)                  .
                   15  fil-cod-mic        pic  x(03)                  .
                   15  fil-cam-agg        pic  9(05)                  .
                   15  fil-def-tar        pic  x(01)                  .
                   15  fil-snv-tar        pic  x(01)                  .
                   15  fil-lst-tar        pic  x(04)                  .
                   15  fil-org-doc        pic  9(02)                  .
                   15  fil-prv-doc        pic  9(02)                  .
                   15  fil-mov-afd        pic  9(02)                  .
                   15  fil-def-tmf        pic  x(05)                  .
                   15  fil-vld-dpz        pic  x(01)                  .
                   15  fil-def-tpr        pic  x(05)                  .
                   15  fil-sta-tus        pic  9(02)                  .
                   15  fil-sta-tud        pic  9(07)       comp-3     .
                   15  fil-sta-tuc        pic  x(05)                  .
                   15  fil-sta-tux        pic  9(02)                  .
                   15  fil-alx-gen.
                       20  filler  occurs 40
                                          pic  x(01)                  .
               10  fil-dti-dpz.
                   15  fil-cod-dsl        pic  x(07)                  .
                   15  fil-alx-dpz.
                       20  filler  occurs 40
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
      *            * Chiave numero 01 : CODTMB                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-tmb        pic  x(05)                  .
                   15  pul-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-des-key        pic  x(30)                  .
                   15  pul-cod-tmb-2      pic  x(05)                  .
                   15  pul-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-dti-gen.
                   15  pul-des-tmb        pic  x(30)                  .
                   15  pul-pwd-tmb        pic  x(08)                  .
                   15  pul-int-ftr        pic  9(02)                  .
                   15  pul-tmo-ftr        pic  x(05)                  .
                   15  pul-cau-mag        pic  9(05)                  .
                   15  pul-cod-mic        pic  x(03)                  .
                   15  pul-cam-agg        pic  9(05)                  .
                   15  pul-def-tar        pic  x(01)                  .
                   15  pul-snv-tar        pic  x(01)                  .
                   15  pul-lst-tar        pic  x(04)                  .
                   15  pul-org-doc        pic  9(02)                  .
                   15  pul-prv-doc        pic  9(02)                  .
                   15  pul-mov-afd        pic  9(02)                  .
                   15  pul-def-tmf        pic  x(05)                  .
                   15  pul-vld-dpz        pic  x(01)                  .
                   15  pul-def-tpr        pic  x(05)                  .
                   15  pul-sta-tus        pic  9(02)                  .
                   15  pul-sta-tud        pic  9(07)       comp-3     .
                   15  pul-sta-tuc        pic  x(05)                  .
                   15  pul-sta-tux        pic  9(02)                  .
                   15  pul-alx-gen.
                       20  filler  occurs 40
                                          pic  x(01)                  .
               10  pul-dti-dpz.
                   15  pul-cod-dsl        pic  x(07)                  .
                   15  pul-alx-dpz.
                       20  puller  occurs 40
                                          pic  x(01)                  .

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
                     "ybf "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bfo/fls/ioc/obj/iofybf              "       .

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
                            "CODTMB    "                              .
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
      *    * Record logico file [ybf]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .

      ******************************************************************
       Procedure Division                using f rf-ybf               .
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
           move      spaces               to   rf-ybf                 .
           move      spaces               to   rf-ybf-cod-tmb         .
           move      zero                 to   rf-ybf-cod-dpz         .
           move      spaces               to   rf-ybf-des-key         .
           move      spaces               to   rf-ybf-des-tmb         .
           move      spaces               to   rf-ybf-pwd-tmb         .
           move      zero                 to   rf-ybf-int-ftr         .
           move      spaces               to   rf-ybf-tmo-ftr         .
           move      zero                 to   rf-ybf-cau-mag         .
           move      spaces               to   rf-ybf-cod-mic         .
           move      zero                 to   rf-ybf-cam-agg         .
           move      spaces               to   rf-ybf-def-tar         .
           move      spaces               to   rf-ybf-snv-tar         .
           move      spaces               to   rf-ybf-lst-tar         .
           move      zero                 to   rf-ybf-org-doc         .
           move      zero                 to   rf-ybf-prv-doc         .
           move      zero                 to   rf-ybf-mov-afd         .
           move      spaces               to   rf-ybf-def-tmf         .
           move      spaces               to   rf-ybf-vld-dpz         .
           move      spaces               to   rf-ybf-def-tpr         .
           move      zero                 to   rf-ybf-sta-tus         .
           move      zero                 to   rf-ybf-sta-tud         .
           move      spaces               to   rf-ybf-sta-tuc         .
           move      zero                 to   rf-ybf-sta-tux         .
           move      spaces               to   rf-ybf-alx-gen         .
           move      spaces               to   rf-ybf-cod-dsl         .
           move      spaces               to   rf-ybf-alx-dpz         .
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
           move      rf-ybf-des-tmb       to   fil-des-tmb            .
           move      rf-ybf-pwd-tmb       to   fil-pwd-tmb            .
           move      rf-ybf-int-ftr       to   fil-int-ftr            .
           move      rf-ybf-tmo-ftr       to   fil-tmo-ftr            .
           move      rf-ybf-cau-mag       to   fil-cau-mag            .
           move      rf-ybf-cod-mic       to   fil-cod-mic            .
           move      rf-ybf-cam-agg       to   fil-cam-agg            .
           move      rf-ybf-def-tar       to   fil-def-tar            .
           move      rf-ybf-snv-tar       to   fil-snv-tar            .
           move      rf-ybf-lst-tar       to   fil-lst-tar            .
           move      rf-ybf-org-doc       to   fil-org-doc            .
           move      rf-ybf-prv-doc       to   fil-prv-doc            .
           move      rf-ybf-mov-afd       to   fil-mov-afd            .
           move      rf-ybf-def-tmf       to   fil-def-tmf            .
           move      rf-ybf-vld-dpz       to   fil-vld-dpz            .
           move      rf-ybf-def-tpr       to   fil-def-tpr            .
           move      rf-ybf-sta-tus       to   fil-sta-tus            .
           move      rf-ybf-sta-tud       to   fil-sta-tud            .
           move      rf-ybf-sta-tuc       to   fil-sta-tuc            .
           move      rf-ybf-sta-tux       to   fil-sta-tux            .
           move      rf-ybf-alx-gen       to   fil-alx-gen            .
           move      rf-ybf-cod-dsl       to   fil-cod-dsl            .
           move      rf-ybf-alx-dpz       to   fil-alx-dpz            .
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
           move      rf-ybf-cod-tmb       to   fil-cod-tmb            .
           move      rf-ybf-cod-dpz       to   fil-cod-dpz            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-ybf-des-key       to   fil-des-key            .
           move      rf-ybf-cod-tmb       to   fil-cod-tmb-2          .
           move      rf-ybf-cod-dpz       to   fil-cod-dpz-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-ybf                 .
           move      fil-cod-tmb          to   rf-ybf-cod-tmb         .
           move      fil-cod-dpz          to   rf-ybf-cod-dpz         .
           move      fil-des-key          to   rf-ybf-des-key         .
           move      fil-des-tmb          to   rf-ybf-des-tmb         .
           move      fil-pwd-tmb          to   rf-ybf-pwd-tmb         .
           move      fil-int-ftr          to   rf-ybf-int-ftr         .
           move      fil-tmo-ftr          to   rf-ybf-tmo-ftr         .
           move      fil-cau-mag          to   rf-ybf-cau-mag         .
           move      fil-cod-mic          to   rf-ybf-cod-mic         .
           move      fil-cam-agg          to   rf-ybf-cam-agg         .
           move      fil-def-tar          to   rf-ybf-def-tar         .
           move      fil-snv-tar          to   rf-ybf-snv-tar         .
           move      fil-lst-tar          to   rf-ybf-lst-tar         .
           move      fil-org-doc          to   rf-ybf-org-doc         .
           move      fil-prv-doc          to   rf-ybf-prv-doc         .
           move      fil-mov-afd          to   rf-ybf-mov-afd         .
           move      fil-def-tmf          to   rf-ybf-def-tmf         .
           move      fil-vld-dpz          to   rf-ybf-vld-dpz         .
           move      fil-def-tpr          to   rf-ybf-def-tpr         .
           move      fil-sta-tus          to   rf-ybf-sta-tus         .
           move      fil-sta-tud          to   rf-ybf-sta-tud         .
           move      fil-sta-tuc          to   rf-ybf-sta-tuc         .
           move      fil-sta-tux          to   rf-ybf-sta-tux         .
           move      fil-alx-gen          to   rf-ybf-alx-gen         .
           move      fil-cod-dsl          to   rf-ybf-cod-dsl         .
           move      fil-alx-dpz          to   rf-ybf-alx-dpz         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-ybf               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-ybf
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

