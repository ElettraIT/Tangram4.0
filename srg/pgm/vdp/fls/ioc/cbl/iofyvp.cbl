       Identification Division.
       Program-Id.                                 iofyvp             .
      *================================================================*
      *                                                                *
      *                  Input-Output File yvp                         *
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
      *            * Chiave numero 01 : CODTMV                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-tmv        pic  x(05)                  .
                   15  fil-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-des-key        pic  x(30)                  .
                   15  fil-cod-tmv-2      pic  x(05)                  .
                   15  fil-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-tmv            pic  x(30)                  .
               10  fil-pwd-tmv            pic  x(08)                  .
               10  fil-cau-car            pic  9(05)       comp-3     .
               10  fil-cmc-car            pic  x(03)                  .
               10  fil-cau-sca            pic  9(05)       comp-3     .
               10  fil-cmc-sca            pic  x(03)                  .
               10  fil-tde-dib            pic  9(02)                  .
               10  fil-snx-cad            pic  x(01)                  .
               10  fil-cau-cad            pic  9(05)       comp-3     .
               10  fil-cmc-cad            pic  x(03)                  .
               10  fil-org-doc            pic  9(02)                  .
               10  fil-prv-doc            pic  9(02)                  .
               10  fil-sgl-num            pic  x(03)                  .
               10  fil-mov-afd            pic  9(02)                  .
               10  fil-chi-com            pic  x(01)                  .
               10  fil-obl-afd            pic  9(02)                  .
               10  fil-def-tmf            pic  x(05)                  .
               10  fil-mod-tmf            pic  x(01)                  .
               10  fil-alx-gen.
                   15  filler  occurs 40  pic  x(01)                  .
               10  fil-cod-dsl            pic  x(07)                  .
               10  fil-alx-dpz.
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
      *            * Chiave numero 01 : CODTMV                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-tmv        pic  x(05)                  .
                   15  pul-cod-dpz        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-des-key        pic  x(30)                  .
                   15  pul-cod-tmv-2      pic  x(05)                  .
                   15  pul-cod-dpz-2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-tmv            pic  x(30)                  .
               10  pul-pwd-tmv            pic  x(08)                  .
               10  pul-cau-car            pic  9(05)       comp-3     .
               10  pul-cmc-car            pic  x(03)                  .
               10  pul-cau-sca            pic  9(05)       comp-3     .
               10  pul-cmc-sca            pic  x(03)                  .
               10  pul-tde-dib            pic  9(02)                  .
               10  pul-snx-cad            pic  x(01)                  .
               10  pul-cau-cad            pic  9(05)       comp-3     .
               10  pul-cmc-cad            pic  x(03)                  .
               10  pul-org-doc            pic  9(02)                  .
               10  pul-prv-doc            pic  9(02)                  .
               10  pul-sgl-num            pic  x(03)                  .
               10  pul-mov-afd            pic  9(02)                  .
               10  pul-chi-com            pic  x(01)                  .
               10  pul-obl-afd            pic  9(02)                  .
               10  pul-def-tmf            pic  x(05)                  .
               10  pul-mod-tmf            pic  x(01)                  .
               10  pul-alx-gen.
                   15  filler  occurs 40  pic  x(01)                  .
               10  pul-cod-dsl            pic  x(07)                  .
               10  pul-alx-dpz.
                   15  filler  occurs 40  pic  x(01)                  .

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
                     "yvp "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/vdp/fls/ioc/obj/iofyvp              "       .

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
                            "CODTMV    "                              .
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
      *    * Record logico file [yvp]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfyvp"                          .

      ******************************************************************
       Procedure Division                using f rf-yvp               .
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
           move      spaces               to   rf-yvp                 .
           move      spaces               to   rf-yvp-cod-tmv         .
           move      zero                 to   rf-yvp-cod-dpz         .
           move      spaces               to   rf-yvp-des-key         .
           move      spaces               to   rf-yvp-des-tmv         .
           move      spaces               to   rf-yvp-pwd-tmv         .
           move      zero                 to   rf-yvp-cau-car         .
           move      spaces               to   rf-yvp-cmc-car         .
           move      zero                 to   rf-yvp-cau-sca         .
           move      spaces               to   rf-yvp-cmc-sca         .
           move      zero                 to   rf-yvp-tde-dib         .
           move      spaces               to   rf-yvp-snx-cad         .
           move      zero                 to   rf-yvp-cau-cad         .
           move      spaces               to   rf-yvp-cmc-cad         .
           move      zero                 to   rf-yvp-org-doc         .
           move      zero                 to   rf-yvp-prv-doc         .
           move      spaces               to   rf-yvp-sgl-num         .
           move      zero                 to   rf-yvp-mov-afd         .
           move      spaces               to   rf-yvp-chi-com         .
           move      zero                 to   rf-yvp-obl-afd         .
           move      spaces               to   rf-yvp-def-tmf         .
           move      spaces               to   rf-yvp-mod-tmf         .
           move      spaces               to   rf-yvp-alx-gen         .
           move      spaces               to   rf-yvp-cod-dsl         .
           move      spaces               to   rf-yvp-alx-dpz         .
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
           move      rf-yvp-des-tmv       to   fil-des-tmv            .
           move      rf-yvp-pwd-tmv       to   fil-pwd-tmv            .
           move      rf-yvp-cau-car       to   fil-cau-car            .
           move      rf-yvp-cmc-car       to   fil-cmc-car            .
           move      rf-yvp-cau-sca       to   fil-cau-sca            .
           move      rf-yvp-cmc-sca       to   fil-cmc-sca            .
           move      rf-yvp-tde-dib       to   fil-tde-dib            .
           move      rf-yvp-snx-cad       to   fil-snx-cad            .
           move      rf-yvp-cau-cad       to   fil-cau-cad            .
           move      rf-yvp-cmc-cad       to   fil-cmc-cad            .
           move      rf-yvp-org-doc       to   fil-org-doc            .
           move      rf-yvp-prv-doc       to   fil-prv-doc            .
           move      rf-yvp-sgl-num       to   fil-sgl-num            .
           move      rf-yvp-mov-afd       to   fil-mov-afd            .
           move      rf-yvp-chi-com       to   fil-chi-com            .
           move      rf-yvp-obl-afd       to   fil-obl-afd            .
           move      rf-yvp-def-tmf       to   fil-def-tmf            .
           move      rf-yvp-mod-tmf       to   fil-mod-tmf            .
           move      rf-yvp-alx-gen       to   fil-alx-gen            .
           move      rf-yvp-cod-dsl       to   fil-cod-dsl            .
           move      rf-yvp-alx-dpz       to   fil-alx-dpz            .
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
           move      rf-yvp-cod-tmv       to   fil-cod-tmv            .
           move      rf-yvp-cod-dpz       to   fil-cod-dpz            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-yvp-des-key       to   fil-des-key            .
           move      rf-yvp-cod-tmv       to   fil-cod-tmv-2          .
           move      rf-yvp-cod-dpz       to   fil-cod-dpz-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-yvp                 .
           move      fil-cod-tmv          to   rf-yvp-cod-tmv         .
           move      fil-cod-dpz          to   rf-yvp-cod-dpz         .
           move      fil-des-key          to   rf-yvp-des-key         .
           move      fil-des-tmv          to   rf-yvp-des-tmv         .
           move      fil-pwd-tmv          to   rf-yvp-pwd-tmv         .
           move      fil-cau-car          to   rf-yvp-cau-car         .
           move      fil-cmc-car          to   rf-yvp-cmc-car         .
           move      fil-cau-sca          to   rf-yvp-cau-sca         .
           move      fil-cmc-sca          to   rf-yvp-cmc-sca         .
           move      fil-tde-dib          to   rf-yvp-tde-dib         .
           move      fil-snx-cad          to   rf-yvp-snx-cad         .
           move      fil-cau-cad          to   rf-yvp-cau-cad         .
           move      fil-cmc-cad          to   rf-yvp-cmc-cad         .
           move      fil-org-doc          to   rf-yvp-org-doc         .
           move      fil-prv-doc          to   rf-yvp-prv-doc         .
           move      fil-sgl-num          to   rf-yvp-sgl-num         .
           move      fil-mov-afd          to   rf-yvp-mov-afd         .
           move      fil-chi-com          to   rf-yvp-chi-com         .
           move      fil-obl-afd          to   rf-yvp-obl-afd         .
           move      fil-def-tmf          to   rf-yvp-def-tmf         .
           move      fil-mod-tmf          to   rf-yvp-mod-tmf         .
           move      fil-alx-gen          to   rf-yvp-alx-gen         .
           move      fil-cod-dsl          to   rf-yvp-cod-dsl         .
           move      fil-alx-dpz          to   rf-yvp-alx-dpz         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-yvp               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-yvp
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

