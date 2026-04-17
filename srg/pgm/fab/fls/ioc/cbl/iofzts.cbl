       Identification Division.
       Program-Id.                                 iofzts             .
      *================================================================*
      *                                                                *
      *                  Input-Output File zts                         *
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
      *            * Chiave numero 01 : TIPSTG                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-dpz        pic  9(02)                  .
                   15  fil-tip-stg        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-cod-dpz-2      pic  9(02)                  .
                   15  fil-des-key        pic  x(15)                  .
                   15  fil-tip-stg-2      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-stg            pic  x(15)                  .
               10  fil-com-stg            pic  x(30)                  .
               10  fil-num-pst            pic  9(01)                  .
               10  fil-tbl-pst occurs 4.
                   15  fil-des-pst        pic  x(10)                  .
                   15  fil-mgi-pst        pic  9(04)                  .
                   15  fil-mgf-pst        pic  9(04)                  .
                   15  fil-uv1-pst        pic  x(04)                  .
                   15  fil-uv2-pst        pic  x(04)                  .
               10  fil-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

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
      *            * Chiave numero 01 : TIPSTG                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-dpz        pic  9(02)                  .
                   15  pul-tip-stg        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DESKEY                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-cod-dpz-2      pic  9(02)                  .
                   15  pul-des-key        pic  x(15)                  .
                   15  pul-tip-stg-2      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-stg            pic  x(15)                  .
               10  pul-com-stg            pic  x(30)                  .
               10  pul-num-pst            pic  9(01)                  .
               10  pul-tbl-pst occurs 4.
                   15  pul-des-pst        pic  x(10)                  .
                   15  pul-mgi-pst        pic  9(04)                  .
                   15  pul-mgf-pst        pic  9(04)                  .
                   15  pul-uv1-pst        pic  x(04)                  .
                   15  pul-uv2-pst        pic  x(04)                  .
               10  pul-alx-exp.
                   15  filler occurs 20   pic  x(01)                  .

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
                     "zts "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/fab/fls/ioc/obj/iofzts              "       .

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
                            "TIPSTG    "                              .
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
      *    * Record logico file [zts]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rfzts"                          .

      ******************************************************************
       Procedure Division                using f rf-zts               .
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
           move      spaces               to   rf-zts                 .
           move      zero                 to   rf-zts-cod-dpz         .
           move      spaces               to   rf-zts-tip-stg         .
           move      spaces               to   rf-zts-des-stg         .
           move      spaces               to   rf-zts-des-key         .
           move      spaces               to   rf-zts-com-stg         .
           move      zero                 to   rf-zts-num-pst         .
           move      spaces               to   rf-zts-des-pst (1)     .
           move      zero                 to   rf-zts-mgi-pst (1)     .
           move      zero                 to   rf-zts-mgf-pst (1)     .
           move      spaces               to   rf-zts-uv1-pst (1)     .
           move      spaces               to   rf-zts-uv2-pst (1)     .
           move      spaces               to   rf-zts-des-pst (2)     .
           move      zero                 to   rf-zts-mgi-pst (2)     .
           move      zero                 to   rf-zts-mgf-pst (2)     .
           move      spaces               to   rf-zts-uv1-pst (2)     .
           move      spaces               to   rf-zts-uv2-pst (2)     .
           move      spaces               to   rf-zts-des-pst (3)     .
           move      zero                 to   rf-zts-mgi-pst (3)     .
           move      zero                 to   rf-zts-mgf-pst (3)     .
           move      spaces               to   rf-zts-uv1-pst (3)     .
           move      spaces               to   rf-zts-uv2-pst (3)     .
           move      spaces               to   rf-zts-des-pst (4)     .
           move      zero                 to   rf-zts-mgi-pst (4)     .
           move      zero                 to   rf-zts-mgf-pst (4)     .
           move      spaces               to   rf-zts-uv1-pst (4)     .
           move      spaces               to   rf-zts-uv2-pst (4)     .
           move      spaces               to   rf-zts-alx-exp         .
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
           move      rf-zts-des-stg       to   fil-des-stg            .
           move      rf-zts-com-stg       to   fil-com-stg            .
           move      rf-zts-num-pst       to   fil-num-pst            .
           move      rf-zts-des-pst (1)   to   fil-des-pst (1)        .
           move      rf-zts-mgi-pst (1)   to   fil-mgi-pst (1)        .
           move      rf-zts-mgf-pst (1)   to   fil-mgf-pst (1)        .
           move      rf-zts-uv1-pst (1)   to   fil-uv1-pst (1)        .
           move      rf-zts-uv2-pst (1)   to   fil-uv2-pst (1)        .
           move      rf-zts-des-pst (2)   to   fil-des-pst (2)        .
           move      rf-zts-mgi-pst (2)   to   fil-mgi-pst (2)        .
           move      rf-zts-mgf-pst (2)   to   fil-mgf-pst (2)        .
           move      rf-zts-uv1-pst (2)   to   fil-uv1-pst (2)        .
           move      rf-zts-uv2-pst (2)   to   fil-uv2-pst (2)        .
           move      rf-zts-des-pst (3)   to   fil-des-pst (3)        .
           move      rf-zts-mgi-pst (3)   to   fil-mgi-pst (3)        .
           move      rf-zts-mgf-pst (3)   to   fil-mgf-pst (3)        .
           move      rf-zts-uv1-pst (3)   to   fil-uv1-pst (3)        .
           move      rf-zts-uv2-pst (3)   to   fil-uv2-pst (3)        .
           move      rf-zts-des-pst (4)   to   fil-des-pst (4)        .
           move      rf-zts-mgi-pst (4)   to   fil-mgi-pst (4)        .
           move      rf-zts-mgf-pst (4)   to   fil-mgf-pst (4)        .
           move      rf-zts-uv1-pst (4)   to   fil-uv1-pst (4)        .
           move      rf-zts-uv2-pst (4)   to   fil-uv2-pst (4)        .
           move      rf-zts-alx-exp       to   fil-alx-exp            .
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
           move      rf-zts-cod-dpz       to   fil-cod-dpz            .
           move      rf-zts-tip-stg       to   fil-tip-stg            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-zts-cod-dpz       to   fil-cod-dpz-2          .
           move      rf-zts-des-key       to   fil-des-key            .
           move      rf-zts-tip-stg       to   fil-tip-stg-2          .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-zts                 .
           move      fil-cod-dpz          to   rf-zts-cod-dpz         .
           move      fil-tip-stg          to   rf-zts-tip-stg         .
           move      fil-des-stg          to   rf-zts-des-stg         .
           move      fil-des-key          to   rf-zts-des-key         .
           move      fil-com-stg          to   rf-zts-com-stg         .
           move      fil-num-pst          to   rf-zts-num-pst         .
           move      fil-des-pst (1)      to   rf-zts-des-pst (1)     .
           move      fil-mgi-pst (1)      to   rf-zts-mgi-pst (1)     .
           move      fil-mgf-pst (1)      to   rf-zts-mgf-pst (1)     .
           move      fil-uv1-pst (1)      to   rf-zts-uv1-pst (1)     .
           move      fil-uv2-pst (1)      to   rf-zts-uv2-pst (1)     .
           move      fil-des-pst (2)      to   rf-zts-des-pst (2)     .
           move      fil-mgi-pst (2)      to   rf-zts-mgi-pst (2)     .
           move      fil-mgf-pst (2)      to   rf-zts-mgf-pst (2)     .
           move      fil-uv1-pst (2)      to   rf-zts-uv1-pst (2)     .
           move      fil-uv2-pst (2)      to   rf-zts-uv2-pst (2)     .
           move      fil-des-pst (3)      to   rf-zts-des-pst (3)     .
           move      fil-mgi-pst (3)      to   rf-zts-mgi-pst (3)     .
           move      fil-mgf-pst (3)      to   rf-zts-mgf-pst (3)     .
           move      fil-uv1-pst (3)      to   rf-zts-uv1-pst (3)     .
           move      fil-uv2-pst (3)      to   rf-zts-uv2-pst (3)     .
           move      fil-des-pst (4)      to   rf-zts-des-pst (4)     .
           move      fil-mgi-pst (4)      to   rf-zts-mgi-pst (4)     .
           move      fil-mgf-pst (4)      to   rf-zts-mgf-pst (4)     .
           move      fil-uv1-pst (4)      to   rf-zts-uv1-pst (4)     .
           move      fil-uv2-pst (4)      to   rf-zts-uv2-pst (4)     .
           move      fil-alx-exp          to   rf-zts-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-zts               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-zts
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

