       Identification Division.
       Program-Id.                                 iofupr             .
      *================================================================*
      *                                                                *
      *                  Input-Output File upr                         *
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
                             file status  is                f-fil-sts .

      *    *===========================================================*
      *    * File Control [pul]                                        *
      *    *-----------------------------------------------------------*
           select  optional  pul   assign to disk           f-pul-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is pul-k01
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
      *            * Chiave numero 01 : UTECOD                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-cod-ute        pic  x(08)                  .
                   15  fil-cod-upr        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-des-upr            pic  x(40)                  .
               10  fil-txt-upr.
                   15  fil-txt-rig occurs 10
                                          pic  x(40)                  .
               10  fil-mdr-upr            pic  9(02)                  .
               10  fil-mda-upr            pic  9(02)                  .
               10  fil-dat-upr            pic  9(07)       comp-3     .
               10  fil-gio-set            pic  9(02)                  .
               10  fil-gio-rif            pic  9(02)                  .
               10  fil-mes-rif            pic  9(02)                  .
               10  fil-ngp-upr            pic  9(03)       comp-3     .
               10  fil-mdp-upr            pic  9(02)                  .
               10  fil-drt-upr            pic  9(02)                  .
               10  fil-dfi-upr            pic  9(07)       comp-3     .
               10  fil-dpv-upr            pic  9(07)       comp-3     .
               10  fil-snx-acp            pic  x(01)                  .
               10  fil-snx-pvs            pic  x(01)                  .
               10  fil-snx-scf            pic  x(01)                  .
               10  fil-alx-exp.
                   15  filler occurs  78  pic  x(01)                  .

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
      *            * Chiave numero 01 : UTECOD                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-cod-ute        pic  x(08)                  .
                   15  pul-cod-upr        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-des-upr            pic  x(40)                  .
               10  pul-txt-upr.
                   15  pul-txt-rig occurs 10
                                          pic  x(40)                  .
               10  pul-mdr-upr            pic  9(02)                  .
               10  pul-mda-upr            pic  9(02)                  .
               10  pul-dat-upr            pic  9(07)       comp-3     .
               10  pul-gio-set            pic  9(02)                  .
               10  pul-gio-rif            pic  9(02)                  .
               10  pul-mes-rif            pic  9(02)                  .
               10  pul-ngp-upr            pic  9(03)       comp-3     .
               10  pul-mdp-upr            pic  9(02)                  .
               10  pul-drt-upr            pic  9(02)                  .
               10  pul-dfi-upr            pic  9(07)       comp-3     .
               10  pul-dpv-upr            pic  9(07)       comp-3     .
               10  pul-snx-acp            pic  x(01)                  .
               10  pul-snx-pvs            pic  x(01)                  .
               10  pul-snx-scf            pic  x(01)                  .
               10  pul-alx-exp.
                   15  filler occurs  78  pic  x(01)                  .

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
                     "upr "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "swd/xpg/fls/ioc/obj/iofupr              "       .

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
           05  k-ctr                      pic  9(02) value 1          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "UTECOD"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    1      pic  x(10)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [upr]                                  *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/fls/rec/rfupr"                          .

      ******************************************************************
       Procedure Division                using f rf-upr               .
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
           move      spaces               to   rf-upr                 .
           move      spaces               to   rf-upr-cod-ute         .
           move      zero                 to   rf-upr-cod-upr         .
           move      spaces               to   rf-upr-des-upr         .
           move      spaces               to   rf-upr-txt-upr         .
           move      zero                 to   rf-upr-mdr-upr         .
           move      zero                 to   rf-upr-mda-upr         .
           move      zero                 to   rf-upr-dat-upr         .
           move      zero                 to   rf-upr-gio-set         .
           move      zero                 to   rf-upr-gio-rif         .
           move      zero                 to   rf-upr-mes-rif         .
           move      zero                 to   rf-upr-ngp-upr         .
           move      zero                 to   rf-upr-mdp-upr         .
           move      zero                 to   rf-upr-drt-upr         .
           move      zero                 to   rf-upr-dfi-upr         .
           move      zero                 to   rf-upr-dpv-upr         .
           move      spaces               to   rf-upr-snx-acp         .
           move      spaces               to   rf-upr-snx-pvs         .
           move      spaces               to   rf-upr-snx-scf         .
           move      spaces               to   rf-upr-alx-exp         .
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
           move      rf-upr-des-upr       to   fil-des-upr            .
           move      rf-upr-txt-upr       to   fil-txt-upr            .
           move      rf-upr-mdr-upr       to   fil-mdr-upr            .
           move      rf-upr-mda-upr       to   fil-mda-upr            .
           move      rf-upr-dat-upr       to   fil-dat-upr            .
           move      rf-upr-gio-set       to   fil-gio-set            .
           move      rf-upr-gio-rif       to   fil-gio-rif            .
           move      rf-upr-mes-rif       to   fil-mes-rif            .
           move      rf-upr-ngp-upr       to   fil-ngp-upr            .
           move      rf-upr-mdp-upr       to   fil-mdp-upr            .
           move      rf-upr-drt-upr       to   fil-drt-upr            .
           move      rf-upr-dfi-upr       to   fil-dfi-upr            .
           move      rf-upr-dpv-upr       to   fil-dpv-upr            .
           move      rf-upr-snx-acp       to   fil-snx-acp            .
           move      rf-upr-snx-pvs       to   fil-snx-pvs            .
           move      rf-upr-snx-scf       to   fil-snx-scf            .
           move      rf-upr-alx-exp       to   fil-alx-exp            .
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
                     depending            on   z-key                  .
       cmp-key-fis-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 1                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k01                .
           move      rf-upr-cod-ute       to   fil-cod-ute            .
           move      rf-upr-cod-upr       to   fil-cod-upr            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-upr                 .
           move      fil-cod-ute          to   rf-upr-cod-ute         .
           move      fil-cod-upr          to   rf-upr-cod-upr         .
           move      fil-des-upr          to   rf-upr-des-upr         .
           move      fil-txt-upr          to   rf-upr-txt-upr         .
           move      fil-mdr-upr          to   rf-upr-mdr-upr         .
           move      fil-mda-upr          to   rf-upr-mda-upr         .
           move      fil-dat-upr          to   rf-upr-dat-upr         .
           move      fil-gio-set          to   rf-upr-gio-set         .
           move      fil-gio-rif          to   rf-upr-gio-rif         .
           move      fil-mes-rif          to   rf-upr-mes-rif         .
           move      fil-ngp-upr          to   rf-upr-ngp-upr         .
           move      fil-mdp-upr          to   rf-upr-mdp-upr         .
           move      fil-drt-upr          to   rf-upr-drt-upr         .
           move      fil-dfi-upr          to   rf-upr-dfi-upr         .
           move      fil-dpv-upr          to   rf-upr-dpv-upr         .
           move      fil-snx-acp          to   rf-upr-snx-acp         .
           move      fil-snx-pvs          to   rf-upr-snx-pvs         .
           move      fil-snx-scf          to   rf-upr-snx-scf         .
           move      fil-alx-exp          to   rf-upr-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-upr               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-upr
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

