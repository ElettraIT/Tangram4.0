       Identification Division.
       Program-Id.                                 iofzsb             .
      *================================================================*
      *                                                                *
      *                  Input-Output File zsb                         *
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
      *            * Chiave numero 01 : TIPPAG                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-tip-pag        pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-tau-spb            pic  9(02)                  .
               10  fil-per-spb            pic  9(02)v9(01)            .
               10  fil-tbs-spb.
                   15  fil-tbe-spb occurs 10.
                       20  fil-tbe-scg    pic  9(11)                  .
                       20  fil-tbe-asc    pic  9(11)                  .
                       20  fil-tbe-psc    pic  9(02)v9(01)            .
               10  fil-tet-spb            pic  9(11)                  .
               10  fil-min-spb            pic  9(11)                  .
               10  fil-max-spb            pic  9(11)                  .
               10  fil-tar-spb            pic  9(02)                  .
               10  fil-var-spb            pic  9(07)                  .
               10  fil-alx-exp            pic  x(80)                  .

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
      *            * Chiave numero 01 : TIPPAG                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-tip-pag        pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-tau-spb            pic  9(02)                  .
               10  pul-per-spb            pic  9(02)v9(01)            .
               10  pul-tbs-spb.
                   15  pul-tbe-spb occurs 10.
                       20  pul-tbe-scg    pic  9(11)                  .
                       20  pul-tbe-asc    pic  9(11)                  .
                       20  pul-tbe-psc    pic  9(02)v9(01)            .
               10  pul-tet-spb            pic  9(11)                  .
               10  pul-min-spb            pic  9(11)                  .
               10  pul-max-spb            pic  9(11)                  .
               10  pul-tar-spb            pic  9(02)                  .
               10  pul-var-spb            pic  9(07)                  .
               10  pul-alx-exp            pic  x(80)                  .

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
                     "zsb "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/gep/fls/ioc/obj/iofzsb              "       .

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
                            "TIPPAG"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    1      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w.
           05  w-inx                      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record logico file [zsb]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzsb"                          .

      ******************************************************************
       Procedure Division                using f rf-zsb               .
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
           move      spaces               to   rf-zsb                 .
           move      zero                 to   rf-zsb-tip-pag         .
           move      zero                 to   rf-zsb-tau-spb         .
           move      zero                 to   rf-zsb-per-spb         .
           move      zero                 to   w-inx                  .
       nor-rec-log-100.
           add       1                    to   w-inx                  .
           if        w-inx                >    10
                     go to nor-rec-log-120.
           move      zero                 to   rf-zsb-tbe-scg (w-inx) .
           move      zero                 to   rf-zsb-tbe-asc (w-inx) .
           move      zero                 to   rf-zsb-tbe-psc (w-inx) .
           go to     nor-rec-log-100.
       nor-rec-log-120.
           move      zero                 to   rf-zsb-tet-spb         .
           move      zero                 to   rf-zsb-min-spb         .
           move      zero                 to   rf-zsb-max-spb         .
           move      zero                 to   rf-zsb-tar-spb         .
           move      zero                 to   rf-zsb-var-spb         .
           move      spaces               to   rf-zsb-alx-exp         .
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
           move      spaces               to   fil-dat                .
           move      rf-zsb-tau-spb       to   fil-tau-spb            .
           move      rf-zsb-per-spb       to   fil-per-spb            .
           move      zero                 to   w-inx                  .
       cmp-log-fis-200.
           add       1                    to   w-inx                  .
           if        w-inx                >    10
                     go to cmp-log-fis-220.
           move      rf-zsb-tbe-scg (w-inx)
                                          to   fil-tbe-scg (w-inx)    .
           move      rf-zsb-tbe-asc (w-inx)
                                          to   fil-tbe-asc (w-inx)    .
           move      rf-zsb-tbe-psc (w-inx)
                                          to   fil-tbe-psc (w-inx)    .
           go to     cmp-log-fis-200.
       cmp-log-fis-220.
           move      rf-zsb-tet-spb       to   fil-tet-spb            .
           move      rf-zsb-min-spb       to   fil-min-spb            .
           move      rf-zsb-max-spb       to   fil-max-spb            .
           move      rf-zsb-tar-spb       to   fil-tar-spb            .
           move      rf-zsb-var-spb       to   fil-var-spb            .
           move      rf-zsb-alx-exp       to   fil-alx-exp            .
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
           move      rf-zsb-tip-pag       to   fil-tip-pag            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-zsb                 .
           move      fil-tip-pag          to   rf-zsb-tip-pag         .
           move      fil-tau-spb          to   rf-zsb-tau-spb         .
           move      fil-per-spb          to   rf-zsb-per-spb         .
           move      zero                 to   w-inx                  .
       dec-fis-log-100.
           add       1                    to   w-inx                  .
           if        w-inx                >    10
                     go to dec-fis-log-120.
           move      fil-tbe-scg (w-inx)  to   rf-zsb-tbe-scg (w-inx) .
           move      fil-tbe-asc (w-inx)  to   rf-zsb-tbe-asc (w-inx) .
           move      fil-tbe-psc (w-inx)  to   rf-zsb-tbe-psc (w-inx) .
           go to     dec-fis-log-100.
       dec-fis-log-120.
           move      fil-tet-spb          to   rf-zsb-tet-spb         .
           move      fil-min-spb          to   rf-zsb-min-spb         .
           move      fil-max-spb          to   rf-zsb-max-spb         .
           move      fil-tar-spb          to   rf-zsb-tar-spb         .
           move      fil-var-spb          to   rf-zsb-var-spb         .
           move      fil-var-spb          to   rf-zsb-var-spb         .
           move      fil-alx-exp          to   rf-zsb-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-zsb               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-zsb
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

