       Identification Division.
       Program-Id.                                 iofbfs             .
      *================================================================*
      *                                                                *
      *                  Input-Output File bfs                         *
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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-num-prt        pic  9(11)       comp-3     .
                   15  fil-num-prg        pic  9(05)       comp-3     .
                   15  fil-num-prr        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-ide-dat            pic  9(07)       comp-3     .
               10  fil-qta-ril            pic s9(10)v9(03) comp-3     .
               10  fil-num-pro            pic  9(07)       comp-3     .
               10  fil-flg-spn            pic  x(01)                  .
               10  fil-cod-rsm            pic  9(05)       comp-3     .
               10  fil-cod-ub1            pic  x(07)                  .
               10  fil-cod-ub2            pic  x(07)                  .
               10  fil-cod-ub3            pic  x(07)                  .
               10  fil-cod-ub4            pic  x(07)                  .
               10  fil-ann-spn            pic  x(80)                  .
               10  fil-cod-ncf            pic  9(03)       comp-3     .
               10  fil-prt-mag            pic  9(11)                  .
               10  fil-drg-mag            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler  occurs 62  pic  x(01)                  .

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
      *            * Chiave numero 01 : NUMPRT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-num-prt        pic  9(11)       comp-3     .
                   15  pul-num-prg        pic  9(05)       comp-3     .
                   15  pul-num-prr        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-ide-dat            pic  9(07)       comp-3     .
               10  pul-qta-ril            pic s9(10)v9(03) comp-3     .
               10  pul-num-pro            pic  9(07)       comp-3     .
               10  pul-flg-spn            pic  x(01)                  .
               10  pul-cod-rsm            pic  9(05)       comp-3     .
               10  pul-cod-ub1            pic  x(07)                  .
               10  pul-cod-ub2            pic  x(07)                  .
               10  pul-cod-ub3            pic  x(07)                  .
               10  pul-cod-ub4            pic  x(07)                  .
               10  pul-ann-spn            pic  x(80)                  .
               10  pul-cod-ncf            pic  9(03)       comp-3     .
               10  pul-prt-mag            pic  9(11)                  .
               10  pul-drg-mag            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler  occurs 62  pic  x(01)                  .

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
                     "bfs "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bfo/fls/ioc/obj/iofbfs              "       .

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
                            "NUMPRT"                                  .
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
      *    * Record logico file [bfs]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfs"                          .

      ******************************************************************
       Procedure Division                using f rf-bfs               .
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
           move      spaces               to   rf-bfs                 .
           move      zero                 to   rf-bfs-num-prt         .
           move      zero                 to   rf-bfs-num-prg         .
           move      zero                 to   rf-bfs-num-prr         .
           move      zero                 to   rf-bfs-ide-dat         .
           move      zero                 to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      spaces               to   rf-bfs-flg-spn         .
           move      zero                 to   rf-bfs-cod-rsm         .
           move      spaces               to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      spaces               to   rf-bfs-ann-spn         .
           move      zero                 to   rf-bfs-cod-ncf         .
           move      zero                 to   rf-bfs-prt-mag         .
           move      zero                 to   rf-bfs-drg-mag         .
           move      spaces               to   rf-bfs-alx-exp         .
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
           move      rf-bfs-ide-dat       to   fil-ide-dat            .
           move      rf-bfs-qta-ril       to   fil-qta-ril            .
           move      rf-bfs-num-pro       to   fil-num-pro            .
           move      rf-bfs-flg-spn       to   fil-flg-spn            .
           move      rf-bfs-cod-rsm       to   fil-cod-rsm            .
           move      rf-bfs-cod-ub1       to   fil-cod-ub1            .
           move      rf-bfs-cod-ub2       to   fil-cod-ub2            .
           move      rf-bfs-cod-ub3       to   fil-cod-ub3            .
           move      rf-bfs-cod-ub4       to   fil-cod-ub4            .
           move      rf-bfs-ann-spn       to   fil-ann-spn            .
           move      rf-bfs-cod-ncf       to   fil-cod-ncf            .
           move      rf-bfs-prt-mag       to   fil-prt-mag            .
           move      rf-bfs-drg-mag       to   fil-drg-mag            .
           move      rf-bfs-alx-exp       to   fil-alx-exp            .
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
           move      rf-bfs-num-prt       to   fil-num-prt            .
           move      rf-bfs-num-prg       to   fil-num-prg            .
           move      rf-bfs-num-prr       to   fil-num-prr            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-bfs                 .
           move      fil-num-prt          to   rf-bfs-num-prt         .
           move      fil-num-prg          to   rf-bfs-num-prg         .
           move      fil-num-prr          to   rf-bfs-num-prr         .
           move      fil-ide-dat          to   rf-bfs-ide-dat         .
           move      fil-qta-ril          to   rf-bfs-qta-ril         .
           move      fil-num-pro          to   rf-bfs-num-pro         .
           move      fil-flg-spn          to   rf-bfs-flg-spn         .
           move      fil-cod-rsm          to   rf-bfs-cod-rsm         .
           move      fil-cod-ub1          to   rf-bfs-cod-ub1         .
           move      fil-cod-ub2          to   rf-bfs-cod-ub2         .
           move      fil-cod-ub3          to   rf-bfs-cod-ub3         .
           move      fil-cod-ub4          to   rf-bfs-cod-ub4         .
           move      fil-ann-spn          to   rf-bfs-ann-spn         .
           move      fil-cod-ncf          to   rf-bfs-cod-ncf         .
           move      fil-prt-mag          to   rf-bfs-prt-mag         .
           move      fil-drg-mag          to   rf-bfs-drg-mag         .
      *
           if        fil-prt-mag          not  numeric
                     move  zero           to   rf-bfs-prt-mag         .   
      *
           if        fil-drg-mag          not  numeric
                     move  zero           to   rf-bfs-drg-mag         .   
      *
           move      fil-alx-exp          to   rf-bfs-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-bfs               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-bfs
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

