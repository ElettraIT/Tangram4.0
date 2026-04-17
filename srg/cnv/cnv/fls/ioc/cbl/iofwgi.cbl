       Identification Division.
       Program-Id.                                 iofwgi             .
      *================================================================*
      *                                                                *
      *                  Input-Output File wgi (ex 'moi')              *
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
      *            * Chiave numero 01 : DATPRT                         *
      *            *---------------------------------------------------*
               10  fil-k01.
                   15  fil-dat-reg        pic  9(07)       comp-3     .
                   15  fil-num-prt        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATDOC                         *
      *            *---------------------------------------------------*
               10  fil-k02.
                   15  fil-tip-rec        pic  x(01)                  .
                   15  fil-dat-reg-2      pic  9(07)       comp-3     .
                   15  fil-cod-num        pic  9(02)                  .
                   15  fil-prt-iva        pic  9(11)       comp-3     .
                   15  fil-num-prt-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-dat-doc            pic  9(07)       comp-3     .
               10  fil-num-doc            pic  x(10)                  .
               10  fil-cod-cau            pic  9(03)       comp-3     .
               10  fil-tip-iva            pic  x(01)                  .
               10  fil-cst-iva   occurs 06.
                   15  fil-ibl-iva        pic s9(11)       comp-3     .
                   15  fil-cod-iva        pic  9(05)       comp-3     .
                   15  fil-imp-iva        pic s9(11)       comp-3     .
               10  fil-tot-doc            pic s9(11)       comp-3     .
               10  fil-cod-arc            pic  9(07)       comp-3     .
               10  fil-flg-gio            pic  x(01)                  .
               10  fil-flg-cop            pic  x(01)                  .
               10  fil-flg-rfp            pic  x(01)                  .
               10  fil-cod-fbd            pic  9(07)                  .
               10  fil-sgl-num            pic  x(03)                  .
               10  fil-dat-acq            pic  9(07)                  .
               10  fil-alx-exp.
                   15  filler  occurs 61  pic  x(01)                  .

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
      *            * Chiave numero 01 : DATPRT                         *
      *            *---------------------------------------------------*
               10  pul-k01.
                   15  pul-dat-reg        pic  9(07)       comp-3     .
                   15  pul-num-prt        pic  9(07)       comp-3     .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : DATDOC                         *
      *            *---------------------------------------------------*
               10  pul-k02.
                   15  pul-tip-rec        pic  x(01)                  .
                   15  pul-dat-reg-2      pic  9(07)       comp-3     .
                   15  pul-cod-num        pic  9(02)                  .
                   15  pul-prt-iva        pic  9(11)       comp-3     .
                   15  pul-num-prt-2      pic  9(07)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-dat-doc            pic  9(07)       comp-3     .
               10  pul-num-doc            pic  x(10)                  .
               10  pul-cod-cau            pic  9(03)       comp-3     .
               10  pul-tip-iva            pic  x(01)                  .
               10  pul-cst-iva   occurs 06.
                   15  pul-ibl-iva        pic s9(11)       comp-3     .
                   15  pul-cod-iva        pic  9(05)       comp-3     .
                   15  pul-imp-iva        pic s9(11)       comp-3     .
               10  pul-tot-doc            pic s9(11)       comp-3     .
               10  pul-cod-arc            pic  9(07)       comp-3     .
               10  pul-flg-gio            pic  x(01)                  .
               10  pul-flg-cop            pic  x(01)                  .
               10  pul-flg-rfp            pic  x(01)                  .
               10  pul-cod-fbd            pic  9(07)                  .
               10  pul-sgl-num            pic  x(03)                  .
               10  pul-dat-acq            pic  9(07)                  .
               10  pul-alx-exp.
                   15  filler  occurs 61  pic  x(01)                  .

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
                     "wgi "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "cnv/cnv/fls/ioc/obj/iofwgi              "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

           copy      "swd/ske/iof/iofcp30"                            .

      *    *===========================================================*
      *    * Area Lunghezza record in bytes ed Elenco chiavi previste  *
      *    *-----------------------------------------------------------*
       01  k.
      *        *-------------------------------------------------------*
      *        * Numero chiavi di accesso                              *
      *        *-------------------------------------------------------*
           05  k-ctr                      pic  9(02) value   2        .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATPRT"                                  .
      *            *---------------------------------------------------*
      *            * Nome chiave numero 2                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "DATDOC"                                  .
      *        *-------------------------------------------------------*
      *        * Ridefinizione nomi chiavi di accesso                  *
      *        *-------------------------------------------------------*
           05  k-ely redefines
               k-elx.
               10  k-ele occurs    2      pic  x(10)                  .

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
      *    * Record logico file [wgi]                                  *
      *    *-----------------------------------------------------------*
           copy      "cnv/cnv/fls/rec/rfwgi"                          .

      ******************************************************************
       Procedure Division                using f rf-wgi               .
      ******************************************************************

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
           move      spaces               to   rf-wgi                 .
           move      spaces               to   rf-wgi-tip-rec         .
           move      zero                 to   rf-wgi-dat-reg         .
           move      zero                 to   rf-wgi-num-prt         .
           move      zero                 to   rf-wgi-dat-doc         .
           move      spaces               to   rf-wgi-num-doc         .
           move      zero                 to   rf-wgi-cod-num         .
           move      zero                 to   rf-wgi-prt-iva         .
           move      zero                 to   rf-wgi-cod-cau         .
           move      spaces               to   rf-wgi-tip-iva         .
           move      zero                 to   w-inx                  .
       nor-rec-log-100.
           add       1                    to   w-inx                  .
      *
           if        w-inx                >    6
                     go to nor-rec-log-200.
      *
           move      zero                 to   rf-wgi-ibl-iva (w-inx)
                                               rf-wgi-cod-iva (w-inx)
                                               rf-wgi-imp-iva (w-inx) .
      *
           go to     nor-rec-log-100.
       nor-rec-log-200.
           move      zero                 to   rf-wgi-tot-doc         .
           move      zero                 to   rf-wgi-cod-arc         .
           move      spaces               to   rf-wgi-flg-gio         .
           move      spaces               to   rf-wgi-flg-cop         .
           move      spaces               to   rf-wgi-flg-rfp         .
           move      zero                 to   rf-wgi-cod-fbd         .
           move      spaces               to   rf-wgi-sgl-num         .
           move      zero                 to   rf-wgi-dat-acq         .
           move      spaces               to   rf-wgi-alx-exp         .
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
           move      rf-wgi-dat-doc       to   fil-dat-doc            .
           move      rf-wgi-num-doc       to   fil-num-doc            .
           move      rf-wgi-cod-cau       to   fil-cod-cau            .
           move      rf-wgi-tip-iva       to   fil-tip-iva            .
           move      zero                 to   w-inx                  .
       cmp-log-fis-110.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to cmp-log-fis-120.
           move      rf-wgi-ibl-iva (w-inx)  
                                          to   fil-ibl-iva (w-inx)    .          
           move      rf-wgi-cod-iva (w-inx)  
                                          to   fil-cod-iva (w-inx)    .          
           move      rf-wgi-imp-iva (w-inx)  
                                          to   fil-imp-iva (w-inx)    .          
           go to     cmp-log-fis-110.
       cmp-log-fis-120.
           move      rf-wgi-tot-doc       to   fil-tot-doc            .
           move      rf-wgi-cod-arc       to   fil-cod-arc            .
           move      rf-wgi-flg-gio       to   fil-flg-gio            .
           move      rf-wgi-flg-cop       to   fil-flg-cop            .
           move      rf-wgi-flg-rfp       to   fil-flg-rfp            .
           move      rf-wgi-cod-fbd       to   fil-cod-fbd            .
           move      rf-wgi-sgl-num       to   fil-sgl-num            .
           move      rf-wgi-dat-acq       to   fil-dat-acq            .
           move      rf-wgi-alx-exp       to   fil-alx-exp            .
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
           move      rf-wgi-dat-reg       to   fil-dat-reg            .
           move      rf-wgi-num-prt       to   fil-num-prt            .
           go to     cmp-key-fis-999.
       cmp-key-fis-200.
      *              *-------------------------------------------------*
      *              * Composizione chiave indice 2                    *
      *              *-------------------------------------------------*
           move      spaces               to   fil-k02                .
           move      rf-wgi-tip-rec       to   fil-tip-rec            .
           move      rf-wgi-dat-reg       to   fil-dat-reg-2          .
           move      rf-wgi-cod-num       to   fil-cod-num            .
           move      rf-wgi-prt-iva       to   fil-prt-iva            .
           move      rf-wgi-num-prt       to   fil-num-prt-2          .
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-wgi                 .
           move      fil-tip-rec          to   rf-wgi-tip-rec         .
           move      fil-dat-reg          to   rf-wgi-dat-reg         .
           move      fil-num-prt          to   rf-wgi-num-prt         .
           move      fil-dat-doc          to   rf-wgi-dat-doc         .
           move      fil-num-doc          to   rf-wgi-num-doc         .
           move      fil-cod-num          to   rf-wgi-cod-num         .
           move      fil-prt-iva          to   rf-wgi-prt-iva         .
           move      fil-cod-cau          to   rf-wgi-cod-cau         .
           move      fil-tip-iva          to   rf-wgi-tip-iva         .
           move      zero                 to   w-inx                  .
       dec-fis-log-100.
           add       1                    to   w-inx                  .
           if        w-inx                >    6
                     go to dec-fis-log-200.
           move      fil-ibl-iva (w-inx)  to   rf-wgi-ibl-iva (w-inx) .          
           move      fil-cod-iva (w-inx)  to   rf-wgi-cod-iva (w-inx) .          
           move      fil-imp-iva (w-inx)  to   rf-wgi-imp-iva (w-inx) .          
           go to     dec-fis-log-100.
       dec-fis-log-200.
           move      fil-tot-doc          to   rf-wgi-tot-doc         .
           move      fil-cod-arc          to   rf-wgi-cod-arc         .
           move      fil-flg-gio          to   rf-wgi-flg-gio         .
           move      fil-flg-cop          to   rf-wgi-flg-cop         .
           move      fil-flg-rfp          to   rf-wgi-flg-rfp         .
           move      fil-cod-fbd          to   rf-wgi-cod-fbd         .
           move      fil-sgl-num          to   rf-wgi-sgl-num         .
           move      fil-dat-acq          to   rf-wgi-dat-acq         .
           move      fil-alx-exp          to   rf-wgi-alx-exp         .
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-wgi               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-wgi
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

