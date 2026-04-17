       Identification Division.
       Program-Id.                                 iofbix             .
      *================================================================*
      *                                                                *
      *                  Input-Output File bix                         *
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
                   15  fil-tip-rec        pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  fil-val-rec.
                   15  fil-rec-001.
                       20  fil-dti-ids.
                           25  fil-rag-ids
                                          pic  x(40)                  .
                           25  fil-via-ids
                                          pic  x(40)                  .
                           25  fil-loc-ids
                                          pic  x(40)                  .
                       20  fil-dti-vet.
                           25  fil-rag-vet
                                          pic  x(40)                  .
                           25  fil-via-vet
                                          pic  x(40)                  .
                           25  fil-loc-vet
                                          pic  x(40)                  .
                       20  fil-ant-dti.
                           25  fil-des-ant.
                               30  fil-rig-ant occurs 03
                                          pic  x(40)                  .
                       20  fil-rs2-ids    pic  x(40)                  .
                   15  fil-rec-011        redefines
                       fil-rec-001.
                       20  fil-des-400.
                           25  fil-rig-400 occurs 10
                                          pic  x(40)                  .

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
                   15  pul-tip-rec        pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  pul-dat.
               10  pul-val-rec.
                   15  pul-rec-001.
                       20  pul-dti-ids.
                           25  pul-rag-ids
                                          pic  x(40)                  .
                           25  pul-via-ids
                                          pic  x(40)                  .
                           25  pul-loc-ids
                                          pic  x(40)                  .
                       20  pul-dti-vet.
                           25  pul-rag-vet
                                          pic  x(40)                  .
                           25  pul-via-vet
                                          pic  x(40)                  .
                           25  pul-loc-vet
                                          pic  x(40)                  .
                       20  pul-ant-dti.
                           25  pul-des-ant.
                               30  pul-rig-ant occurs 03
                                          pic  x(40)                  .
                       20  pul-rs2-ids    pic  x(40)                  .
                   15  pul-rec-011        redefines
                       pul-rec-001.
                       20  pul-des-400.
                           25  pul-rig-400 occurs 10
                                          pic  x(40)                  .

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
                     "bix "                                           .
      *        *-------------------------------------------------------*
      *        * Pathname completo del modulo oggetto                  *
      *        *-------------------------------------------------------*
           02  i-ide-pmo                  pic  x(40) value
                     "pgm/bol/fls/ioc/obj/iofbix              "       .

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
           05  k-ctr                      pic  9(02) value 1          .
      *        *-------------------------------------------------------*
      *        * Nomi chiavi di accesso                                *
      *        *-------------------------------------------------------*
           05  k-elx.
      *            *---------------------------------------------------*
      *            * Nome chiave numero 1                              *
      *            *---------------------------------------------------*
               10  filler                 pic  x(10) value
                            "NUMPRT    "                              .
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
      *    * Record logico file [bix]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbix"                          .

      ******************************************************************
       Procedure Division                using f rf-bix               .
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
           move      e-not-fnd            to   f-sts                  .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione record logico                             *
      *    *-----------------------------------------------------------*
       nor-rec-log-000.
           move      spaces               to   rf-bix                 .
           move      zero                 to   rf-bix-num-prt         .
           move      zero                 to   rf-bix-num-prg         .
           move      zero                 to   rf-bix-tip-rec         .
           move      spaces               to   rf-bix-rag-ids         .
           move      spaces               to   rf-bix-rs2-ids         .
           move      spaces               to   rf-bix-via-ids         .
           move      spaces               to   rf-bix-loc-ids         .
           move      spaces               to   rf-bix-rag-vet         .
           move      spaces               to   rf-bix-via-vet         .
           move      spaces               to   rf-bix-loc-vet         .
           move      spaces               to   rf-bix-des-ant         .
           move      spaces               to   rf-bix-des-400         .
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
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo record      *
      *                  *---------------------------------------------*
           if        rf-bix-tip-rec       =    01
                     go to cmp-log-fis-110
           else if   rf-bix-tip-rec       =    11
                     go to cmp-log-fis-120
           else      go to cmp-log-fis-999.
       cmp-log-fis-110.
      *                  *---------------------------------------------*
      *                  * Tipo record : 01                            *
      *                  *---------------------------------------------*
           move      rf-bix-rag-ids       to   fil-rag-ids            .
           move      rf-bix-via-ids       to   fil-via-ids            .
           move      rf-bix-loc-ids       to   fil-loc-ids            .
           move      rf-bix-rag-vet       to   fil-rag-vet            .
           move      rf-bix-via-vet       to   fil-via-vet            .
           move      rf-bix-loc-vet       to   fil-loc-vet            .
           move      rf-bix-des-ant       to   fil-des-ant            .
           move      rf-bix-rs2-ids       to   fil-rs2-ids            .
           go to     cmp-log-fis-999.
       cmp-log-fis-120.
      *                  *---------------------------------------------*
      *                  * Tipo record : 11                            *
      *                  *---------------------------------------------*
           move      rf-bix-des-400       to   fil-des-400            .
           go to     cmp-log-fis-999.
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
           move      rf-bix-num-prt       to   fil-num-prt            .
           move      rf-bix-num-prg       to   fil-num-prg            .
           move      rf-bix-tip-rec       to   fil-tip-rec            .
           go to     cmp-key-fis-999.
       cmp-key-fis-999.
           exit.

      *    *===========================================================*
      *    * Decomposizione record da fisico a logico                  *
      *    *-----------------------------------------------------------*
       dec-fis-log-000.
           move      spaces               to   rf-bix                 .
           move      fil-num-prt          to   rf-bix-num-prt         .
           move      fil-num-prg          to   rf-bix-num-prg         .
           move      fil-tip-rec          to   rf-bix-tip-rec         .
       dec-fis-log-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo record      *
      *                  *---------------------------------------------*
           if        fil-tip-rec          =    01
                     go to dec-fis-log-110
           else if   fil-tip-rec          =    11
                     go to dec-fis-log-120
           else      go to dec-fis-log-999.
       dec-fis-log-110.
      *                  *---------------------------------------------*
      *                  * Tipo record : 01                            *
      *                  *---------------------------------------------*
           move      fil-rag-ids          to   rf-bix-rag-ids         .
           move      fil-via-ids          to   rf-bix-via-ids         .
           move      fil-loc-ids          to   rf-bix-loc-ids         .
           move      fil-rag-vet          to   rf-bix-rag-vet         .
           move      fil-via-vet          to   rf-bix-via-vet         .
           move      fil-loc-vet          to   rf-bix-loc-vet         .
           move      fil-des-ant          to   rf-bix-des-ant         .
           move      fil-rs2-ids          to   rf-bix-rs2-ids         .
           go to     dec-fis-log-999.
       dec-fis-log-120.
      *                  *---------------------------------------------*
      *                  * Tipo record : 11                            *
      *                  *---------------------------------------------*
           move      fil-des-400          to   rf-bix-des-400         .
           go to     dec-fis-log-999.
       dec-fis-log-999.
           exit.

      *    *===========================================================*
      *    * Unstring record logico in porzioni da 80 caratteri in     *
      *    * s-alf, per la funzione di sequenzializzazione             *
      *    *-----------------------------------------------------------*
       uns-rec-log-000.
           unstring  rf-bix               into s-alf
                                  with pointer z-inx                  .
       uns-rec-log-999.
           exit.

      *    *===========================================================*
      *    * String record logico in porzioni da 80 caratteri da s-alf *
      *    * per la funzione di indicizzazione                         *
      *    *-----------------------------------------------------------*
       stg-rec-log-000.
           string    s-alf      delimited by   size
                                          into rf-bix
                                  with pointer z-inx                  .
       stg-rec-log-999.
           exit.

