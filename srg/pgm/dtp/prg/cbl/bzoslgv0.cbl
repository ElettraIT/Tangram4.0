       Identification Division.
       Program-Id.                                 bzoslgv0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dtp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/12/93    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la selezione secondo l'area di   *
      *                    interfaccia per l'ordinamento e la sele-    *
      *                    zione del file [lgv]                        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *                                                                *
      *       Input         : f-ope  : "OP"                            *
      *                                                                *
      *                                                                *
      *       Output        : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *                                                                *
      *       Input         : f-ope  : "CL"                            *
      *                                                                *
      *                                                                *
      *       Output        : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ST"  Esecuzione ordinamento                                   *
      *                                                                *
      *                                                                *
      *       Input         : f-ope  : "ST"                            *
      *                                                                *
      *           l-zos-lgc-tip-ord  : Tipo di ordinamento             *
      *                                                                *
      *           l-zos-lgc-cod-min  : Codice alfanumerico minimo      *
      *                                                                *
      *           l-zos-lgc-cod-max  : Codice alfanumerico massimo     *
      *                                                                *
      *           l-zos-lgc-des-min  : Descrizione minima              *
      *                                                                *
      *           l-zos-lgc-des-max  : Descrizione massima             *
      *                                                                *
      *           l-zos-lgc-sin-min  : Sinonimo minimo                 *
      *                                                                *
      *           l-zos-lgc-sin-max  : Sinonimo massimo                *
      *                                                                *
      *                                                                *
      *       Output        : f-sts  : e-not-err : OK                  *
      *                                e-end-fil = Nessun record       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "RN"  Read Next record successivo                              *
      *                                                                *
      *                                                                *
      *       Input         : f-ope  : "RN"                            *
      *                                                                *
      *                                                                *
      *       Output        : f-sts  : e-not-err : OK                  *
      *                                e-end-fil = At End              *
      *                                                                *
      *                       rf-lgv : Record [lgv]                    *
      *                                                                *
      *       -------------------------------------------------------- *
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
      *    * File Control [sss]                                        *
      *    *-----------------------------------------------------------*
           select            sss   assign to sort                     .

      *    *===========================================================*
      *    * File Control [ttt]                                        *
      *    *-----------------------------------------------------------*
           select            ttt   assign to input-output   f-ttt-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-ttt-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * Sort file per [lgv]                                       *
      *    *-----------------------------------------------------------*
       sd  sss.
           copy      "pgm/dtp/fls/rec/rflgv"
                                     replacing rf-lgv
                                          by   rf-sss                 .

      *    *===========================================================*
      *    * Work file per uscita da sort di [lgv]                     *
      *    *-----------------------------------------------------------*
       fd  ttt  label record omitted.
           copy      "pgm/dtp/fls/rec/rflgv"
                                     replacing rf-lgv
                                          by   rf-ttt                 .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [ttt]                *
      *    *-----------------------------------------------------------*
       01  f-ttt.
           05  f-ttt-nam                  pic  x(04) value spaces     .
           05  f-ttt-pat                  pic  x(40) value spaces     .
           05  f-ttt-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [lgv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"                          .

      *    *===========================================================*
      *    * Work-area per i parametri di ordinamento e selezione      *
      *    *-----------------------------------------------------------*
       01  w-zos-lgv.
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento                                   *
      *        *  - 01 : Per codice alfanumerico                       *
      *        *  - 02 : Per descrizione                               *
      *        *  - 03 : Per sinonimo                                  *
      *        *-------------------------------------------------------*
           05  w-zos-lgv-tip-ord          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico minimo e massimo                  *
      *        *-------------------------------------------------------*
           05  w-zos-lgv-cod-min          pic  x(14)                  .
           05  w-zos-lgv-cod-max          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione minima e massima                          *
      *        *-------------------------------------------------------*
           05  w-zos-lgv-des-min          pic  x(40)                  .
           05  w-zos-lgv-des-max          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sinonimo minimo e massimo                             *
      *        *-------------------------------------------------------*
           05  w-zos-lgv-sin-min          pic  x(13)                  .
           05  w-zos-lgv-sin-max          pic  x(13)                  .

      *    *===========================================================*
      *    * Work-area per la funzione di ordinamento e selezione per  *
      *    * mezzo dei parametri                                       *
      *    *-----------------------------------------------------------*
       01  w-ord-sel.
      *        *-------------------------------------------------------*
      *        * Status del work file [ttt]                            *
      *        * - 00 : Chiuso                                         *
      *        * - 01 : Aperto in output                               *
      *        * - 02 : Aperto in input                                *
      *        *-------------------------------------------------------*
           05  w-ord-sel-sts-ttt          pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Contatore numero records effettivamente selezionati e *
      *        * scritti                                               *
      *        *-------------------------------------------------------*
           05  w-ord-sel-max-scr          pic  9(07) value zero       .
      *        *-------------------------------------------------------*
      *        * Contatore numero records effettivamente riletti       *
      *        *-------------------------------------------------------*
           05  w-ord-sel-max-let          pic  9(07) value zero       .
      *        *-------------------------------------------------------*
      *        * Flag di At End in rilettura                           *
      *        *-------------------------------------------------------*
           05  w-ord-sel-flg-end          pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Flag di selezione mediante parametri del record letto *
      *        *-------------------------------------------------------*
           05  w-ord-sel-flg-srp          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per padding campi alfanumerici                  *
      *    *-----------------------------------------------------------*
       01  w-pad-alf.
           05  w-pad-alf-max              pic  9(02)                  .
           05  w-pad-alf-cdp              pic  x(01)                  .
           05  w-pad-alf-str.
               10  w-pad-alf-chr occurs 80
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per pathname unico file temporaneo [ttt]        *
      *    *-----------------------------------------------------------*
       01  w-pat-uni-ttt.
      *        *-------------------------------------------------------*
      *        * Pathname unico                                        *
      *        *-------------------------------------------------------*
           05  w-pat-uni-ttt-pat          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Numero di Open Output eseguite sul file               *
      *        *-------------------------------------------------------*
           05  w-pat-uni-ttt-noo          pic  9(07)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione specifica per il modulo 'bzoslgv0'  *
      *    *-----------------------------------------------------------*
       01  l-zos-lgv.
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento                                   *
      *        *  - C : Per codice alfanumerico                        *
      *        *  - D : Per descrizione                                *
      *        *  - S : Per sinonimo                                   *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-tip-ord          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico minimo e massimo                  *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-cod-min          pic  x(14)                  .
           05  l-zos-lgv-cod-max          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione minima e massima                          *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-des-min          pic  x(40)                  .
           05  l-zos-lgv-des-max          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sinonimo minimo e massimo                             *
      *        *-------------------------------------------------------*
           05  l-zos-lgv-sin-min          pic  x(13)                  .
           05  l-zos-lgv-sin-max          pic  x(13)                  .

      *    *===========================================================*
      *    * Record file [lgv]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"
                                     replacing rf-lgv
                                          by   rf-lll                 .

      ******************************************************************
       Procedure Division                using f
                                               l-zos-lgv
                                               rf-lll                 .
      ******************************************************************

      *================================================================*
      *      Declaratives                                              *
      *================================================================*
       Declaratives.
       Decl Section.
           Use after standard error procedure on ttt                  .
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-ttt-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-ttt                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-ttt-sts            to   e-sts                  .
       End Declaratives.

      *================================================================*
      *       Main                                                     *
      *================================================================*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        f-ope                =    "OP"
                     perform   opn-000    thru opn-999
           else if   f-ope                =    "CL"
                     perform   cls-000    thru cls-999
           else if   f-ope                =    "ST"
                     perform   str-000    thru str-999
           else if   f-ope                =    "RN"
                     perform   rnx-000    thru rnx-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Richiesta di un pathname unico temporaneo per   *
      *              * il file di transito [ttt] ed azzeramento nume-  *
      *              * ro di Open Output sullo stesso                  *
      *              *-------------------------------------------------*
           perform   pat-uni-ttt-000      thru pat-uni-ttt-999        .
      *              *-------------------------------------------------*
      *              * Status del work file [ttt] : Chiuso             *
      *              *-------------------------------------------------*
           move      00                   to   w-ord-sel-sts-ttt      .
       opn-100.
      *              *-------------------------------------------------*
      *              * Open [lgv]                                      *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
       opn-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      e-not-err            to   f-sts                  .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Pathname unico temporaneo per il file di transito [ttt]   *
      *    *-----------------------------------------------------------*
       pat-uni-ttt-000.
      *              *-------------------------------------------------*
      *              * Richiesta di un pathname unico alla segreteria  *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione pathname unico ottenuto          *
      *              *-------------------------------------------------*
           move      s-pat                to   w-pat-uni-ttt-pat      .
      *              *-------------------------------------------------*
      *              * Numero di Open Output eseguite sul file tempo-  *
      *              * raneo a zero                                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-pat-uni-ttt-noo      .
       pat-uni-ttt-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Chiusura work file [ttt]                        *
      *              *-------------------------------------------------*
           if        w-ord-sel-sts-ttt    =    01
                     go to cls-005
           else if   w-ord-sel-sts-ttt    =    02
                     go to cls-010
           else      go to cls-050.
       cls-005.
           perform   cls-out-ttt-000      thru cls-out-ttt-999        .
           go to     cls-050.
       cls-010.
           perform   cls-inp-ttt-000      thru cls-inp-ttt-999        .
           go to     cls-050.
       cls-050.
      *              *-------------------------------------------------*
      *              * Cancellazione work-file [ttt]                   *
      *              *-------------------------------------------------*
           if        w-pat-uni-ttt-noo    =    zero
                     go to cls-100.
           move      "PD"                 to   s-ope                  .
           move      w-pat-uni-ttt-pat    to   s-pat                  .
           move      "S"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       cls-100.
      *              *-------------------------------------------------*
      *              * Close [lgv]                                     *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
       cls-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      e-not-err            to   f-sts                  .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Start                                                     *
      *    *-----------------------------------------------------------*
       str-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione parametri di ordinamento e sele- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           perform   mem-prm-sel-000      thru mem-prm-sel-999        .
      *              *-------------------------------------------------*
      *              * Ordinamento e selezione [lgv] su parametri      *
      *              *-------------------------------------------------*
           perform   ord-sel-prm-000      thru ord-sel-prm-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita : se numero    *
      *              * records effettivamente selezionati a zero si    *
      *              * segnala errore, altrimenti si segnala Ok        *
      *              *-------------------------------------------------*
           if        w-ord-sel-max-scr    =    zero
                     move  e-end-fil      to   f-sts
           else      move  e-not-err      to   f-sts                  .
       str-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione parametri di ordinamento e selezione       *
      *    *-----------------------------------------------------------*
       mem-prm-sel-000.
      *              *-------------------------------------------------*
      *              * Memorizzazioni                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento                         *
      *                  *---------------------------------------------*
           if        l-zos-lgv-tip-ord    =    "C"
                     move  01             to   w-zos-lgv-tip-ord
           else if   l-zos-lgv-tip-ord    =    "D"
                     move  02             to   w-zos-lgv-tip-ord
           else if   l-zos-lgv-tip-ord    =    "S"
                     move  03             to   w-zos-lgv-tip-ord
           else      move  zero           to   w-zos-lgv-tip-ord      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico minimo                  *
      *                  *---------------------------------------------*
           move      l-zos-lgv-cod-min    to   w-zos-lgv-cod-min      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico massimo                 *
      *                  *---------------------------------------------*
           move      l-zos-lgv-cod-max    to   w-zos-lgv-cod-max      .
      *                  *---------------------------------------------*
      *                  * Descrizione minima                          *
      *                  *---------------------------------------------*
           move      l-zos-lgv-des-min    to   w-zos-lgv-des-min      .
      *                  *---------------------------------------------*
      *                  * Descrizione massima                         *
      *                  *---------------------------------------------*
           move      l-zos-lgv-des-max    to   w-zos-lgv-des-max      .
      *                  *---------------------------------------------*
      *                  * Sinonimo minimo                             *
      *                  *---------------------------------------------*
           move      l-zos-lgv-sin-min    to   w-zos-lgv-sin-min      .
      *                  *---------------------------------------------*
      *                  * Sinonimo massimo                            *
      *                  *---------------------------------------------*
           move      l-zos-lgv-sin-max    to   w-zos-lgv-sin-max      .
       mem-prm-sel-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
       mem-prm-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento                            *
      *                  *---------------------------------------------*
           if        w-zos-lgv-tip-ord    not  = 01 and
                     w-zos-lgv-tip-ord    not  = 02 and
                     w-zos-lgv-tip-ord    not  = 03
                     move  01             to   w-zos-lgv-tip-ord      .
       mem-prm-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico massimo                 *
      *                  *---------------------------------------------*
           if        w-zos-lgv-cod-max    not  = spaces
                     go to mem-prm-sel-400.
           if        w-zos-lgv-cod-min    =    spaces
                     move  all "z"        to   w-zos-lgv-cod-max
           else      move  w-zos-lgv-cod-min
                                          to   w-zos-lgv-cod-max      .
       mem-prm-sel-400.
      *                  *---------------------------------------------*
      *                  * Descrizione massima                         *
      *                  *---------------------------------------------*
           if        w-zos-lgv-des-max    =    spaces
                     if    w-zos-lgv-des-min
                                          =    spaces
                           move  all "z"  to   w-zos-lgv-des-max
                     else  move  w-zos-lgv-des-min
                                          to   w-zos-lgv-des-max      .
           move      "z"                  to   w-pad-alf-cdp          .
           move      40                   to   w-pad-alf-max          .
           move      w-zos-lgv-des-max    to   w-pad-alf-str          .
           perform   pad-alf-cdp-000      thru pad-alf-cdp-999        .
           move      w-pad-alf-str        to   w-zos-lgv-des-max      .
       mem-prm-sel-500.
      *                  *---------------------------------------------*
      *                  * Sinonimo massimo                            *
      *                  *---------------------------------------------*
           if        w-zos-lgv-sin-max    not  = spaces
                     go to mem-prm-sel-900.
           if        w-zos-lgv-sin-min    =    spaces
                     move  all "z"        to   w-zos-lgv-sin-max
           else      move  w-zos-lgv-sin-min
                                          to   w-zos-lgv-sin-max      .
       mem-prm-sel-900.
      *              *-------------------------------------------------*
      *              * Uscita da routine di memorizzazione             *
      *              *-------------------------------------------------*
           go to     mem-prm-sel-999.
       mem-prm-sel-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [lgv] su parametri                *
      *    *-----------------------------------------------------------*
       ord-sel-prm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale del numero records ef- *
      *              * fettivamente selezionati                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-ord-sel-max-scr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale del numero records ef- *
      *              * fettivamente riletti                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-ord-sel-max-let      .
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale del flag di At End in  *
      *              * rilettura                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ord-sel-flg-end      .
       ord-sel-prm-025.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri implicita- *
      *              * mente indicano una selezione vuota ; se si : u- *
      *              * scita                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice alfanumerico min-max         *
      *                  *---------------------------------------------*
           if        w-zos-lgv-cod-max    <    w-zos-lgv-cod-min
                     go to ord-sel-prm-999.
      *                  *---------------------------------------------*
      *                  * Test su descrizione min-max                 *
      *                  *---------------------------------------------*
           if        w-zos-lgv-des-max    <    w-zos-lgv-des-min
                     go to ord-sel-prm-999.
      *                  *---------------------------------------------*
      *                  * Test su sinonimo min-max                    *
      *                  *---------------------------------------------*
           if        w-zos-lgv-sin-max    <    w-zos-lgv-sin-min
                     go to ord-sel-prm-999.
       ord-sel-prm-050.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri implicita- *
      *              * mente indicano una selezione composta da un so- *
      *              * lo elemento ; in caso affermativo si seleziona  *
      *              * questo elemento e lo si salva nell'area sort.   *
      *              *-------------------------------------------------*
       ord-sel-prm-055.
      *                  *---------------------------------------------*
      *                  * Se il codice alfanumerico minimo e' diverso *
      *                  * dal codice alfanumerico massimo non si puo' *
      *                  * avere questa certezza e si prosegue pertan- *
      *                  * to con il normale metodo di selezione       *
      *                  *---------------------------------------------*
           if        w-zos-lgv-cod-min    not  = w-zos-lgv-cod-max
                     go to ord-sel-prm-500.
      *                  *---------------------------------------------*
      *                  * Se il codice alfanumerico minimo e' a spa-  *
      *                  * ces ed anche il codice alfanumerico massimo *
      *                  * e' a spaces non si puo' avere questa cer-   *
      *                  * tezza e si prosegue pertanto con il norma-  *
      *                  * le metodo di selezione                      *
      *                  *---------------------------------------------*
           if        w-zos-lgv-cod-min    =    spaces and
                     w-zos-lgv-cod-max    =    spaces
                     go to ord-sel-prm-500.
       ord-sel-prm-060.
      *                  *---------------------------------------------*
      *                  * Lettura del record relativo all'unico ele-  *
      *                  * mento                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start per codice alfanumerico su [lgv]  *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "ALFLGV    "         to   f-key                  .
           move      w-zos-lgv-cod-min    to   rf-lgv-alf-lgv of
                                               rf-lgv                 .
           move      zero                 to   rf-lgv-num-lgv of
                                               rf-lgv                 .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita per selezione  *
      *                      * vuota                                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-prm-999.
      *                      *-----------------------------------------*
      *                      * Read Next su [lgv]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *                      *-----------------------------------------*
      *                      * Se At End : uscita per selezione vuota  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-prm-999.
      *                      *-----------------------------------------*
      *                      * Test Max, se errore : uscita per sele-  *
      *                      * zione vuota                             *
      *                      *-----------------------------------------*
           if        rf-lgv-alf-lgv of
                     rf-lgv               not  = w-zos-lgv-cod-min
                     go to ord-sel-prm-999.
       ord-sel-prm-065.
      *                  *---------------------------------------------*
      *                  * Selezione, mediante parametri, del record   *
      *                  * letto, se non superata : uscita per sele-   *
      *                  * zione vuota                                 *
      *                  *---------------------------------------------*
           perform   sel-rec-prm-000      thru sel-rec-prm-999        .
           if        w-ord-sel-flg-srp    not  = spaces
                     go to ord-sel-prm-999.
       ord-sel-prm-070.
      *                  *---------------------------------------------*
      *                  * Se l'unico record da selezionare esiste ed  *
      *                  * ha superato la selezione                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Mumero records effettivamente selezio-  *
      *                      * nati : pari a 1                         *
      *                      *-----------------------------------------*
           move      1                    to   w-ord-sel-max-scr      .
      *                      *-----------------------------------------*
      *                      * Salvataggio del record in area sort     *
      *                      *-----------------------------------------*
           move      rf-lgv               to   rf-sss                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ord-sel-prm-999.
       ord-sel-prm-500.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo ordinamento       *
      *              *-------------------------------------------------*
           if        w-zos-lgv-tip-ord    =    01
                     go to ord-sel-prm-510
           else if   w-zos-lgv-tip-ord    =    02
                     go to ord-sel-prm-520
           else if   w-zos-lgv-tip-ord    =    03
                     go to ord-sel-prm-530.
       ord-sel-prm-510.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 01 : Codice alfanumerico  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-001-000      thru ord-sel-001-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-prm-999.
       ord-sel-prm-520.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 02 : Descrizione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-002-000      thru ord-sel-002-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-prm-999.
       ord-sel-prm-530.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 03 : Sinonimo             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-003-000      thru ord-sel-003-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-prm-999.
       ord-sel-prm-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [lgv]     Tipo ordinamento  01    *
      *    *                                                           *
      *    * Per codice alfanumerico                                   *
      *    *-----------------------------------------------------------*
       ord-sel-001-000.
      *              *-------------------------------------------------*
      *              * Open output file [ttt]                          *
      *              *-------------------------------------------------*
           perform   opn-out-ttt-000      thru opn-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
       ord-sel-001-100.
      *              *-------------------------------------------------*
      *              * Start su [lgv] per codice alfanumerico          *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "ALFLGV    "         to   f-key                  .
           move      w-zos-lgv-cod-min    to   rf-lgv-alf-lgv of
                                               rf-lgv                 .
           move      zero                 to   rf-lgv-num-lgv of
                                               rf-lgv                 .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
       ord-sel-001-200.
      *              *-------------------------------------------------*
      *              * Read Next su [lgv]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
       ord-sel-001-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-lgv-alf-lgv of
                     rf-lgv               >    w-zos-lgv-cod-max
                     go to ord-sel-001-900.
       ord-sel-001-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante parametri, del record let-  *
      *              * to, se non superata : riciclo a Read Next       *
      *              *-------------------------------------------------*
           perform   sel-rec-prm-000      thru sel-rec-prm-999        .
           if        w-ord-sel-flg-srp    not  = spaces
                     go to ord-sel-001-200.
       ord-sel-001-500.
      *              *-------------------------------------------------*
      *              * Se record da includere nella selezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records effettivamente    *
      *                  * selezionati                                 *
      *                  *---------------------------------------------*
           add       1                    to   w-ord-sel-max-scr      .
      *                  *---------------------------------------------*
      *                  * Se e' il primo record : salvataggio in area *
      *                  * di sort                                     *
      *                  *---------------------------------------------*
           if        w-ord-sel-max-scr    not  = 1
                     go to ord-sel-001-600.
           move      rf-lgv               to   rf-sss                 .
       ord-sel-001-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-lgv               to   rf-ttt                 .
      *                  *---------------------------------------------*
      *                  * Scrittura su work file                      *
      *                  *---------------------------------------------*
           perform   wrt-out-ttt-000      thru wrt-out-ttt-999        .
      *                  *---------------------------------------------*
      *                  * Se errori : numero di records effettivamen- *
      *                  * te selezionati a zero e ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-ord-sel-max-scr
                     go to ord-sel-001-900.
      *                  *---------------------------------------------*
      *                  * Se Ok : riciclo a Read Next                 *
      *                  *---------------------------------------------*
           go to     ord-sel-001-200.
       ord-sel-001-900.
      *              *-------------------------------------------------*
      *              * Close output file [ttt]                         *
      *              *-------------------------------------------------*
           perform   cls-out-ttt-000      thru cls-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : numero records effettivamente sele- *
      *              * zionati a zero                                  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-ord-sel-max-scr      .
       ord-sel-001-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [lgv]     Tipo ordinamento  01    *
      *    *                                                           *
      *    * Per descrizione                                           *
      *    *-----------------------------------------------------------*
       ord-sel-002-000.
      *              *-------------------------------------------------*
      *              * Open output file [ttt]                          *
      *              *-------------------------------------------------*
           perform   opn-out-ttt-000      thru opn-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
       ord-sel-002-100.
      *              *-------------------------------------------------*
      *              * Start su [lgv] per descrizione                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "DESKEY    "         to   f-key                  .
           move      w-zos-lgv-des-min    to   rf-lgv-des-key of
                                               rf-lgv                 .
           move      zero                 to   rf-lgv-num-lgv of
                                               rf-lgv                 .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
       ord-sel-002-200.
      *              *-------------------------------------------------*
      *              * Read Next su [lgv]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
       ord-sel-002-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-lgv-des-key of
                     rf-lgv               >    w-zos-lgv-des-max
                     go to ord-sel-002-900.
       ord-sel-002-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante parametri, del record let-  *
      *              * to, se non superata : riciclo a Read Next       *
      *              *-------------------------------------------------*
           perform   sel-rec-prm-000      thru sel-rec-prm-999        .
           if        w-ord-sel-flg-srp    not  = spaces
                     go to ord-sel-002-200.
       ord-sel-002-500.
      *              *-------------------------------------------------*
      *              * Se record da includere nella selezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records effettivamente    *
      *                  * selezionati                                 *
      *                  *---------------------------------------------*
           add       1                    to   w-ord-sel-max-scr      .
      *                  *---------------------------------------------*
      *                  * Se e' il primo record : salvataggio in area *
      *                  * di sort                                     *
      *                  *---------------------------------------------*
           if        w-ord-sel-max-scr    not  = 1
                     go to ord-sel-002-600.
           move      rf-lgv               to   rf-sss                 .
       ord-sel-002-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-lgv               to   rf-ttt                 .
      *                  *---------------------------------------------*
      *                  * Scrittura su work file                      *
      *                  *---------------------------------------------*
           perform   wrt-out-ttt-000      thru wrt-out-ttt-999        .
      *                  *---------------------------------------------*
      *                  * Se errori : numero di records effettivamen- *
      *                  * te selezionati a zero e ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-ord-sel-max-scr
                     go to ord-sel-002-900.
      *                  *---------------------------------------------*
      *                  * Se Ok : riciclo a Read Next                 *
      *                  *---------------------------------------------*
           go to     ord-sel-002-200.
       ord-sel-002-900.
      *              *-------------------------------------------------*
      *              * Close output file [ttt]                         *
      *              *-------------------------------------------------*
           perform   cls-out-ttt-000      thru cls-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : numero records effettivamente sele- *
      *              * zionati a zero                                  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-ord-sel-max-scr      .
       ord-sel-002-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [lgv]     Tipo ordinamento  01    *
      *    *                                                           *
      *    * Per descrizione                                           *
      *    *-----------------------------------------------------------*
       ord-sel-003-000.
      *              *-------------------------------------------------*
      *              * Open output file [ttt]                          *
      *              *-------------------------------------------------*
           perform   opn-out-ttt-000      thru opn-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-100.
      *              *-------------------------------------------------*
      *              * Start su [lgv] per descrizione                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SYNLGV    "         to   f-key                  .
           move      w-zos-lgv-sin-min    to   rf-lgv-syn-lgv of
                                               rf-lgv                 .
           move      zero                 to   rf-lgv-num-lgv of
                                               rf-lgv                 .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-200.
      *              *-------------------------------------------------*
      *              * Read Next su [lgv]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dtp/fls/ioc/obj/ioflgv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lgv                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-lgv-syn-lgv of
                     rf-lgv               >    w-zos-lgv-sin-max
                     go to ord-sel-003-900.
       ord-sel-003-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante parametri, del record let-  *
      *              * to, se non superata : riciclo a Read Next       *
      *              *-------------------------------------------------*
           perform   sel-rec-prm-000      thru sel-rec-prm-999        .
           if        w-ord-sel-flg-srp    not  = spaces
                     go to ord-sel-003-200.
       ord-sel-003-500.
      *              *-------------------------------------------------*
      *              * Se record da includere nella selezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero records effettivamente    *
      *                  * selezionati                                 *
      *                  *---------------------------------------------*
           add       1                    to   w-ord-sel-max-scr      .
      *                  *---------------------------------------------*
      *                  * Se e' il primo record : salvataggio in area *
      *                  * di sort                                     *
      *                  *---------------------------------------------*
           if        w-ord-sel-max-scr    not  = 1
                     go to ord-sel-003-600.
           move      rf-lgv               to   rf-sss                 .
       ord-sel-003-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-lgv               to   rf-ttt                 .
      *                  *---------------------------------------------*
      *                  * Scrittura su work file                      *
      *                  *---------------------------------------------*
           perform   wrt-out-ttt-000      thru wrt-out-ttt-999        .
      *                  *---------------------------------------------*
      *                  * Se errori : numero di records effettivamen- *
      *                  * te selezionati a zero e ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-ord-sel-max-scr
                     go to ord-sel-003-900.
      *                  *---------------------------------------------*
      *                  * Se Ok : riciclo a Read Next                 *
      *                  *---------------------------------------------*
           go to     ord-sel-003-200.
       ord-sel-003-900.
      *              *-------------------------------------------------*
      *              * Close output file [ttt]                         *
      *              *-------------------------------------------------*
           perform   cls-out-ttt-000      thru cls-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : numero records effettivamente sele- *
      *              * zionati a zero                                  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  zero           to   w-ord-sel-max-scr      .
       ord-sel-003-999.
           exit.

      *    *===========================================================*
      *    * Selezione, mediante parametri, del record in 'rf-lgv'     *
      *    *-----------------------------------------------------------*
       sel-rec-prm-000.
      *              *-------------------------------------------------*
      *              * Selezione su codice alfanumerico min-max        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per codice alfanumerico *
      *                  * : no selezione                              *
      *                  *---------------------------------------------*
           if        w-zos-lgv-tip-ord    =    01
                     go to sel-rec-prm-100.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-lgv-alf-lgv of
                     rf-lgv               <    w-zos-lgv-cod-min or
                     rf-lgv-alf-lgv of
                     rf-lgv               >    w-zos-lgv-cod-max
                     go to sel-rec-prm-900.
       sel-rec-prm-100.
      *              *-------------------------------------------------*
      *              * Selezione su descrizione uppercase min-max      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per descrizione : no    *
      *                  * selezione                                   *
      *                  *---------------------------------------------*
           if        w-zos-lgv-tip-ord    =    02
                     go to sel-rec-prm-200.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-lgv-des-key of
                     rf-lgv               <    w-zos-lgv-des-min or
                     rf-lgv-des-key of
                     rf-lgv               >    w-zos-lgv-des-max
                     go to sel-rec-prm-900.
       sel-rec-prm-200.
      *              *-------------------------------------------------*
      *              * Selezione su sinonimo min-max                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per sinonimo : no       *
      *                  * selezione                                   *
      *                  *---------------------------------------------*
           if        w-zos-lgv-tip-ord    =    03
                     go to sel-rec-prm-300.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-lgv-syn-lgv of
                     rf-lgv               <    w-zos-lgv-sin-min or
                     rf-lgv-syn-lgv of
                     rf-lgv               >    w-zos-lgv-sin-max
                     go to sel-rec-prm-900.
       sel-rec-prm-300.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ok                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-ord-sel-flg-srp      .
           go to     sel-rec-prm-999.
       sel-rec-prm-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ko                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ord-sel-flg-srp      .
           go to     sel-rec-prm-999.
       sel-rec-prm-999.
           exit.

      *    *===========================================================*
      *    * Read Next                                                 *
      *    *-----------------------------------------------------------*
       rnx-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del numero di letture fino *
      *              * ad ora eseguite                                 *
      *              *-------------------------------------------------*
           if        w-ord-sel-max-let    =    zero
                     go to rnx-100
           else      go to rnx-500.
       rnx-100.
      *              *-------------------------------------------------*
      *              * Se numero di letture fino ad ora eseguite pari  *
      *              * a zero, cioe' si e' alla prima richiesta        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero records   *
      *                  * effettivamente selezionati                  *
      *                  *---------------------------------------------*
           if        w-ord-sel-max-scr    =    zero
                     go to rnx-200
           else if   w-ord-sel-max-scr    =    1
                     go to rnx-300
           else      go to rnx-400.
       rnx-200.
      *                  *---------------------------------------------*
      *                  * Se numero records effettivamente seleziona- *
      *                  * ti pari a zero                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di At End in rilettura : On        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-ord-sel-flg-end
      *                      *-----------------------------------------*
      *                      * Status in uscita ad : At End            *
      *                      *-----------------------------------------*
           move      e-end-fil            to   f-sts                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rnx-999.
       rnx-300.
      *                  *---------------------------------------------*
      *                  * Se numero records effettivamente seleziona- *
      *                  * ti pari a 1                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spostamento record da area sort ad area *
      *                      * di link                                 *
      *                      *-----------------------------------------*
           move      rf-sss               to   rf-lll                 .
      *                      *-----------------------------------------*
      *                      * Incremento numero records fino ad ora   *
      *                      * letti                                   *
      *                      *-----------------------------------------*
           add       1                    to   w-ord-sel-max-let      .
      *                      *-----------------------------------------*
      *                      * Status in uscita ad : Ok                *
      *                      *-----------------------------------------*
           move      e-not-err            to   f-sts                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rnx-999.
       rnx-400.
      *                  *---------------------------------------------*
      *                  * Se numero records effettivamente seleziona- *
      *                  * ti superiore a 1                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Open input file [ttt]                   *
      *                      *-----------------------------------------*
           perform   opn-inp-ttt-000      thru opn-inp-ttt-999        .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se errori o no     *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to rnx-425
           else      go to rnx-450.
       rnx-425.
      *                      *-----------------------------------------*
      *                      * Se nessun errore in Open Input          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A lettura da file                   *
      *                          *-------------------------------------*
           go to     rnx-700.
       rnx-450.
      *                      *-----------------------------------------*
      *                      * Se errori in Open Input                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Close input file [ttt]              *
      *                          *-------------------------------------*
           perform   opn-inp-ttt-000      thru opn-inp-ttt-999        .
      *                          *-------------------------------------*
      *                          * Flag di At End in rilettura : On    *
      *                          *-------------------------------------*
           move      "#"                  to   w-ord-sel-flg-end
      *                          *-------------------------------------*
      *                          * Status in uscita ad : At End        *
      *                          *-------------------------------------*
           move      e-end-fil            to   f-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     rnx-999.
       rnx-500.
      *              *-------------------------------------------------*
      *              * Se numero di letture fino ad ora eseguite supe- *
      *              * riore a zero, cioe' si e' ad una richiesta suc- *
      *              * cessiva alla prima                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero records   *
      *                  * effettivamente selezionati                  *
      *                  *---------------------------------------------*
           if        w-ord-sel-max-scr    =    1
                     go to rnx-600
           else      go to rnx-700.
       rnx-600.
      *                  *---------------------------------------------*
      *                  * Se numero records effettivamente seleziona- *
      *                  * ti pari a 1                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di At End in rilettura : On        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-ord-sel-flg-end
      *                      *-----------------------------------------*
      *                      * Status in uscita ad : At End            *
      *                      *-----------------------------------------*
           move      e-end-fil            to   f-sts                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rnx-999.
       rnx-700.
      *                  *---------------------------------------------*
      *                  * Se numero records effettivamente seleziona- *
      *                  * ti superiore a 1                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Raed input file [ttt]                   *
      *                      *-----------------------------------------*
           perform   rea-inp-ttt-000      thru rea-inp-ttt-999        .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se errori o no     *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to rnx-725
           else      go to rnx-750.
       rnx-725.
      *                      *-----------------------------------------*
      *                      * Se nessun errore in Read Input          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Spostamento record da area work ad  *
      *                          * area di link                        *
      *                          *-------------------------------------*
           move      rf-ttt               to   rf-lll                 .
      *                          *-------------------------------------*
      *                          * Incremento numero records fino ad   *
      *                          * ora letti                           *
      *                          *-------------------------------------*
           add       1                    to   w-ord-sel-max-let      .
      *                          *-------------------------------------*
      *                          * Status in uscita ad : Ok            *
      *                          *-------------------------------------*
           move      e-not-err            to   f-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     rnx-999.
       rnx-750.
      *                      *-----------------------------------------*
      *                      * Se errori in Read Input                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Close input file [ttt]              *
      *                          *-------------------------------------*
           perform   opn-inp-ttt-000      thru opn-inp-ttt-999        .
      *                          *-------------------------------------*
      *                          * Flag di At End in rilettura : On    *
      *                          *-------------------------------------*
           move      "#"                  to   w-ord-sel-flg-end
      *                          *-------------------------------------*
      *                          * Status in uscita ad : At End        *
      *                          *-------------------------------------*
           move      e-end-fil            to   f-sts                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     rnx-999.
       rnx-999.
           exit.

      *    *===========================================================*
      *    * Open output work file [ttt]                               *
      *    *-----------------------------------------------------------*
       opn-out-ttt-000.
      *              *-------------------------------------------------*
      *              * Se lo status del work file [ttt] indica che es- *
      *              * so e' aperto, lo si chiude                      *
      *              *-------------------------------------------------*
           if        w-ord-sel-sts-ttt    =    01
                     go to opn-out-ttt-020
           else if   w-ord-sel-sts-ttt    =    02
                     go to opn-out-ttt-040
           else      go to opn-out-ttt-100.
       opn-out-ttt-020.
           perform   cls-out-ttt-000      thru cls-out-ttt-999        .
           if        f-sts                =    e-not-err
                     go to opn-out-ttt-100
           else      go to opn-out-ttt-999.
       opn-out-ttt-040.
           perform   cls-inp-ttt-000      thru cls-inp-ttt-999        .
           if        f-sts                =    e-not-err
                     go to opn-out-ttt-100
           else      go to opn-out-ttt-999.
       opn-out-ttt-100.
      *              *-------------------------------------------------*
      *              * Status del work file [ttt] : Open Output        *
      *              *-------------------------------------------------*
           move      01                   to   w-ord-sel-sts-ttt      .
      *              *-------------------------------------------------*
      *              * Preparazione name [ttt]                         *
      *              *-------------------------------------------------*
           move      "ttt "               to   f-ttt-nam              .
      *              *-------------------------------------------------*
      *              * Preparazione pathname [ttt] da pathname otte-   *
      *              * nuto inizialmeente dalla segreteria             *
      *              *-------------------------------------------------*
           move      w-pat-uni-ttt-pat    to   f-ttt-pat              .
       opn-out-ttt-200.
      *              *-------------------------------------------------*
      *              * Esecuzione Open                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione cobol i-o status            *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           open      output ttt                                       .
      *                  *---------------------------------------------*
      *                  * Spostamento cobol i-o status in uscita      *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
       opn-out-ttt-400.
      *              *-------------------------------------------------*
      *              * Incremento numero di Open Output eseguite sul   *
      *              * file temporaneo di appoggio [ttt]               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se errore in Open : no incremento           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to opn-out-ttt-999.
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       1                    to   w-pat-uni-ttt-noo      .
       opn-out-ttt-999.
           exit.

      *    *===========================================================*
      *    * Close output work file [ttt]                              *
      *    *-----------------------------------------------------------*
       cls-out-ttt-000.
      *              *-------------------------------------------------*
      *              * Status del work file [ttt] : Chiuso             *
      *              *-------------------------------------------------*
           move      00                   to   w-ord-sel-sts-ttt      .
      *              *-------------------------------------------------*
      *              * Esecuzione Close                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione cobol i-o status            *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           close     ttt                                              .
      *                  *---------------------------------------------*
      *                  * Spostamento cobol i-o status in uscita      *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
       cls-out-ttt-999.
           exit.

      *    *===========================================================*
      *    * Write output work file [ttt]                              *
      *    *-----------------------------------------------------------*
       wrt-out-ttt-000.
      *              *-------------------------------------------------*
      *              * Esecuzione Write                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione cobol i-o status            *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           write     rf-ttt                                           .
      *                  *---------------------------------------------*
      *                  * Spostamento cobol i-o status in uscita      *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
       wrt-out-ttt-999.
           exit.

      *    *===========================================================*
      *    * Open input work file [ttt]                                *
      *    *-----------------------------------------------------------*
       opn-inp-ttt-000.
      *              *-------------------------------------------------*
      *              * Se lo status del work file [ttt] indica che es- *
      *              * so e' aperto, lo si chiude                      *
      *              *-------------------------------------------------*
           if        w-ord-sel-sts-ttt    =    01
                     go to opn-inp-ttt-020
           else if   w-ord-sel-sts-ttt    =    02
                     go to opn-inp-ttt-040
           else      go to opn-inp-ttt-100.
       opn-inp-ttt-020.
           perform   cls-out-ttt-000      thru cls-out-ttt-999        .
           if        f-sts                =    e-not-err
                     go to opn-inp-ttt-100
           else      go to opn-inp-ttt-999.
       opn-inp-ttt-040.
           perform   cls-inp-ttt-000      thru cls-inp-ttt-999        .
           if        f-sts                =    e-not-err
                     go to opn-inp-ttt-100
           else      go to opn-inp-ttt-999.
       opn-inp-ttt-100.
      *              *-------------------------------------------------*
      *              * Status del work file [ttt] : Open Input         *
      *              *-------------------------------------------------*
           move      02                   to   w-ord-sel-sts-ttt      .
      *              *-------------------------------------------------*
      *              * Esecuzione Open                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione cobol i-o status            *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           open      input  ttt                                       .
      *                  *---------------------------------------------*
      *                  * Spostamento cobol i-o status in uscita      *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
       opn-inp-ttt-999.
           exit.

      *    *===========================================================*
      *    * Close input work file [ttt]                               *
      *    *-----------------------------------------------------------*
       cls-inp-ttt-000.
      *              *-------------------------------------------------*
      *              * Status del work file [ttt] : Chiuso             *
      *              *-------------------------------------------------*
           move      00                   to   w-ord-sel-sts-ttt      .
      *              *-------------------------------------------------*
      *              * Esecuzione Close                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione cobol i-o status            *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           close     ttt                                              .
      *                  *---------------------------------------------*
      *                  * Spostamento cobol i-o status in uscita      *
      *                  *---------------------------------------------*
           move      e-sts                to   f-sts                  .
       cls-inp-ttt-999.
           exit.

      *    *===========================================================*
      *    * Read input work file [ttt]                                *
      *    *-----------------------------------------------------------*
       rea-inp-ttt-000.
      *              *-------------------------------------------------*
      *              * Esecuzione Read                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione cobol i-o status            *
      *                  *---------------------------------------------*
           move      "00"                 to   e-sts                  .
      *                  *---------------------------------------------*
      *                  * Operazione di i-o                           *
      *                  *---------------------------------------------*
           read      ttt    at end
                            go to rea-inp-ttt-400.
       rea-inp-ttt-200.
      *              *-------------------------------------------------*
      *              * Spostamento cobol i-o status in uscita          *
      *              *-------------------------------------------------*
           move      e-sts                to   f-sts                  .
      *              *-------------------------------------------------*
      *              * Se errori : uscita                              *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to rea-inp-ttt-999.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-inp-ttt-999.
       rea-inp-ttt-400.
      *              *-------------------------------------------------*
      *              * Se At End                                       *
      *              *-------------------------------------------------*
           move      e-end-fil            to   f-sts                  .
       rea-inp-ttt-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per padding finale campi alfanumerici :        *
      *    * - Con carattere di padding   'w-pad-alf-cdp'              *
      *    * - Per il numero di caratteri 'w-pad-alf-max'              *
      *    *-----------------------------------------------------------*
       pad-alf-cdp-000.
           if        w-pad-alf-max        >    zero
                     if    w-pad-alf-chr
                          (w-pad-alf-max) =    spaces
                           move  w-pad-alf-cdp
                                          to   w-pad-alf-chr
                                              (w-pad-alf-max)
                           subtract  1    from w-pad-alf-max
                           go to pad-alf-cdp-000.
       pad-alf-cdp-999.
           exit.
