       Identification Division.
       Program-Id.                                 bzosdps0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dps                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/08/91    *
      *                       Ultima revisione:    NdK del 15/11/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la selezione secondo il filtro   *
      *                    di ordinamento e selezione per file [dps]   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : f-ope  : "OP"                            *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : f-ope  : "CL"                            *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "TO"  Richiesta tipo ordinamento                               *
      *                                                                *
      *              Input  : f-ope  : "TO"                            *
      *                       f-key  : codice del filtro [dps]         *
      *                                                                *
      *                                                                *
      *              Output : f-sts  : "01" : Per classe, gruppo, sot- *
      *                                       togruppo, codice semila- *
      *                                       vorato                   *
      *                                "02" : Per classe, gruppo, sot- *
      *                                       togruppo, descr. semila- *
      *                                       vorato                   *
      *                                "03" : Per codice semilavorato  *
      *                                "04" : Per descrizione semilav. *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "SE"  Richiesta di sola selezione su record                    *
      *                                                                *
      *              Input  : f-ope  : "SE"                            *
      *                       rf-dps : Record [dps]                    *
      *                                                                *
      *                                                                *
      *              Output : f-sts  : e-not-err : Selezione superata  *
      *                                "01"      : Selezione non su-   *
      *                                            perata              *
      *                                                                *
      *              Nota   : E' necessario avere preventivamente e-   *
      *                       seguito la funzione 'TO'                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ST"  Esecuzione ordinamento                                   *
      *                                                                *
      *              Input  : f-ope  : "ST"                            *
      *                       f-key  : codice del filtro [dps]         *
      *                                                                *
      *                                                                *
      *              Output : f-sts  : e-not-err : OK                  *
      *                                e-end-fil = Nessun record       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "RN"  Read Next record successivo                              *
      *                                                                *
      *              Input  : f-ope  : "RN"                            *
      *                                                                *
      *                                                                *
      *              Output : f-sts  : e-not-err : OK                  *
      *                                e-end-fil = At End              *
      *                                                                *
      *                       rf-dps : Record [dps]                    *
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
      *    * Sort file per [dps]                                       *
      *    *-----------------------------------------------------------*
       sd  sss.
           copy      "pgm/dps/fls/rec/rfdps"
                                     replacing rf-dps
                                          by   rf-sss                 .

      *    *===========================================================*
      *    * Work file per uscita da sort di [dps]                     *
      *    *-----------------------------------------------------------*
       fd  ttt  label record omitted.
           copy      "pgm/dps/fls/rec/rfdps"
                                     replacing rf-dps
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
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [zs1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs1"                          .
      *        *-------------------------------------------------------*
      *        * [zs2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs2"                          .
      *        *-------------------------------------------------------*
      *        * [zs3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs3"                          .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dps '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/wzosdps0.wkl"                   .

      *    *===========================================================*
      *    * Work-area per la lettura del filtro                       *
      *    *-----------------------------------------------------------*
       01  w-let-flt.
      *        *-------------------------------------------------------*
      *        * Area per ridefinizione campo 'f-key' per ottenere il  *
      *        * codice filtro                                         *
      *        *-------------------------------------------------------*
           05  w-let-flt-eff-key.
      *            *---------------------------------------------------*
      *            * Codice filtro                                     *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-flt      pic  9(08)                  .
               10  w-let-flt-cod-r01 redefines
                   w-let-flt-cod-flt.
                   15  w-let-flt-cod-tco  pic  9(01)                  .
                   15  w-let-flt-cod-cco  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Area per salvataggio codice mnemonico del filtro      *
      *        *-------------------------------------------------------*
           05  w-let-flt-mne-flt          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Area per salvataggio descrizione del filtro           *
      *        *-------------------------------------------------------*
           05  w-let-flt-des-flt          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Area per parametri contenuti nel filtro               *
      *        *-------------------------------------------------------*
           05  w-let-flt-prm-flt.
      *            *---------------------------------------------------*
      *            * Tipo ordinamento                                  *
      *            *---------------------------------------------------*
               10  w-let-flt-tip-ord      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice classe merceologica min-max                *
      *            *---------------------------------------------------*
               10  w-let-flt-cla-min      pic  9(05)                  .
               10  w-let-flt-cla-max      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Nr. di sequenza classe merceologica min-max       *
      *            *---------------------------------------------------*
               10  w-let-flt-nsc-min      pic  9(07)                  .
               10  w-let-flt-nsc-max      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice gruppo merceologico min-max                *
      *            *---------------------------------------------------*
               10  w-let-flt-gru-min      pic  9(05)                  .
               10  w-let-flt-gru-max      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Nr. di sequenza gruppo merceologico min-max       *
      *            *---------------------------------------------------*
               10  w-let-flt-nsg-min      pic  9(07)                  .
               10  w-let-flt-nsg-max      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice sottogruppo merceologica min-max           *
      *            *---------------------------------------------------*
               10  w-let-flt-sgr-min      pic  9(05)                  .
               10  w-let-flt-sgr-max      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Nr. di sequenza sottogruppo merceologico min-max  *
      *            *---------------------------------------------------*
               10  w-let-flt-nss-min      pic  9(07)                  .
               10  w-let-flt-nss-max      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice semilavorato alfanumerico min-max          *
      *            *---------------------------------------------------*
               10  w-let-flt-alf-min      pic  x(14)                  .
               10  w-let-flt-alf-max      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Descrizione semilavorato, uppercase, min-max      *
      *            *---------------------------------------------------*
               10  w-let-flt-des-min      pic  x(40)                  .
               10  w-let-flt-des-max      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Tipo semilavorato                                 *
      *            *---------------------------------------------------*
               10  w-let-flt-tip-sem      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura semilavorato                     *
      *            *---------------------------------------------------*
               10  w-let-flt-umi-prd      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Codice statistico 1 semilavorato                  *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-s01      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice statistico 2 semilavorato                  *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-s02      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice statistico 3 semilavorato                  *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-s03      pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per la funzione di ordinamento e selezione per  *
      *    * mezzo del filtro                                          *
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
      *        * Flag di selezione mediante il filtro del record letto *
      *        *-------------------------------------------------------*
           05  w-ord-sel-flg-srf          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per la pulizia dei records senza mnemonico      *
      *    *-----------------------------------------------------------*
       01  w-pul-zos.
      *        *-------------------------------------------------------*
      *        * Data attuale da segreteria                            *
      *        *-------------------------------------------------------*
           05  w-pul-zos-dat-att          pic  9(07)                  .

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
      *    * Record file [dps]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"
                                     replacing rf-dps
                                          by   rf-lll                 .

      ******************************************************************
       Procedure Division                using f
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
           else if   f-ope                =    "TO"
                     perform   tor-000    thru tor-999
           else if   f-ope                =    "SE"
                     perform   sel-000    thru sel-999
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
      *              *-------------------------------------------------*
      *              * Status del work file [ttt] : Chiuso             *
      *              *-------------------------------------------------*
           move      00                   to   w-ord-sel-sts-ttt      .
       opn-100.
      *              *-------------------------------------------------*
      *              * [zos]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       opn-200.
      *              *-------------------------------------------------*
      *              * [dps]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       opn-300.
      *              *-------------------------------------------------*
      *              * [zs1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
       opn-400.
      *              *-------------------------------------------------*
      *              * [zs2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
       opn-500.
      *              *-------------------------------------------------*
      *              * [zs3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
       opn-600.
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
                     go to cls-075.
           move      "PD"                 to   s-ope                  .
           move      w-pat-uni-ttt-pat    to   s-pat                  .
           move      "S"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       cls-075.
      *              *-------------------------------------------------*
      *              * Pulizia file [zos] per mnemonici a Spaces       *
      *              *-------------------------------------------------*
           perform   pul-mne-spc-000      thru pul-mne-spc-999        .
       cls-100.
      *              *-------------------------------------------------*
      *              * [zos]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       cls-200.
      *              *-------------------------------------------------*
      *              * [dps]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       cls-300.
      *              *-------------------------------------------------*
      *              * [zs1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
       cls-400.
      *              *-------------------------------------------------*
      *              * [zs2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
       cls-500.
      *              *-------------------------------------------------*
      *              * [zs3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
       cls-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      e-not-err            to   f-sts                  .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Pulizia file [zos] per mnemonici a Spaces                 *
      *    *-----------------------------------------------------------*
       pul-mne-spc-000.
      *              *-------------------------------------------------*
      *              * Data attuale da segreteria                      *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-pul-zos-dat-att      .
       pul-mne-spc-100.
      *              *-------------------------------------------------*
      *              * Start per mnemonico, se errata : uscita         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "dps "               to   rf-zos-tip-rec         .
           move      spaces               to   rf-zos-cod-mne         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                not  = e-not-err
                     go to pul-mne-spc-999.
       pul-mne-spc-200.
      *              *-------------------------------------------------*
      *              * Get Next, se At End : uscita                    *
      *              *-------------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                not  = e-not-err
                     go to pul-mne-spc-999.
       pul-mne-spc-300.
      *              *-------------------------------------------------*
      *              * Test Max, se errato : ad unlock ed uscita       *
      *              *-------------------------------------------------*
           if        rf-zos-tip-rec       not  = "dps "
                     go to pul-mne-spc-800.
           if        rf-zos-cod-mne       not  = spaces
                     go to pul-mne-spc-800.
       pul-mne-spc-350.
      *              *-------------------------------------------------*
      *              * Selezione                                       *
      *              *-------------------------------------------------*
           if        rf-zos-cod-flt       =    zero or
                     rf-zos-cod-flt       =    9999999
                     go to pul-mne-spc-500.
           if        rf-zos-des-flt       not  = spaces
                     go to pul-mne-spc-500.
       pul-mne-spc-400.
      *              *-------------------------------------------------*
      *              * Delete record con mnemonico a Spaces, se erro-  *
      *              * ri : ad unlock ed uscita                        *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
           if        f-sts                not  = e-not-err
                     go to pul-mne-spc-800.
       pul-mne-spc-500.
      *              *-------------------------------------------------*
      *              * Unlock record                                   *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       pul-mne-spc-600.
      *              *-------------------------------------------------*
      *              * Riciclo a Get Next                              *
      *              *-------------------------------------------------*
           go to     pul-mne-spc-200.
       pul-mne-spc-800.
      *              *-------------------------------------------------*
      *              * Unlock record ed uscita                         *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       pul-mne-spc-999.
           exit.

      *    *===========================================================*
      *    * Tipo ordinamento                                          *
      *    *-----------------------------------------------------------*
       tor-000.
      *              *-------------------------------------------------*
      *              * Salvataggio del codice del filtro               *
      *              *-------------------------------------------------*
           perform   sav-cod-flt-000      thru sav-cod-flt-999        .
      *              *-------------------------------------------------*
      *              * Lettura del filtro                              *
      *              *-------------------------------------------------*
           perform   let-cod-flt-000      thru let-cod-flt-999        .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento in uscita                      *
      *              *-------------------------------------------------*
           if        w-let-flt-tip-ord    =    01
                     move  "01"           to   f-sts
           else if   w-let-flt-tip-ord    =    02
                     move  "02"           to   f-sts
           else if   w-let-flt-tip-ord    =    03
                     move  "03"           to   f-sts
           else if   w-let-flt-tip-ord    =    04
                     move  "04"           to   f-sts
           else      move  "01"           to   f-sts                  .
       tor-999.
           exit.

      *    *===========================================================*
      *    * Selezione su record                                       *
      *    *-----------------------------------------------------------*
       sel-000.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record pas-  *
      *              * sato                                            *
      *              *-------------------------------------------------*
           perform   sel-rec-sel-000      thru sel-rec-sel-999        .
      *              *-------------------------------------------------*
      *              * Status di uscita a seconda dell'esito della se- *
      *              * lezione                                         *
      *              *-------------------------------------------------*
           if        w-ord-sel-flg-srf    =    spaces
                     move  "00"           to   f-sts
           else      move  "01"           to   f-sts                  .
       sel-999.
           exit.

      *    *===========================================================*
      *    * Start                                                     *
      *    *-----------------------------------------------------------*
       str-000.
      *              *-------------------------------------------------*
      *              * Salvataggio del codice del filtro               *
      *              *-------------------------------------------------*
           perform   sav-cod-flt-000      thru sav-cod-flt-999        .
      *              *-------------------------------------------------*
      *              * Lettura del filtro                              *
      *              *-------------------------------------------------*
           perform   let-cod-flt-000      thru let-cod-flt-999        .
      *              *-------------------------------------------------*
      *              * Ordinamento e selezione [dps] su filtro         *
      *              *-------------------------------------------------*
           perform   ord-sel-flt-000      thru ord-sel-flt-999        .
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
      *    * Salvataggio codice filtro                                 *
      *    *-----------------------------------------------------------*
       sav-cod-flt-000.
      *              *-------------------------------------------------*
      *              * Salvataggio del codice del filtro contenuto nel *
      *              * campo passato come 'f-key'                      *
      *              *-------------------------------------------------*
           move      f-key                to   w-let-flt-eff-key      .
      *              *-------------------------------------------------*
      *              * Normalizzazione del codice del filtro           *
      *              *-------------------------------------------------*
           if        w-let-flt-cod-flt    not  numeric
                     move  zero           to   w-let-flt-cod-flt      .
      *              *-------------------------------------------------*
      *              * Normalizzazione mnemonico filtro a Spaces       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-flt-mne-flt      .
      *              *-------------------------------------------------*
      *              * Normalizzazione descrizione filtro a Spaces     *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-flt-des-flt      .
       sav-cod-flt-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione della work-area per la  ridefinizione     *
      *    * dell'area libera per il filtro 'dps'                      *
      *    *-----------------------------------------------------------*
       nor-zos-dps-000.
      *              *-------------------------------------------------*
      *              * Tipo ordinamento : Per classe, gruppo, sotto-   *
      *              *                        gruppo, codice semilav.  *
      *              *-------------------------------------------------*
           move      01                   to   w-zos-dps-tip-ord      .
      *              *-------------------------------------------------*
      *              * Classe merceologica min-max                     *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dps-cla-min      .
           move      99999                to   w-zos-dps-cla-max      .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico min-max                     *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dps-gru-min      .
           move      99999                to   w-zos-dps-gru-max      .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico min-max                *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dps-sgr-min      .
           move      99999                to   w-zos-dps-sgr-max      .
      *              *-------------------------------------------------*
      *              * Codice semilavorato alfanumerico min e max      *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dps-alf-min      .
           move      all   "z"            to   w-zos-dps-alf-max      .
      *              *-------------------------------------------------*
      *              * Descrizione semilavorato uppercase min-max      *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dps-des-min      .
           move      all   "z"            to   w-zos-dps-des-max      .
      *              *-------------------------------------------------*
      *              * Tipo semilavorato                               *
      *              *-------------------------------------------------*
           move      00                   to   w-zos-dps-tip-sem      .
      *              *-------------------------------------------------*
      *              * Unita' di misura semilavorato                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dps-umi-prd      .
      *              *-------------------------------------------------*
      *              * Codice statistico 1 semilavorato                *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dps-cod-s01      .
      *              *-------------------------------------------------*
      *              * Codice statistico 2 semilavorato                *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dps-cod-s02      .
      *              *-------------------------------------------------*
      *              * Codice statistico 3 semilavorato                *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dps-cod-s03      .
       nor-zos-dps-999.
           exit.

      *    *===========================================================*
      *    * Lettura del codice filtro                                 *
      *    *-----------------------------------------------------------*
       let-cod-flt-000.
      *              *-------------------------------------------------*
      *              * Se il codice filtro indica un singolo codice    *
      *              * anagrafico si va all'apposita gestione          *
      *              *-------------------------------------------------*
           if        w-let-flt-cod-tco    not  = zero
                     go to let-cod-flt-700.
       let-cod-flt-025.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se codice filtro a zero o  *
      *              * no                                              *
      *              *-------------------------------------------------*
           if        w-let-flt-cod-flt    =    zero
                     go to let-cod-flt-050
           else      go to let-cod-flt-100.
       let-cod-flt-050.
      *              *-------------------------------------------------*
      *              * Se codice filtro a zero                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura del codice filtro di default con  *
      *                  * valore numerico : 9999999                   *
      *                  *---------------------------------------------*
           move      0                    to   w-let-flt-cod-tco      .
           move      9999999              to   w-let-flt-cod-cco      .
       let-cod-flt-100.
      *              *-------------------------------------------------*
      *              * Se codice filtro diverso da zero                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura del filtro da [zos]                 *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      "dps "               to   rf-zos-tip-rec         .
           move      w-let-flt-cod-flt    to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record trovato op-  *
      *                  * pure no                                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-cod-flt-150
           else      go to let-cod-flt-200.
       let-cod-flt-150.
      *                  *---------------------------------------------*
      *                  * Se codice filtro non esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura del codice filtro a zero      *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-flt-cod-flt      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione della work-area per la  *
      *                      * ridefinizione dell'area libera filtro   *
      *                      * 'dps'                                   *
      *                      *-----------------------------------------*
           perform   nor-zos-dps-000      thru nor-zos-dps-999        .
      *                      *-----------------------------------------*
      *                      * A normalizzazioni finali                *
      *                      *-----------------------------------------*
           go to     let-cod-flt-300.
       let-cod-flt-200.
      *                  *---------------------------------------------*
      *                  * Se codice filtro esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spostamento area libera filtro in work  *
      *                      * area per la ridefinizione               *
      *                      *-----------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-dps              .
      *                      *-----------------------------------------*
      *                      * Test su tipo ordinamento : se non pre-  *
      *                      * visto, come se filtro non esistente     *
      *                      *-----------------------------------------*
           if        w-zos-dps-tip-ord    <    01 or
                     w-zos-dps-tip-ord    >    04
                     go to let-cod-flt-150.
      *                      *-----------------------------------------*
      *                      * Salvataggio mnemonico del filtro        *
      *                      *-----------------------------------------*
           move      rf-zos-cod-mne       to   w-let-flt-mne-flt      .
      *                      *-----------------------------------------*
      *                      * Salvataggio descrizione del filtro      *
      *                      *-----------------------------------------*
           move      rf-zos-des-flt       to   w-let-flt-des-flt      .
      *                      *-----------------------------------------*
      *                      * A normalizzazioni finali                *
      *                      *-----------------------------------------*
           go to     let-cod-flt-300.
       let-cod-flt-300.
      *              *-------------------------------------------------*
      *              * Normalizzazioni finali con spostamento in area  *
      *              * definitiva 'w-let-flt'                          *
      *              *-------------------------------------------------*
       let-cod-flt-310.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento : semplice spostamento     *
      *                  *---------------------------------------------*
           move      w-zos-dps-tip-ord    to   w-let-flt-tip-ord      .
       let-cod-flt-350.
      *                  *---------------------------------------------*
      *                  * Codice classe, gruppo, sottogruppo          *
      *                  *---------------------------------------------*
       let-cod-flt-360.
      *                      *-----------------------------------------*
      *                      * Codice classe                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice classe min                   *
      *                          *-------------------------------------*
           move      w-zos-dps-cla-min    to   w-let-flt-cla-min      .
      *                          *-------------------------------------*
      *                          * Codice classe max                   *
      *                          *-------------------------------------*
           move      w-zos-dps-cla-max    to   w-let-flt-cla-max      .
      *                          *-------------------------------------*
      *                          * Codice classe min-max               *
      *                          *-------------------------------------*
           if        w-let-flt-cla-max    =    zero
                     if    w-let-flt-cla-min
                                          =    zero
                           move  99999    to   w-let-flt-cla-max
                     else  move  w-let-flt-cla-min
                                          to   w-let-flt-cla-max      .
       let-cod-flt-370.
      *                      *-----------------------------------------*
      *                      * Codice gruppo                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se codice classe min diverso da co- *
      *                          * dice classe max : normalizzazione   *
      *                          *-------------------------------------*
           if        w-let-flt-cla-min    not  = w-let-flt-cla-max
                     move  zero           to   w-let-flt-gru-min
                     move  99999          to   w-let-flt-gru-max
                     go to let-cod-flt-380.
      *                          *-------------------------------------*
      *                          * Codice gruppo min                   *
      *                          *-------------------------------------*
           move      w-zos-dps-gru-min    to   w-let-flt-gru-min      .
      *                          *-------------------------------------*
      *                          * Codice gruppo max                   *
      *                          *-------------------------------------*
           move      w-zos-dps-gru-max    to   w-let-flt-gru-max      .
      *                          *-------------------------------------*
      *                          * Codice gruppo min-max               *
      *                          *-------------------------------------*
           if        w-let-flt-gru-max    =    zero
                     if    w-let-flt-gru-min
                                          =    zero
                           move  99999    to   w-let-flt-gru-max
                     else  move  w-let-flt-gru-min
                                          to   w-let-flt-gru-max      .
       let-cod-flt-380.
      *                      *-----------------------------------------*
      *                      * Codice sottogruppo                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se codice classe min diverso da co- *
      *                          * dice classe max : normalizzazione   *
      *                          *-------------------------------------*
           if        w-let-flt-cla-min    not  = w-let-flt-cla-max
                     move  zero           to   w-let-flt-sgr-min
                     move  99999          to   w-let-flt-sgr-max
                     go to let-cod-flt-400.
      *                          *-------------------------------------*
      *                          * Se codice gruppo min diverso da co- *
      *                          * dice gruppo max : normalizzazione   *
      *                          *-------------------------------------*
           if        w-let-flt-gru-min    not  = w-let-flt-gru-max
                     move  zero           to   w-let-flt-sgr-min
                     move  99999          to   w-let-flt-sgr-max
                     go to let-cod-flt-400.
      *                          *-------------------------------------*
      *                          * Codice sottogruppo min              *
      *                          *-------------------------------------*
           move      w-zos-dps-sgr-min    to   w-let-flt-sgr-min      .
      *                          *-------------------------------------*
      *                          * Codice sottogruppo max              *
      *                          *-------------------------------------*
           move      w-zos-dps-sgr-max    to   w-let-flt-sgr-max      .
      *                          *-------------------------------------*
      *                          * Codice sottogruppo min-max          *
      *                          *-------------------------------------*
           if        w-let-flt-sgr-max    =    zero
                     if    w-let-flt-sgr-min
                                          =    zero
                           move  99999    to   w-let-flt-sgr-max
                     else  move  w-let-flt-sgr-min
                                          to   w-let-flt-sgr-max      .
       let-cod-flt-400.
      *                  *---------------------------------------------*
      *                  * Nr. di sequenza classe, gruppo, sottogruppo *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se si tratta di u- *
      *                      * na unica classe o di piu' classi        *
      *                      *-----------------------------------------*
           if        w-let-flt-cla-min    =    w-let-flt-cla-max
                     go to let-cod-flt-405
           else      go to let-cod-flt-430.
       let-cod-flt-405.
      *                      *-----------------------------------------*
      *                      * Se si tratta di una classe sola         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione del numero di sequenza *
      *                          * min-max per la classe               *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs1-sqz-num         .
           move      rf-zs1-sqz-num       to   w-let-flt-nsc-min      .
           move      rf-zs1-sqz-num       to   w-let-flt-nsc-max      .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se si tratta   *
      *                          * di un unico gruppo o di piu' grup-  *
      *                          * pi                                  *
      *                          *-------------------------------------*
           if        w-let-flt-gru-min    =    w-let-flt-gru-max
                     go to let-cod-flt-410
           else      go to let-cod-flt-425.
       let-cod-flt-410.
      *                          *-------------------------------------*
      *                          * Se si tratta di un gruppo solo      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione del numero di se-  *
      *                              * quenza min-max per il gruppo    *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs2-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zs2-cod-gru         .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs2-sqz-num         .
           move      rf-zs2-sqz-num       to   w-let-flt-nsg-min      .
           move      rf-zs2-sqz-num       to   w-let-flt-nsg-max      .
      *                              *---------------------------------*
      *                              * Deviazione a seconda se si trat-*
      *                              * ta di un unico sottogruppo o di *
      *                              * piu' sottogruppi                *
      *                              *---------------------------------*
           if        w-let-flt-sgr-min    =    w-let-flt-sgr-max
                     go to let-cod-flt-415
           else      go to let-cod-flt-420.
       let-cod-flt-415.
      *                              *---------------------------------*
      *                              * Se si tratta di un sottogruppo  *
      *                              * solo                            *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione del numero di  *
      *                                  * sequenza min-max per il     *
      *                                  * sottogruppo                 *
      *                                  *-----------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs3-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zs3-cod-gru         .
           move      w-let-flt-sgr-min    to   rf-zs3-cod-sgr         .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs3-sqz-num         .
           move      rf-zs3-sqz-num       to   w-let-flt-nss-min      .
           move      rf-zs3-sqz-num       to   w-let-flt-nss-max      .
      *                                  *-----------------------------*
      *                                  * Continuazione con il resto  *
      *                                  * dei parametri               *
      *                                  *-----------------------------*
           go to     let-cod-flt-500.
       let-cod-flt-420.
      *                              *---------------------------------*
      *                              * Se si tratta di piu' sottogrup- *
      *                              * pi                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione del numero di  *
      *                                  * sequenza min per il sotto-  *
      *                                  * gruppo                      *
      *                                  *-----------------------------*
           if        w-let-flt-sgr-min    =    zero
                     move  zero           to   w-let-flt-nss-min
                     go to let-cod-flt-421.
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs3-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zs3-cod-gru         .
           move      w-let-flt-sgr-min    to   rf-zs3-cod-sgr         .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs3-sqz-num         .
           move      rf-zs3-sqz-num       to   w-let-flt-nss-min      .
       let-cod-flt-421.
      *                                  *-----------------------------*
      *                                  * Preparazione del numero di  *
      *                                  * sequenza max per il sotto-  *
      *                                  * gruppo                      *
      *                                  *-----------------------------*
           if        w-let-flt-sgr-max    =    99999
                     move  9999999        to   w-let-flt-nss-max
                     go to let-cod-flt-422.
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs3-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zs3-cod-gru         .
           move      w-let-flt-sgr-max    to   rf-zs3-cod-sgr         .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs3-sqz-num         .
           move      rf-zs3-sqz-num       to   w-let-flt-nss-max      .
           if        w-let-flt-nss-max    =    zero
                     if    w-let-flt-nss-min
                                          =    zero
                           move  9999999  to   w-let-flt-nss-max
                     else  move  w-let-flt-nss-min
                                          to   w-let-flt-nss-max      .
       let-cod-flt-422.
      *                                  *-----------------------------*
      *                                  * Continuazione con il resto  *
      *                                  * dei parametri               *
      *                                  *-----------------------------*
           go to     let-cod-flt-500.
       let-cod-flt-425.
      *                          *-------------------------------------*
      *                          * Se si tratta di piu' gruppi         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione del numero di se-  *
      *                              * quenza min per il gruppo        *
      *                              *---------------------------------*
           if        w-let-flt-gru-min    =    zero
                     move  zero           to   w-let-flt-nsg-min
                     go to let-cod-flt-426.
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs2-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zs2-cod-gru         .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs2-sqz-num         .
           move      rf-zs2-sqz-num       to   w-let-flt-nsg-min      .
       let-cod-flt-426.
      *                              *---------------------------------*
      *                              * Preparazione del numero di se-  *
      *                              * quenza max per il gruppo        *
      *                              *---------------------------------*
           if        w-let-flt-gru-max    =    99999
                     move  9999999        to   w-let-flt-nsg-max
                     go to let-cod-flt-427.
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs2-cod-cla         .
           move      w-let-flt-gru-max    to   rf-zs2-cod-gru         .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs2-sqz-num         .
           move      rf-zs2-sqz-num       to   w-let-flt-nsg-max      .
           if        w-let-flt-nsg-max    =    zero
                     if    w-let-flt-nsg-min
                                          =    zero
                           move  9999999  to   w-let-flt-nsg-max
                     else  move  w-let-flt-nsg-min
                                          to   w-let-flt-nsg-max      .
       let-cod-flt-427.
      *                              *---------------------------------*
      *                              * Normalizzazione numeri di se-   *
      *                              * quenza min-max per sottogrup-   *
      *                              * pi                              *
      *                              *---------------------------------*
           move      zero                 to   w-let-flt-nss-min      .
           move      9999999              to   w-let-flt-nss-max      .
      *                              *---------------------------------*
      *                              * Continuazione con il resto dei  *
      *                              * parametri                       *
      *                              *---------------------------------*
           go to     let-cod-flt-500.
       let-cod-flt-430.
      *                      *-----------------------------------------*
      *                      * Se si tratta di piu' classi             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione del numero di sequenza *
      *                          * min per la classe                   *
      *                          *-------------------------------------*
           if        w-let-flt-cla-min    =    zero
                     move  zero           to   w-let-flt-nsc-min
                     go to let-cod-flt-431.
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-flt-cla-min    to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs1-sqz-num         .
           move      rf-zs1-sqz-num       to   w-let-flt-nsc-min      .
       let-cod-flt-431.
      *                          *-------------------------------------*
      *                          * Preparazione del numero di sequenza *
      *                          * max per la classe                   *
      *                          *-------------------------------------*
           if        w-let-flt-cla-max    =    99999
                     move  9999999        to   w-let-flt-nsc-max
                     go to let-cod-flt-432.
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-flt-cla-max    to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs1-sqz-num         .
           move      rf-zs1-sqz-num       to   w-let-flt-nsc-max      .
           if        w-let-flt-nsc-max    =    zero
                     if    w-let-flt-nsc-min
                                          =    zero
                           move  9999999  to   w-let-flt-nsc-max
                     else  move  w-let-flt-nsc-min
                                          to   w-let-flt-nsc-max      .
       let-cod-flt-432.
      *                          *-------------------------------------*
      *                          * Normalizzazione numeri di sequenza  *
      *                          * min-max per gruppi                  *
      *                          *-------------------------------------*
           move      zero                 to   w-let-flt-nsg-min      .
           move      9999999              to   w-let-flt-nsg-max      .
      *                          *-------------------------------------*
      *                          * Normalizzazione numeri di sequenza  *
      *                          * min-max per sottogruppi             *
      *                          *-------------------------------------*
           move      zero                 to   w-let-flt-nss-min      .
           move      9999999              to   w-let-flt-nss-max      .
      *                          *-------------------------------------*
      *                          * Continuazione con il resto dei pa-  *
      *                          * rametri                             *
      *                          *-------------------------------------*
           go to     let-cod-flt-500.
       let-cod-flt-500.
      *                  *---------------------------------------------*
      *                  * Codice s.l. min : semplice spostamento      *
      *                  *---------------------------------------------*
           move      w-zos-dps-alf-min    to   w-let-flt-alf-min      .
       let-cod-flt-510.
      *                  *---------------------------------------------*
      *                  * Codice s.l. max : semplice spostamento      *
      *                  *---------------------------------------------*
           move      w-zos-dps-alf-max    to   w-let-flt-alf-max      .
       let-cod-flt-520.
      *                  *---------------------------------------------*
      *                  * Codice s.l. min-max                         *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-max    =    spaces
                     if    w-let-flt-alf-min
                                          =    spaces
                           move  all "z"  to   w-let-flt-alf-max
                     else  move  w-let-flt-alf-min
                                          to   w-let-flt-alf-max      .
       let-cod-flt-530.
      *                  *---------------------------------------------*
      *                  * Descrizione s.l. min : semplice spostamento *
      *                  *---------------------------------------------*
           move      w-zos-dps-des-min    to   w-let-flt-des-min      .
       let-cod-flt-540.
      *                  *---------------------------------------------*
      *                  * Descrizione s.l. max : semplice spostamento *
      *                  *---------------------------------------------*
           move      w-zos-dps-des-max    to   w-let-flt-des-max      .
       let-cod-flt-550.
      *                  *---------------------------------------------*
      *                  * Descrizione semilavorato min-max            *
      *                  *---------------------------------------------*
           if        w-let-flt-des-max    =    spaces
                     if    w-let-flt-des-min
                                          =    spaces
                           move  all "z"  to   w-let-flt-des-max
                     else  move  w-let-flt-des-min
                                          to   w-let-flt-des-max      .
           move      "z"                  to   w-pad-alf-cdp          .
           move      40                   to   w-pad-alf-max          .
           move      w-let-flt-des-max    to   w-pad-alf-str          .
           perform   pad-alf-cdp-000      thru pad-alf-cdp-999        .
           move      w-pad-alf-str        to   w-let-flt-des-max      .
       let-cod-flt-560.
      *                  *---------------------------------------------*
      *                  * Tipo semilavorato : semplice spostamento    *
      *                  *---------------------------------------------*
           move      w-zos-dps-tip-sem    to   w-let-flt-tip-sem      .
       let-cod-flt-590.
      *                  *---------------------------------------------*
      *                  * Unita' di misura s.l. : semplice spostamen- *
      *                  * to                                          *
      *                  *---------------------------------------------*
           move      w-zos-dps-umi-prd    to   w-let-flt-umi-prd      .
       let-cod-flt-600.
      *                  *---------------------------------------------*
      *                  * Codice statistico 1 s.l. : semplice sposta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-zos-dps-cod-s01    to   w-let-flt-cod-s01      .
       let-cod-flt-610.
      *                  *---------------------------------------------*
      *                  * Codice statistico 2 s.l. : semplice sposta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-zos-dps-cod-s02    to   w-let-flt-cod-s02      .
       let-cod-flt-620.
      *                  *---------------------------------------------*
      *                  * Codice statistico 3 s.l. : semplice sposta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-zos-dps-cod-s03    to   w-let-flt-cod-s03      .
       let-cod-flt-630.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-cod-flt-900.
       let-cod-flt-700.
      *              *-------------------------------------------------*
      *              * Se il codice filtro indica un singolo codice    *
      *              * anagrafico                                      *
      *              *-------------------------------------------------*
       let-cod-flt-720.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di codice a-  *
      *                  * nagrafico                                   *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-tco    =    1
                     go to let-cod-flt-740
           else      go to let-cod-flt-760.
       let-cod-flt-740.
      *                  *---------------------------------------------*
      *                  * Se tipo di codice anagrafico : [dps]        *
      *                  *---------------------------------------------*
       let-cod-flt-742.
      *                      *-----------------------------------------*
      *                      * Lettura codice semilavorato             *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM    "         to   f-key                  .
           move      w-let-flt-cod-cco    to   rf-dps-num-sem of
                                               rf-dps                 .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-dps-alf-sem of
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Mnemonico del filtro                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-mne-flt      .
      *                      *-----------------------------------------*
      *                      * Descrizione del filtro                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-des-flt      .
           if        rf-dps-alf-sem of
                     rf-dps               =    spaces
                     go to let-cod-flt-744.
           string    "Codice semilavorato = "
                                delimited by   size
                     rf-dps-alf-sem of
                     rf-dps
                                delimited by   size
                                          into w-let-flt-des-flt      .
       let-cod-flt-744.
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento : per codice           *
      *                      *-----------------------------------------*
           move      03                   to   w-zos-dps-tip-ord      .
      *                      *-----------------------------------------*
      *                      * Classe merceologica min e max           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cla-min      .
           move      99999                to   w-zos-dps-cla-max      .
      *                      *-----------------------------------------*
      *                      * Gruppo merceologico min e max           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-gru-min      .
           move      99999                to   w-zos-dps-gru-max      .
      *                      *-----------------------------------------*
      *                      * Sottogruppo merceologico min e max      *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-sgr-min      .
           move      99999                to   w-zos-dps-sgr-max      .
      *                      *-----------------------------------------*
      *                      * Codice semilavorato alfanumerico min e  *
      *                      * max                                     *
      *                      *-----------------------------------------*
           move      rf-dps-alf-sem of
                     rf-dps               to   w-zos-dps-alf-min      .
           move      rf-dps-alf-sem of
                     rf-dps               to   w-zos-dps-alf-max      .
      *                      *-----------------------------------------*
      *                      * Descrizione, uppercase, min e max       *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dps-des-min      .
           move      all   "Z"            to   w-zos-dps-des-max      .
      *                      *-----------------------------------------*
      *                      * Tipo semilavorato                       *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-tip-sem      .
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dps-umi-prd      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 1 semilavorato        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cod-s01      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 2 semilavorato        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cod-s02      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 3 semilavorato        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cod-s03      .
       let-cod-flt-746.
      *                      *-----------------------------------------*
      *                      * Area di lettura                         *
      *                      *-----------------------------------------*
           move      w-zos-dps-tip-ord    to   w-let-flt-tip-ord      .
           move      w-zos-dps-cla-min    to   w-let-flt-cla-min      .
           move      w-zos-dps-cla-max    to   w-let-flt-cla-max      .
           move      0000000              to   w-let-flt-nsc-min      .
           move      9999999              to   w-let-flt-nsc-max      
           move      w-zos-dps-gru-min    to   w-let-flt-gru-min      .
           move      w-zos-dps-gru-max    to   w-let-flt-gru-max      .
           move      0000000              to   w-let-flt-nsg-min      .
           move      9999999              to   w-let-flt-nsg-max      
           move      w-zos-dps-sgr-min    to   w-let-flt-sgr-min      .
           move      w-zos-dps-sgr-max    to   w-let-flt-sgr-max      .
           move      0000000              to   w-let-flt-nss-min      .
           move      9999999              to   w-let-flt-nss-max      
           move      w-zos-dps-alf-min    to   w-let-flt-alf-min      .
           move      w-zos-dps-alf-max    to   w-let-flt-alf-max      .
           move      w-zos-dps-des-min    to   w-let-flt-des-min      .
           move      w-zos-dps-des-max    to   w-let-flt-des-max      .
           move      w-zos-dps-tip-sem    to   w-let-flt-tip-sem      .
           move      w-zos-dps-umi-prd    to   w-let-flt-umi-prd      .
           move      w-zos-dps-cod-s01    to   w-let-flt-cod-s01      .
           move      w-zos-dps-cod-s02    to   w-let-flt-cod-s02      .
           move      w-zos-dps-cod-s03    to   w-let-flt-cod-s03      .
       let-cod-flt-748.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     let-cod-flt-900.
       let-cod-flt-760.
      *                  *---------------------------------------------*
      *                  * Se tipo di codice anagrafico : [zs1]        *
      *                  *---------------------------------------------*
       let-cod-flt-762.
      *                      *-----------------------------------------*
      *                      * Lettura codice [zs1]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-flt-cod-cco    to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zs1-sqz-num         .
      *                      *-----------------------------------------*
      *                      * Mnemonico del filtro                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-mne-flt      .
      *                      *-----------------------------------------*
      *                      * Descrizione del filtro                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-des-flt      .
           if        rf-zs1-sqz-num       =    zero
                     go to let-cod-flt-764.
           string    "Codice classe merceologica = "
                                delimited by   size
                     rf-zs1-cod-cla
                                delimited by   size
                                          into w-let-flt-des-flt      .
       let-cod-flt-764.
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento : per classe, gruppo,  *
      *                      *                    sottogruppo, codice  *
      *                      *-----------------------------------------*
           move      01                   to   w-zos-dps-tip-ord      .
      *                      *-----------------------------------------*
      *                      * Classe merceologica min e max           *
      *                      *-----------------------------------------*
           move      rf-zs1-cod-cla       to   w-zos-dps-cla-min      .
           move      rf-zs1-cod-cla       to   w-zos-dps-cla-max      .
      *                      *-----------------------------------------*
      *                      * Gruppo merceologico min e max           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-gru-min      .
           move      99999                to   w-zos-dps-gru-max      .
      *                      *-----------------------------------------*
      *                      * Sottogruppo merceologico min e max      *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-sgr-min      .
           move      99999                to   w-zos-dps-sgr-max      .
      *                      *-----------------------------------------*
      *                      * Codice semilavorato alfanumerico min e  *
      *                      * max                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dps-alf-min      .
           move      all   "Z"            to   w-zos-dps-alf-max      .
      *                      *-----------------------------------------*
      *                      * Descrizione uppercase, min e max        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dps-des-min      .
           move      all   "Z"            to   w-zos-dps-des-max      .
      *                      *-----------------------------------------*
      *                      * Tipo semilavorato                       *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-tip-sem      .
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dps-umi-prd      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 1 semilavorato        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cod-s01      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 2 semilavorato        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cod-s02      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 3 semilavorato        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dps-cod-s03      .
       let-cod-flt-766.
      *                      *-----------------------------------------*
      *                      * Area di lettura                         *
      *                      *-----------------------------------------*
           move      w-zos-dps-tip-ord    to   w-let-flt-tip-ord      .
           move      w-zos-dps-cla-min    to   w-let-flt-cla-min      .
           move      w-zos-dps-cla-max    to   w-let-flt-cla-max      .
           move      rf-zs1-sqz-num       to   w-let-flt-nsc-min      .
           move      rf-zs1-sqz-num       to   w-let-flt-nsc-max      .
           move      w-zos-dps-gru-min    to   w-let-flt-gru-min      .
           move      w-zos-dps-gru-max    to   w-let-flt-gru-max      .
           move      0000000              to   w-let-flt-nsg-min      .
           move      9999999              to   w-let-flt-nsg-max      
           move      w-zos-dps-sgr-min    to   w-let-flt-sgr-min      .
           move      w-zos-dps-sgr-max    to   w-let-flt-sgr-max      .
           move      0000000              to   w-let-flt-nss-min      .
           move      9999999              to   w-let-flt-nss-max      
           move      w-zos-dps-alf-min    to   w-let-flt-alf-min      .
           move      w-zos-dps-alf-max    to   w-let-flt-alf-max      .
           move      w-zos-dps-des-min    to   w-let-flt-des-min      .
           move      w-zos-dps-des-max    to   w-let-flt-des-max      .
           move      w-zos-dps-tip-sem    to   w-let-flt-tip-sem      .
           move      w-zos-dps-umi-prd    to   w-let-flt-umi-prd      .
           move      w-zos-dps-cod-s01    to   w-let-flt-cod-s01      .
           move      w-zos-dps-cod-s02    to   w-let-flt-cod-s02      .
           move      w-zos-dps-cod-s03    to   w-let-flt-cod-s03      .
       let-cod-flt-768.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     let-cod-flt-900.
       let-cod-flt-900.
      *              *-------------------------------------------------*
      *              * Uscita da routine di lettura                    *
      *              *-------------------------------------------------*
           go to     let-cod-flt-999.
       let-cod-flt-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [dps] su filtro                   *
      *    *-----------------------------------------------------------*
       ord-sel-flt-000.
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
       ord-sel-flt-025.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri implicita- *
      *              * mente indicano una selezione vuota ; se si : u- *
      *              * scita                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su numero di sequenza classe merceolo- *
      *                  * gica min-max                                *
      *                  *---------------------------------------------*
           if        w-let-flt-nsc-max    <    w-let-flt-nsc-min
                     go to ord-sel-flt-999.
      *                  *---------------------------------------------*
      *                  * Test su numero di sequenza gruppo merceolo- *
      *                  * gico min-max                                *
      *                  *---------------------------------------------*
           if        w-let-flt-nsg-max    <    w-let-flt-nsg-min
                     go to ord-sel-flt-999.
      *                  *---------------------------------------------*
      *                  * Test su numero di sequenza sottogruppo mer- *
      *                  * ceologico min-max                           *
      *                  *---------------------------------------------*
           if        w-let-flt-nss-max    <    w-let-flt-nss-min
                     go to ord-sel-flt-999.
      *                  *---------------------------------------------*
      *                  * Test su codice alfanumerico del semilavora- *
      *                  * to min-max                                  *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-max    <    w-let-flt-alf-min
                     go to ord-sel-flt-999.
      *                  *---------------------------------------------*
      *                  * Test su descrizione del semilavorato min-   *
      *                  * max                                         *
      *                  *---------------------------------------------*
           if        w-let-flt-des-max    <    w-let-flt-des-min
                     go to ord-sel-flt-999.
       ord-sel-flt-050.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri implicita- *
      *              * mente indicano una selezione composta da un so- *
      *              * lo elemento ; in caso affermativo si seleziona  *
      *              * questo elemento e lo si salva nell'area sort.   *
      *              *-------------------------------------------------*
       ord-sel-flt-055.
      *                  *---------------------------------------------*
      *                  * Se il codice alfanumerico del s.l. min      *
      *                  * e' diverso dal codice alfanumerico del      *
      *                  * s.l. max non si puo' avere questa certezza  *
      *                  * e si prosegue pertanto con il normale meto- *
      *                  * do di selezione                             *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-min    not  = w-let-flt-alf-max
                     go to ord-sel-flt-500.
      *                  *---------------------------------------------*
      *                  * Se il codice alfanumerico del s.l. min e'   *
      *                  * a spaces ed anche il codice alfanumerico    *
      *                  * del s.l. max e' a spaces non si puo' avere  *
      *                  * questa certezza e si prosegue pertanto con  *
      *                  * il normale metodo di selezione              *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-min    =    spaces and
                     w-let-flt-alf-max    =    spaces
                     go to ord-sel-flt-500.
       ord-sel-flt-060.
      *                  *---------------------------------------------*
      *                  * Lettura del record relativo all'unico ele-  *
      *                  * mento                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start per codice alfanumerico su [dps]  *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "ALFSEM    "         to   f-key                  .
           move      w-let-flt-alf-min    to   rf-dps-alf-sem of
                                               rf-dps                 .
           move      zero                 to   rf-dps-num-sem of
                                               rf-dps                 .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita per selezione  *
      *                      * vuota                                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-flt-999.
      *                      *-----------------------------------------*
      *                      * Read Next su [dps]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                      *-----------------------------------------*
      *                      * Se At End : uscita per selezione vuota  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-flt-999.
      *                      *-----------------------------------------*
      *                      * Test Max, se errore : uscita per sele-  *
      *                      * zione vuota                             *
      *                      *-----------------------------------------*
           if        rf-dps-alf-sem of
                     rf-dps               not  = w-let-flt-alf-min
                     go to ord-sel-flt-999.
       ord-sel-flt-065.
      *                  *---------------------------------------------*
      *                  * Selezione, mediante il filtro, del record   *
      *                  * letto, se non superata : uscita per sele-   *
      *                  * zione vuota                                 *
      *                  *---------------------------------------------*
           perform   sel-rec-flt-000      thru sel-rec-flt-999        .
           if        w-ord-sel-flg-srf    not  = spaces
                     go to ord-sel-flt-999.
       ord-sel-flt-070.
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
           move      rf-dps               to   rf-sss                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ord-sel-flt-999.
       ord-sel-flt-500.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo ordinamento       *
      *              *-------------------------------------------------*
           if        w-let-flt-tip-ord    =    01
                     go to ord-sel-flt-510
           else if   w-let-flt-tip-ord    =    02
                     go to ord-sel-flt-520
           else if   w-let-flt-tip-ord    =    03
                     go to ord-sel-flt-530
           else if   w-let-flt-tip-ord    =    04
                     go to ord-sel-flt-540.
       ord-sel-flt-510.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 01 : Classe, gruppo, sot- *
      *              *                            togruppo, codice     *
      *              *                            semilavorato         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-001-000      thru ord-sel-001-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-flt-999.
       ord-sel-flt-520.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 02 : Classe, gruppo, sot- *
      *              *                            togruppo, descrizio- *
      *              *                            ne semilavorato      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-002-000      thru ord-sel-002-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-flt-999.
       ord-sel-flt-530.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 03 : Codice semilavorato  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-003-000      thru ord-sel-003-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-flt-999.
       ord-sel-flt-540.
      *              *-------------------------------------------------*
      *              * Se tipo ordinamento : 04 : Descrizione semilav. *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   ord-sel-004-000      thru ord-sel-004-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-flt-999.
       ord-sel-flt-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [dps]     Tipo ordinamento  01    *
      *    *                                                           *
      *    * Classe, gruppo, sottogruppo, codice semilavorato          *
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
      *              * Start su [zs1] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-let-flt-nsc-min    to   rf-zs1-sqz-num         .
           move      zero                 to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
       ord-sel-001-125.
      *              *-------------------------------------------------*
      *              * Read Next su [zs1]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-zs1-sqz-num       >    w-let-flt-nsc-max
                     go to ord-sel-001-900.
      *              *-------------------------------------------------*
      *              * Se classe non suddivisa in gruppi               *
      *              *-------------------------------------------------*
           if        rf-zs1-ult-sud       =    02
                     go to ord-sel-001-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record gruppo               *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento semilavorato                  *
      *                  *---------------------------------------------*
           go to     ord-sel-001-275.
       ord-sel-001-150.
      *              *-------------------------------------------------*
      *              * Start su [zs2] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zs1-cod-cla       to   rf-zs2-cod-cla         .
           move      w-let-flt-nsg-min    to   rf-zs2-sqz-num         .
           move      zero                 to   rf-zs2-cod-gru         .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a classe successiva           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-125.
       ord-sel-001-175.
      *              *-------------------------------------------------*
      *              * Read Next su [zs2]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
      *              *-------------------------------------------------*
      *              * Se At End : a classe successiva                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-125.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a classe successiva *
      *              *-------------------------------------------------*
           if        rf-zs2-cod-cla       not  = rf-zs1-cod-cla
                     go to ord-sel-001-125.
           if        rf-zs2-sqz-num       >    w-let-flt-nsg-max
                     go to ord-sel-001-125.
      *              *-------------------------------------------------*
      *              * Se gruppo non suddiviso in sottogruppi          *
      *              *-------------------------------------------------*
           if        rf-zs2-ult-sud       =    02
                     go to ord-sel-001-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento semilavorato                  *
      *                  *---------------------------------------------*
           go to     ord-sel-001-275.
       ord-sel-001-200.
      *              *-------------------------------------------------*
      *              * Start su [zs3] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zs1-cod-cla       to   rf-zs3-cod-cla         .
           move      rf-zs2-cod-gru       to   rf-zs3-cod-gru         .
           move      w-let-flt-nss-min    to   rf-zs3-sqz-num         .
           move      zero                 to   rf-zs3-cod-sgr         .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a gruppo successivo           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-175.
       ord-sel-001-225.
      *              *-------------------------------------------------*
      *              * Read Next su [zs3]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *              *-------------------------------------------------*
      *              * Se At End : a gruppo successivo                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-175.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a gruppo successivo *
      *              *-------------------------------------------------*
           if        rf-zs3-cod-cla       not  = rf-zs1-cod-cla
                     go to ord-sel-001-175.
           if        rf-zs3-cod-gru       not  = rf-zs2-cod-gru
                     go to ord-sel-001-175.
           if        rf-zs3-sqz-num       >    w-let-flt-nss-max
                     go to ord-sel-001-175.
      *              *-------------------------------------------------*
      *              * A trattamento semilavorato                      *
      *              *-------------------------------------------------*
           go to     ord-sel-001-275.
       ord-sel-001-250.
      *              *-------------------------------------------------*
      *              * Riciclo a :                                     *
      *              * - classe                                        *
      *              * - gruppo                                        *
      *              * - sottogruppo                                   *
      *              * a seconda dei tipi di suddivisione              *
      *              *-------------------------------------------------*
           if        rf-zs1-ult-sud       not  = 02
                     go to ord-sel-001-125
           else if   rf-zs2-ult-sud       not  = 02
                     go to ord-sel-001-175
           else      go to ord-sel-001-225.
       ord-sel-001-275.
      *              *-------------------------------------------------*
      *              * Start su [dps] per Classe, gruppo, sottogruppo  *
      *              *                  e codice semilavorato          *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "CGSALF    "         to   f-key                  .
           move      rf-zs1-cod-cla       to   rf-dps-cla-sem of
                                               rf-dps                 .
           if        rf-zs1-ult-sud       not  = 02
                     move  zero           to   rf-dps-gru-sem of
                                               rf-dps
                     move  zero           to   rf-dps-sgr-sem of
                                               rf-dps
                     go to ord-sel-001-280.
           move      rf-zs2-cod-gru       to   rf-dps-gru-sem of
                                               rf-dps                 .
           if        rf-zs2-ult-sud       not  = 02
                     move  zero           to   rf-dps-sgr-sem of
                                               rf-dps
                     go to ord-sel-001-280.
           move      rf-zs3-cod-sgr       to   rf-dps-sgr-sem of
                                               rf-dps                 .
       ord-sel-001-280.
           move      w-let-flt-alf-min    to   rf-dps-alf-sem of
                                               rf-dps                 .
           move      zero                 to   rf-dps-num-sem of
                                               rf-dps                 .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : riciclo a classe o gruppo o   *
      *              * sottogruppo                                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-250.
       ord-sel-001-300.
      *              *-------------------------------------------------*
      *              * Read Next su [dps]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se At End : riciclo a classe o gruppo o sotto-  *
      *              * gruppo                                          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-250.
       ord-sel-001-325.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : riciclo a classe o  *
      *              * gruppo o sottogruppo                            *
      *              *-------------------------------------------------*
           if        rf-dps-cla-sem of
                     rf-dps               not  = rf-zs1-cod-cla
                     go to ord-sel-001-250.
           if        rf-zs1-ult-sud       not  = 02
                     go to ord-sel-001-327.
           if        rf-dps-gru-sem of
                     rf-dps               not  = rf-zs2-cod-gru
                     go to ord-sel-001-250.
           if        rf-zs2-ult-sud       not  =  02
                     go to ord-sel-001-327.
           if        rf-dps-sgr-sem of
                     rf-dps               not  = rf-zs3-cod-sgr
                     go to ord-sel-001-250.
       ord-sel-001-327.
           if        rf-dps-alf-sem of
                     rf-dps               >    w-let-flt-alf-max
                     go to ord-sel-001-250.
       ord-sel-001-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record let-  *
      *              * to, se non superata : riciclo a Read Next sul   *
      *              * file [dps]                                      *
      *              *-------------------------------------------------*
           perform   sel-rec-flt-000      thru sel-rec-flt-999        .
           if        w-ord-sel-flg-srf    not  = spaces
                     go to ord-sel-001-300.
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
           move      rf-dps               to   rf-sss                 .
       ord-sel-001-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dps               to   rf-ttt                 .
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
           go to     ord-sel-001-300.
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
      *    * Ordinamento e selezione [dps]     Tipo ordinamento  02    *
      *    *                                                           *
      *    * Classe, gruppo, sottogruppo, descrizione semilavorato     *
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
      *              * Start su [zs1] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-let-flt-nsc-min    to   rf-zs1-sqz-num         .
           move      zero                 to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
       ord-sel-002-125.
      *              *-------------------------------------------------*
      *              * Read Next su [zs1]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-zs1-sqz-num       >    w-let-flt-nsc-max
                     go to ord-sel-002-900.
      *              *-------------------------------------------------*
      *              * Se classe non suddivisa in gruppi               *
      *              *-------------------------------------------------*
           if        rf-zs1-ult-sud       =    02
                     go to ord-sel-002-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record gruppo               *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento semilavorato                  *
      *                  *---------------------------------------------*
           go to     ord-sel-002-275.
       ord-sel-002-150.
      *              *-------------------------------------------------*
      *              * Start su [zs2] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zs1-cod-cla       to   rf-zs2-cod-cla         .
           move      w-let-flt-nsg-min    to   rf-zs2-sqz-num         .
           move      zero                 to   rf-zs2-cod-gru         .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a classe successiva           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-125.
       ord-sel-002-175.
      *              *-------------------------------------------------*
      *              * Read Next su [zs2]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
      *              *-------------------------------------------------*
      *              * Se At End : a classe successiva                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-125.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a classe successiva *
      *              *-------------------------------------------------*
           if        rf-zs2-cod-cla       not  = rf-zs1-cod-cla
                     go to ord-sel-002-125.
           if        rf-zs2-sqz-num       >    w-let-flt-nsg-max
                     go to ord-sel-002-125.
      *              *-------------------------------------------------*
      *              * Se gruppo non suddiviso in sottogruppi          *
      *              *-------------------------------------------------*
           if        rf-zs2-ult-sud       =    02
                     go to ord-sel-002-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento semilavorato                  *
      *                  *---------------------------------------------*
           go to     ord-sel-002-275.
       ord-sel-002-200.
      *              *-------------------------------------------------*
      *              * Start su [zs3] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zs1-cod-cla       to   rf-zs3-cod-cla         .
           move      rf-zs2-cod-gru       to   rf-zs3-cod-gru         .
           move      w-let-flt-nss-min    to   rf-zs3-sqz-num         .
           move      zero                 to   rf-zs3-cod-sgr         .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a gruppo successivo           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-175.
       ord-sel-002-225.
      *              *-------------------------------------------------*
      *              * Read Next su [zs3]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
      *              *-------------------------------------------------*
      *              * Se At End : a gruppo successivo                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-175.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a gruppo successivo *
      *              *-------------------------------------------------*
           if        rf-zs3-cod-cla       not  = rf-zs1-cod-cla
                     go to ord-sel-002-175.
           if        rf-zs3-cod-gru       not  = rf-zs2-cod-gru
                     go to ord-sel-002-175.
           if        rf-zs3-sqz-num       >    w-let-flt-nss-max
                     go to ord-sel-002-175.
      *              *-------------------------------------------------*
      *              * A trattamento semilavorato                      *
      *              *-------------------------------------------------*
           go to     ord-sel-002-275.
       ord-sel-002-250.
      *              *-------------------------------------------------*
      *              * Riciclo a :                                     *
      *              * - classe                                        *
      *              * - gruppo                                        *
      *              * - sottogruppo                                   *
      *              * a seconda dei tipi di suddivisione              *
      *              *-------------------------------------------------*
           if        rf-zs1-ult-sud       not  = 02
                     go to ord-sel-002-125
           else if   rf-zs2-ult-sud       not  = 02
                     go to ord-sel-002-175
           else      go to ord-sel-002-225.
       ord-sel-002-275.
      *              *-------------------------------------------------*
      *              * Start su [dps] per Classe, gruppo, sottogruppo  *
      *              *                  e descrizione semilavorato     *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "CGSDES    "         to   f-key                  .
           move      rf-zs1-cod-cla       to   rf-dps-cla-sem of
                                               rf-dps                 .
           if        rf-zs1-ult-sud       not  = 02
                     move  zero           to   rf-dps-gru-sem of
                                               rf-dps
                     move  zero           to   rf-dps-sgr-sem of
                                               rf-dps
                     go to ord-sel-002-280.
           move      rf-zs2-cod-gru       to   rf-dps-gru-sem of
                                               rf-dps                 .
           if        rf-zs2-ult-sud       not  = 02
                     move  zero           to   rf-dps-sgr-sem of
                                               rf-dps
                     go to ord-sel-002-280.
           move      rf-zs3-cod-sgr       to   rf-dps-sgr-sem of
                                               rf-dps                 .
       ord-sel-002-280.
           move      w-let-flt-des-min    to   rf-dps-des-key of
                                               rf-dps                 .
           move      zero                 to   rf-dps-num-sem of
                                               rf-dps                 .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : riciclo a classe o gruppo o   *
      *              * sottogruppo                                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-250.
       ord-sel-002-300.
      *              *-------------------------------------------------*
      *              * Read Next su [dps]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se At End : riciclo a classe o gruppo o sotto-  *
      *              * gruppo                                          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-250.
       ord-sel-002-325.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : riciclo a classe o  *
      *              * gruppo o sottogruppo                            *
      *              *-------------------------------------------------*
           if        rf-dps-cla-sem of
                     rf-dps               not  = rf-zs1-cod-cla
                     go to ord-sel-002-250.
           if        rf-zs1-ult-sud       not  = 02
                     go to ord-sel-002-327.
           if        rf-dps-gru-sem of
                     rf-dps               not  = rf-zs2-cod-gru
                     go to ord-sel-002-250.
           if        rf-zs2-ult-sud       not  = 02
                     go to ord-sel-002-327.
           if        rf-dps-sgr-sem of
                     rf-dps               not  = rf-zs3-cod-sgr
                     go to ord-sel-002-250.
       ord-sel-002-327.
           if        rf-dps-des-key of
                     rf-dps               >    w-let-flt-des-max
                     go to ord-sel-002-250.
       ord-sel-002-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record let-  *
      *              * to, se non superata : riciclo a Read Next sul   *
      *              * file [dps]                                      *
      *              *-------------------------------------------------*
           perform   sel-rec-flt-000      thru sel-rec-flt-999        .
           if        w-ord-sel-flg-srf    not  = spaces
                     go to ord-sel-002-300.
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
           move      rf-dps               to   rf-sss                 .
       ord-sel-002-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dps               to   rf-ttt                 .
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
           go to     ord-sel-002-300.
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
      *    * Ordinamento e selezione [dps]     Tipo ordinamento  03    *
      *    *                                                           *
      *    * Codice semilavorato                                       *
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
      *              * Start su [dps] per codice alfanumerico          *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "ALFSEM    "         to   f-key                  .
           move      w-let-flt-alf-min    to   rf-dps-alf-sem of
                                               rf-dps                 .
           move      zero                 to   rf-dps-num-sem of
                                               rf-dps                 .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-200.
      *              *-------------------------------------------------*
      *              * Read Next su [dps]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-dps-alf-sem of
                     rf-dps               >    w-let-flt-alf-max
                     go to ord-sel-003-900.
       ord-sel-003-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record let-  *
      *              * to, se non superata : riciclo a Read Next       *
      *              *-------------------------------------------------*
           perform   sel-rec-flt-000      thru sel-rec-flt-999        .
           if        w-ord-sel-flg-srf    not  = spaces
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
           move      rf-dps               to   rf-sss                 .
       ord-sel-003-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dps               to   rf-ttt                 .
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
      *    * Ordinamento e selezione [dps]     Tipo ordinamento  04    *
      *    *                                                           *
      *    * Descrizione semilavorato                                  *
      *    *-----------------------------------------------------------*
       ord-sel-004-000.
      *              *-------------------------------------------------*
      *              * Open output file [ttt]                          *
      *              *-------------------------------------------------*
           perform   opn-out-ttt-000      thru opn-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-004-900.
       ord-sel-004-100.
      *              *-------------------------------------------------*
      *              * Start su [dps] per descrizione in uppercase     *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "DESKEY    "         to   f-key                  .
           move      w-let-flt-des-min    to   rf-dps-des-key of
                                               rf-dps                 .
           move      zero                 to   rf-dps-num-sem of
                                               rf-dps                 .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-004-900.
       ord-sel-004-200.
      *              *-------------------------------------------------*
      *              * Read Next su [dps]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-004-900.
       ord-sel-004-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-dps-des-key of
                     rf-dps               >    w-let-flt-des-max
                     go to ord-sel-004-900.
       ord-sel-004-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record let-  *
      *              * to, se non superata : riciclo a Read Next       *
      *              *-------------------------------------------------*
           perform   sel-rec-flt-000      thru sel-rec-flt-999        .
           if        w-ord-sel-flg-srf    not  = spaces
                     go to ord-sel-004-200.
       ord-sel-004-500.
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
                     go to ord-sel-004-600.
           move      rf-dps               to   rf-sss                 .
       ord-sel-004-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dps               to   rf-ttt                 .
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
                     go to ord-sel-004-900.
      *                  *---------------------------------------------*
      *                  * Se Ok : riciclo a Read Next                 *
      *                  *---------------------------------------------*
           go to     ord-sel-004-200.
       ord-sel-004-900.
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
       ord-sel-004-999.
           exit.

      *    *===========================================================*
      *    * Selezione, mediante il filtro, del record in 'rf-dps'     *
      *    *-----------------------------------------------------------*
       sel-rec-flt-000.
      *              *-------------------------------------------------*
      *              * Selezione su classe, gruppo, sottogruppo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per classe, gruppo, e   *
      *                  * sottogruppo : no selezione                  *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    01 or
                     w-let-flt-tip-ord    =    02
                     go to sel-rec-flt-150.
       sel-rec-flt-025.
      *                  *---------------------------------------------*
      *                  * Selezione su classe                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza classe min-max    *
      *                      * pari ai limiti massimi : no selezione   *
      *                      *-----------------------------------------*
           if        w-let-flt-nsc-min    =    0000000 and
                     w-let-flt-nsc-max    =    9999999
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se una sola classe, e il record letto   *
      *                      * e' proprio di quella classe : no sele-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-let-flt-cla-min    =    w-let-flt-cla-max and
                     rf-dps-cla-sem of
                     rf-dps               =    w-let-flt-cla-min
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Lettura codice classe, se record non    *
      *                      * trovato : no selezione                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      rf-dps-cla-sem of
                     rf-dps               to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza per la classe non *
      *                      * compreso tra quelli da selezionare : la *
      *                      * selezione si considera fallita          *
      *                      *-----------------------------------------*
           if        rf-zs1-sqz-num       <    w-let-flt-nsc-min or
                     rf-zs1-sqz-num       >    w-let-flt-nsc-max
                     go to sel-rec-flt-900.
       sel-rec-flt-050.
      *                  *---------------------------------------------*
      *                  * Selezione su gruppo                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se classe non suddivisa in gruppi : no  *
      *                      * selezione                               *
      *                      *-----------------------------------------*
           if        rf-zs1-ult-sud       not  = 02
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza gruppo min-max    *
      *                      * pari ai limiti massimi : no selezione   *
      *                      *-----------------------------------------*
           if        w-let-flt-nsg-min    =    0000000 and
                     w-let-flt-nsg-max    =    9999999
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se un solo gruppo, e il record letto e' *
      *                      * proprio di quel gruppo : no selezione   *
      *                      *-----------------------------------------*
           if        w-let-flt-gru-min    =    w-let-flt-gru-max and
                     rf-dps-gru-sem of
                     rf-dps               =    w-let-flt-gru-min
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Lettura codice gruppo, se record non    *
      *                      * trovato : no selezione                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      rf-dps-cla-sem of
                     rf-dps               to   rf-zs2-cod-cla         .
           move      rf-dps-gru-sem of
                     rf-dps               to   rf-zs2-cod-gru         .
           move      "pgm/dps/fls/ioc/obj/iofzs2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs2                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza per il gruppo non *
      *                      * compreso tra quelli da selezionare : la *
      *                      * selezione si considera fallita          *
      *                      *-----------------------------------------*
           if        rf-zs2-sqz-num       <    w-let-flt-nsg-min or
                     rf-zs2-sqz-num       >    w-let-flt-nsg-max
                     go to sel-rec-flt-900.
       sel-rec-flt-075.
      *                  *---------------------------------------------*
      *                  * Selezione su sottogruppo                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se gruppo non suddiviso in sottogruppi  *
      *                      * : no selezione                          *
      *                      *-----------------------------------------*
           if        rf-zs2-ult-sud       not  = 02
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza del sottogruppo   *
      *                      * min-max pari ai limiti massimi : no se- *
      *                      * lezione                                 *
      *                      *-----------------------------------------*
           if        w-let-flt-nss-min    =    0000000 and
                     w-let-flt-nss-max    =    9999999
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se un solo sottogruppo, e il record     *
      *                      * letto e' proprio di quel sottogruppo    *
      *                      * : no selezione                          *
      *                      *-----------------------------------------*
           if        w-let-flt-sgr-min    =    w-let-flt-sgr-max and
                     rf-dps-sgr-sem of
                     rf-dps               =    w-let-flt-sgr-min
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Lettura codice sottogruppo, se record   *
      *                      * non trovato : no selezione              *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      rf-dps-cla-sem of
                     rf-dps               to   rf-zs3-cod-cla         .
           move      rf-dps-gru-sem of
                     rf-dps               to   rf-zs3-cod-gru         .
           move      rf-dps-sgr-sem of
                     rf-dps               to   rf-zs3-cod-sgr         .
           move      "pgm/dps/fls/ioc/obj/iofzs3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs3                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza per il sottogrup- *
      *                      * po non compreso tra quelli da selezio-  *
      *                      *  nare : la selezione si considera fal-  *
      *                      * lita                                    *
      *                      *-----------------------------------------*
           if        rf-zs3-sqz-num       <    w-let-flt-nss-min or
                     rf-zs3-sqz-num       >    w-let-flt-nss-max
                     go to sel-rec-flt-900.
       sel-rec-flt-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice alfanumerico min-max        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per classe, gruppo, e   *
      *                  * sottogruppo : no selezione                  *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    01 or
                     w-let-flt-tip-ord    =    02
                     go to sel-rec-flt-200.
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per codice alfanumerico *
      *                  * : no selezione                              *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    03
                     go to sel-rec-flt-200.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-alf-sem of
                     rf-dps               <    w-let-flt-alf-min or
                     rf-dps-alf-sem of
                     rf-dps               >    w-let-flt-alf-max
                     go to sel-rec-flt-900.
       sel-rec-flt-200.
      *              *-------------------------------------------------*
      *              * Selezione su descrizione uppercase min-max      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per classe, gruppo, e   *
      *                  * sottogruppo : no selezione                  *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    01 or
                     w-let-flt-tip-ord    =    02
                     go to sel-rec-flt-250.
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per descrizione : no    *
      *                  * selezione                                   *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    04
                     go to sel-rec-flt-250.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-des-key of
                     rf-dps               <    w-let-flt-des-min or
                     rf-dps-des-key of
                     rf-dps               >    w-let-flt-des-max
                     go to sel-rec-flt-900.
       sel-rec-flt-250.
      *              *-------------------------------------------------*
      *              * Selezione su tipo semilavorato                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo s.l. a zero : no selezione          *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-sem    =    zero
                     go to sel-rec-flt-300.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-tip-sem of
                     rf-dps               not  = w-let-flt-tip-sem
                     go to sel-rec-flt-900.
       sel-rec-flt-300.
      *              *-------------------------------------------------*
      *              * Selezione su unita' di misura semilavorato      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se unita' di misura a Spaces : no selezione *
      *                  *---------------------------------------------*
           if        w-let-flt-umi-prd    =    spaces
                     go to sel-rec-flt-450.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-umi-prd of
                     rf-dps               not  = w-let-flt-umi-prd
                     go to sel-rec-flt-900.
       sel-rec-flt-450.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 1 semilavorato   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 1 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s01    =    zero
                     go to sel-rec-flt-500.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-cod-s01 of
                     rf-dps               not  = w-let-flt-cod-s01
                     go to sel-rec-flt-900.
       sel-rec-flt-500.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 2 semilavorato   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 2 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s02    =    zero
                     go to sel-rec-flt-550.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-cod-s02 of
                     rf-dps               not  = w-let-flt-cod-s02
                     go to sel-rec-flt-900.
       sel-rec-flt-550.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 3 semilavorato   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 3 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s03    =    zero
                     go to sel-rec-flt-800.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-cod-s03 of
                     rf-dps               not  = w-let-flt-cod-s03
                     go to sel-rec-flt-900.
       sel-rec-flt-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ok                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-ord-sel-flg-srf      .
           go to     sel-rec-flt-999.
       sel-rec-flt-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ko                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ord-sel-flg-srf      .
           go to     sel-rec-flt-999.
       sel-rec-flt-999.
           exit.

      *    *===========================================================*
      *    * Selezione, mediante il filtro, del record in 'rf-lll'     *
      *    *-----------------------------------------------------------*
       sel-rec-sel-000.
      *              *-------------------------------------------------*
      *              * Spostamento record da link-area a comodo per la *
      *              * ridefinizione dei campi                         *
      *              *-------------------------------------------------*
           move      rf-lll               to   rf-dps                 .
      *              *-------------------------------------------------*
      *              * Se il codice filtro di selezione indica un sin- *
      *              * golo codice anagrafico : ad apposito trattamen- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        w-let-flt-cod-tco    not  = zero
                     go to sel-rec-sel-700.
       sel-rec-sel-025.
      *              *-------------------------------------------------*
      *              * Selezione su classe, gruppo, sottogruppo        *
      *              *-------------------------------------------------*
       sel-rec-sel-050.
      *                  *---------------------------------------------*
      *                  * Selezione su classe                         *
      *                  *---------------------------------------------*
           if        rf-dps-cla-sem of
                     rf-dps               <    w-let-flt-cla-min or
                     rf-dps-cla-sem of
                     rf-dps               >    w-let-flt-cla-max
                     go to sel-rec-sel-900.
       sel-rec-sel-075.
      *                  *---------------------------------------------*
      *                  * Selezione su gruppo                         *
      *                  *---------------------------------------------*
           if        rf-dps-gru-sem of
                     rf-dps               <    w-let-flt-gru-min or
                     rf-dps-gru-sem of
                     rf-dps               >    w-let-flt-gru-max
                     go to sel-rec-sel-900.
       sel-rec-sel-100.
      *                  *---------------------------------------------*
      *                  * Selezione su sottogruppo                    *
      *                  *---------------------------------------------*
           if        rf-dps-sgr-sem of
                     rf-dps               <    w-let-flt-sgr-min or
                     rf-dps-sgr-sem of
                     rf-dps               >    w-let-flt-sgr-max
                     go to sel-rec-sel-900.
       sel-rec-sel-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice alfanumerico min-max        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-alf-sem of
                     rf-dps               <    w-let-flt-alf-min or
                     rf-dps-alf-sem of
                     rf-dps               >    w-let-flt-alf-max
                     go to sel-rec-sel-900.
       sel-rec-sel-200.
      *              *-------------------------------------------------*
      *              * Selezione su descrizione uppercase min-max      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-des-key of
                     rf-dps               <    w-let-flt-des-min or
                     rf-dps-des-key of
                     rf-dps               >    w-let-flt-des-max
                     go to sel-rec-sel-900.
       sel-rec-sel-250.
      *              *-------------------------------------------------*
      *              * Selezione su tipo semilavorato                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo s.l. a zero : no selezione          *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-sem    =    zero
                     go to sel-rec-sel-300.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-tip-sem of
                     rf-dps               not  = w-let-flt-tip-sem
                     go to sel-rec-sel-900.
       sel-rec-sel-300.
      *              *-------------------------------------------------*
      *              * Selezione su unita' di misura semilavorato      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se unita' di misura a Spaces : no selezione *
      *                  *---------------------------------------------*
           if        w-let-flt-umi-prd    =    spaces
                     go to sel-rec-sel-450.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-umi-prd of
                     rf-dps               not  = w-let-flt-umi-prd
                     go to sel-rec-sel-900.
       sel-rec-sel-450.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 1 semilavorato   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 1 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s01    =    zero
                     go to sel-rec-sel-500.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-cod-s01 of
                     rf-dps               not  = w-let-flt-cod-s01
                     go to sel-rec-sel-900.
       sel-rec-sel-500.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 2 semilavorato   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 2 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s02    =    zero
                     go to sel-rec-sel-550.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-cod-s02 of
                     rf-dps               not  = w-let-flt-cod-s02
                     go to sel-rec-sel-900.
       sel-rec-sel-550.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 3 semilavorato   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 3 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s03    =    zero
                     go to sel-rec-sel-600.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dps-cod-s03 of
                     rf-dps               not  = w-let-flt-cod-s03
                     go to sel-rec-sel-900.
       sel-rec-sel-600.
      *              *-------------------------------------------------*
      *              * Ad uscita per selezione Ok                      *
      *              *-------------------------------------------------*
           go to     sel-rec-sel-800.
       sel-rec-sel-700.
      *              *-------------------------------------------------*
      *              * Se il codice filtro di selezione indica un sin- *
      *              * golo codice anagrafico                          *
      *              *-------------------------------------------------*
       sel-rec-sel-720.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di codice a-  *
      *                  * nagrafico                                   *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-tco    =    1
                     go to sel-rec-sel-740
           else      go to sel-rec-sel-760.
       sel-rec-sel-740.
      *                  *---------------------------------------------*
      *                  * Se tipo di codice anagrafico : [dps]        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selezione su codice numerico            *
      *                      *-----------------------------------------*
           if        rf-dps-num-sem of
                     rf-dps               =    w-let-flt-cod-cco
                     go to sel-rec-sel-800
           else      go to sel-rec-sel-900.
       sel-rec-sel-760.
      *                  *---------------------------------------------*
      *                  * Se tipo di codice anagrafico : [zs1]        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selezione su codice classe              *
      *                      *-----------------------------------------*
           if        rf-dps-cla-sem of
                     rf-dps               =    w-let-flt-cod-cco
                     go to sel-rec-sel-800
           else      go to sel-rec-sel-900.
       sel-rec-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ok                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-ord-sel-flg-srf      .
           go to     sel-rec-sel-999.
       sel-rec-sel-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ko                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ord-sel-flg-srf      .
           go to     sel-rec-sel-999.
       sel-rec-sel-999.
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
