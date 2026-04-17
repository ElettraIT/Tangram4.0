       Identification Division.
       Program-Id.                                 bzosdcp0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/08/91    *
      *                       Ultima revisione:    NdK del 28/05/14    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la selezione secondo il filtro   *
      *                    di ordinamento e selezione per file [dcp]   *
      *                                                                *
      *                    _________________________________________   *
      *                                                                *
      *                    MEMO : Creare un'area di interfaccia        *
      *                          'bzosdcp0.dtl' per risolvere il       *
      *                           tipo ordinamento e altri parametri   *
      *                           eventuali                            *
      *                    _________________________________________   *
      *                                                                *
      *                    MEMO : Attenzione a stampe o interrogazio-  *
      *                           ni su 'cpv' - cataloghi per via      *
      *                           del ripristino start ...             *
      *                    _________________________________________   *
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
      *                       f-key  : codice del filtro [dcp]         *
      *                                                                *
      *                                                                *
      *              Output : f-sts  : "01" : Per classe, gruppo, sot- *
      *                                       togruppo, codice prodot- *
      *                                       to                       *
      *                                "02" : Per classe, gruppo, sot- *
      *                                       togruppo, descr. prodot- *
      *                                       to                       *
      *                                "03" : Per codice prodotto      *
      *                                "04" : Per descrizione prodotto *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "SE"  Richiesta di sola selezione su record                    *
      *                                                                *
      *              Input  : f-ope  : "SE"                            *
      *                       rf-dcp : Record [dcp]                    *
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
      *                       f-key  : codice del filtro [dcp]         *
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
      *                       rf-dcp : Record [dcp]                    *
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

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

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
      *    * Sort file per [dcp]                                       *
      *    *-----------------------------------------------------------*
       sd  sss.
           copy      "pgm/dcp/fls/rec/rfdcp"
                                     replacing rf-dcp
                                          by   rf-sss                 .

      *    *===========================================================*
      *    * Work file per uscita da sort di [dcp]                     *
      *    *-----------------------------------------------------------*
       fd  ttt  label record omitted.
           copy      "pgm/dcp/fls/rec/rfdcp"
                                     replacing rf-dcp
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
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [zp2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp2"                          .
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .
      *        *-------------------------------------------------------*
      *        * [cpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfcpv"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento prodotti nell'ambito della classe    *
      *        * merceologica in caso di richiesta filtro di tipo ':C' *
      *        *                                                       *
      *        *  - '01' : Per classe, gruppo, sottogruppo, codice     *
      *        *           prodotto                                    *
      *        *  - '02' : Per classe, gruppo, sottogruppo, descrizio- *
      *        *           ne prodotto                                 *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-prs-top-cgs              pic  9(02)                  .
               
      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dcp '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/wzosdcp0.wkl"                   .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dcp', se selezione casuale prodotti               *
      *    *-----------------------------------------------------------*
       01  w-zos-esl-dcp.
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento, selezione                           *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-tip          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero di elementi selezionati                        *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-els          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Filler                                                *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-fil.
               10  filler     occurs  4   pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Selezione prodotti                                    *
      *        *-------------------------------------------------------*
           05  w-zos-esl-dcp-sel.
               10  w-zos-esl-dcp-ele occurs 72
                                 indexed  by   w-zos-esl-dcp-inx      .
                   15  w-zos-esl-dcp-num  pic  9(07)                  .

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
               10  w-let-flt-cla-red  redefines
                   w-let-flt-cla-min.
                   15  w-let-flt-num-ele  pic  9(02)                  .
                   15  w-let-flt-fil-ler  pic  9(03)                  .
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
      *            * Codice prodotto alfanumerico min-max              *
      *            *---------------------------------------------------*
               10  w-let-flt-alf-min      pic  x(14)                  .
               10  w-let-flt-alf-max      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Descrizione prodotto, uppercase, min-max          *
      *            *---------------------------------------------------*
               10  w-let-flt-des-min      pic  x(40)                  .
               10  w-let-flt-des-max      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Tipo prodotto                                     *
      *            *---------------------------------------------------*
               10  w-let-flt-tip-pro      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice Iva prodotto                               *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-iva      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice contropartita prodotto                     *
      *            *---------------------------------------------------*
               10  w-let-flt-ctp-ven      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura prodotto                         *
      *            *---------------------------------------------------*
               10  w-let-flt-umi-ven      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Codice statistico 1 prodotto                      *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-s01      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice statistico 2 prodotto                      *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-s02      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice statistico 3 prodotto                      *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-s03      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Status prodotto                                   *
      *            *---------------------------------------------------*
               10  w-let-flt-sta-tus      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Status prodotto (nuovo)                           *
      *            *---------------------------------------------------*
               10  w-let-flt-sta-tuw.
                   15  w-let-flt-sta-nor  pic  x(01)                  .
                   15  w-let-flt-sta-esa  pic  x(01)                  .
                   15  w-let-flt-sta-sos  pic  x(01)                  .
                   15  w-let-flt-sta-ces  pic  x(01)                  .
                   15  w-let-flt-sta-cms  pic  x(01)                  .
                   15  w-let-flt-sta-obs  pic  x(01)                  .
                   15  w-let-flt-sta-oms  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice fornitore                                  *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-fnt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice casa produttrice                           *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-pdt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Specifica libera                                  *
      *            *---------------------------------------------------*
               10  w-let-flt-spc-lib      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Data inizio commercializzazione min-max           *
      *            *---------------------------------------------------*
               10  w-let-flt-icm-min      pic  9(07)                  .
               10  w-let-flt-icm-max      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto fornitore preferenz., min e max   *
      *            *---------------------------------------------------*
               10  w-let-flt-cfp-min      pic  x(20)                  .
               10  w-let-flt-cfp-max      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Codice catalogo                                   *
      *            *---------------------------------------------------*
               10  w-let-flt-cod-cpv      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Area per scansione filtro selezione casuale           *
      *        *-------------------------------------------------------*
           05  w-let-flt-ctr-001          pic  9(02)                  .
           05  w-let-flt-ctr-max          pic  9(02) value 72         .

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
      *        *-------------------------------------------------------*
      *        * Primo carattere della descrizione                     *
      *        *-------------------------------------------------------*
           05  w-pul-zos-pcr-des          pic  x(01)                  .

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
      *    * Record file [dcp]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"
                                     replacing rf-dcp
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
      *              * Status del work file [ttt] : Chiuso             *
      *              *-------------------------------------------------*
           move      00                   to   w-ord-sel-sts-ttt      .
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
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * [cpv]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofcpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpv                 .
       opn-600.
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione per Tipo ordina-  *
      *                  * mento prodotti nell'ambito della classe     *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp/zos[top-cgs]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-top-cgs
           else      move  01             to   w-prs-top-cgs          .
      *                      *-----------------------------------------*
      *                      * Normalizzazione personalizzazione       *
      *                      *-----------------------------------------*
           if        w-prs-top-cgs        <    01 or
                     w-prs-top-cgs        >    02
                     move  01             to   w-prs-top-cgs          .
       opn-800.
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
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * [cpv]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofcpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpv                 .
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
           move      "dcp "               to   rf-zos-tip-rec         .
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
           if        rf-zos-tip-rec       not  = "dcp "
                     go to pul-mne-spc-800.
           if        rf-zos-cod-mne       not  = spaces
                     go to pul-mne-spc-800.
       pul-mne-spc-350.
      *              *-------------------------------------------------*
      *              * Selezione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice filtro                       *
      *                  *---------------------------------------------*
           if        rf-zos-cod-flt       =    zero or
                     rf-zos-cod-flt       =    9999999
                     go to pul-mne-spc-500.
      *                  *---------------------------------------------*
      *                  * Test su descrizione filtro                  *
      *                  *---------------------------------------------*
           move      rf-zos-des-flt       to   w-pul-zos-pcr-des      .
           if        w-pul-zos-pcr-des    not  = spaces
                     go to pul-mne-spc-500.
      *                  *---------------------------------------------*
      *                  * Test su data immissione filtro              *
      *                  *---------------------------------------------*
           if        rf-zos-ide-dat       not  < w-pul-zos-dat-att
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
           else      move  "03"           to   f-sts                  .
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
      *              * Ordinamento e selezione [dcp] su filtro         *
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
      *    * dell'area libera per il filtro 'dcp'                      *
      *    *-----------------------------------------------------------*
       nor-zos-dcp-000.
      *              *-------------------------------------------------*
      *              * Tipo ordinamento : Per classe, gruppo, sotto-   *
      *              *                        gruppo, codice prodotto  *
      *              *-------------------------------------------------*
           move      01                   to   w-zos-dcp-tip-ord      .
      *              *-------------------------------------------------*
      *              * Classe merceologica min-max                     *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-cla-min      .
           move      99999                to   w-zos-dcp-cla-max      .
      *              *-------------------------------------------------*
      *              * Gruppo merceologico min-max                     *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-gru-min      .
           move      99999                to   w-zos-dcp-gru-max      .
      *              *-------------------------------------------------*
      *              * Sottogruppo merceologico min-max                *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-sgr-min      .
           move      99999                to   w-zos-dcp-sgr-max      .
      *              *-------------------------------------------------*
      *              * Codice prodotto alfanumerico min e max          *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dcp-alf-min      .
           move      all   "z"            to   w-zos-dcp-alf-max      .
      *              *-------------------------------------------------*
      *              * Descrizione prodotto uppercase min-max          *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dcp-des-min      .
           move      all   "z"            to   w-zos-dcp-des-max      .
      *              *-------------------------------------------------*
      *              * Tipo prodotto                                   *
      *              *-------------------------------------------------*
           move      00                   to   w-zos-dcp-tip-pro      .
      *              *-------------------------------------------------*
      *              * Codice Iva prodotto                             *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-cod-iva      .
      *              *-------------------------------------------------*
      *              * Codice contropartita prodotto                   *
      *              *-------------------------------------------------*
           move      0000000              to   w-zos-dcp-ctp-ven      .
      *              *-------------------------------------------------*
      *              * Unita' di misura prodotto                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dcp-umi-ven      .
      *              *-------------------------------------------------*
      *              * Codice statistico 1 prodotto                    *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-cod-s01      .
      *              *-------------------------------------------------*
      *              * Codice statistico 2 prodotto                    *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-cod-s02      .
      *              *-------------------------------------------------*
      *              * Codice statistico 3 prodotto                    *
      *              *-------------------------------------------------*
           move      00000                to   w-zos-dcp-cod-s03      .
      *              *-------------------------------------------------*
      *              * Status prodotto                                 *
      *              *-------------------------------------------------*
           move      00                   to   w-zos-dcp-sta-tus      .
      *              *-------------------------------------------------*
      *              * Status prodotto (nuovo)                         *
      *              *-------------------------------------------------*
           move      all spaces           to   w-zos-dcp-sta-tuw      .
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
           move      0000000              to   w-zos-dcp-cod-fnt      .
      *              *-------------------------------------------------*
      *              * Specifica libera prodotto                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dcp-spc-lib      .
      *              *-------------------------------------------------*
      *              * Data inizio commercializzazione min-max         *
      *              *-------------------------------------------------*
           move      0000000              to   w-zos-dcp-icm-min      .
           move      9999999              to   w-zos-dcp-icm-max      .
      *              *-------------------------------------------------*
      *              * Codice prodotto per il fornitore preferenziale  *
      *              *-------------------------------------------------*
           move      spaces               to   w-zos-dcp-cfp-min      .
           move      all   "Z"            to   w-zos-dcp-cfp-max      .
       nor-zos-dcp-999.
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
           move      "dcp "               to   rf-zos-tip-rec         .
           move      w-let-flt-cod-cco    to   rf-zos-cod-flt         .
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
      *                      * 'dcp'                                   *
      *                      *-----------------------------------------*
           perform   nor-zos-dcp-000      thru nor-zos-dcp-999        .
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
           move      rf-zos-dat-flt       to   w-zos-dcp              .
      *                      *-----------------------------------------*
      *                      * Test se tipo ordinamento prevede una    *
      *                      * selezione su codice catalogo            *
      *                      *-----------------------------------------*
           if        w-zos-dcp-tip-ord    =    89
                     move  w-zos-dcp-tip-ord
                                          to   w-let-flt-tip-ord
                     move  w-zos-dcp-cod-cpv
                                          to   w-let-flt-cod-cpv
                     go to let-cod-flt-900.
      *                      *-----------------------------------------*
      *                      * Test se tipo ordinamento prevede una    *
      *                      * selezione casuale                       *
      *                      *-----------------------------------------*
           if        w-zos-dcp-tip-ord    =    99
                     move  w-zos-dcp-tip-ord
                                          to   w-let-flt-tip-ord
                     move  w-zos-dcp-cla-min
                                          to   w-let-flt-cla-min
                     go to let-cod-flt-900.
      *                      *-----------------------------------------*
      *                      * Test su tipo ordinamento : se non pre-  *
      *                      * visto, come se filtro non esistente     *
      *                      *-----------------------------------------*
           if        w-zos-dcp-tip-ord    <    01 or
                     w-zos-dcp-tip-ord    >    04
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
           move      w-zos-dcp-tip-ord    to   w-let-flt-tip-ord      .
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
           move      w-zos-dcp-cla-min    to   w-let-flt-cla-min      .
      *                          *-------------------------------------*
      *                          * Codice classe max                   *
      *                          *-------------------------------------*
           move      w-zos-dcp-cla-max    to   w-let-flt-cla-max      .
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
           move      w-zos-dcp-gru-min    to   w-let-flt-gru-min      .
      *                          *-------------------------------------*
      *                          * Codice gruppo max                   *
      *                          *-------------------------------------*
           move      w-zos-dcp-gru-max    to   w-let-flt-gru-max      .
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
           move      w-zos-dcp-sgr-min    to   w-let-flt-sgr-min      .
      *                          *-------------------------------------*
      *                          * Codice sottogruppo max              *
      *                          *-------------------------------------*
           move      w-zos-dcp-sgr-max    to   w-let-flt-sgr-max      .
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
           move      w-let-flt-cla-min    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp1-sqz-num         .
           move      rf-zp1-sqz-num       to   w-let-flt-nsc-min      .
           move      rf-zp1-sqz-num       to   w-let-flt-nsc-max      .
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
           move      w-let-flt-cla-min    to   rf-zp2-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp2-sqz-num         .
           move      rf-zp2-sqz-num       to   w-let-flt-nsg-min      .
           move      rf-zp2-sqz-num       to   w-let-flt-nsg-max      .
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
           move      w-let-flt-cla-min    to   rf-zp3-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zp3-cod-gru         .
           move      w-let-flt-sgr-min    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp3-sqz-num         .
           move      rf-zp3-sqz-num       to   w-let-flt-nss-min      .
           move      rf-zp3-sqz-num       to   w-let-flt-nss-max      .
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
           move      w-let-flt-cla-min    to   rf-zp3-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zp3-cod-gru         .
           move      w-let-flt-sgr-min    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp3-sqz-num         .
           move      rf-zp3-sqz-num       to   w-let-flt-nss-min      .
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
           move      w-let-flt-cla-min    to   rf-zp3-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zp3-cod-gru         .
           move      w-let-flt-sgr-max    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp3-sqz-num         .
           move      rf-zp3-sqz-num       to   w-let-flt-nss-max      .
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
           move      w-let-flt-cla-min    to   rf-zp2-cod-cla         .
           move      w-let-flt-gru-min    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp2-sqz-num         .
           move      rf-zp2-sqz-num       to   w-let-flt-nsg-min      .
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
           move      w-let-flt-cla-min    to   rf-zp2-cod-cla         .
           move      w-let-flt-gru-max    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp2-sqz-num         .
           move      rf-zp2-sqz-num       to   w-let-flt-nsg-max      .
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
           move      w-let-flt-cla-min    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp1-sqz-num         .
           move      rf-zp1-sqz-num       to   w-let-flt-nsc-min      .
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
           move      w-let-flt-cla-max    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp1-sqz-num         .
           move      rf-zp1-sqz-num       to   w-let-flt-nsc-max      .
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
      *                  * Codice prodotto min : semplice spostamento  *
      *                  *---------------------------------------------*
           move      w-zos-dcp-alf-min    to   w-let-flt-alf-min      .
       let-cod-flt-510.
      *                  *---------------------------------------------*
      *                  * Codice prodotto max : semplice spostamento  *
      *                  *---------------------------------------------*
           move      w-zos-dcp-alf-max    to   w-let-flt-alf-max      .
       let-cod-flt-520.
      *                  *---------------------------------------------*
      *                  * Codice prodotto min-max                     *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-max    =    spaces
                     if    w-let-flt-alf-min
                                          =    spaces
                           move  all "z"  to   w-let-flt-alf-max
                     else  move  w-let-flt-alf-min
                                          to   w-let-flt-alf-max      .
      *                      *-----------------------------------------*
      *                      * Padding                                 *
      *                      *-----------------------------------------*
           move      "z"                  to   w-pad-alf-cdp          .
           move      14                   to   w-pad-alf-max          .
           move      w-let-flt-alf-max    to   w-pad-alf-str          .
           perform   pad-alf-cdp-000      thru pad-alf-cdp-999        .
           move      w-pad-alf-str        to   w-let-flt-alf-max      .
       let-cod-flt-530.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto min : semplice sposta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-zos-dcp-des-min    to   w-let-flt-des-min      .
       let-cod-flt-540.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto max : semplice sposta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-zos-dcp-des-max    to   w-let-flt-des-max      .
       let-cod-flt-550.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto min-max                *
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
      *                  * Tipo prodotto : semplice spostamento        *
      *                  *---------------------------------------------*
           move      w-zos-dcp-tip-pro    to   w-let-flt-tip-pro      .
       let-cod-flt-570.
      *                  *---------------------------------------------*
      *                  * Codice Iva prodotto : semplice spostamento  *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cod-iva    to   w-let-flt-cod-iva      .
       let-cod-flt-580.
      *                  *---------------------------------------------*
      *                  * Codice contropartita prodotto : semplice    *
      *                  * spostamento                                 *
      *                  *---------------------------------------------*
           move      w-zos-dcp-ctp-ven    to   w-let-flt-ctp-ven      .
       let-cod-flt-590.
      *                  *---------------------------------------------*
      *                  * Unita' di misura prodotto : semplice spo-   *
      *                  * stamento                                    *
      *                  *---------------------------------------------*
           move      w-zos-dcp-umi-ven    to   w-let-flt-umi-ven      .
       let-cod-flt-600.
      *                  *---------------------------------------------*
      *                  * Codice statistico 1 prodotto : semplice     *
      *                  * spostamento                                 *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cod-s01    to   w-let-flt-cod-s01      .
       let-cod-flt-610.
      *                  *---------------------------------------------*
      *                  * Codice statistico 2 prodotto : semplice     *
      *                  * spostamento                                 *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cod-s02    to   w-let-flt-cod-s02      .
       let-cod-flt-620.
      *                  *---------------------------------------------*
      *                  * Codice statistico 3 prodotto : semplice     *
      *                  * spostamento                                 *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cod-s03    to   w-let-flt-cod-s03      .
       let-cod-flt-630.
      *                  *---------------------------------------------*
      *                  * Status prodotto : semplice spostamento      *
      *                  *---------------------------------------------*
           move      w-zos-dcp-sta-tuw    to   w-let-flt-sta-tuw      .
           move      w-zos-dcp-sta-tus    to   w-let-flt-sta-tus      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione cautelativa dello       *
      *                      * status prodotto in quanto aggiunto in   *
      *                      * un secondo tempo ai campi gestiti dai   *
      *                      * filtri di selezione                     *
      *                      *-----------------------------------------*
           if        w-let-flt-sta-tus    not  numeric
                     move  zero           to   w-let-flt-sta-tus      .
       let-cod-flt-640.
      *                  *---------------------------------------------*
      *                  * Codice fornitore : semplice spostamento     *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cod-fnt    to   w-let-flt-cod-fnt      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione cautelativa del codice  *
      *                      *-----------------------------------------*
           if        w-let-flt-cod-fnt    not  numeric
                     move  zero           to   w-let-flt-cod-fnt      .
      *                  *---------------------------------------------*
      *                  * Specifica libera : semplice spostamento     *
      *                  *---------------------------------------------*
           move      w-zos-dcp-spc-lib    to   w-let-flt-spc-lib      .
      *                  *---------------------------------------------*
      *                  * Data inizio commercializzazione             *
      *                  *---------------------------------------------*
           move      w-zos-dcp-icm-min    to   w-let-flt-icm-min      .
           move      w-zos-dcp-icm-max    to   w-let-flt-icm-max      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione cautelativa             *
      *                      *-----------------------------------------*
           if        w-zos-dcp-icm-min    not  numeric
                     move  zero           to   w-let-flt-icm-min      .
           if        w-zos-dcp-icm-max    not  numeric
                     move  zero           to   w-let-flt-icm-max      .
      *                      *-----------------------------------------*
      *                      * Regolarizzazione                        *
      *                      *-----------------------------------------*
           if        w-let-flt-icm-max    not  = zero
                     go to let-cod-flt-650.
           if        w-let-flt-icm-min    =    zero
                     move 9999999         to   w-let-flt-icm-max
           else      move w-let-flt-icm-min
                                          to   w-let-flt-icm-max      .
       let-cod-flt-650.
      *                  *---------------------------------------------*
      *                  * Codice produttore : semplice spostamento    *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cod-pdt    to   w-let-flt-cod-pdt      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione cautelativa del codice  *
      *                      *-----------------------------------------*
           if        w-let-flt-cod-pdt    not  numeric
                     move  zero           to   w-let-flt-cod-pdt      .
       let-cod-flt-660.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il fornitore min :      *
      *                  * semplice spostamento                        *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cfp-min    to   w-let-flt-cfp-min      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il fornitore max :      *
      *                  * semplice spostamento                        *
      *                  *---------------------------------------------*
           move      w-zos-dcp-cfp-max    to   w-let-flt-cfp-max      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il fornitore max :      *
      *                  * regolarizzazione                            *
      *                  *---------------------------------------------*
           if        w-let-flt-cfp-max    =    spaces
                     if    w-let-flt-cfp-min
                                          =    spaces
                           move  all "z"  to   w-let-flt-cfp-max
                     else  move  w-let-flt-cfp-min
                                          to   w-let-flt-cfp-max      .
           move      "z"                  to   w-pad-alf-cdp          .
           move      40                   to   w-pad-alf-max          .
           move      w-let-flt-cfp-max    to   w-pad-alf-str          .
           perform   pad-alf-cdp-000      thru pad-alf-cdp-999        .
           move      w-pad-alf-str        to   w-let-flt-cfp-max      .
       let-cod-flt-690.
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
      *                  * Se tipo di codice anagrafico : [dcp]        *
      *                  *---------------------------------------------*
       let-cod-flt-742.
      *                      *-----------------------------------------*
      *                      * Lettura codice prodotto                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-let-flt-cod-cco    to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-dcp-alf-pro of
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Mnemonico del filtro                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-mne-flt      .
      *                      *-----------------------------------------*
      *                      * Descrizione del filtro                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-des-flt      .
           if        rf-dcp-alf-pro of
                     rf-dcp               =    spaces
                     go to let-cod-flt-744.
           string    "Codice prodotto = "
                                delimited by   size
                     rf-dcp-alf-pro of
                     rf-dcp
                                delimited by   size
                                          into w-let-flt-des-flt      .
       let-cod-flt-744.
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento : per codice           *
      *                      *-----------------------------------------*
           move      03                   to   w-zos-dcp-tip-ord      .
      *                      *-----------------------------------------*
      *                      * Classe merceologica min e max           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cla-min      .
           move      99999                to   w-zos-dcp-cla-max      .
      *                      *-----------------------------------------*
      *                      * Gruppo merceologico min e max           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-gru-min      .
           move      99999                to   w-zos-dcp-gru-max      .
      *                      *-----------------------------------------*
      *                      * Sottogruppo merceologico min e max      *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-sgr-min      .
           move      99999                to   w-zos-dcp-sgr-max      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto alfanumerico min e max  *
      *                      *-----------------------------------------*
           move      rf-dcp-alf-pro of
                     rf-dcp               to   w-zos-dcp-alf-min      .
           move      rf-dcp-alf-pro of
                     rf-dcp               to   w-zos-dcp-alf-max      .
      *                      *-----------------------------------------*
      *                      * Descrizione uppercase, min e max        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-des-min      .
           move      all   "Z"            to   w-zos-dcp-des-max      .
      *                      *-----------------------------------------*
      *                      * Tipo prodotto                           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-tip-pro      .
      *                      *-----------------------------------------*
      *                      * Codice Iva prodotto                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-iva      .
      *                      *-----------------------------------------*
      *                      * Codice contropartita prodotto           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-ctp-ven      .
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-umi-ven      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 1 prodotto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-s01      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 2 prodotto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-s02      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 3 prodotto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-s03      .
      *                      *-----------------------------------------*
      *                      * Status prodotto                         *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-sta-tus      .
           move      all "#"              to   w-zos-dcp-sta-tuw      .
      *                      *-----------------------------------------*
      *                      * Codice fornitore                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-fnt      .
      *                      *-----------------------------------------*
      *                      * Specifica libera                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-spc-lib      .
      *                      *-----------------------------------------*
      *                      * Data inizio commercializzazione         *
      *                      *-----------------------------------------*
           move      0000000              to   w-zos-dcp-icm-min      .
           move      9999999              to   w-zos-dcp-icm-max      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per il fornitore pre-   *
      *                      * ferenziale                              *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-cfp-min      .
           move      all   "Z"            to   w-zos-dcp-cfp-max      .
       let-cod-flt-746.
      *                      *-----------------------------------------*
      *                      * Area di lettura                         *
      *                      *-----------------------------------------*
           move      w-zos-dcp-tip-ord    to   w-let-flt-tip-ord      .
           move      w-zos-dcp-cla-min    to   w-let-flt-cla-min      .
           move      w-zos-dcp-cla-max    to   w-let-flt-cla-max      .
           move      0000000              to   w-let-flt-nsc-min      .
           move      9999999              to   w-let-flt-nsc-max      
           move      w-zos-dcp-gru-min    to   w-let-flt-gru-min      .
           move      w-zos-dcp-gru-max    to   w-let-flt-gru-max      .
           move      0000000              to   w-let-flt-nsg-min      .
           move      9999999              to   w-let-flt-nsg-max      
           move      w-zos-dcp-sgr-min    to   w-let-flt-sgr-min      .
           move      w-zos-dcp-sgr-max    to   w-let-flt-sgr-max      .
           move      0000000              to   w-let-flt-nss-min      .
           move      9999999              to   w-let-flt-nss-max      
           move      w-zos-dcp-alf-min    to   w-let-flt-alf-min      .
           move      w-zos-dcp-alf-max    to   w-let-flt-alf-max      .
           move      w-zos-dcp-des-min    to   w-let-flt-des-min      .
           move      w-zos-dcp-des-max    to   w-let-flt-des-max      .
           move      w-zos-dcp-tip-pro    to   w-let-flt-tip-pro      .
           move      w-zos-dcp-cod-iva    to   w-let-flt-cod-iva      .
           move      w-zos-dcp-ctp-ven    to   w-let-flt-ctp-ven      .
           move      w-zos-dcp-umi-ven    to   w-let-flt-umi-ven      .
           move      w-zos-dcp-cod-s01    to   w-let-flt-cod-s01      .
           move      w-zos-dcp-cod-s02    to   w-let-flt-cod-s02      .
           move      w-zos-dcp-cod-s03    to   w-let-flt-cod-s03      .
           move      w-zos-dcp-sta-tus    to   w-let-flt-sta-tus      .
           move      w-zos-dcp-sta-tuw    to   w-let-flt-sta-tuw      .
           move      w-zos-dcp-cod-fnt    to   w-let-flt-cod-fnt      .
           move      w-zos-dcp-spc-lib    to   w-let-flt-spc-lib      .
           move      w-zos-dcp-icm-min    to   w-let-flt-icm-min      .
           move      w-zos-dcp-icm-max    to   w-let-flt-icm-max      .
           move      w-zos-dcp-cod-pdt    to   w-let-flt-cod-pdt      .
           move      w-zos-dcp-cfp-min    to   w-let-flt-cfp-min      .
           move      w-zos-dcp-cfp-max    to   w-let-flt-cfp-max      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione cautelativa dei campi   *
      *                      * aggiunti successivamente                *
      *                      *-----------------------------------------*
           if        w-zos-dcp-sta-tus    not  numeric
                     move  zero           to   w-let-flt-sta-tus      .
           if        w-zos-dcp-cod-fnt    not  numeric
                     move  zero           to   w-let-flt-cod-fnt      .
           if        w-zos-dcp-icm-min    not  numeric
                     move  zero           to   w-let-flt-icm-min      .
           if        w-zos-dcp-icm-max    not  numeric
                     move  zero           to   w-let-flt-icm-max      .
           if        w-zos-dcp-cod-pdt    not  numeric
                     move  zero           to   w-let-flt-cod-pdt      .
      *                      *-----------------------------------------*
      *                      * Regolarizzazione                        *
      *                      *-----------------------------------------*
           if        w-let-flt-icm-max    not  = zero
                     go to let-cod-flt-748.
           if        w-let-flt-icm-min    =    zero
                     move 9999999         to   w-let-flt-icm-max
           else      move w-let-flt-icm-min
                                          to   w-let-flt-icm-max      .
       let-cod-flt-748.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     let-cod-flt-900.
       let-cod-flt-760.
      *                  *---------------------------------------------*
      *                  * Se tipo di codice anagrafico : [zp1]        *
      *                  *---------------------------------------------*
       let-cod-flt-762.
      *                      *-----------------------------------------*
      *                      * Lettura codice [zp1]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-flt-cod-cco    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-zp1-sqz-num         .
      *                      *-----------------------------------------*
      *                      * Mnemonico del filtro                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-mne-flt      .
      *                      *-----------------------------------------*
      *                      * Descrizione del filtro                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-flt-des-flt      .
           if        rf-zp1-sqz-num       =    zero
                     go to let-cod-flt-764.
           string    "Codice classe merceologica = "
                                delimited by   size
                     rf-zp1-cod-cla
                                delimited by   size
                                          into w-let-flt-des-flt      .
       let-cod-flt-764.
      *                      *-----------------------------------------*
      *                      * Tipo ordinamento : da personalizzazione *
      *                      *                                         *
      *                      *  - '01' : Per classe, gruppo, sotto-    *
      *                      *           gruppo, codice prodotto       *
      *                      *  - '02' : Per classe, gruppo, sotto-    *
      *                      *           gruppo, descrizione prodotto  *
      *                      *                                         *
      *                      *-----------------------------------------*
           move      w-prs-top-cgs        to   w-zos-dcp-tip-ord      .
      *                      *-----------------------------------------*
      *                      * Classe merceologica min e max           *
      *                      *-----------------------------------------*
           move      rf-zp1-cod-cla       to   w-zos-dcp-cla-min      .
           move      rf-zp1-cod-cla       to   w-zos-dcp-cla-max      .
      *                      *-----------------------------------------*
      *                      * Gruppo merceologico min e max           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-gru-min      .
           move      99999                to   w-zos-dcp-gru-max      .
      *                      *-----------------------------------------*
      *                      * Sottogruppo merceologico min e max      *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-sgr-min      .
           move      99999                to   w-zos-dcp-sgr-max      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto alfanumerico min e max  *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-alf-min      .
           move      all   "Z"            to   w-zos-dcp-alf-max      .
      *                      *-----------------------------------------*
      *                      * Descrizione uppercase, min e max        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-des-min      .
           move      all   "Z"            to   w-zos-dcp-des-max      .
      *                      *-----------------------------------------*
      *                      * Tipo prodotto                           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-tip-pro      .
      *                      *-----------------------------------------*
      *                      * Codice Iva prodotto                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-iva      .
      *                      *-----------------------------------------*
      *                      * Codice contropartita prodotto           *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-ctp-ven      .
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-umi-ven      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 1 prodotto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-s01      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 2 prodotto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-s02      .
      *                      *-----------------------------------------*
      *                      * Codice statistico 3 prodotto            *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-s03      .
      *                      *-----------------------------------------*
      *                      * Status prodotto                         *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-sta-tus      .
           move      all "#"              to   w-zos-dcp-sta-tuw      .
      *                      *-----------------------------------------*
      *                      * Codice fornitore                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-zos-dcp-cod-fnt      .
      *                      *-----------------------------------------*
      *                      * Specifica libera                        *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-spc-lib      .
      *                      *-----------------------------------------*
      *                      * Data inizio commercializzazione         *
      *                      *-----------------------------------------*
           move      0000000              to   w-zos-dcp-icm-min      .
           move      9999999              to   w-zos-dcp-icm-max      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per il fornitore pre-   *
      *                      * ferenziale                              *
      *                      *-----------------------------------------*
           move      spaces               to   w-zos-dcp-cfp-min      .
           move      all   "Z"            to   w-zos-dcp-cfp-max      .
       let-cod-flt-766.
      *                      *-----------------------------------------*
      *                      * Area di lettura                         *
      *                      *-----------------------------------------*
           move      w-zos-dcp-tip-ord    to   w-let-flt-tip-ord      .
           move      w-zos-dcp-cla-min    to   w-let-flt-cla-min      .
           move      w-zos-dcp-cla-max    to   w-let-flt-cla-max      .
           move      rf-zp1-sqz-num       to   w-let-flt-nsc-min      .
           move      rf-zp1-sqz-num       to   w-let-flt-nsc-max      .
           move      w-zos-dcp-gru-min    to   w-let-flt-gru-min      .
           move      w-zos-dcp-gru-max    to   w-let-flt-gru-max      .
           move      0000000              to   w-let-flt-nsg-min      .
           move      9999999              to   w-let-flt-nsg-max      
           move      w-zos-dcp-sgr-min    to   w-let-flt-sgr-min      .
           move      w-zos-dcp-sgr-max    to   w-let-flt-sgr-max      .
           move      0000000              to   w-let-flt-nss-min      .
           move      9999999              to   w-let-flt-nss-max      
           move      w-zos-dcp-alf-min    to   w-let-flt-alf-min      .
           move      w-zos-dcp-alf-max    to   w-let-flt-alf-max      .
           move      w-zos-dcp-des-min    to   w-let-flt-des-min      .
           move      w-zos-dcp-des-max    to   w-let-flt-des-max      .
           move      w-zos-dcp-tip-pro    to   w-let-flt-tip-pro      .
           move      w-zos-dcp-cod-iva    to   w-let-flt-cod-iva      .
           move      w-zos-dcp-ctp-ven    to   w-let-flt-ctp-ven      .
           move      w-zos-dcp-umi-ven    to   w-let-flt-umi-ven      .
           move      w-zos-dcp-cod-s01    to   w-let-flt-cod-s01      .
           move      w-zos-dcp-cod-s02    to   w-let-flt-cod-s02      .
           move      w-zos-dcp-cod-s03    to   w-let-flt-cod-s03      .
           move      w-zos-dcp-sta-tus    to   w-let-flt-sta-tus      .
           move      w-zos-dcp-sta-tuw    to   w-let-flt-sta-tuw      .
           move      w-zos-dcp-cod-fnt    to   w-let-flt-cod-fnt      .
           move      w-zos-dcp-spc-lib    to   w-let-flt-spc-lib      .
           move      w-zos-dcp-icm-min    to   w-let-flt-icm-min      .
           move      w-zos-dcp-icm-max    to   w-let-flt-icm-max      .
           move      w-zos-dcp-cod-pdt    to   w-let-flt-cod-pdt      .
           move      w-zos-dcp-cfp-min    to   w-let-flt-cfp-min      .
           move      w-zos-dcp-cfp-max    to   w-let-flt-cfp-max      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione cautelativa dei campi   *
      *                      * aggiunti successivamente                *
      *                      *-----------------------------------------*
           if        w-zos-dcp-sta-tus    not  numeric
                     move  zero           to   w-let-flt-sta-tus      .
           if        w-zos-dcp-cod-fnt    not  numeric
                     move  zero           to   w-let-flt-cod-fnt      .
           if        w-zos-dcp-icm-min    not  numeric
                     move  zero           to   w-let-flt-icm-min      .
           if        w-zos-dcp-icm-max    not  numeric
                     move  zero           to   w-let-flt-icm-max      .
           if        w-zos-dcp-cod-pdt    not  numeric
                     move  zero           to   w-let-flt-cod-pdt      .
      *                      *-----------------------------------------*
      *                      * Regolarizzazione                        *
      *                      *-----------------------------------------*
           if        w-let-flt-icm-max    not  = zero
                     go to let-cod-flt-768.
           if        w-let-flt-icm-min    =    zero
                     move 9999999         to   w-let-flt-icm-max
           else      move w-let-flt-icm-min
                                          to   w-let-flt-icm-max      .
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
      *    * Ordinamento e selezione [dcp] su filtro                   *
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
       ord-sel-flt-010.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri indicano   *
      *              * selezione su catalogo prodotti                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo di ordinamento                 *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    not  = 89
                     go to ord-sel-flt-020.
      *                  *---------------------------------------------*
      *                  * Test su codice catalogo                     *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-cpv    =    spaces
                     go to ord-sel-flt-020.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento codice catalogo   *
      *                  *---------------------------------------------*
           perform   ord-sel-089-000      thru ord-sel-089-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-flt-999.
       ord-sel-flt-020.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri indicano   *
      *              * selezione casuale di prodotti                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo di ordinamento                 *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    not  = 99
                     go to ord-sel-flt-025.
      *                  *---------------------------------------------*
      *                  * Test sul numero elementi memorizzati        *
      *                  *---------------------------------------------*
           if        w-let-flt-num-ele    not  > 1
                     go to ord-sel-flt-999.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento selezione casuale *
      *                  *---------------------------------------------*
           perform   ord-sel-099-000      thru ord-sel-099-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ord-sel-flt-999.
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
      *                  * Test su codice alfanumerico del prodotto    *
      *                  * min-max                                     *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-max    <    w-let-flt-alf-min
                     go to ord-sel-flt-999.
      *                  *---------------------------------------------*
      *                  * Test su descrizione del prodotto min-max    *
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
      *                  * Se il codice alfanumerico del prodotto min  *
      *                  * e' diverso dal codice alfanumerico del pro- *
      *                  * dotto max non si puo' avere questa certezza *
      *                  * e si prosegue pertanto con il normale meto- *
      *                  * do di selezione                             *
      *                  *---------------------------------------------*
           if        w-let-flt-alf-min    not  = w-let-flt-alf-max
                     go to ord-sel-flt-500.
      *                  *---------------------------------------------*
      *                  * Se il codice alfanumerico del prodotto min  *
      *                  * e' a spaces ed anche il codice alfanumerico *
      *                  * del prodotto max e' a spaces non si puo' a- *
      *                  * vere questa certezza e si prosegue pertanto *
      *                  * con il normale metodo di selezione          *
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
      *                      * Start per codice alfanumerico su [dcp]  *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-let-flt-alf-min    to   rf-dcp-alf-pro of
                                               rf-dcp                 .
           move      zero                 to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita per selezione  *
      *                      * vuota                                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-flt-999.
      *                      *-----------------------------------------*
      *                      * Read Next su [dcp]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se At End : uscita per selezione vuota  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-flt-999.
      *                      *-----------------------------------------*
      *                      * Test Max, se errore : uscita per sele-  *
      *                      * zione vuota                             *
      *                      *-----------------------------------------*
           if        rf-dcp-alf-pro of
                     rf-dcp               not  = w-let-flt-alf-min
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
      *                      * Numero records effettivamente selezio-  *
      *                      * nati : pari a 1                         *
      *                      *-----------------------------------------*
           move      1                    to   w-ord-sel-max-scr      .
      *                      *-----------------------------------------*
      *                      * Salvataggio del record in area sort     *
      *                      *-----------------------------------------*
           move      rf-dcp               to   rf-sss                 .
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
      *              *                            togruppo, codice del *
      *              *                            prodotto             *
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
      *              *                            ne del prodotto      *
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
      *              * Se tipo ordinamento : 03 : Codice prodotto      *
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
      *              * Se tipo ordinamento : 04 : Descrizione prodotto *
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
      *    * Ordinamento e selezione [dcp]     Tipo ordinamento  01    *
      *    *                                                           *
      *    * Classe, gruppo, sottogruppo, codice prodotto              *
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
      *              * Start su [zp1] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-let-flt-nsc-min    to   rf-zp1-sqz-num         .
           move      zero                 to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
       ord-sel-001-125.
      *              *-------------------------------------------------*
      *              * Read Next su [zp1]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-900.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-zp1-sqz-num       >    w-let-flt-nsc-max
                     go to ord-sel-001-900.
      *              *-------------------------------------------------*
      *              * Se classe non suddivisa in gruppi               *
      *              *-------------------------------------------------*
           if        rf-zp1-ult-sud       =    02
                     go to ord-sel-001-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record gruppo               *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento prodotto                      *
      *                  *---------------------------------------------*
           go to     ord-sel-001-275.
       ord-sel-001-150.
      *              *-------------------------------------------------*
      *              * Start su [zp2] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zp1-cod-cla       to   rf-zp2-cod-cla         .
           move      w-let-flt-nsg-min    to   rf-zp2-sqz-num         .
           move      zero                 to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a classe successiva           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-125.
       ord-sel-001-175.
      *              *-------------------------------------------------*
      *              * Read Next su [zp2]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * Se At End : a classe successiva                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-125.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a classe successiva *
      *              *-------------------------------------------------*
           if        rf-zp2-cod-cla       not  = rf-zp1-cod-cla
                     go to ord-sel-001-125.
           if        rf-zp2-sqz-num       >    w-let-flt-nsg-max
                     go to ord-sel-001-125.
      *              *-------------------------------------------------*
      *              * Se gruppo non suddiviso in sottogruppi          *
      *              *-------------------------------------------------*
           if        rf-zp2-ult-sud       =    02
                     go to ord-sel-001-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento prodotto                      *
      *                  *---------------------------------------------*
           go to     ord-sel-001-275.
       ord-sel-001-200.
      *              *-------------------------------------------------*
      *              * Start su [zp3] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zp1-cod-cla       to   rf-zp3-cod-cla         .
           move      rf-zp2-cod-gru       to   rf-zp3-cod-gru         .
           move      w-let-flt-nss-min    to   rf-zp3-sqz-num         .
           move      zero                 to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a gruppo successivo           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-175.
       ord-sel-001-225.
      *              *-------------------------------------------------*
      *              * Read Next su [zp3]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * Se At End : a gruppo successivo                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-175.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a gruppo successivo *
      *              *-------------------------------------------------*
           if        rf-zp3-cod-cla       not  = rf-zp1-cod-cla
                     go to ord-sel-001-175.
           if        rf-zp3-cod-gru       not  = rf-zp2-cod-gru
                     go to ord-sel-001-175.
           if        rf-zp3-sqz-num       >    w-let-flt-nss-max
                     go to ord-sel-001-175.
      *              *-------------------------------------------------*
      *              * A trattamento prodotto                          *
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
           if        rf-zp1-ult-sud       not  = 02
                     go to ord-sel-001-125
           else if   rf-zp2-ult-sud       not  = 02
                     go to ord-sel-001-175
           else      go to ord-sel-001-225.
       ord-sel-001-275.
      *              *-------------------------------------------------*
      *              * Start su [dcp] per Classe, gruppo, sottogruppo  *
      *              *                  e codice prodotto              *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "CGSALF    "         to   f-key                  .
           move      rf-zp1-cod-cla       to   rf-dcp-cla-pro of
                                               rf-dcp                 .
           if        rf-zp1-ult-sud       not  = 02
                     move  zero           to   rf-dcp-gru-pro of
                                               rf-dcp
                     move  zero           to   rf-dcp-sgr-pro of
                                               rf-dcp
                     go to ord-sel-001-280.
           move      rf-zp2-cod-gru       to   rf-dcp-gru-pro of
                                               rf-dcp                 .
           if        rf-zp2-ult-sud       not  = 02
                     move  zero           to   rf-dcp-sgr-pro of
                                               rf-dcp
                     go to ord-sel-001-280.
           move      rf-zp3-cod-sgr       to   rf-dcp-sgr-pro of
                                               rf-dcp                 .
       ord-sel-001-280.
           move      w-let-flt-alf-min    to   rf-dcp-alf-pro of
                                               rf-dcp                 .
           move      zero                 to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : riciclo a classe o gruppo o   *
      *              * sottogruppo                                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-001-250.
       ord-sel-001-300.
      *              *-------------------------------------------------*
      *              * Read Next su [dcp]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
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
           if        rf-dcp-cla-pro of
                     rf-dcp               not  = rf-zp1-cod-cla
                     go to ord-sel-001-250.
           if        rf-zp1-ult-sud       not  = 02
                     go to ord-sel-001-327.
           if        rf-dcp-gru-pro of
                     rf-dcp               not  = rf-zp2-cod-gru
                     go to ord-sel-001-250.
           if        rf-zp2-ult-sud       not  =  02
                     go to ord-sel-001-327.
           if        rf-dcp-sgr-pro of
                     rf-dcp               not  = rf-zp3-cod-sgr
                     go to ord-sel-001-250.
       ord-sel-001-327.
           if        rf-dcp-alf-pro of
                     rf-dcp               >    w-let-flt-alf-max
                     go to ord-sel-001-250.
       ord-sel-001-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record let-  *
      *              * to, se non superata : riciclo a Read Next sul   *
      *              * file [dcp]                                      *
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
           move      rf-dcp               to   rf-sss                 .
       ord-sel-001-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dcp               to   rf-ttt                 .
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
      *    * Ordinamento e selezione [dcp]     Tipo ordinamento  02    *
      *    *                                                           *
      *    * Classe, gruppo, sottogruppo, descrizione prodotto         *
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
      *              * Start su [zp1] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      w-let-flt-nsc-min    to   rf-zp1-sqz-num         .
           move      zero                 to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
       ord-sel-002-125.
      *              *-------------------------------------------------*
      *              * Read Next su [zp1]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-900.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-zp1-sqz-num       >    w-let-flt-nsc-max
                     go to ord-sel-002-900.
      *              *-------------------------------------------------*
      *              * Se classe non suddivisa in gruppi               *
      *              *-------------------------------------------------*
           if        rf-zp1-ult-sud       =    02
                     go to ord-sel-002-150.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record gruppo               *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento prodotto                      *
      *                  *---------------------------------------------*
           go to     ord-sel-002-275.
       ord-sel-002-150.
      *              *-------------------------------------------------*
      *              * Start su [zp2] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zp1-cod-cla       to   rf-zp2-cod-cla         .
           move      w-let-flt-nsg-min    to   rf-zp2-sqz-num         .
           move      zero                 to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a classe successiva           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-125.
       ord-sel-002-175.
      *              *-------------------------------------------------*
      *              * Read Next su [zp2]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * Se At End : a classe successiva                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-125.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a classe successiva *
      *              *-------------------------------------------------*
           if        rf-zp2-cod-cla       not  = rf-zp1-cod-cla
                     go to ord-sel-002-125.
           if        rf-zp2-sqz-num       >    w-let-flt-nsg-max
                     go to ord-sel-002-125.
      *              *-------------------------------------------------*
      *              * Se gruppo non suddiviso in sottogruppi          *
      *              *-------------------------------------------------*
           if        rf-zp2-ult-sud       =    02
                     go to ord-sel-002-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record sottogruppo          *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                  *---------------------------------------------*
      *                  * A trattamento prodotto                      *
      *                  *---------------------------------------------*
           go to     ord-sel-002-275.
       ord-sel-002-200.
      *              *-------------------------------------------------*
      *              * Start su [zp3] per numero di sequenza           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "SQZNUM    "         to   f-key                  .
           move      rf-zp1-cod-cla       to   rf-zp3-cod-cla         .
           move      rf-zp2-cod-gru       to   rf-zp3-cod-gru         .
           move      w-let-flt-nss-min    to   rf-zp3-sqz-num         .
           move      zero                 to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a gruppo successivo           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-175.
       ord-sel-002-225.
      *              *-------------------------------------------------*
      *              * Read Next su [zp3]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *              *-------------------------------------------------*
      *              * Se At End : a gruppo successivo                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-175.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : a gruppo successivo *
      *              *-------------------------------------------------*
           if        rf-zp3-cod-cla       not  = rf-zp1-cod-cla
                     go to ord-sel-002-175.
           if        rf-zp3-cod-gru       not  = rf-zp2-cod-gru
                     go to ord-sel-002-175.
           if        rf-zp3-sqz-num       >    w-let-flt-nss-max
                     go to ord-sel-002-175.
      *              *-------------------------------------------------*
      *              * A trattamento prodotto                          *
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
           if        rf-zp1-ult-sud       not  = 02
                     go to ord-sel-002-125
           else if   rf-zp2-ult-sud       not  = 02
                     go to ord-sel-002-175
           else      go to ord-sel-002-225.
       ord-sel-002-275.
      *              *-------------------------------------------------*
      *              * Start su [dcp] per Classe, gruppo, sottogruppo  *
      *              *                  e descrizione prodotto         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "CGSDES    "         to   f-key                  .
           move      rf-zp1-cod-cla       to   rf-dcp-cla-pro of
                                               rf-dcp                 .
           if        rf-zp1-ult-sud       not  = 02
                     move  zero           to   rf-dcp-gru-pro of
                                               rf-dcp
                     move  zero           to   rf-dcp-sgr-pro of
                                               rf-dcp
                     go to ord-sel-002-280.
           move      rf-zp2-cod-gru       to   rf-dcp-gru-pro of
                                               rf-dcp                 .
           if        rf-zp2-ult-sud       not  = 02
                     move  zero           to   rf-dcp-sgr-pro of
                                               rf-dcp
                     go to ord-sel-002-280.
           move      rf-zp3-cod-sgr       to   rf-dcp-sgr-pro of
                                               rf-dcp                 .
       ord-sel-002-280.
           move      w-let-flt-des-min    to   rf-dcp-des-key of
                                               rf-dcp                 .
           move      zero                 to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : riciclo a classe o gruppo o   *
      *              * sottogruppo                                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-002-250.
       ord-sel-002-300.
      *              *-------------------------------------------------*
      *              * Read Next su [dcp]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
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
           if        rf-dcp-cla-pro of
                     rf-dcp               not  = rf-zp1-cod-cla
                     go to ord-sel-002-250.
           if        rf-zp1-ult-sud       not  = 02
                     go to ord-sel-002-327.
           if        rf-dcp-gru-pro of
                     rf-dcp               not  = rf-zp2-cod-gru
                     go to ord-sel-002-250.
           if        rf-zp2-ult-sud       not  = 02
                     go to ord-sel-002-327.
           if        rf-dcp-sgr-pro of
                     rf-dcp               not  = rf-zp3-cod-sgr
                     go to ord-sel-002-250.
       ord-sel-002-327.
           if        rf-dcp-des-key of
                     rf-dcp               >    w-let-flt-des-max
                     go to ord-sel-002-250.
       ord-sel-002-400.
      *              *-------------------------------------------------*
      *              * Selezione, mediante il filtro, del record let-  *
      *              * to, se non superata : riciclo a Read Next sul   *
      *              * file [dcp]                                      *
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
           move      rf-dcp               to   rf-sss                 .
       ord-sel-002-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dcp               to   rf-ttt                 .
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
      *    * Ordinamento e selezione [dcp]     Tipo ordinamento  03    *
      *    *                                                           *
      *    * Codice prodotto                                           *
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
      *              * Start su [dcp] per codice alfanumerico          *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-let-flt-alf-min    to   rf-dcp-alf-pro of
                                               rf-dcp                 .
           move      zero                 to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-200.
      *              *-------------------------------------------------*
      *              * Read Next su [dcp]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-003-900.
       ord-sel-003-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-dcp-alf-pro of
                     rf-dcp               >    w-let-flt-alf-max
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
           move      rf-dcp               to   rf-sss                 .
       ord-sel-003-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dcp               to   rf-ttt                 .
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
      *    * Ordinamento e selezione [dcp]     Tipo ordinamento  04    *
      *    *                                                           *
      *    * Descrizione prodotto                                      *
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
      *              * Start su [dcp] per descrizione in uppercase     *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "DESKEY    "         to   f-key                  .
           move      w-let-flt-des-min    to   rf-dcp-des-key of
                                               rf-dcp                 .
           move      zero                 to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-004-900.
       ord-sel-004-200.
      *              *-------------------------------------------------*
      *              * Read Next su [dcp]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-004-900.
       ord-sel-004-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-dcp-des-key of
                     rf-dcp               >    w-let-flt-des-max
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
           move      rf-dcp               to   rf-sss                 .
       ord-sel-004-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dcp               to   rf-ttt                 .
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
      *    * Ordinamento e selezione [dcp]        Tipo ordinamento  89 *
      *    *                                                           *
      *    * Codice catalogo                                           *
      *    *-----------------------------------------------------------*
       ord-sel-089-000.
      *              *-------------------------------------------------*
      *              * Open output file [ttt]                          *
      *              *-------------------------------------------------*
           perform   opn-out-ttt-000      thru opn-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-089-900.
       ord-sel-089-100.
      *              *-------------------------------------------------*
      *              * Start su file [cpv]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CPVPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-cpv-tip-rec         .
           move      w-let-flt-cod-cpv    to   rf-cpv-cod-cpv         .
           move      zero                 to   rf-cpv-num-pro         .
           move      zero                 to   rf-cpv-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofcpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpv                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-089-900.
       ord-sel-089-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [cpv]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofcpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpv                 .
      *              *-------------------------------------------------*
      *              * Se At End : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-089-900.
       ord-sel-089-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-cpv-tip-rec       not  = 01            or
                     rf-cpv-cod-cpv       not  = w-let-flt-cod-cpv
                     go to ord-sel-089-900.
       ord-sel-089-400.
      *              *-------------------------------------------------*
      *              * Lettura codice prodotto                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-cpv-num-pro       to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-dcp-alf-pro of
                                               rf-dcp                 .
       ord-sel-089-500.
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
                     go to ord-sel-089-600.
           move      rf-dcp               to   rf-sss                 .
       ord-sel-089-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dcp               to   rf-ttt                 .
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
                     go to ord-sel-089-900.
      *                  *---------------------------------------------*
      *                  * Se Ok : riciclo a Read Next                 *
      *                  *---------------------------------------------*
           go to     ord-sel-089-200.
       ord-sel-089-900.
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
       ord-sel-089-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento e selezione [dcp]        Tipo ordinamento  99 *
      *    *                                                           *
      *    * Selezione casuale prodotti                                *
      *    *-----------------------------------------------------------*
       ord-sel-099-000.
      *              *-------------------------------------------------*
      *              * Open output file [ttt]                          *
      *              *-------------------------------------------------*
           perform   opn-out-ttt-000      thru opn-out-ttt-999        .
      *              *-------------------------------------------------*
      *              * Se errori : ad uscita                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ord-sel-099-900.
      *              *-------------------------------------------------*
      *              * Area dati in work di trattamento                *
      *              *-------------------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-esl-dcp          .
       ord-sel-099-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-flt-ctr-001      .
       ord-sel-099-200.
           add       1                    to   w-let-flt-ctr-001      .
           if        w-let-flt-ctr-001    >    w-let-flt-ctr-max
                     go to ord-sel-099-900.
           if        w-let-flt-ctr-001    >    w-let-flt-num-ele
                     go to ord-sel-099-900.
       ord-sel-099-400.
      *              *-------------------------------------------------*
      *              * Test su codice letto                            *
      *              *-------------------------------------------------*
           if        w-zos-esl-dcp-num
                    (w-let-flt-ctr-001)   not  numeric  
                     go to ord-sel-099-900.
           if        w-zos-esl-dcp-num
                    (w-let-flt-ctr-001)   =    zero
                     go to ord-sel-099-900.
       ord-sel-099-500.
      *              *-------------------------------------------------*
      *              * Se record da includere nella selezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice prodotto                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-zos-esl-dcp-num
                    (w-let-flt-ctr-001)   to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-dcp-alf-pro of
                                               rf-dcp                 .
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
                     go to ord-sel-099-600.
           move      rf-dcp               to   rf-sss                 .
       ord-sel-099-600.
      *                  *---------------------------------------------*
      *                  * Spostamento record da area file ad area di  *
      *                  * work                                        *
      *                  *---------------------------------------------*
           move      rf-dcp               to   rf-ttt                 .
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
                     go to ord-sel-099-900.
      *                  *---------------------------------------------*
      *                  * Se Ok : riciclo                             *
      *                  *---------------------------------------------*
           go to     ord-sel-099-200.
       ord-sel-099-900.
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
       ord-sel-099-999.
           exit.

      *    *===========================================================*
      *    * Selezione, mediante il filtro, del record in 'rf-dcp'     *
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
                     rf-dcp-cla-pro of
                     rf-dcp               =    w-let-flt-cla-min
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Lettura codice classe, se record non    *
      *                      * trovato : no selezione                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      rf-dcp-cla-pro of
                     rf-dcp               to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza per la classe non *
      *                      * compreso tra quelli da selezionare : la *
      *                      * selezione si considera fallita          *
      *                      *-----------------------------------------*
           if        rf-zp1-sqz-num       <    w-let-flt-nsc-min or
                     rf-zp1-sqz-num       >    w-let-flt-nsc-max
                     go to sel-rec-flt-900.
       sel-rec-flt-050.
      *                  *---------------------------------------------*
      *                  * Selezione su gruppo                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se classe non suddivisa in gruppi : no  *
      *                      * selezione                               *
      *                      *-----------------------------------------*
           if        rf-zp1-ult-sud       not  = 02
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
                     rf-dcp-gru-pro of
                     rf-dcp               =    w-let-flt-gru-min
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Lettura codice gruppo, se record non    *
      *                      * trovato : no selezione                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      rf-dcp-cla-pro of
                     rf-dcp               to   rf-zp2-cod-cla         .
           move      rf-dcp-gru-pro of
                     rf-dcp               to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza per il gruppo non *
      *                      * compreso tra quelli da selezionare : la *
      *                      * selezione si considera fallita          *
      *                      *-----------------------------------------*
           if        rf-zp2-sqz-num       <    w-let-flt-nsg-min or
                     rf-zp2-sqz-num       >    w-let-flt-nsg-max
                     go to sel-rec-flt-900.
       sel-rec-flt-075.
      *                  *---------------------------------------------*
      *                  * Selezione su sottogruppo                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se gruppo non suddiviso in sottogruppi  *
      *                      * : no selezione                          *
      *                      *-----------------------------------------*
           if        rf-zp2-ult-sud       not  = 02
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
                     rf-dcp-sgr-pro of
                     rf-dcp               =    w-let-flt-sgr-min
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Lettura codice sottogruppo, se record   *
      *                      * non trovato : no selezione              *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      rf-dcp-cla-pro of
                     rf-dcp               to   rf-zp3-cod-cla         .
           move      rf-dcp-gru-pro of
                     rf-dcp               to   rf-zp3-cod-gru         .
           move      rf-dcp-sgr-pro of
                     rf-dcp               to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-flt-150.
      *                      *-----------------------------------------*
      *                      * Se numero di sequenza per il sottogrup- *
      *                      * po non compreso tra quelli da selezio-  *
      *                      *  nare : la selezione si considera fal-  *
      *                      * lita                                    *
      *                      *-----------------------------------------*
           if        rf-zp3-sqz-num       <    w-let-flt-nss-min or
                     rf-zp3-sqz-num       >    w-let-flt-nss-max
                     go to sel-rec-flt-900.
       sel-rec-flt-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice alfanumerico min-max        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per classe, gruppo, e   *
      *                  * sottogruppo : no selezione                  *
      *                  *                                             *
      *                  * N.B.: Soppresso 13/01/03                    *
      *                  *---------------------------------------------*
______*    if        w-let-flt-tip-ord    =    01 or
______*              w-let-flt-tip-ord    =    02
______*              go to sel-rec-flt-200.
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per codice alfanumerico *
      *                  * : no selezione                              *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    03
                     go to sel-rec-flt-200.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-alf-pro of
                     rf-dcp               <    w-let-flt-alf-min or
                     rf-dcp-alf-pro of
                     rf-dcp               >    w-let-flt-alf-max
                     go to sel-rec-flt-900.
       sel-rec-flt-200.
      *              *-------------------------------------------------*
      *              * Selezione su descrizione uppercase min-max      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per classe, gruppo, e   *
      *                  * sottogruppo : no selezione                  *
      *                  *                                             *
      *                  * N.B.: Soppresso 13/01/03                    *
      *                  *---------------------------------------------*
______*    if        w-let-flt-tip-ord    =    01 or
______*              w-let-flt-tip-ord    =    02
______*              go to sel-rec-flt-250.
      *                  *---------------------------------------------*
      *                  * Se tipo ordinamento per descrizione : no    *
      *                  * selezione                                   *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    =    04
                     go to sel-rec-flt-250.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-des-key of
                     rf-dcp               <    w-let-flt-des-min or
                     rf-dcp-des-key of
                     rf-dcp               >    w-let-flt-des-max
                     go to sel-rec-flt-900.
       sel-rec-flt-250.
      *              *-------------------------------------------------*
      *              * Selezione su tipo prodotto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo prodotto a zero : no selezione      *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-pro    =    zero
                     go to sel-rec-flt-300.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-tip-pro of
                     rf-dcp               not  = w-let-flt-tip-pro
                     go to sel-rec-flt-900.
       sel-rec-flt-300.
      *              *-------------------------------------------------*
      *              * Selezione su codice iva prodotto                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice iva a zero : no selezione         *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-iva    =    zero
                     go to sel-rec-flt-350.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-cod-iva of
                     rf-dcp               not  = w-let-flt-cod-iva
                     go to sel-rec-flt-900.
       sel-rec-flt-350.
      *              *-------------------------------------------------*
      *              * Selezione su codice contropartita prodotto      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice contropartita a zero : no sele-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-let-flt-ctp-ven    =    zero
                     go to sel-rec-flt-400.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-ctp-ven of
                     rf-dcp               not  = w-let-flt-ctp-ven
                     go to sel-rec-flt-900.
       sel-rec-flt-400.
      *              *-------------------------------------------------*
      *              * Selezione su unita' di misura prodotto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se unita' di misura a Spaces : no selezione *
      *                  *---------------------------------------------*
           if        w-let-flt-umi-ven    =    spaces
                     go to sel-rec-flt-450.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-umi-ven of
                     rf-dcp               not  = w-let-flt-umi-ven
                     go to sel-rec-flt-900.
       sel-rec-flt-450.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 1 prodotto       *
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
           if        rf-dcp-cod-s01 of
                     rf-dcp               not  = w-let-flt-cod-s01
                     go to sel-rec-flt-900.
       sel-rec-flt-500.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 2 prodotto       *
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
           if        rf-dcp-cod-s02 of
                     rf-dcp               not  = w-let-flt-cod-s02
                     go to sel-rec-flt-900.
       sel-rec-flt-550.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 3 prodotto       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 3 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s03    =    zero
                     go to sel-rec-flt-560.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-cod-s03 of
                     rf-dcp               not  = w-let-flt-cod-s03
                     go to sel-rec-flt-900.
       sel-rec-flt-560.
      *              *-------------------------------------------------*
      *              * Selezione su status prodotto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tutti gli status prodotto sono a spaces  *
      *                  * si forza lo status normale come default     *
      *                  *---------------------------------------------*
           if        w-let-flt-sta-tuw    =    spaces
                     move  "#"            to   w-let-flt-sta-nor      .
      *                  *---------------------------------------------*
      *                  * Se tutti gli status prodotto sono spuntati  *
      *                  *---------------------------------------------*
           if        w-let-flt-sta-nor    not  = spaces and
                     w-let-flt-sta-esa    not  = spaces and
                     w-let-flt-sta-sos    not  = spaces and
                     w-let-flt-sta-ces    not  = spaces and
                     w-let-flt-sta-cms    not  = spaces and
                     w-let-flt-sta-obs    not  = spaces and
                     w-let-flt-sta-oms    not  = spaces
                     go to sel-rec-flt-580.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tus of
                     rf-dcp               =    01  and
                     w-let-flt-sta-nor    =    spaces
                     go to sel-rec-flt-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    11  and
                     w-let-flt-sta-esa    =    spaces
                     go to sel-rec-flt-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    21  and
                     w-let-flt-sta-sos    =    spaces
                     go to sel-rec-flt-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    51  and
                     w-let-flt-sta-ces    =    spaces
                     go to sel-rec-flt-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    52  and
                     w-let-flt-sta-cms    =    spaces
                     go to sel-rec-flt-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    71  and
                     w-let-flt-sta-obs    =    spaces
                     go to sel-rec-flt-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    72  and
                     w-let-flt-sta-oms    =    spaces
                     go to sel-rec-flt-900.
       sel-rec-flt-580.
      *              *-------------------------------------------------*
      *              * Selezione su codice fornitore preferenziale     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice fornitore a zero : no selezione   *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-fnt    =    zero
                     go to sel-rec-flt-600.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-dcf-pfz of
                     rf-dcp               not  = w-let-flt-cod-fnt
                     go to sel-rec-flt-900.
       sel-rec-flt-600.
      *              *-------------------------------------------------*
      *              * Selezione su codice casa produttrice            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice produttore a zero : no selezione  *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-pdt    =    zero
                     go to sel-rec-flt-620.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-cod-pdt of
                     rf-dcp               not  = w-let-flt-cod-pdt
                     go to sel-rec-flt-900.
       sel-rec-flt-620.
      *              *-------------------------------------------------*
      *              * Selezione su specifica libera prodotto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se specifica libera a Spaces : no selezione *
      *                  *---------------------------------------------*
           if        w-let-flt-spc-lib    =    spaces
                     go to sel-rec-flt-640.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-spc-lib of
                     rf-dcp               not  = w-let-flt-spc-lib
                     go to sel-rec-flt-900.
       sel-rec-flt-640.
      *              *-------------------------------------------------*
      *              * Selezione su data inizio commercializzazione    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su max                            *
      *                  *---------------------------------------------*
           if        rf-dcp-dat-icm of
                     rf-dcp               >    w-let-flt-icm-max
                     go to sel-rec-flt-900.
      *                  *---------------------------------------------*
      *                  * Selezione su min                            *
      *                  *---------------------------------------------*
           if        rf-dcp-dat-icm of
                     rf-dcp               <    w-let-flt-icm-min
                     go to sel-rec-flt-900.
       sel-rec-flt-660.
      *              *-------------------------------------------------*
      *              * Selezione su codice del prodotto per il forni-  *
      *              * tore preferenziale                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su max                            *
      *                  *---------------------------------------------*
           if        rf-dcp-cop-sfn of
                     rf-dcp               >    w-let-flt-cfp-max
                     go to sel-rec-flt-900.
      *                  *---------------------------------------------*
      *                  * Selezione su min                            *
      *                  *---------------------------------------------*
           if        rf-dcp-cop-sfn of
                     rf-dcp               <    w-let-flt-cfp-min
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
      *              * Tests per controllare se i parametri indicano   *
      *              * selezione su codice catalogo                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo di ordinamento                 *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    not  = 89
                     go to sel-rec-sel-010.
      *                  *---------------------------------------------*
      *                  * Test su codice catalogo                     *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-cpv    =    spaces
                     go to sel-rec-sel-010.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento codice catalogo   *
      *                  *---------------------------------------------*
           perform   sel-rec-089-000      thru sel-rec-089-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-rec-sel-999.
       sel-rec-sel-010.
      *              *-------------------------------------------------*
      *              * Tests per controllare se i parametri indicano   *
      *              * selezione casuale di prodotti                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo di ordinamento                 *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-ord    not  = 99
                     go to sel-rec-sel-020.
      *                  *---------------------------------------------*
      *                  * Test sul numero elementi memorizzati        *
      *                  *---------------------------------------------*
           if        w-let-flt-num-ele    not  > zero
                     go to sel-rec-sel-020.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento selezione casuale *
      *                  *---------------------------------------------*
           perform   sel-rec-099-000      thru sel-rec-099-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-rec-sel-999.
       sel-rec-sel-020.
      *              *-------------------------------------------------*
      *              * Spostamento record da link-area a comodo per la *
      *              * ridefinizione dei campi                         *
      *              *-------------------------------------------------*
           move      rf-lll               to   rf-dcp                 .
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
           if        rf-dcp-cla-pro of
                     rf-dcp               <    w-let-flt-cla-min or
                     rf-dcp-cla-pro of
                     rf-dcp               >    w-let-flt-cla-max
                     go to sel-rec-sel-900.
       sel-rec-sel-075.
      *                  *---------------------------------------------*
      *                  * Selezione su gruppo                         *
      *                  *---------------------------------------------*
           if        rf-dcp-gru-pro of
                     rf-dcp               <    w-let-flt-gru-min or
                     rf-dcp-gru-pro of
                     rf-dcp               >    w-let-flt-gru-max
                     go to sel-rec-sel-900.
       sel-rec-sel-100.
      *                  *---------------------------------------------*
      *                  * Selezione su sottogruppo                    *
      *                  *---------------------------------------------*
           if        rf-dcp-sgr-pro of
                     rf-dcp               <    w-let-flt-sgr-min or
                     rf-dcp-sgr-pro of
                     rf-dcp               >    w-let-flt-sgr-max
                     go to sel-rec-sel-900.
       sel-rec-sel-150.
      *              *-------------------------------------------------*
      *              * Selezione su codice alfanumerico min-max        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-alf-pro of
                     rf-dcp               <    w-let-flt-alf-min or
                     rf-dcp-alf-pro of
                     rf-dcp               >    w-let-flt-alf-max
                     go to sel-rec-sel-900.
       sel-rec-sel-200.
      *              *-------------------------------------------------*
      *              * Selezione su descrizione uppercase min-max      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-des-key of
                     rf-dcp               <    w-let-flt-des-min or
                     rf-dcp-des-key of
                     rf-dcp               >    w-let-flt-des-max
                     go to sel-rec-sel-900.
       sel-rec-sel-250.
      *              *-------------------------------------------------*
      *              * Selezione su tipo prodotto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo prodotto a zero : no selezione      *
      *                  *---------------------------------------------*
           if        w-let-flt-tip-pro    =    zero
                     go to sel-rec-sel-300.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-tip-pro of
                     rf-dcp               not  = w-let-flt-tip-pro
                     go to sel-rec-sel-900.
       sel-rec-sel-300.
      *              *-------------------------------------------------*
      *              * Selezione su codice iva prodotto                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice iva a zero : no selezione         *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-iva    =    zero
                     go to sel-rec-sel-350.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-cod-iva of
                     rf-dcp               not  = w-let-flt-cod-iva
                     go to sel-rec-sel-900.
       sel-rec-sel-350.
      *              *-------------------------------------------------*
      *              * Selezione su codice contropartita prodotto      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice contropartita a zero : no sele-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-let-flt-ctp-ven    =    zero
                     go to sel-rec-sel-400.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-ctp-ven of
                     rf-dcp               not  = w-let-flt-ctp-ven
                     go to sel-rec-sel-900.
       sel-rec-sel-400.
      *              *-------------------------------------------------*
      *              * Selezione su unita' di misura prodotto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se unita' di misura a Spaces : no selezione *
      *                  *---------------------------------------------*
           if        w-let-flt-umi-ven    =    spaces
                     go to sel-rec-sel-450.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-umi-ven of
                     rf-dcp               not  = w-let-flt-umi-ven
                     go to sel-rec-sel-900.
       sel-rec-sel-450.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 1 prodotto       *
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
           if        rf-dcp-cod-s01 of
                     rf-dcp               not  = w-let-flt-cod-s01
                     go to sel-rec-sel-900.
       sel-rec-sel-500.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 2 prodotto       *
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
           if        rf-dcp-cod-s02 of
                     rf-dcp               not  = w-let-flt-cod-s02
                     go to sel-rec-sel-900.
       sel-rec-sel-550.
      *              *-------------------------------------------------*
      *              * Selezione su codice statistico 3 prodotto       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice statistico 3 a zero : no selezio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-s03    =    zero
                     go to sel-rec-sel-560.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-cod-s03 of
                     rf-dcp               not  = w-let-flt-cod-s03
                     go to sel-rec-sel-900.
       sel-rec-sel-560.
      *              *-------------------------------------------------*
      *              * Selezione su status prodotto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tutti gli status prodotto sono a spaces  *
      *                  *                                             *
      *                  * N.B.: caso, ad esempio, di filtro cablato   *
      *                  *       al volo                               *
      *                  *---------------------------------------------*
           if        w-let-flt-sta-nor    =    spaces and
                     w-let-flt-sta-esa    =    spaces and
                     w-let-flt-sta-sos    =    spaces and
                     w-let-flt-sta-ces    =    spaces and
                     w-let-flt-sta-cms    =    spaces and
                     w-let-flt-sta-obs    =    spaces and
                     w-let-flt-sta-oms    =    spaces
                     go to sel-rec-sel-600.
      *                  *---------------------------------------------*
      *                  * Se tutti gli status prodotto sono spuntati  *
      *                  *---------------------------------------------*
           if        w-let-flt-sta-nor    not  = spaces and
                     w-let-flt-sta-esa    not  = spaces and
                     w-let-flt-sta-sos    not  = spaces and
                     w-let-flt-sta-ces    not  = spaces and
                     w-let-flt-sta-cms    not  = spaces and
                     w-let-flt-sta-obs    not  = spaces and
                     w-let-flt-sta-oms    not  = spaces
                     go to sel-rec-sel-600.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tus of
                     rf-dcp               =    01  and
                     w-let-flt-sta-nor    =    spaces
                     go to sel-rec-sel-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    11  and
                     w-let-flt-sta-esa    =    spaces
                     go to sel-rec-sel-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    21  and
                     w-let-flt-sta-sos    =    spaces
                     go to sel-rec-sel-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    51  and
                     w-let-flt-sta-ces    =    spaces
                     go to sel-rec-sel-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    52  and
                     w-let-flt-sta-cms    =    spaces
                     go to sel-rec-sel-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    71  and
                     w-let-flt-sta-obs    =    spaces
                     go to sel-rec-sel-900.
      *
           if        rf-dcp-sta-tus of
                     rf-dcp               =    72  and
                     w-let-flt-sta-oms    =    spaces
                     go to sel-rec-sel-900.
       sel-rec-sel-600.
       sel-rec-sel-620.
      *              *-------------------------------------------------*
      *              * Selezione su specifica libera prodotto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se specifica libera a Spaces : no selezione *
      *                  *---------------------------------------------*
           if        w-let-flt-spc-lib    =    spaces
                     go to sel-rec-sel-640.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-spc-lib of
                     rf-dcp               not  = w-let-flt-spc-lib
                     go to sel-rec-sel-900.
       sel-rec-sel-640.
      *              *-------------------------------------------------*
      *              * Selezione su data inizio commercializzazione    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su max                            *
      *                  *---------------------------------------------*
           if        rf-dcp-dat-icm of
                     rf-dcp               >    w-let-flt-icm-max
                     go to sel-rec-sel-900.
      *                  *---------------------------------------------*
      *                  * Selezione su min                            *
      *                  *---------------------------------------------*
           if        rf-dcp-dat-icm of
                     rf-dcp               <    w-let-flt-icm-min
                     go to sel-rec-sel-900.
       sel-rec-sel-650.
      *              *-------------------------------------------------*
      *              * Selezione su codice fornitore preferenziale     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice fornitore a zero : no selezione   *
      *                  *---------------------------------------------*
           if        w-let-flt-cod-fnt    =    zero
                     go to sel-rec-sel-660.
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           if        rf-dcp-dcf-pfz of
                     rf-dcp               not  = w-let-flt-cod-fnt
                     go to sel-rec-sel-900.
       sel-rec-sel-660.
      *              *-------------------------------------------------*
      *              * Selezione su codice del prodotto per il forni-  *
      *              * tore preferenziale                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su max                            *
      *                  *---------------------------------------------*
           if        rf-dcp-cop-sfn of
                     rf-dcp               >    w-let-flt-cfp-max
                     go to sel-rec-sel-900.
      *                  *---------------------------------------------*
      *                  * Selezione su min                            *
      *                  *---------------------------------------------*
           if        rf-dcp-cop-sfn of
                     rf-dcp               <    w-let-flt-cfp-min
                     go to sel-rec-sel-900.
       sel-rec-sel-690.
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
      *                  * Se tipo di codice anagrafico : [dcp]        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selezione su codice numerico            *
      *                      *-----------------------------------------*
           if        rf-dcp-num-pro of
                     rf-dcp               =    w-let-flt-cod-cco
                     go to sel-rec-sel-800
           else      go to sel-rec-sel-900.
       sel-rec-sel-760.
      *                  *---------------------------------------------*
      *                  * Se tipo di codice anagrafico : [zp1]        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Selezione su codice classe              *
      *                      *-----------------------------------------*
           if        rf-dcp-cla-pro of
                     rf-dcp               =    w-let-flt-cod-cco
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
      *    * Selezione di un singolo elemento per codice catalogo      *
      *    *-----------------------------------------------------------*
       sel-rec-089-000.
      *              *-------------------------------------------------*
      *              * Spostamento record da link-area a comodo per la *
      *              * ridefinizione dei campi                         *
      *              *-------------------------------------------------*
           move      rf-lll               to   rf-dcp                 .
       sel-rec-089-100.
      *              *-------------------------------------------------*
      *              * Start su file [cpv]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CPVPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-cpv-tip-rec         .
           move      w-let-flt-cod-cpv    to   rf-cpv-cod-cpv         .
           move      zero                 to   rf-cpv-num-pro         .
           move      zero                 to   rf-cpv-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofcpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpv                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sel-rec-089-900.
       sel-rec-089-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [cpv]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofcpv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cpv                 .
      *                  *---------------------------------------------*
      *                  * Se At End : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sel-rec-089-900.
       sel-rec-089-300.
      *              *-------------------------------------------------*
      *              * Test Max, se non superato : ad uscita           *
      *              *-------------------------------------------------*
           if        rf-cpv-tip-rec       not  = 01            or
                     rf-cpv-cod-cpv       not  = w-let-flt-cod-cpv
                     go to sel-rec-089-900.
       sel-rec-089-400.
      *              *-------------------------------------------------*
      *              * Selezione su codice prodotto                    *
      *              *-------------------------------------------------*
           if        rf-cpv-num-pro       not  = rf-dcp-num-pro of
                                                 rf-dcp
                     go to sel-rec-089-200.
       sel-rec-089-600.
      *              *-------------------------------------------------*
      *              * Lettura codice prodotto                         *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-cpv-num-pro       to   rf-dcp-num-pro of
                                               rf-dcp                 .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to sel-rec-089-900.
       sel-rec-089-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ok                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-ord-sel-flg-srf      .
           go to     sel-rec-089-999.
       sel-rec-089-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ko                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ord-sel-flg-srf      .
           go to     sel-rec-089-999.
       sel-rec-089-999.
           exit.

      *    *===========================================================*
      *    * Selezione di un singolo elemento in selezione casuale     *
      *    *-----------------------------------------------------------*
       sel-rec-099-000.
      *              *-------------------------------------------------*
      *              * Spostamento record da link-area a comodo per la *
      *              * ridefinizione dei campi                         *
      *              *-------------------------------------------------*
           move      rf-lll               to   rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Area dati in work di trattamento                *
      *              *-------------------------------------------------*
           move      rf-zos-dat-flt       to   w-zos-esl-dcp          .
      *              *-------------------------------------------------*
      *              * Ricerca del codice nella selezione casuale      *
      *              *-------------------------------------------------*
           set       w-zos-esl-dcp-inx    to   1                      .
           search    w-zos-esl-dcp-ele
                     when    w-zos-esl-dcp-num
                            (w-zos-esl-dcp-inx)
                                          =    rf-dcp-num-pro of
                                               rf-dcp
                     go to   sel-rec-099-800.
           go to     sel-rec-099-900.
       sel-rec-099-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ok                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-ord-sel-flg-srf      .
           go to     sel-rec-099-999.
       sel-rec-099-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione Ko                         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ord-sel-flg-srf      .
           go to     sel-rec-099-999.
       sel-rec-099-999.
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
      *                      * Read input file [ttt]                   *
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
