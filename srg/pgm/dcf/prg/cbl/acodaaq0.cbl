       Identification Division.
       Program-Id.                                 acodaaq0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/06/92    *
      *                       Ultima revisione:    Ndk del 10/03/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice per 'aaq'        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-aaq-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-aaq-tac : Tipo accettazione    *
      *                                           - N : Numerico       *
      *                                           - A : Alfanumerico   *
      *                                           - S : Sinonimo       *
      *                                           - L : Libera, alfa-  *
      *                                                 numerica non   *
      *                                                 controllata    *
      *                                                                *
      *                       w-cod-cod-aaq-tco : Tipo di codice       *
      *                                           - 01 : per [dcp]     *
      *                                           - 03 : per [dpm]     *
      *                                           - 04 : per [mtv]     *
      *                                                                *
      *                       w-cod-cod-aaq-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-aaq-alf : Codice alfanumerico  *
      *                                             Inutile se tipo    *
      *                                             accettazione 'N'   *
      *                                                                *
      *                       w-cod-cod-aaq-lin : linea a video        *
      *                                                                *
      *                       w-cod-cod-aaq-pos : posizione a video    *
      *                                                                *
      *                       w-cod-cod-aaq-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-aaq-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-aaq-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-aaq-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-aaq-tco : Tipo di codice       *
      *                                           - 01 : per [dcp]     *
      *                                           - 03 : per [dpm]     *
      *                                           - 04 : per [mtv]     *
      *                                                                *
      *                       w-cod-cod-aaq-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-aaq-alf : Codice alfanumerico  *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'A' o   *
      *                                             tipo acc.  'S' o   *
      *                                             tipo acc.  'L'     *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-aaq-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-aaq-tco : Tipo di codice       *
      *                                           - 01 : per [dcp]     *
      *                                           - 03 : per [dpm]     *
      *                                           - 04 : per [mtv]     *
      *                                                                *
      *                       w-cod-cod-aaq-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-aaq-alf : Codice alfanumerico  *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'A' o   *
      *                                             tipo acc.  'S' o   *
      *                                             tipo acc.  'L'     *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-cod-aaq-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-aaq-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-aaq-tco : Tipo di codice       *
      *                                           - 01 : per [dcp]     *
      *                                           - 03 : per [dpm]     *
      *                                           - 04 : per [mtv]     *
      *                                                                *
      *                       w-cod-cod-aaq-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-aaq-alf : Codice alfanumerico  *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'A' o   *
      *                                             tipo acc.  'S' o   *
      *                                             tipo acc.  'L'     *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione materie prime attiva                   *
      *        *-------------------------------------------------------*
           05  w-prs-dpm-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione materiali vari attiva                  *
      *        *-------------------------------------------------------*
           05  w-prs-mtv-snx              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per editings                                         *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Per editing del valore numerico                       *
      *        *-------------------------------------------------------*
           05  w-edt-num.
      *            *---------------------------------------------------*
      *            * Valore numerico da editare                        *
      *            *---------------------------------------------------*
               10  w-edt-num-cod          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Valore numerico editato                           *
      *            *---------------------------------------------------*
               10  w-edt-num-edt          pic  x(14)                  .

      *    *===========================================================*
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio valore alfanumerico proveniente dall'im-  *
      *        * postazione effettivamente eseguita nel chiamante      *
      *        *-------------------------------------------------------*
           05  w-sav-dac-xxx              pic  x(14)                  .

      *    *===========================================================*
      *    * Work per manipolazioni su ridefinizioni                   *
      *    *-----------------------------------------------------------*
       01  w-man.
      *        *-------------------------------------------------------*
      *        * Work per manipolazione area contenente impostazione   *
      *        *-------------------------------------------------------*
           05  w-man-x14.
      *            *---------------------------------------------------*
      *            * Valore da scomporre in parti                      *
      *            *---------------------------------------------------*
               10  w-man-x14-vlr.
                   15  w-man-x14-cha
                                occurs 14 pic  x(01)                  .
               10  w-man-x14-vlx redefines
                   w-man-x14-vlr.
                   15  w-man-x14-chn
                                occurs 14 pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Flag in uscita                                    *
      *            * - # : Valore non accettabile                      *
      *            * - N : Codice prodotto numerico                    *
      *            * - A : Codice prodotto alfanumerico                *
      *            * - S : Sinonimo                                    *
      *            * - ? : Richiesta di codice numerico corrispondente *
      *            * - - : Valore per richiesta ricerca per descriz.   *
      *            *---------------------------------------------------*
               10  w-man-x14-flg          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'N' : Valore numerico                   *
      *            *---------------------------------------------------*
               10  w-man-x14-num          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'A' : Valore alfanumerico               *
      *            *---------------------------------------------------*
               10  w-man-x14-alf          pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'S' : Valore sinonimo                   *
      *            *---------------------------------------------------*
               10  w-man-x14-sin          pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'S' : Carattere di Wildcard             *
      *            *---------------------------------------------------*
               10  w-man-x14-wlc          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella caratteri riconosciuti come segnalazione  *
      *            * di sinonimo                                       *
      *            *---------------------------------------------------*
               10  w-man-x14-tss          pic  x(04) value "=+.,"     .
      *            *---------------------------------------------------*
      *            * Tabella caratteri riconosciuti come segnalazione  *
      *            * di richiesta codice numerico                      *
      *            *---------------------------------------------------*
               10  w-man-x14-trc          pic  x(01) value "?"        .
      *            *---------------------------------------------------*
      *            * Work locale campi, contatori, indici              *
      *            *---------------------------------------------------*
               10  w-man-x14-w00.
                   15  w-man-x14-n14      pic  9(14)                  .
                   15  w-man-x14-i01      pic  9(02)                  .
                   15  w-man-x14-i02      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per le impostazioni per la ricerca per mezzo di un   *
      *    * box locale                                                *
      *    *-----------------------------------------------------------*
       01  w-rbl.
      *        *-------------------------------------------------------*
      *        * Tipo di ricerca                                       *
      *        * - S : Su sinonimo                                     *
      *        * - D : Su descrizione prodotto                         *
      *        *-------------------------------------------------------*
           05  w-rbl-tip-ric              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore sinonimo minimo                                *
      *        *-------------------------------------------------------*
           05  w-rbl-sin-min              pic  x(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore sinonimo massimo                               *
      *        *-------------------------------------------------------*
           05  w-rbl-sin-max.
               10  w-rbl-sin-mxx
                               occurs 13  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore descrizione per l'accettazione                 *
      *        *-------------------------------------------------------*
           05  w-rbl-des-acc              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valore descrizione minimo                             *
      *        *-------------------------------------------------------*
           05  w-rbl-des-min              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valore descrizione massimo                            *
      *        *-------------------------------------------------------*
           05  w-rbl-des-max.
               10  w-rbl-des-mxx
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work locale contatori ed indici                       *
      *        *-------------------------------------------------------*
           05  w-rbl-wrk-loc.
               10  w-rbl-ctr-001          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per il buffer relativo al box locale                 *
      *    *-----------------------------------------------------------*
       01  w-buf.
      *        *-------------------------------------------------------*
      *        * Contatore records caricati nel buffer                 *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-rec              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Max nr records caricabili nel buffer                  *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-max              pic  9(04) value 60         .
      *        *-------------------------------------------------------*
      *        * Indice elemento da selezionare dal buffer             *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-ibx              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Tabella records caricati                              *
      *        *-------------------------------------------------------*
           05  w-buf-tbl-buf.
      *            *---------------------------------------------------*
      *            * Elemento della tabella records caricati           *
      *            *---------------------------------------------------*
               10  w-buf-ele-buf occurs 60.
      *                *-----------------------------------------------*
      *                * Codice numerico [dcp], [dpm] o [mtv]          *
      *                *-----------------------------------------------*
                   15  w-buf-cod-num      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice alfanumerico [dcp], [dpm] o [mtv]      *
      *                *-----------------------------------------------*
                   15  w-buf-cod-alf      pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Codice sinonimo [dcp], [dpm] o [mtv]          *
      *                *-----------------------------------------------*
                   15  w-buf-cod-sin      pic  x(13)                  .
      *                *-----------------------------------------------*
      *                * Descrizione [dcp], [dpm] o [mtv]              *
      *                *-----------------------------------------------*
                   15  w-buf-des-pro      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine visualizzabili del buffer               *
      *        *-------------------------------------------------------*
           05  w-buf-tot-pag              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina attualmente trattata                    *
      *        *-------------------------------------------------------*
           05  w-buf-pag-att              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice primo elemento pagina trattata                 *
      *        *-------------------------------------------------------*
           05  w-buf-pag-ipe              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice ultimo elemento pagina trattata                *
      *        *-------------------------------------------------------*
           05  w-buf-pag-iue              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice per elemento del buffer attualmente trattato   *
      *        *-------------------------------------------------------*
           05  w-buf-inx-buf              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Linea a video per elemento del buffer trattato        *
      *        *-------------------------------------------------------*
           05  w-buf-num-lnv              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici di comodo locali                  *
      *        *-------------------------------------------------------*
           05  w-buf-i01-lnv              pic  9(04)                  .
           05  w-buf-ctr-005              pic  9(04)                  .
           05  w-buf-lit-pag.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-buf-lit-att          pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-buf-lit-tot          pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-ble.
           05  w-ble-max                  pic  9(02)                  .
           05  w-ble-flg                  pic  x(01)                  .
           05  w-ble-str.
               10  w-ble-chr    occurs 40 pic  x(01)                  .
           05  w-ble-ctr                  pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto d'acquisto     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acodaaq0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-aaq
                                               v
                                               s                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-aaq-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-aaq-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-aaq-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-aaq-ope    =    "A+" or
                     w-cod-cod-aaq-ope    =    "I+" or
                     w-cod-cod-aaq-ope    =    "F+"
                     perform   aco-000    thru aco-999                .
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
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' la prima Open per il modulo si *
      *              * esce                                            *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    1
                     go to opn-999.
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa e' la prima Open per il modulo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazioni                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/No gestione materie prime attiva     *
      *                      *-----------------------------------------*
           perform   prs-dpm-snx-000      thru prs-dpm-snx-999        .
      *                      *-----------------------------------------*
      *                      * Si/No gestione materiali vari attiva    *
      *                      *-----------------------------------------*
           perform   prs-mtv-snx-000      thru prs-mtv-snx-999        .
      *                  *---------------------------------------------*
      *                  * Open file [aaq]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Open file [dcp]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Open file [dpm]                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se gestione materie prime attiva   *
      *                      *-----------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to opn-120.
      *                      *-----------------------------------------*
      *                      * Apertura file                           *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       opn-120.
      *                  *---------------------------------------------*
      *                  * Open file [mtv]                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se gestione materiali vari attiva  *
      *                      *-----------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to opn-999.
      *                      *-----------------------------------------*
      *                      * Apertura file                           *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materie prime  *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-dpm-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dpm[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dpm-snx
           else      move  "N"            to   w-prs-dpm-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dpm-snx        not  = "S" and
                     w-prs-dpm-snx        not  = "N"
                     move  "N"            to   w-prs-dpm-snx          .
       prs-dpm-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione materiali vari *
      *    *                             attiva                        *
      *    *-----------------------------------------------------------*
       prs-mtv-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mtv[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-mtv-snx
           else      move  "N"            to   w-prs-mtv-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-mtv-snx        not  = "S" and
                     w-prs-mtv-snx        not  = "N"
                     move  "N"            to   w-prs-mtv-snx          .
       prs-mtv-snx-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' l'ultima Close per il modulo   *
      *              * si esce                                         *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    zero
                     go to cls-999.
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [aaq]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Close file [dcp]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Close file [dpm]                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se gestione materie prime attiva   *
      *                      *-----------------------------------------*
           if        w-prs-dpm-snx        not  = "S"
                     go to cls-120.
      *                      *-----------------------------------------*
      *                      * Chiusura file                           *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
       cls-120.
      *                  *---------------------------------------------*
      *                  * Close file [mtv]                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se gestione materiali vari attiva  *
      *                      *-----------------------------------------*
           if        w-prs-mtv-snx        not  = "S"
                     go to cls-999.
      *                      *-----------------------------------------*
      *                      * Chiusura file                           *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test di cancellabilita' per il modulo                     *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore delle Open in corso per il mo-  *
      *              * dulo e' pari a zero si dichiara che e' cancel-  *
      *              * labile, altrimento che non lo e'                *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        =    zero
                     move  spaces         to   w-cod-cod-aaq-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find, a secon- *
      *              * da del tipo codice                              *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           if        w-cod-cod-aaq-tco    =    01
                     move  "pdcp4010"     to   s-pro
           else if   w-cod-cod-aaq-tco    =    03
                     move  "pdpm4010"     to   s-pro
           else      move  "pmtv4010"     to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr, a secon- *
      *              * da del tipo codice                              *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcf3000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-150.
           if        v-pfk (04)           =    "INSR"
                     move  spaces         to   v-pfk (04)             .
       acc-150.
      *              *-------------------------------------------------*
      *              * Abilitazione eventuale del tasto Pf2            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di impostazione Pf2    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-cod-aaq-pf2      .
      *                  *---------------------------------------------*
      *                  * Se tipo accettazione diverso da 'A' : no    *
      *                  *---------------------------------------------*
           if        w-cod-cod-aaq-tac    not  = "A"
                     go to acc-200.
      *                  *---------------------------------------------*
      *                  * Test se programma di interrogazione su di-  *
      *                  * sponibilita' di magazzino gia' attivo       *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pmag3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to acc-200.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura variabile globale di   *
      *                  * i.p.c. 'acodaaq0.[2]'                       *
      *                  *---------------------------------------------*
           move      "GV"                 to   s-ope                  .
           move      "acodaaq0.[2]"       to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se variabile esistente : no abilitazione    *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to acc-200.
      *                  *---------------------------------------------*
      *                  * Abilitazione tasto Pf2                      *
      *                  *---------------------------------------------*
           move      "[2] "               to   v-pfk (15)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di codice                              *
      *                  *---------------------------------------------*
           move      w-cod-cod-aaq-tco    to   w-cod-cod-aaq-s00      .
      *                  *---------------------------------------------*
      *                  * Codice numerico                             *
      *                  *---------------------------------------------*
           if        w-cod-cod-aaq-tac    =    "N" or
                     w-cod-cod-aaq-tac    =    "A"
                     move  w-cod-cod-aaq-num
                                          to   w-cod-cod-aaq-s01
           else      move  zero           to   w-cod-cod-aaq-s01      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico                         *
      *                  *---------------------------------------------*
           if        w-cod-cod-aaq-tac    =    "N"
                     move  spaces         to   w-cod-cod-aaq-s02
           else      move  w-cod-cod-aaq-alf
                                          to   w-cod-cod-aaq-s02      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing v-edm                   *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-cod-aaq-s70      .
      *                  *---------------------------------------------*
      *                  * User function-keys eventualmente epurate    *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-cod-aaq-s90      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo accettazione    *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-tac    =    "N"
                     go to acc-500
           else      go to acc-600.
       acc-500.
      *              *-------------------------------------------------*
      *              * Se tipo accettazione 'N'                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore numerico editato        *
      *                  *---------------------------------------------*
           move      w-cod-cod-aaq-s01    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione                   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-s90    to   v-ufk                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : continuazione           *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cod-cod-aaq-ope      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-999.
       acc-600.
      *              *-------------------------------------------------*
      *              * Se tipo accettazione 'A' - 'S' - 'L'            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione                   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cod-cod-aaq-tac    =    "S"
                     move  13             to   v-car
           else      move  14             to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-s90    to   v-ufk                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-cod-cod-aaq-s02    to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : continuazione           *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cod-cod-aaq-ope      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-999.
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-ope    =    "A+"
                     go to aco-300
           else if   w-cod-cod-aaq-ope    =    "F+"
                     go to aco-200.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Insr                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Reimpostazione, a seconda del tipo di ac-   *
      *                  * cettazione                                  *
      *                  *---------------------------------------------*
           if        w-cod-cod-aaq-tac    =    "N"
                     go to aco-125
           else      go to aco-150.
       aco-125.
      *                  *---------------------------------------------*
      *                  * Se tipo accettazione 'N'                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione valore numerico editato    *
      *                      *-----------------------------------------*
           move      w-cod-cod-aaq-s01    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-s90    to   v-ufk                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-aaq-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-150.
      *                  *---------------------------------------------*
      *                  * Se tipo accettazione 'A'  'S'  'L'          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cod-cod-aaq-tac    =    "S"
                     move  13             to   v-car
           else      move  14             to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-s90    to   v-ufk                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-cod-cod-aaq-s02    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-aaq-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Find                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della variabile di i.p.c. di ritor- *
      *                  * no dall'interrogazione, e deviazione in     *
      *                  * funzione dell'esito della lettura.          *
      *                  * A seconda del tipo codice.                  *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           if        w-cod-cod-aaq-s00    =    03
                     move  "num-map"      to   s-var
           else if   w-cod-cod-aaq-s00    =    04
                     move  "num-mtv"      to   s-var
           else      move  "num-pro"      to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     go to aco-220.
       aco-210.
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. non esistente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-100.
       aco-220.
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. esistente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di accet- *
      *                      * tazione                                 *
      *                      *-----------------------------------------*
           if        w-cod-cod-aaq-tac    =    "N"
                     go to aco-230
           else      go to aco-240.
       aco-230.
      *                      *-----------------------------------------*
      *                      * Se tipo di accettazione 'N'             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore selezionato in area di usci- *
      *                          * ta numerica                         *
      *                          *-------------------------------------*
           move      s-num                to   w-cod-cod-aaq-num      .
      *                          *-------------------------------------*
      *                          * Area di uscita alfanumerica a spa-  *
      *                          * ces                                 *
      *                          *-------------------------------------*
           move      spaces               to   w-cod-cod-aaq-alf      .
      *                          *-------------------------------------*
      *                          * Editing del valore in uscita        *
      *                          *-------------------------------------*
           move      w-cod-cod-aaq-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                          *-------------------------------------*
      *                          * Visualizzazione del valore editato  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo operazione : non-continuazione *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                          *-------------------------------------*
      *                          * Normalizzazione function-key        *
      *                          *-------------------------------------*
           move      spaces               to   v-key                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-240.
      *                      *-----------------------------------------*
      *                      * Se tipo di accettazione 'A'  'S '  'L'  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del record relativo al co-  *
      *                          * dice numerico ritornato, e devia-   *
      *                          * zione a seconda dell'esito della    *
      *                          * lettura                             *
      *                          *-------------------------------------*
           perform   aco-rkx-000          thru aco-rkx-999            .
           if        f-sts                =    e-not-err
                     go to aco-260
           else      go to aco-250.
       aco-250.
      *                          *-------------------------------------*
      *                          * Se record non esistente             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     aco-100.
       aco-260.
      *                          *-------------------------------------*
      *                          * Se record esistente                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-tac    =    "A"
                     go to aco-280.
       aco-270.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'S'  'L'   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Area di uscita numerica in  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     move  rf-dpm-num-map to   w-cod-cod-aaq-num
           else if   w-cod-cod-aaq-s00    =    04
                     move  rf-mtv-num-mtv to   w-cod-cod-aaq-num
           else      move  rf-dcp-num-pro to   w-cod-cod-aaq-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico conte-  *
      *                                  * nuto nel record in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     move  rf-dpm-alf-map to   w-cod-cod-aaq-alf
           else if   w-cod-cod-aaq-s00    =    04
                     move  rf-mtv-alf-mtv to   w-cod-cod-aaq-alf
           else      move  rf-dcp-alf-pro to   w-cod-cod-aaq-alf      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico selezionato    *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-cod-cod-aaq-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-280.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'A'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore numerico contenuto   *
      *                                  * nel record in area di usci- *
      *                                  * ta                          *
      *                                  *-----------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     move  rf-dpm-num-map to   w-cod-cod-aaq-num
           else if   w-cod-cod-aaq-s00    =    04
                     move  rf-mtv-num-mtv to   w-cod-cod-aaq-num
           else      move  rf-dcp-num-pro to   w-cod-cod-aaq-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico conte-  *
      *                                  * nuto nel record in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     move  rf-dpm-alf-map to   w-cod-cod-aaq-alf
           else if   w-cod-cod-aaq-s00    =    04
                     move  rf-mtv-alf-mtv to   w-cod-cod-aaq-alf
           else      move  rf-dcp-alf-pro to   w-cod-cod-aaq-alf      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico selezionato    *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-cod-cod-aaq-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-300.
      *              *-------------------------------------------------*
      *              * Se rientro ne' dopo Insr ne' dopo Find          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt :                            *
      *                  * - Tipo operazione : non-continuazione       *
      *                  * - Uscita                                    *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "DELT"
                     move  "AC"           to   w-cod-cod-aaq-ope
                     go to aco-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore alfanumerico proveniente *
      *                  * dall'impostazione effettivamente eseguita   *
      *                  * nel chiamante                               *
      *                  *---------------------------------------------*
           move      v-alf                to   w-sav-dac-xxx          .
      *                  *---------------------------------------------*
      *                  * Se valore impostato a Spaces                *
      *                  *---------------------------------------------*
           if        w-sav-dac-xxx        not  = spaces
                     go to aco-320.
      *                      *-----------------------------------------*
      *                      * Valore numerico in uscita a zero        *
      *                      *-----------------------------------------*
           move      zero                 to   w-cod-cod-aaq-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico in uscita a Spaces  *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-cod-aaq-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione dell'area destinata al- *
      *                      * l'impostazione del codice a Spaces      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione : non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-320.
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded                     *
      *                  *---------------------------------------------*
           move      w-sav-dac-xxx        to   w-ble-str              .
           move      14                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            =    spaces
                     go to aco-360.
       aco-340.
      *                  *---------------------------------------------*
      *                  * Reimpostazione con default pari al valore   *
      *                  * alfanumerico salvato proveniente dal chia-  *
      *                  * mante                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cod-cod-aaq-tac    =    "S"
                     move  13             to   v-car
           else      move  14             to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-s90    to   v-ufk                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-sav-dac-xxx        to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-aaq-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-360.
      *                  *---------------------------------------------*
      *                  * Suddivisione ed analisi codice impostato di *
      *                  * 14 caratteri alfanumerici proveniente dal   *
      *                  * chiamante                                   *
      *                  *---------------------------------------------*
           move      w-sav-dac-xxx        to   w-man-x14-vlr          .
           perform   sac-000              thru sac-999                .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del risultato del-   *
      *                  * la suddivisione ed analisi                  *
      *                  *---------------------------------------------*
           if        w-man-x14-flg        =    "N"
                     go to aco-380
           else if   w-man-x14-flg        =    "A"
                     go to aco-400
           else if   w-man-x14-flg        =    "S"
                     go to aco-500
           else if   w-man-x14-flg        =    "?"
                     go to aco-530
           else if   w-man-x14-flg        =    "-"
                     go to aco-540
           else      go to aco-340.
       aco-380.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'N'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di accettazione non e' 'N'   *
      *                      * si torna alla reimpostazione con de-    *
      *                      * fault pari al valore salvato            *
      *                      *-----------------------------------------*
           if        w-cod-cod-aaq-tac    not  = "N"
                     go to aco-340.
      *                      *-----------------------------------------*
      *                      * Se il tipo di accettazione e' 'N' si e- *
      *                      * sce e si ritorna al chiamante           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore impostato in area di uscita  *
      *                          * ta numerica                         *
      *                          *-------------------------------------*
           move      w-man-x14-num        to   w-cod-cod-aaq-num      .
      *                          *-------------------------------------*
      *                          * Area di uscita alfanumerica a spa-  *
      *                          * ces                                 *
      *                          *-------------------------------------*
           move      spaces               to   w-cod-cod-aaq-alf      .
      *                          *-------------------------------------*
      *                          * Editing del valore in uscita        *
      *                          *-------------------------------------*
           move      w-cod-cod-aaq-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                          *-------------------------------------*
      *                          * Visualizzazione del valore editato  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo operazione : non-continuazione *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'A'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di accet- *
      *                      * tazione                                 *
      *                      *-----------------------------------------*
           if        w-cod-cod-aaq-tac    =    "A" or
                     w-cod-cod-aaq-tac    =    "N"
                     go to aco-440.
       aco-420.
      *                      *-----------------------------------------*
      *                      * Se tipo accettazione 'S'  'L'           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore impostato in area di uscita  *
      *                          * ta alfanumerica                     *
      *                          *-------------------------------------*
           move      w-man-x14-alf        to   w-cod-cod-aaq-alf      .
      *                          *-------------------------------------*
      *                          * Area di uscita numerica a zero      *
      *                          *-------------------------------------*
           move      zero                 to   w-cod-cod-aaq-num      .
      *                          *-------------------------------------*
      *                          * Tipo operazione : non-continuazione *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-440.
      *                      *-----------------------------------------*
      *                      * Se tipo accettazione 'A'  'N'           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo codi- *
      *                          * ce per la lettura del record in a-  *
      *                          * nagrafica di base relativo al codi- *
      *                          * ce alfanumerico impostato, con de-  *
      *                          * viazione secondo l'esito della let- *
      *                          * tura.                               *
      *                          *-------------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-442
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-443.
       aco-441.
      *                          *-------------------------------------*
      *                          * Se tipo codice 01 : [dcp]           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Start                           *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ALFPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-man-x14-alf        to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * Se start errata : a record non  *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Read next per il primo record   *
      *                              *---------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * Se At End : a record non esist. *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Se il codice alfanumerico con-  *
      *                              * tenuto nel record non e' pari a *
      *                              * quello cercato : a record non   *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        rf-dcp-alf-pro       not  = w-man-x14-alf
                     go to aco-480.
      *                              *---------------------------------*
      *                              * A record trovato                *
      *                              *---------------------------------*
           go to     aco-460.
       aco-442.
      *                          *-------------------------------------*
      *                          * Se tipo codice 03 : [dpm]           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Start                           *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ALFMAP    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-man-x14-alf        to   rf-dpm-alf-map         .
           move      zero                 to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                              *---------------------------------*
      *                              * Se start errata : a record non  *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Read next per il primo record   *
      *                              *---------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                              *---------------------------------*
      *                              * Se At End : a record non esist. *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Se il codice alfanumerico con-  *
      *                              * tenuto nel record non e' pari a *
      *                              * quello cercato : a record non   *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        rf-dpm-alf-map       not  = w-man-x14-alf
                     go to aco-480.
      *                              *---------------------------------*
      *                              * A record trovato                *
      *                              *---------------------------------*
           go to     aco-460.
       aco-443.
      *                          *-------------------------------------*
      *                          * Se tipo codice 04 : [mtv]           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Start                           *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ALFMTV    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-man-x14-alf        to   rf-mtv-alf-mtv         .
           move      zero                 to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *                              *---------------------------------*
      *                              * Se start errata : a record non  *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Read next per il primo record   *
      *                              *---------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *                              *---------------------------------*
      *                              * Se At End : a record non esist. *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Se il codice alfanumerico con-  *
      *                              * tenuto nel record non e' pari a *
      *                              * quello cercato : a record non   *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        rf-mtv-alf-mtv       not  = w-man-x14-alf
                     go to aco-480.
      *                              *---------------------------------*
      *                              * A record trovato                *
      *                              *---------------------------------*
           go to     aco-460.
       aco-460.
      *                          *-------------------------------------*
      *                          * Se trovato il record [dcp],[dpm] o  *
      *                          * [mtv] cercato                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in area di u-  *
      *                              * scita alfanumerica              *
      *                              *---------------------------------*
           move      w-man-x14-alf        to   w-cod-cod-aaq-alf      .
      *                              *---------------------------------*
      *                              * Codice numerico contenuto nel   *
      *                              * record in area di uscita nume-  *
      *                              * rica                            *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     move  rf-dpm-num-map to   w-cod-cod-aaq-num
           else if   w-cod-cod-aaq-s00    =    04
                     move  rf-mtv-num-mtv to   w-cod-cod-aaq-num
           else      move  rf-dcp-num-pro to   w-cod-cod-aaq-num      .
       aco-465.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-tac    =    "N"
                     go to aco-475.
       aco-470.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'A'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se flag di impostazione ta- *
      *                                  * sto Pf2 attivo : uscita con *
      *                                  * valore chiave impostata a   *
      *                                  * spazi e tipo operazione F+  *
      *                                  *-----------------------------*
           if        w-cod-cod-aaq-pf2    not  = spaces
                     move  spaces         to   v-key
                     move  "F+"           to   w-cod-cod-aaq-ope
                     go to aco-999.
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-475.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'N'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing del valore numerico *
      *                                  * in uscita                   *
      *                                  *-----------------------------*
           move      w-cod-cod-aaq-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * numerico in uscita editato  *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-480.
      *                          *-------------------------------------*
      *                          * Se record 'dcp' relativo al codice  *
      *                          * alfanumerico impostato non esisten- *
      *                          * te : a reimpostazione con default   *
      *                          * pari al valore salvato              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione puntini in area *
      *                              * per la descrizione              *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-dln    =    zero
                     go to aco-482.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-dln    to   v-lin                  .
           move      w-cod-cod-aaq-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-482.
      *                              *---------------------------------*
      *                              * Normalizzazione flag di impo-   *
      *                              * stazione tasto Pf2              *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-pf2    not  = spaces
                     move  spaces         to   w-cod-cod-aaq-pf2      .
      *                              *---------------------------------*
      *                              * A reimpostazione con defualt    *
      *                              * pari al valore salvato          *
      *                              *---------------------------------*
           go to     aco-340.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'S'            *
      *                  *---------------------------------------------*
       aco-510.
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'ese-   *
      *                      * cuzione della ricerca per mezzo di un   *
      *                      * box locale per la selezione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca                     *
      *                          *-------------------------------------*
           move      "S"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Valore sinonimo iniziale            *
      *                          *-------------------------------------*
           move      w-man-x14-sin        to   w-rbl-sin-min          .
      *                          *-------------------------------------*
      *                          * Valore sinonimo finale              *
      *                          *-------------------------------------*
           move      w-man-x14-sin        to   w-rbl-sin-max          .
      *                          *-------------------------------------*
      *                          * Se valore wildcard diverso da '+' : *
      *                          * oltre                               *
      *                          *-------------------------------------*
           if        w-man-x14-wlc        not  = "+"
                     go to aco-520.
       aco-515.
      *                          *-------------------------------------*
      *                          * Padding del sinonimo finale con 'z' *
      *                          *-------------------------------------*
           move      13                   to   w-rbl-ctr-001          .
       aco-516.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-sin-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-sin-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-516.
       aco-520.
      *                          *-------------------------------------*
      *                          * Alla ricerca per mezzo del box lo-  *
      *                          * cale di selezione                   *
      *                          *-------------------------------------*
           go to     aco-600.
       aco-530.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo '?' per ri-    *
      *                  * chiesta del codice numerico corrispondente  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di accettazione non e' 'A'   *
      *                      * si torna alla reimpostazione con de-    *
      *                      * fault pari al valore originale          *
      *                      *-----------------------------------------*
           if        w-cod-cod-aaq-tac    not  = "A"
                     go to aco-100.
      *                      *-----------------------------------------*
      *                      * Altrimenti si mostra il codice numeri-  *
      *                      * co corrispondente in un box locale      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Salvataggio immagine video          *
      *                          *-------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Visualizzazione box vuoto           *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      13                   to   v-lto                  .
           move      67                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Interno del box                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Titolo                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "Codice numerico corrispondente : "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Codice numerico editato         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing                     *
      *                                  *-----------------------------*
           move      w-cod-cod-aaq-s01    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * numerico editato            *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Parentesi quadre                *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Accettazione presa visione          *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      64                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ripristino immagine video           *
      *                          *-------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Quindi si torna alla reimpostazione con *
      *                      * default pari al valore originale        *
      *                      *-----------------------------------------*
           go to     aco-100.
       aco-540.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo '-'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non e' specificata una linea per la  *
      *                      * descrizione si ritorna alla reimposta-  *
      *                      * zione con default pari al valore sal-   *
      *                      * vato                                    *
      *                      *-----------------------------------------*
           if        w-cod-cod-aaq-dln    =    zero or
                     w-cod-cod-aaq-dps    =    zero
                     go to aco-340.
       aco-545.
      *                      *-----------------------------------------*
      *                      * Accettazione descrizione per ricerca    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Spaces in comodo per impostazione   *
      *                          * descrizione                         *
      *                          *-------------------------------------*
           move      spaces               to   w-rbl-des-acc          .
      *                          *-------------------------------------*
      *                          * Visualizzazione spaces in area di   *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-aaq-dln    to   v-lin                  .
           move      w-cod-cod-aaq-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-550.
      *                          *-------------------------------------*
      *                          * Accettazione descrizione minima di  *
      *                          * ricerca, in uppercase               *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-aaq-dln    to   v-lin                  .
           move      w-cod-cod-aaq-dps    to   v-pos                  .
           move      w-rbl-des-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Valore accettato in area di comodo  *
      *                          * di destinazione                     *
      *                          *-------------------------------------*
           move      v-alf                to   w-rbl-des-acc          .
       aco-555.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-560.
      *                              *---------------------------------*
      *                              * Visualizzazione spaces in area  *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-aaq-dln    to   v-lin                  .
           move      w-cod-cod-aaq-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-557.
      *                              *---------------------------------*
      *                              * Alla reimpostazione con default *
      *                              * pari al valore salvato          *
      *                              *---------------------------------*
           go to     aco-340.
       aco-560.
      *                          *-------------------------------------*
      *                          * Se Exit                             *
      *                          *-------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-565.
      *                              *---------------------------------*
      *                              * Ripristino valori originali     *
      *                              *---------------------------------*
           move      w-cod-cod-aaq-s01    to   w-cod-cod-aaq-num      .
           move      w-cod-cod-aaq-s02    to   w-cod-cod-aaq-alf      .
      *                              *---------------------------------*
      *                              * Tipo operazione a : non conti-  *
      *                              * nuazione                        *
      *                              *---------------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-565.
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se Valore impostato a Spaces :  *
      *                              * trattamento come per Up         *
      *                              *---------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-557.
       aco-570.
      *                              *---------------------------------*
      *                              * Preparazione dei parametri per  *
      *                              * l'esecuzione della ricerca per  *
      *                              * mezzo di un box locale per la   *
      *                              * selezione                       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo di ricerca             *
      *                                  *-----------------------------*
           move      "D"                  to   w-rbl-tip-ric          .
      *                                  *-----------------------------*
      *                                  * Valore descrizione iniziale *
      *                                  *-----------------------------*
           move      w-rbl-des-acc        to   w-rbl-des-min          .
       aco-575.
      *                                  *-----------------------------*
      *                                  * Valore descrizione finale   *
      *                                  * con padding di 'z'          *
      *                                  *-----------------------------*
           move      w-rbl-des-acc        to   w-rbl-des-max          .
           move      40                   to   w-rbl-ctr-001          .
       aco-576.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-des-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-des-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-576.
       aco-580.
      *                                  *-----------------------------*
      *                                  * Alla ricerca mediante box   *
      *                                  * locale di selezione         *
      *                                  *-----------------------------*
           go to     aco-600.
       aco-600.
      *                  *---------------------------------------------*
      *                  * Ricerca mediante box locale di selezione    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Contatore records caricati nel buffer a *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
       aco-605.
      *                      *-----------------------------------------*
      *                      * Start                                   *
      *                      *-----------------------------------------*
           perform   aco-str-000          thru aco-str-999            .
       aco-610.
      *                      *-----------------------------------------*
      *                      * Test esito Start : se errata a tratta-  *
      *                      * mento per fine file, altrimenti conti-  *
      *                      * nuazione                                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-650.
       aco-615.
      *                      *-----------------------------------------*
      *                      * Read Next                               *
      *                      *-----------------------------------------*
           perform   aco-rnx-000          thru aco-rnx-999            .
      *                      *-----------------------------------------*
      *                      * Se At End : a trattamento per fine fi-  *
      *                      * le, altrimenti continuazione            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-650.
       aco-620.
      *                      *-----------------------------------------*
      *                      * Test max                                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * ricerca                             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-621
           else      go to aco-625.
       aco-621.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Sinonimo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Test : se non supe-  *
      *                              * rato si va' a trattamento per   *
      *                              * fine file, altrimenti si con-   *
      *                              * tinua                           *
      *                              * In funzione del tipo prodotto   *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-622
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-623.
           if        rf-dcp-syn-pro       >    w-rbl-sin-max
                     go to aco-650
           else      go to aco-629.
       aco-622.
           if        rf-dpm-syn-map       >    w-rbl-sin-max
                     go to aco-650
           else      go to aco-629.
       aco-623.
           if        rf-mtv-syn-mtv       >    w-rbl-sin-max
                     go to aco-650
           else      go to aco-629.
       aco-625.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Descrizione     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Test : se non supe-  *
      *                              * rato si va' a trattamento per   *
      *                              * fine file, altrimenti si con-   *
      *                              * tinua                           *
      *                              * In funzione del tipo prodotto   *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-626
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-627.
           if        rf-dcp-des-key       >    w-rbl-des-max
                     go to aco-650
           else      go to aco-629.
       aco-626.
           if        rf-dpm-des-key       >    w-rbl-des-max
                     go to aco-650
           else      go to aco-629.
       aco-627.
           if        rf-mtv-des-key       >    w-rbl-des-max
                     go to aco-650
           else      go to aco-629.
       aco-629.
      *                      *-----------------------------------------*
      *                      * Incremento numero records caricati nel  *
      *                      * buffer                                  *
      *                      *-----------------------------------------*
           add       1                    to   w-buf-ctr-rec          .
      *                      *-----------------------------------------*
      *                      * Se oltre il max : a trattamento per fi- *
      *                      * ne file                                 *
      *                      *-----------------------------------------*
           if        w-buf-ctr-rec        >    w-buf-ctr-max
                     go to aco-650.
       aco-630.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record letto            *
      *                      *-----------------------------------------*
           perform   aco-buf-000          thru aco-buf-999            .
       aco-635.
      *                      *-----------------------------------------*
      *                      * Riciclo a Read Next                     *
      *                      *-----------------------------------------*
           go to     aco-615.
       aco-650.
      *                      *-----------------------------------------*
      *                      * Trattamento per fine file               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero di  *
      *                          * records caricati nel buffer         *
      *                          *-------------------------------------*
           if        w-buf-ctr-rec        =    zero
                     go to aco-480
           else if   w-buf-ctr-rec        =    1
                     go to aco-655
           else if   w-buf-ctr-rec        >    w-buf-ctr-max
                     go to aco-675
           else      go to aco-700.
       aco-655.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer pari a 1                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Indice elemento da selezionare  *
      *                              * dal buffer a 1                  *
      *                              *---------------------------------*
           move      1                    to   w-buf-ctr-ibx          .
       aco-660.
      *                          *-------------------------------------*
      *                          * Selezione valori dall'elemento del  *
      *                          * buffer di indice (w-buf-ctr-ibx) e  *
      *                          * ritorno al chiamante                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-aaq-tac    =    "N"
                     go to aco-665
           else      go to aco-670.
       aco-665.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'N'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore numerico in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-aaq-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico in area *
      *                                  * di uscita a Spaces          *
      *                                  *-----------------------------*
           move      spaces               to   w-cod-cod-aaq-alf      .
      *                                  *-----------------------------*
      *                                  * Editing del valore numerico *
      *                                  * in uscita                   *
      *                                  *-----------------------------*
           move      w-cod-cod-aaq-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * numerico editato            *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione a non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-670.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'A'  'S'   *
      *                              *                      'L'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore numerico in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-aaq-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico in area *
      *                                  * di uscita                   *
      *                                  *-----------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-ibx)       to   w-cod-cod-aaq-alf      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico                *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-aaq-lin    to   v-lin                  .
           move      w-cod-cod-aaq-pos    to   v-pos                  .
           move      w-cod-cod-aaq-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione a non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-675.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer maggiore del massimo nume-   *
      *                          * ro di records caricabili            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-681
           else      go to aco-682.
       aco-681.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Sinoni-  *
      *                              * mo                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'tip-int'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'syn-min'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "syn-min"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      13                   to   s-car                  .
           move      w-rbl-sin-min        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'syn-max'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "syn-max"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      13                   to   s-car                  .
           move      w-rbl-sin-max        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * A completamento comune      *
      *                                  *-----------------------------*
           go to     aco-683.
       aco-682.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Descri-  *
      *                              * zione                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'tip-int'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "D"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'des-min'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "des-min"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-rbl-des-min        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'des-max'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "des-max"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-rbl-des-max        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * A completamento comune      *
      *                                  *-----------------------------*
           go to     aco-683.
       aco-683.
      *                              *---------------------------------*
      *                              * Completamento comune pre-uscita *
      *                              * per Find                        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo operazione a : esecu-  *
      *                                  * zione Find                  *
      *                                  *-----------------------------*
           move      "F+"                 to   w-cod-cod-aaq-ope      .
      *                                  *-----------------------------*
      *                                  * Simulazione tasto Find      *
      *                                  *-----------------------------*
           move      "FIND"               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-700.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer compreso tra 2 ed il massi-  *
      *                          * mo numero records caricabili        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * totali nel buffer               *
      *                              *---------------------------------*
           move      w-buf-ctr-rec        to   w-buf-tot-pag          .
           subtract  1                    from w-buf-tot-pag          .
           divide    10                   into w-buf-tot-pag          .
           add       1                    to   w-buf-tot-pag          .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-buf-inx-buf          .
      *                              *---------------------------------*
      *                              * Salvataggio immagine video      *
      *                              *---------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione box vuoto       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           if        w-cod-cod-aaq-s00    =    03
                     move  "            Selezionare il codice materia pr
      -                    "ima desiderato              "
                                          to   v-alf
           else if   w-cod-cod-aaq-s00    =    04
                     move  "           Selezionare il codice materiale v
      -                    "ario desiderato             "
                                          to   v-alf
           else      move  "               Selezionare il codice prodott
      -                    "o desiderato                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "----------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "----------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina video    *
      *                              * contenente il record attual-    *
      *                              * mente trattato                  *
      *                              *---------------------------------*
           perform   vpg-000              thru vpg-999                .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-720.
      *                              *---------------------------------*
      *                              * Determinazione numero linea a   *
      *                              * video in funzione del numero    *
      *                              * elemento in tabella trattato,   *
      *                              * di indice (w-buf-inx-buf)       *
      *                              *---------------------------------*
           divide    10                   into w-buf-inx-buf
                                        giving w-buf-ctr-005
                                     remainder w-buf-num-lnv          .
           if        w-buf-num-lnv        =    zero
                     move  10             to   w-buf-num-lnv          .
           add       07                   to   w-buf-num-lnv          .
       aco-740.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key alla linea calcolata        *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-inx-buf        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-inx-buf        <    w-buf-ctr-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-pag-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-pag-att        <    w-buf-tot-pag
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-num-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-750.
      *                              *---------------------------------*
      *                              * Deviazione a seconda della fun- *
      *                              * ction key impostata             *
      *                              *---------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-760
           else if   v-key                =    "UP  "
                     go to aco-770
           else if   v-key                =    "DOWN"
                     go to aco-780
           else if   v-key                =    "EXIT"
                     go to aco-790
           else if   v-key                =    "NXSC"
                     go to aco-800
           else if   v-key                =    "PRSC"
                     go to aco-810
           else      go to aco-740.
       aco-760.
      *                              *---------------------------------*
      *                              * Se Do o Slct                    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-buf-inx-buf        to   w-buf-ctr-ibx          .
      *                                  *-----------------------------*
      *                                  * A selezione valori indiriz- *
      *                                  * zati da (w-buf-ctr-ibx) ed  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           go to     aco-660.
       aco-770.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    08
                     go to aco-850
           else      go to aco-720.
       aco-780.
      *                              *---------------------------------*
      *                              * Se Return o Down                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-buf-inx-buf        =    w-buf-ctr-rec
                     go to aco-740.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    17
                     go to aco-850
           else      go to aco-720.
       aco-790.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Se il tipo ricerca e' per   *
      *                                  * Descrizione si ritorna al-  *
      *                                  * l'impostazione della de-    *
      *                                  * scrizione di ricerca mini-  *
      *                                  * ma, altrimenti si ritorna   *
      *                                  * alla impostazione con de-   *
      *                                  * fault del valore salvato    *
      *                                  *-----------------------------*
           if        w-rbl-tip-ric        =    "D"
                     go to aco-545
           else      go to aco-340.
       aco-800.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  10                   by   w-buf-inx-buf          .
           subtract  9                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-850.
       aco-810.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  10                   by   w-buf-inx-buf          .
           subtract  9                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-850.
       aco-850.
      *                              *---------------------------------*
      *                              * Visualizzazione nuova pagina    *
      *                              * video                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione pagina vi-  *
      *                                  * deo contenente il record    *
      *                                  * attualmente trattato        *
      *                                  *-----------------------------*
           perform   vpg-000              thru vpg-999                .
      *                                  *-----------------------------*
      *                                  * A reimpostazione funct-key  *
      *                                  *-----------------------------*
           go to     aco-720.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per Start per ricerca                          *
      *    *-----------------------------------------------------------*
       aco-str-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo codice trattato   *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-str-200
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-str-300.
       aco-str-100.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 01 : [dcp]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di ricerca    *
      *                  *---------------------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-str-110
           else      go to aco-str-120.
       aco-str-110.
      *                  *---------------------------------------------*
      *                  * Se tipo ricerca per Sinonimo                *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SYNPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-sin-min        to   rf-dcp-syn-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           go to     aco-str-999.
       aco-str-120.
      *                  *---------------------------------------------*
      *                  * Se tipo ricerca per Descrizione             *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-des-min        to   rf-dcp-des-key         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           go to     aco-str-999.
       aco-str-200.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 03 : [dpm]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di ricerca    *
      *                  *---------------------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-str-210
           else      go to aco-str-220.
       aco-str-210.
      *                  *---------------------------------------------*
      *                  * Se tipo ricerca per Sinonimo                *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SYNMAP    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-sin-min        to   rf-dpm-syn-map         .
           move      spaces               to   rf-dpm-alf-map         .
           move      zero                 to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           go to     aco-str-999.
       aco-str-220.
      *                  *---------------------------------------------*
      *                  * Se tipo ricerca per Descrizione             *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-des-min        to   rf-dpm-des-key         .
           move      zero                 to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           go to     aco-str-999.
       aco-str-300.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 04 : [mtv]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di ricerca    *
      *                  *---------------------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-str-310
           else      go to aco-str-320.
       aco-str-310.
      *                  *---------------------------------------------*
      *                  * Se tipo ricerca per Sinonimo                *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SYNMTV    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-sin-min        to   rf-mtv-syn-mtv         .
           move      zero                 to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           go to     aco-str-999.
       aco-str-320.
      *                  *---------------------------------------------*
      *                  * Se tipo ricerca per Descrizione             *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-des-min        to   rf-mtv-des-key         .
           move      zero                 to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           go to     aco-str-999.
       aco-str-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per Read-next per ricerca                      *
      *    *-----------------------------------------------------------*
       aco-rnx-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo codice trattato   *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-rnx-200
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-rnx-300.
       aco-rnx-100.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 01 : [dcp]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           go to     aco-rnx-999.
       aco-rnx-200.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 03 : [dpm]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           go to     aco-rnx-999.
       aco-rnx-300.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 04 : [mtv]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           go to     aco-rnx-999.
       aco-rnx-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per Read-key per controllo                     *
      *    *-----------------------------------------------------------*
       aco-rkx-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo codice trattato   *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-rkx-200
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-rkx-300.
       aco-rkx-100.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 01 : [dcp]              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      s-num                to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           go to     aco-rkx-999.
       aco-rkx-200.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 03 : [dpm]              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP    "         to   f-key                  .
           move      s-num                to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           go to     aco-rkx-999.
       aco-rkx-300.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 04 : [mtv]              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP    "         to   f-key                  .
           move      s-num                to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           go to     aco-rkx-999.
       aco-rkx-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per bufferizzazioni                            *
      *    *-----------------------------------------------------------*
       aco-buf-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo codice trattato   *
      *              *-------------------------------------------------*
           if        w-cod-cod-aaq-s00    =    03
                     go to aco-buf-200
           else if   w-cod-cod-aaq-s00    =    04
                     go to aco-buf-300.
       aco-buf-100.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 01 : [dcp]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico del prodotto                *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice sinonimo del prodotto                *
      *                  *---------------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-cod-sin
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione del prodotto                    *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
           go to     aco-buf-999.
       aco-buf-200.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 03 : [dpm]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico della materia prima         *
      *                  *---------------------------------------------*
           move      rf-dpm-num-map       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico della materia prima     *
      *                  *---------------------------------------------*
           move      rf-dpm-alf-map       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice sinonimo della materia prima         *
      *                  *---------------------------------------------*
           move      rf-dpm-syn-map       to   w-buf-cod-sin
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione della materia prima             *
      *                  *---------------------------------------------*
           move      rf-dpm-des-map       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
           go to     aco-buf-999.
       aco-buf-300.
      *              *-------------------------------------------------*
      *              * Se tipo codice trattato 04 : [mtv]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico materiale vario             *
      *                  *---------------------------------------------*
           move      rf-mtv-num-mtv       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico materiale vario         *
      *                  *---------------------------------------------*
           move      rf-mtv-alf-mtv       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice sinonimo materiale vario             *
      *                  *---------------------------------------------*
           move      rf-mtv-syn-mtv       to   w-buf-cod-sin
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione materiale vario                 *
      *                  *---------------------------------------------*
           move      rf-mtv-des-mtv       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
           go to     aco-buf-999.
       aco-buf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina video, per box locale di selezio-  *
      *    * ne, contenente il record indirizzato da (w-buf-inx-buf)   *
      *    *-----------------------------------------------------------*
       vpg-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina trattata           *
      *              *-------------------------------------------------*
           move      w-buf-inx-buf        to   w-buf-pag-att          .
           add       9                    to   w-buf-pag-att          .
           divide    10                   into w-buf-pag-att          .
      *              *-------------------------------------------------*
      *              * Determinazione indice primo elemento della pa-  *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-att        to   w-buf-pag-ipe          .
           multiply  10                   by   w-buf-pag-ipe          .
           subtract  9                    from w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Determinazione indice ultimo elemento della pa- *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-ipe        to   w-buf-pag-iue          .
           add       9                    to   w-buf-pag-iue          .
           if        w-buf-pag-iue        >    w-buf-ctr-rec
                     move  w-buf-ctr-rec  to   w-buf-pag-iue          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per numero linea a vi-  *
      *              * deo relativa al primo elemento da visualizzare  *
      *              *-------------------------------------------------*
           move      08                   to   w-buf-i01-lnv          .
       vpg-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
       vpg-125.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-150.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-175.
      *                  *---------------------------------------------*
      *                  * Codice prodotto sinonimo                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal esplicativo                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      63                   to   v-pos                  .
      *
           if        w-buf-cod-sin
                    (w-buf-pag-ipe)       =    spaces
                     move  spaces         to   v-alf
           else      move  "="            to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sinonimo                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      64                   to   v-pos                  .
           move      w-buf-cod-sin
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-300.
      *              *-------------------------------------------------*
      *              * Incremento indice primo elemento della pagina   *
      *              * trattata                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Incremento indice per numero linea a video re-  *
      *              * relativa al primo elemento da visualizzare      *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-i01-lnv          .
      *              *-------------------------------------------------*
      *              * Se non si e' oltre l'ultimo elemento da tratta- *
      *              * re nella pagina si ricicla a visualizzare       *
      *              *-------------------------------------------------*
           if        w-buf-pag-ipe        not  > w-buf-pag-iue
                     go to vpg-100.
      *              *-------------------------------------------------*
      *              * Se si e' a pagina 1 : si esce                   *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        =    1
                     go to vpg-600.
      *              *-------------------------------------------------*
      *              * Se non si e' all'ultima pagina 1 : si esce      *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        <    w-buf-tot-pag
                     go to vpg-600.
       vpg-400.
      *              *-------------------------------------------------*
      *              * Eventuali linee residue all'interno del box di  *
      *              * selezione a Spaces                              *
      *              *-------------------------------------------------*
           if        w-buf-i01-lnv        >    17
                     go to vpg-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
           go to     vpg-400.
       vpg-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero pagina su pagine totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      w-buf-pag-att        to   w-buf-lit-att          .
           move      w-buf-tot-pag        to   w-buf-lit-tot          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-buf-lit-pag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
       vpg-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico, contenuto in w-edt-num-cod,  *
      *    * ritornando il valore editato in un formato di 14 carat-   *
      *    * teri in w-edt-num-edt                                     *
      *    *-----------------------------------------------------------*
       edn-000.
      *              *-------------------------------------------------*
      *              * Se valore zero : uscita con Spaces              *
      *              *-------------------------------------------------*
           if        w-edt-num-cod        =    zero
                     move  spaces         to   w-edt-num-edt
                     go to edn-999.
      *              *-------------------------------------------------*
      *              * Esecuzione editing                              *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-edt-num-cod        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Valore editato in area di ritorno               *
      *              *-------------------------------------------------*
           move      spaces               to   w-edt-num-edt          .
           string    "("        delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-edt-num-edt          .
       edn-999.
           exit.

      *    *===========================================================*
      *    * Test se blanks embedded in w-ble-str                      *
      *    *-----------------------------------------------------------*
       ble-000.
           move      spaces               to   w-ble-flg              .
           if        w-ble-str            =    spaces
                     go to ble-999.
           if        w-ble-chr (1)        =    spaces
                     move  "#"            to   w-ble-flg
                     go to ble-999.
           move      1                    to   w-ble-ctr              .
       ble-100.
           add       1                    to   w-ble-ctr              .
           if        w-ble-ctr            >    w-ble-max
                     go to ble-999.
           if        w-ble-chr
                    (w-ble-ctr)           not  = spaces
                     go to ble-100.
       ble-200.
           add       1                    to   w-ble-ctr              .
           if        w-ble-ctr            >    w-ble-max
                     go to ble-999.
           if        w-ble-chr
                    (w-ble-ctr)           =    spaces
                     go to ble-200.
           move      "#"                  to   w-ble-flg              .
       ble-999.
           exit.

      *    *===========================================================*
      *    * Suddivisione ed analisi codice impostato di 14 caratteri  *
      *    *-----------------------------------------------------------*
       sac-000.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per descrizione       *
      *              *-------------------------------------------------*
           if        w-man-x14-vlr        not  = "-"
                     go to sac-100.
           move      "-"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           go to     sac-999.
       sac-100.
      *              *-------------------------------------------------*
      *              * Test se sinonimo                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-tss    tallying w-man-x14-i01
                                      for all  w-man-x14-cha (1)      .
           if        w-man-x14-i01        =    zero
                     go to sac-150.
           move      "S"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           move      02                   to   w-man-x14-i01          .
           unstring  w-man-x14-vlr        into w-man-x14-sin
                                  with pointer w-man-x14-i01          .
           move      w-man-x14-cha (1)    to   w-man-x14-wlc          .
           go to     sac-999.
       sac-150.
      *              *-------------------------------------------------*
      *              * Test se richiesta di codice numerico            *
      *              *-------------------------------------------------*
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-trc    tallying w-man-x14-i01
                                      for all  w-man-x14-cha (1)      .
           if        w-man-x14-i01        =    zero
                     go to sac-200.
           move      "?"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           move      02                   to   w-man-x14-i01          .
           unstring  w-man-x14-vlr        into w-man-x14-sin
                                  with pointer w-man-x14-i01          .
           move      w-man-x14-cha (1)    to   w-man-x14-wlc          .
           go to     sac-999.
       sac-200.
      *              *-------------------------------------------------*
      *              * Test se codice prodotto numerico                *
      *              *-------------------------------------------------*
           if        w-man-x14-cha (1)    not  = "("
                     go to sac-300.
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-vlr    tallying w-man-x14-i01
                                      for all  ")"                    .
           if        w-man-x14-i01        =    1
                     go to sac-210.
       sac-205.
           move      "#"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           go to     sac-999.
       sac-210.
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-vlr    tallying w-man-x14-i01
                     for   characters   before initial ")"            .
           if        w-man-x14-i01        not  > 1
                     go to sac-205.
           move      zero                 to   w-man-x14-i02          .
           inspect   w-man-x14-vlr    tallying w-man-x14-i02
                                          for  all   " "              .
           add       w-man-x14-i01        to   w-man-x14-i02          .
           if        w-man-x14-i02        not  = 13
                     go to sac-205.
       sac-215.
           move      zero                 to   w-man-x14-n14          .
           move      1                    to   w-man-x14-i01          .
       sac-220.
           add       1                    to   w-man-x14-i01          .
           if        w-man-x14-cha
                    (w-man-x14-i01)       =    ")"
                     go to sac-225.
           if        w-man-x14-cha
                    (w-man-x14-i01)       <    "0" or
                     w-man-x14-cha
                    (w-man-x14-i01)       >    "9"
                     go to sac-205.
           multiply  10                   by   w-man-x14-n14          .
           add       w-man-x14-chn
                    (w-man-x14-i01)       to   w-man-x14-n14          .
           go to     sac-220.
       sac-225.
           if        w-man-x14-n14        =    zero
                     go to sac-205.
           if        w-man-x14-n14        >    9999999
                     go to sac-205.
           move      "N"                  to   w-man-x14-flg          .
           move      w-man-x14-n14        to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           go to     sac-999.
       sac-300.
      *              *-------------------------------------------------*
      *              * Test se codice prodotto alfanumerico            *
      *              *-------------------------------------------------*
           if        w-man-x14-cha (1)    not  < "0" and
                     w-man-x14-cha (1)    not  > "9"
                     go to sac-325
           else if   w-man-x14-cha (1)    not  < "A" and
                     w-man-x14-cha (1)    not  > "Z"
                     go to sac-325
           else      go to sac-205.
       sac-325.
           move      "A"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      w-man-x14-vlr        to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
       sac-999.
           exit.

