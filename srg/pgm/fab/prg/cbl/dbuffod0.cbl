       Identification Division.
       Program-Id.                                 dbuffod0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    mov                 *
      *                                   Fase:    bol300b             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 20/09/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione del buffer dei fabbisogni/dispo-  *
      * nibilita' per un prodotto                                      *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * PROMEMORIA: - bisogna rivedere le giacenze per dislocazione    *
      *                                                                *
      *             - implementare la chiamata come 'batch' per "IE"   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * NOTE OPERATIVE                                                 *
      *                                                                *
      * Creazione del buffer dei fabbisogni/disponibilita'             *
      * --------------------------------------------------             *
      *                                                                *
      * Consiste nella scansione e conseguente bufferizzazione di:     *
      *                                                                *
      *  Tipo elementi                                         TR      *
      *  ----------------------------------------------------  --      *
      *  - giacenza alla data richiesta                        00      *
      *  - righe ordini fornitori inevase (entro o.t.)         10      *
      *  - righe ordini fornitori inevase (oltre o.t.)         40      *
      *  - orizzonte temporale (o.t.)                          30      *
      *                                                                *
      * I dati bufferizzati vengono poi ordinati per tipo e data e     *
      * successivamente si procede alla scansione delle righe ordini   *
      * clienti                                                        *
      *                                                                *
      *  Tipo elementi                                         TR      *
      *  ----------------------------------------------------  --      *
      *  - righe ordini clienti inevase (entro o.t.)           20      *
      *  - righe ordini clienti inevase (oltre o.t.)           20      *
      *                                                                *
      * La scansione degli ordini clienti fa si che venga realizzato   *
      * il castelletto della disponibilita'. Il criterio di posiziona- *
      * mento e' il seguente:                                          *
      *                                                                *
      *  - Si ricerca l'elemento di disponibilita' con data consegna   *
      *    prevista superiore alla data consegna richiesta dell'ordine *
      *    cliente. Si parte dal secondo elemento in quanto il primo   *
      *    e' sicuramente quello della giacenza. Per ciascun elemento  *
      *    incontrato si verifica la quantita' libera da poter utiliz- *
      *    zare, passando eventualmente agli elementi successivi se la *
      *    quantita' utilizzabile non e' sufficiente                   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *                                                                *
      *        Input  : d-buf-fod-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-buf-fod-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-buf-fod-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-buf-fod-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione buffer dei fabbisogni/disponibilita'     *
      *                                                                *
      *                                                                *
      *        Input  : d-buf-fod-tip-ope = "DT"                       *
      *                                                                *
      *                 d-buf-fod-dat-ela = Data elaborazione          *
      *                                                                *
      *                 d-buf-fod-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-buf-fod-tip-mag = Tipo codice di magazzino   *
      *                                                                *
      *                 d-buf-fod-num-mag = Codice numerico di magaz-  *
      *                                     zino                       *
      *                                                                *
      *                 d-buf-fod-tip-crg = Tipo correttivo alla gia-  *
      *                                     cenza da considerare :     *
      *                                     - 01 : Nessun correttivo   *
      *                                     - 10 : Scorta minima       *
      *                                                                *
      *                 d-buf-fod-snx-oav = Si/no esclusione ordini da *
      *                                     verificare :               *
      *                                     - 01 : Nessuna esclusione  *
      *                                     - 02 : Esclusione          *
      *                                                                *
      *                                                                *
      *        Output : d-buf-fod-buf-ele = Buffer dei fabbisogni/di-  *
      *                                     sponibilita'               *
      *                                                                *
      *                 d-buf-fod-qta-gia = Quantita' in giacenza      *
      *                                                                *
      *                 d-buf-fod-gia-ian = Giacenza a inizio anno     *
      *                                                                *
      *                 d-buf-fod-pca-ian = Progressivo carichi da i-  *
      *                                     nizio anno                 *
      *                                                                *
      *                 d-buf-fod-psc-ian = Progressivo scarichi da i- *
      *                                     nizio anno                 *
      *                                                                *
      *                 d-buf-fod-qta-crg = Quantita' di correttivo    *
      *                                     alla giacenza              *
      *                                                                *
      *                 d-buf-fod-qta-oaf = Quantita' ordinata a for-  *
      *                                     nitori                     *
      *                                                                *
      *                 d-buf-fod-qta-idc = Quantita' impegnata da     *
      *                                     clienti                    *
      *                                                                *
      *                 d-buf-fod-qta-ics = Quantita' in corso di spe- *
      *                                     dizione                    *
      *                                                                *
      *                 d-buf-fod-qtt-fod = Quantita' totale di fab-   *
      *                                     bisogno/disponibilita'     *
      *                                                                *
      *                 d-buf-fod-fpf-eot = Flag di presenza di fab-   *
      *                                     bisogno entro l'orizzonte  *
      *                                     temporale                  *
      *                                                                *
      *                 d-buf-fod-fpf-aot = Flag di presenza di fab-   *
      *                                     bisogno all'orizzonte tem- *
      *                                     porale                     *
      *                                                                *
      *                 d-buf-fod-fpf-oot = Flag di presenza di fab-   *
      *                                     bisogno oltre l'orizzonte  *
      *                                     temporale                  *
      *                                                                *
      *                 d-buf-fod-dot-teo = Data orizzonte temporale   *
      *                                     teorica                    *
      *                                                                *
      *                 d-buf-fod-dot-eff = Data orizzonte temporale   *
      *                                     effettiva                  *
      *                                                                *
      *                 d-buf-fod-tmp-apv = Tempo di approvigionamento *
      *                                     medio in giorni            *
      *                                                                *
      *                 d-buf-fod-lot-acq = Lotto di acquisto          *
      *                                                                *
      *                 d-buf-fod-sco-min = Scorta minima              *
      *                                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DO" - Determinazione ordinato da dipendenze a Sede            *
      *                                                                *
      *                                                                *
      *        Input  : d-buf-fod-tip-ope = "DO"                       *
      *                                                                *
      *                 d-buf-fod-dat-ela = Data elaborazione          *
      *                                                                *
      *                 d-buf-fod-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-buf-fod-tip-mag = Tipo codice di magazzino   *
      *                                                                *
      *                 d-buf-fod-num-mag = Codice numerico di magaz-  *
      *                                     zino                       *
      *                                                                *
      *                                                                *
      *        Output : d-buf-fod-qta-oaf = Quantita' ordinata a Sede  *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [fbs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rffbs"                          .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da     *
      *    * spedire riga ordine di spedizione                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No funzionamento con Cache-Memory sullo stesso     *
      *        * prodotto                                              *
      *        *-------------------------------------------------------*
           05  w-prs-snx-chm              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no ordine in attesa di verifica                    *
      *        *-------------------------------------------------------*
           05  w-prs-snx-oav.
      *            *---------------------------------------------------*
      *            * Si/No gestione verifica ordine                    *
      *            *                                                   *
      *            * - N : No                                          *
      *            * - S : Si                                          *
      *            * - P : Parziale ___ DA IMPLEMENTARE ___            *
      *            *---------------------------------------------------*
               10  w-prs-snx-oav-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Default per accettazione (se 'S' precedente)      *
      *            *                                                   *
      *            * - D : Da eseguire                                 *
      *            * - E : Eseguita                                    *
      *            *---------------------------------------------------*
               10  w-prs-snx-oav-def      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Date consegna da utilizzare per disponibilita'        *
      *        *-------------------------------------------------------*
           05  w-prs-dcn-dsp.
               10  w-prs-dcn-dsp-orf      pic  x(01)                  .
               10  w-prs-dcn-dsp-orc      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Default tempo di approvigionamento medio in giorni    *
      *        *-------------------------------------------------------*
           05  w-ref-def-tam              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) trailing
                                                     separate
                                                     character
                                                     value zero       .

      *    *===========================================================*
      *    * Work-area per controllo funzionamento con Cache-Memory    *
      *    * per lo stesso prodotto                                    *
      *    *-----------------------------------------------------------*
       01  w-chm.
           05  w-chm-dat-ela              pic  9(07) value zero       .
           05  w-chm-cod-dpz              pic  9(02) value zero       .
           05  w-chm-tip-mag              pic  9(02) value zero       .
           05  w-chm-num-mag              pic  9(07) value zero       .
           05  w-chm-tip-crg              pic  9(02) value zero       .
           05  w-chm-snx-oav              pic  9(02) value zero       .
           05  w-chm-buf-fod.
               10  filler occurs 65000    pic  x(01)                  .

      *    *===========================================================*
      *    * Work per routine det-dat-ort-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-det-dat-ort.
      *        *-------------------------------------------------------*
      *        * Numero giorni utilizzati                       [Work] *
      *        *-------------------------------------------------------*
           05  w-det-dat-ort-ngu          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Tempo approvigionamento medio in giorni      [Output] *
      *        *-------------------------------------------------------*
           05  w-det-dat-ort-tam          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data orizzonte temporale                     [Output] *
      *        *-------------------------------------------------------*
           05  w-det-dat-ort-dat          pic  9(07)                  .
           05  w-det-dat-ort-dat-r redefines
               w-det-dat-ort-dat.
               10  w-det-dat-ort-dta      pic  9(03)                  .
               10  w-det-dat-ort-dtm      pic  9(02)                  .
               10  w-det-dat-ort-dtg      pic  9(02)                  .
           05  w-det-dat-ort-dwk          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Lotto di acquisto                            [Output] *
      *        *-------------------------------------------------------*
           05  w-det-dat-ort-lda          pic  9(06)v9(03)            .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *    *===========================================================*
      *    * Work per routine crz-buf-fod-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-crz-buf-fod.
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione tipo riga               [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-wtr.
               10  w-crz-buf-fod-wtp      pic  x(01)                  .
               10  w-crz-buf-fod-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 1                          [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-c01          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 2                          [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-c02          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per quantita' da essegnare              [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-wqa          pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Comodo per quantita' utilizzabile              [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-wqu          pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Comodo per quantita' da ricevere               [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-dri          pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Comodo per quantita' di debug                  [Work] *
      *        *-------------------------------------------------------*
           05  w-crz-buf-fod-wqd          pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work per routine det-qta-fod-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-det-qta-fod.
      *        *-------------------------------------------------------*
      *        * Comodo per saldo quantita' progressivo         [Work] *
      *        *-------------------------------------------------------*
           05  w-det-qta-fod-wsp          pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 1                          [Work] *
      *        *-------------------------------------------------------*
           05  w-det-qta-fod-c01          pic  9(05)                  .

      *    *===========================================================*
      *    * Work per routine ord-buf-fod-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-ord-buf-fod.
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento buffer   *
      *        * dei fabbisogni/disponibilita'                  [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-buf-fod-wsk          pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 1                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-buf-fod-c01          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 2                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-buf-fod-c02          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 3                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-buf-fod-c03          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 4                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-buf-fod-c04          pic  9(05)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione buffer dei fab-  *
      *    * bisogni/disponibilita' per un prodotto                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/fab/prg/cpy/dbuffod0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-buf-fod              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-buf-fod-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-buf-fod-tip-ope    =    "DT"
                     perform det-buf-fod-000
                                          thru det-buf-fod-999
      *                  *---------------------------------------------*
      *                  * Ordinato a Sede                             *
      *                  *---------------------------------------------*
           else if   d-buf-fod-tip-ope    =    "DO"
                     perform det-ord-dpz-000
                                          thru det-ord-dpz-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-buf-fod-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-buf-fod-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-buf-fod-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No funzionamento con Cache-Memory sullo  *
      *                  * stesso prodotto                             *
      *                  *---------------------------------------------*
           perform   prs-snx-chm-000      thru prs-snx-chm-999        .
      *                  *---------------------------------------------*
      *                  * Si/no ordine in attesa di verifica          *
      *                  *---------------------------------------------*
           perform   prs-snx-oav-000      thru prs-snx-oav-999        .
      *                  *---------------------------------------------*
      *                  * Data consegna da utilizzare per disponib.   *
      *                  *---------------------------------------------*
           perform   prs-dcn-dsp-000      thru prs-dcn-dsp-999        .
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default tempo di approvigionamento medio in *
      *                  * giorni                                      *
      *                  *---------------------------------------------*
           perform   ref-def-tam-000      thru ref-def-tam-999        .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [aaq]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [osr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * [fbs]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine cliente                      *
      *              *-------------------------------------------------*
           perform   det-qev-roc-opn-000  thru det-qev-roc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine fornitore                    *
      *              *-------------------------------------------------*
           perform   det-qev-rof-opn-000  thru det-qev-rof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da      *
      *              * spedire riga ordine di spedizione               *
      *              *-------------------------------------------------*
           perform   det-qds-ros-opn-000  thru det-qds-ros-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magazz.  *
      *              *-------------------------------------------------*
           perform   det-sld-mag-opn-000  thru det-sld-mag-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No funzionamento con Cache *
      *    *                             Memory sullo stesso prodotto  *
      *    *-----------------------------------------------------------*
       prs-snx-chm-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fab/dbuffod[snx-chm]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-chm
           else      move  spaces         to   w-prs-snx-chm          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-chm        not  = "S" and
                     w-prs-snx-chm        not  = "N"
                     move  "N"            to   w-prs-snx-chm          .
       prs-snx-chm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no ordine in attesa di     *
      *    *                             verifica                      *
      *    *-----------------------------------------------------------*
       prs-snx-oav-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orc/mov/orc300[snx-oav]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-oav
           else      move  spaces         to   w-prs-snx-oav          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-oav-snx    not   = "S"
                     move  "N"            to   w-prs-snx-oav-snx      .
           if        w-prs-snx-oav-def    not   = "E"
                     move  "D"            to   w-prs-snx-oav-def      .
           if        w-prs-snx-oav-snx    not   = "S"
                     move  "D"            to   w-prs-snx-oav-def      .
       prs-snx-oav-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Data consegna da utilizzare   *
      *    *                             per disponibilita'            *
      *    *-----------------------------------------------------------*
       prs-dcn-dsp-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fab/dbuffod[dcn-dsp]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dcn-dsp
           else      move  spaces         to   w-prs-dcn-dsp          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazioni               *
      *              *-------------------------------------------------*
           if        w-prs-dcn-dsp-orf    not   = "R" and
                     w-prs-dcn-dsp-orf    not   = "C"
                     move  "P"            to   w-prs-dcn-dsp-orf      .
           if        w-prs-dcn-dsp-orc    not   = "R" and
                     w-prs-dcn-dsp-orc    not   = "C"
                     move  "P"            to   w-prs-dcn-dsp-orc      .
       prs-dcn-dsp-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza : Default tempo di approvigionamento    *
      *    *                     medio in giorni                       *
      *    *-----------------------------------------------------------*
       ref-def-tam-000.
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/dcf[def-tam]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-ref-def-tam
           else      move  zero           to   w-ref-def-tam          .
      *              *-------------------------------------------------*
      *              * Normalizzazione referenza                       *
      *              *-------------------------------------------------*
           if        w-ref-def-tam        not  < 00 and
                     w-ref-def-tam        not  > 99
                     go to ref-def-tam-999.
           move      00                   to   w-ref-def-tam          .
       ref-def-tam-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [aaq]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [osr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * [fbs]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
       rou-cls-fls-100.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine cliente                      *
      *              *-------------------------------------------------*
           perform   det-qev-roc-cls-000  thru det-qev-roc-cls-999    .
       rou-cls-fls-200.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine fornitore                    *
      *              *-------------------------------------------------*
           perform   det-qev-rof-cls-000  thru det-qev-rof-cls-999    .
       rou-cls-fls-300.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da     *
      *              * spedire riga ordine di spedizione               *
      *              *-------------------------------------------------*
           perform   det-qds-ros-cls-000  thru det-qds-ros-cls-999    .
       rou-cls-fls-400.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione saldo di magazz. *
      *              *-------------------------------------------------*
           perform   det-sld-mag-cls-000  thru det-sld-mag-cls-999    .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tst-cnc-mod-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   d-buf-fod-exi-sts
           else      move  "#"            to   d-buf-fod-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer fabbisogni/disponibilita' per un    *
      *    * prodotto                                                  *
      *    *-----------------------------------------------------------*
       det-buf-fod-000.
      *              *-------------------------------------------------*
      *              * Definizione numero elementi massimo             *
      *              *-------------------------------------------------*
           move      999                  to   d-buf-fod-max-ele      .
       det-buf-fod-010.
      *              *-------------------------------------------------*
      *              * Test preliminare se codice in input non zero    *
      *              *-------------------------------------------------*
           if        d-buf-fod-num-mag    =    zero
                     go to det-buf-fod-999.
       det-buf-fod-030.
      *              *-------------------------------------------------*
      *              * Se funzionamento con Cache-Memory               *
      *              *-------------------------------------------------*
           if        w-prs-snx-chm        not  = "S"
                     go to det-buf-fod-050.
      *                  *---------------------------------------------*
      *                  * Test se richiesta uguale alla precedente    *
      *                  *---------------------------------------------*
           if        d-buf-fod-dat-ela    not  = w-chm-dat-ela or
                     d-buf-fod-cod-dpz    not  = w-chm-cod-dpz or
                     d-buf-fod-tip-mag    not  = w-chm-tip-mag or
                     d-buf-fod-num-mag    not  = w-chm-num-mag or
                     d-buf-fod-tip-crg    not  = w-chm-tip-crg or
                     d-buf-fod-snx-oav    not  = w-chm-snx-oav
                     go to det-buf-fod-050.
      *                  *---------------------------------------------*
      *                  * Uscita con buffer inalterato                *
      *                  *---------------------------------------------*
           move      w-chm-buf-fod        to   d-buf-fod              .
           go to     det-buf-fod-999.
       det-buf-fod-050.
      *              *-------------------------------------------------*
      *              * Determinazione data orizzonte temporale per il  *
      *              * prodotto                                        *
      *              *-------------------------------------------------*
           perform   det-dat-ort-000      thru det-dat-ort-999        .
           move      w-det-dat-ort-dat    to   d-buf-fod-dot-teo      .
           move      w-det-dat-ort-tam    to   d-buf-fod-tmp-apv      .
           move      w-det-dat-ort-lda    to   d-buf-fod-lot-acq      .
       det-buf-fod-100.
      *              *-------------------------------------------------*
      *              * Creazione buffer fabbisogni/disponibilita'      *
      *              *-------------------------------------------------*
           perform   crz-buf-fod-000      thru crz-buf-fod-999        .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' di fabbisogno/disponi- *
      *              * bilita'                                         *
      *              *-------------------------------------------------*
           perform   det-qta-fod-000      thru det-qta-fod-999        .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' totale di fabbisogno/  *
      *              * disponibilita' per il prodotto in esame         *
      *              *-------------------------------------------------*
           move      d-buf-fod-qta-gia    to   d-buf-fod-qtt-fod      .
           subtract  d-buf-fod-qta-crg    from d-buf-fod-qtt-fod      .
           subtract  d-buf-fod-qta-ics    from d-buf-fod-qtt-fod      .
           add       d-buf-fod-qta-oaf    to   d-buf-fod-qtt-fod      .
           subtract  d-buf-fod-qta-idc    from d-buf-fod-qtt-fod      .
      *              *-------------------------------------------------*
      *              * Aggiornamento area di controllo per Cache-Memo- *
      *              * ry                                              *
      *              *-------------------------------------------------*
           move      d-buf-fod-dat-ela    to   w-chm-dat-ela          .
           move      d-buf-fod-cod-dpz    to   w-chm-cod-dpz          .
           move      d-buf-fod-tip-mag    to   w-chm-tip-mag          .
           move      d-buf-fod-num-mag    to   w-chm-num-mag          .
           move      d-buf-fod-tip-crg    to   w-chm-tip-crg          .
           move      d-buf-fod-snx-oav    to   w-chm-snx-oav          .
           move      d-buf-fod            to   w-chm-buf-fod          .
       det-buf-fod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione data orizzonte temporale per un prodotto   *
      *    *-----------------------------------------------------------*
       det-dat-ort-000.
      *              *-------------------------------------------------*
      *              * Data orizzonte temporale pari a data di base    *
      *              *-------------------------------------------------*
           move      d-buf-fod-dat-ela    to   w-det-dat-ort-dat      .
      *              *-------------------------------------------------*
      *              * Lettura record [aaq] per il prodotto in esame   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      d-buf-fod-tip-mag    to   rf-aaq-tip-mag         .
           move      d-buf-fod-num-mag    to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-aaq-tmp-apv         .
       det-dat-ort-100.
      *              *-------------------------------------------------*
      *              * Bufferizzazione tempo di approvigionamento me-  *
      *              * dio in giorni                                   *
      *              *-------------------------------------------------*
           move      rf-aaq-tmp-apv       to   w-det-dat-ort-tam      .
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento tempo di approvigiona-  *
      *              * mento medio in giorni con valore di default     *
      *              *-------------------------------------------------*
           if        w-ref-def-tam        not  = zero and
                     w-det-dat-ort-tam    =    zero
                     move  w-ref-def-tam  to   w-det-dat-ort-tam      .
      *              *-------------------------------------------------*
      *              * Determinazione data maggiorata del numero gior- *
      *              * ni di approvigionamento                         *
      *              *-------------------------------------------------*
           move      w-det-dat-ort-dat    to   w-det-dat-nrg-dtb      .
           move      w-det-dat-ort-tam    to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   w-det-dat-ort-dat      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione altri valori dedotti da record  *
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      rf-aaq-lot-acq       to   w-det-dat-ort-lda      .
       det-dat-ort-999.
           exit.

      *    *===========================================================*
      *    * Creazione buffer dei fabbisogni/disponibilita'            *
      *    *-----------------------------------------------------------*
       crz-buf-fod-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero elementi                *
      *              *-------------------------------------------------*
           move      zero                 to   d-buf-fod-num-ele      .
       crz-buf-fod-100.
      *              *-------------------------------------------------*
      *              * Creazione elemento per giacenza attuale         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione quantita' in giacenza        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione link-area                  *
      *                      *-----------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0000                 to   d-sld-mag-tip-sld      .
           move      d-buf-fod-dat-ela    to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      d-buf-fod-cod-dpz    to   d-sld-mag-cod-dpz      .
           move      d-buf-fod-tip-mag    to   d-sld-mag-tip-mag      .
           move      d-buf-fod-num-mag    to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "T"                  to   d-sld-mag-uot-dsl      .
           move      spaces               to   d-sld-mag-cod-dsl      .
      *                      *-----------------------------------------*
      *                      * Richiamo del sottoprogramma             *
      *                      *-----------------------------------------*
           perform   det-sld-mag-cll-000  thru det-sld-mag-cll-999    .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori relativi alla gia-   *
      *                  * cenza                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Giacenza alla data richiesta            *
      *                      *-----------------------------------------*
           move      d-sld-mag-sld-mag    to   d-buf-fod-qta-gia      .
      *                      *-----------------------------------------*
      *                      * Giacenza a inizio anno                  *
      *                      *-----------------------------------------*
           move      d-sld-mag-sub-s01    to   d-buf-fod-gia-ian      .
      *                      *-----------------------------------------*
      *                      * Progressivo carichi da inizio anno      *
      *                      *-----------------------------------------*
           move      d-sld-mag-sub-s02    to   d-buf-fod-pca-ian      .
      *                      *-----------------------------------------*
      *                      * Progressivo scarichi da inizio anno     *
      *                      *-----------------------------------------*
           move      d-sld-mag-sub-s03    to   d-buf-fod-psc-ian      .
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi                  *
      *                  *---------------------------------------------*
           move      1                    to   d-buf-fod-num-ele      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiave di ordinamento                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data consegna prevista              *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-dcn-prv
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Tipo record per ordinamento         *
      *                          *-------------------------------------*
           move      00                   to   d-buf-fod-tip-rec
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Data documento                      *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-dat-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero documento                    *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-num-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero progressivo riga ordine      *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-num-prg
                                              (d-buf-fod-num-ele)     .
      *                      *-----------------------------------------*
      *                      * Dati del buffer                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data consegna richiesta             *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-dcn-ric
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di impegno/copertura      *
      *                          *-------------------------------------*
           move      d-sld-mag-sld-mag    to   d-buf-fod-qta-ioc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di fabb./disponib.        *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-qta-fod
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Codice archivio                     *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-cod-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Dipendenza archivio                 *
      *                          *-------------------------------------*
           move      spaces               to   d-buf-fod-dpz-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Formato di stampa data consegna     *
      *                          * richiesta in ordini fornitori       *
      *                          *-------------------------------------*
           move      spaces               to   d-buf-fod-fds-dcr
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Flag di conferma riga ordine        *
      *                          *-------------------------------------*
           move      spaces               to   d-buf-fod-flg-cnf
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero protocollo ordine            *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-num-prt
                                              (d-buf-fod-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' fabbisogno/disponi- *
      *                  * bilita' degli elementi di disponibilita'    *
      *                  *---------------------------------------------*
           add       d-buf-fod-qta-ioc
                    (d-buf-fod-num-ele)   to   d-buf-fod-qta-fod
                                              (d-buf-fod-num-ele)     .
       crz-buf-fod-200.
      *              *-------------------------------------------------*
      *              * Creazione elementi da righe ordini fornitori    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione quantita' ordinata a for-  *
      *                  * nitori                                      *
      *                  *---------------------------------------------*
           move      zero                 to   d-buf-fod-qta-oaf      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione data orizzonte temporale   *
      *                  * effettiva                                   *
      *                  *---------------------------------------------*
           move      d-buf-fod-dot-teo    to   d-buf-fod-dot-eff      .
      *                  *---------------------------------------------*
      *                  * Start su file [ofr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      d-buf-fod-cod-dpz    to   rf-ofr-cod-dpz         .
           move      spaces               to   rf-ofr-flg-rch         .
           move      d-buf-fod-tip-mag    to   rf-ofr-tip-mag         .
           move      d-buf-fod-num-mag    to   rf-ofr-num-mag         .
           move      zero                 to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to crz-buf-fod-260.
       crz-buf-fod-220.
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
______*    move      "IE"                 to   s-ope                  .
______*    call      "swd/mod/prg/obj/msegrt"
______*                                  using s                      .
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [ofr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At end'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to crz-buf-fod-260.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ofr-cod-dpz       not  = d-buf-fod-cod-dpz  or
                     rf-ofr-flg-rch       not  = spaces             or
                     rf-ofr-tip-mag       not  = d-buf-fod-tip-mag  or
                     rf-ofr-num-mag       not  = d-buf-fod-num-mag
                     go to crz-buf-fod-260.
      *                  *---------------------------------------------*
      *                  * Selezione sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-ofr-dat-doc       >    d-buf-fod-dat-ela
                     go to crz-buf-fod-220.
      *                      *-----------------------------------------*
      *                      * Test su tipo ordine                     *
      *                      *-----------------------------------------*
           if        rf-ofr-tip-ord       =    "R"
                     go to crz-buf-fod-220.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo riga                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo riga in comodo ridefinito      *
      *                          *-------------------------------------*
           move      rf-ofr-tip-rig       to   w-crz-buf-fod-wtr      .
      *                          *-------------------------------------*
      *                          * Test su tipo funzionamento          *
      *                          *-------------------------------------*
           if        w-crz-buf-fod-wtf    not  = spaces
                     go to crz-buf-fod-220.
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' da evadere ri- *
      *                      * ga ordine fornitore                     *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           perform   det-qev-rof-cll-000  thru det-qev-rof-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su quantita' ordinata              *
      *                      *-----------------------------------------*
           if        d-qev-rof-qta-ord    not  > zero
                     go to crz-buf-fod-220.
      *                      *-----------------------------------------*
      *                      * Se quantita' da ricevere a zero : ri-   *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        d-qev-rof-qta-dri    =    zero
                     go to crz-buf-fod-220.
       crz-buf-fod-240.
      *                  *---------------------------------------------*
      *                  * Se data consegna del fornitore superiore    *
      *                  * alla data orizzonte temporale, aggiustamen- *
      *                  * to data orizzonte temporale effettiva       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In base alle personalizzazioni si sta-  *
      *                      * bilisce che data utilizzare             *
      *                      *-----------------------------------------*
           if        w-prs-dcn-dsp-orf    =    "R"
                     move  rf-ofr-dcn-ric to   w-det-dat-ort-dwk
           else if   w-prs-dcn-dsp-orf    =    "C"
                     move  rf-ofr-dcn-cnf to   w-det-dat-ort-dwk
           else      move  rf-ofr-dcn-prv to   w-det-dat-ort-dwk      .
      *                      *-----------------------------------------*
      *                      * Eventuale aggiornamento orizzonte       *
      *                      * temporale                               *
      *                      *-----------------------------------------*
           if        w-det-dat-ort-dwk    >    d-buf-fod-dot-eff and
                    (rf-ofr-flg-cnf       =    "S" or
                     rf-ofr-flg-cnf       =    "Z"  )
                     move  w-det-dat-ort-dwk
                                          to   d-buf-fod-dot-eff      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' ordinata a fornito- *
      *                  * ri                                          *
      *                  *---------------------------------------------*
           if        d-qev-rof-snx-tum    =    "S"
                     move  d-qev-rof-fda-dri
                                          to   w-crz-buf-fod-dri
           else      move  d-qev-rof-qta-dri
                                          to   w-crz-buf-fod-dri      .
           add       w-crz-buf-fod-dri    to   d-buf-fod-qta-oaf      .
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi nel buffer, se   *
      *                  * raggiunto il limite, oltre                  *
      *                  *---------------------------------------------*
           if        d-buf-fod-num-ele    <    d-buf-fod-max-ele
                     add   1              to   d-buf-fod-num-ele
           else      go to crz-buf-fod-260.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiave di ordinamento                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * In base alle personalizzazioni si   *
      *                          * stabilisce che data utilizzare      *
      *                          *-------------------------------------*
           if        w-prs-dcn-dsp-orf    =    "R"
                     move  rf-ofr-dcn-ric to   w-det-dat-ort-dwk
           else if   w-prs-dcn-dsp-orf    =    "C"
                     move  rf-ofr-dcn-cnf to   w-det-dat-ort-dwk
           else      move  rf-ofr-dcn-prv to   w-det-dat-ort-dwk      .
      *                          *-------------------------------------*
      *                          * Data consegna prevista              *
      *                          *-------------------------------------*
           move      w-det-dat-ort-dwk    to   d-buf-fod-dcn-prv
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Tipo record per ordinamento         *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-tip-rec
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Data documento                      *
      *                          *-------------------------------------*
           move      rf-ofr-dat-doc       to   d-buf-fod-dat-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero documento                    *
      *                          *-------------------------------------*
           move      rf-ofr-num-doc       to   d-buf-fod-num-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero progressivo riga ordine      *
      *                          *-------------------------------------*
           move      rf-ofr-num-prg       to   d-buf-fod-num-prg
                                              (d-buf-fod-num-ele)     .
      *                      *-----------------------------------------*
      *                      * Dati del buffer                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data consegna richiesta             *
      *                          *-------------------------------------*
           move      rf-ofr-dcn-ric       to   d-buf-fod-dcn-ric
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di impegno/copertura      *
      *                          *-------------------------------------*
           move      w-crz-buf-fod-dri    to   d-buf-fod-qta-ioc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di fabb./disponib.        *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-qta-fod
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Codice archivio                     *
      *                          *-------------------------------------*
           move      rf-ofr-cod-arc       to   d-buf-fod-cod-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Dipendenza archivio                 *
      *                          *-------------------------------------*
           move      rf-ofr-dpz-arc       to   d-buf-fod-dpz-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Formato di stampa data consegna     *
      *                          * richiesta in ordini fornitori       *
      *                          *-------------------------------------*
           move      rf-ofr-fds-dcr       to   d-buf-fod-fds-dcr
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Flag di conferma riga ordine        *
      *                          *-------------------------------------*
           move      rf-ofr-flg-cnf       to   d-buf-fod-flg-cnf
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero protocollo ordine            *
      *                          *-------------------------------------*
           move      rf-ofr-num-prt       to   d-buf-fod-num-prt
                                              (d-buf-fod-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' fabbisogno/disponi- *
      *                  * bilita' degli elementi di disponibilita'    *
      *                  *---------------------------------------------*
           add       d-buf-fod-qta-ioc
                    (d-buf-fod-num-ele)   to   d-buf-fod-qta-fod
                                              (d-buf-fod-num-ele)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     crz-buf-fod-220.
       crz-buf-fod-260.
      *              *-------------------------------------------------*
      *              * Determinazione tipo record per ordini fornitori *
      *              *-------------------------------------------------*
           move      1                    to   w-crz-buf-fod-c01      .
       crz-buf-fod-280.
           add       1                    to   w-crz-buf-fod-c01      .
           if        w-crz-buf-fod-c01    >    d-buf-fod-num-ele
                     go to crz-buf-fod-300.
           if        d-buf-fod-dcn-prv
                    (w-crz-buf-fod-c01)   >    d-buf-fod-dot-eff
                     move  40             to   d-buf-fod-tip-rec
                                              (w-crz-buf-fod-c01)
           else      move  10             to   d-buf-fod-tip-rec
                                              (w-crz-buf-fod-c01)     .
           go to     crz-buf-fod-280.
       crz-buf-fod-300.
      *              *-------------------------------------------------*
      *              * Creazione elemento per orizzonte temporale      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [fbs]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [fbs]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGDPZ    "         to   f-key                  .
           move      d-buf-fod-tip-mag    to   rf-fbs-tip-mag         .
           move      d-buf-fod-num-mag    to   rf-fbs-num-mag         .
           move      spaces               to   rf-fbs-var-mag         .
           move      d-buf-fod-cod-dpz    to   rf-fbs-cod-dpz         .
           move      "pgm/fab/fls/ioc/obj/ioffbs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fbs                 .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore scorta minima        *
      *                  *---------------------------------------------*
           move      rf-fbs-sco-min (1)   to   d-buf-fod-sco-min      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo correttivo    *
      *                  * alla giacenza da considerare                *
      *                  *---------------------------------------------*
           if        d-buf-fod-tip-crg    =    01
                     go to crz-buf-fod-302
           else if   d-buf-fod-tip-crg    =    10
                     go to crz-buf-fod-304.
       crz-buf-fod-302.
      *                  *---------------------------------------------*
      *                  * Tipo correttivo alla giacenza : Nessuno     *
      *                  *---------------------------------------------*
           move      zero                 to   d-buf-fod-qta-crg      .
           go to     crz-buf-fod-320.
       crz-buf-fod-304.
      *                  *---------------------------------------------*
      *                  * Tipo correttivo alla giacenza : Scorta Mi-  *
      *                  * nima                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valore                  *
      *                      *-----------------------------------------*
           move      d-buf-fod-sco-min    to   d-buf-fod-qta-crg      .
           go to     crz-buf-fod-320.
       crz-buf-fod-320.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi nel buffer, se   *
      *                  * raggiunto il limite, oltre                  *
      *                  *---------------------------------------------*
           if        d-buf-fod-num-ele    <    d-buf-fod-max-ele
                     add   1              to   d-buf-fod-num-ele
           else      go to crz-buf-fod-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiave di ordinamento                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data consegna prevista              *
      *                          *-------------------------------------*
           move      d-buf-fod-dot-eff    to   d-buf-fod-dcn-prv
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Tipo record per ordinamento         *
      *                          *-------------------------------------*
           move      30                   to   d-buf-fod-tip-rec
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Data documento                      *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-dat-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero documento                    *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-num-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero progressivo riga ordine      *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-num-prg
                                              (d-buf-fod-num-ele)     .
      *                      *-----------------------------------------*
      *                      * Dati del buffer                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data consegna richiesta             *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-dcn-ric
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di impegno/copertura      *
      *                          *-------------------------------------*
           move      d-buf-fod-qta-crg    to   d-buf-fod-qta-ioc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di fabb./disponib.        *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-qta-fod
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Codice archivio                     *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-cod-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Dipendenza archivio                 *
      *                          *-------------------------------------*
           move      spaces               to   d-buf-fod-dpz-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Formato di stampa data consegna     *
      *                          * richiesta in ordini fornitori       *
      *                          *-------------------------------------*
           move      spaces               to   d-buf-fod-fds-dcr
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Flag di conferma riga ordine        *
      *                          *-------------------------------------*
           move      spaces               to   d-buf-fod-flg-cnf
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero protocollo ordine            *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-num-prt
                                              (d-buf-fod-num-ele)     .
       crz-buf-fod-400.
      *              *-------------------------------------------------*
      *              * Ordinamento buffer fabbisogni/disponibilita'    *
      *              *-------------------------------------------------*
           perform   ord-buf-fod-000      thru ord-buf-fod-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero elementi attuale conte-   *
      *              * nuti nel buffer                                 *
      *              *-------------------------------------------------*
           move      d-buf-fod-num-ele    to   w-crz-buf-fod-c01      .
       crz-buf-fod-500.
      *              *-------------------------------------------------*
      *              * Creazione elementi da righe ordini clienti      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione quantita' impegnata da     *
      *                  * clienti                                     *
      *                  *---------------------------------------------*
           move      zero                 to   d-buf-fod-qta-idc      .
      *                  *---------------------------------------------*
      *                  * Start su file [ocr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      d-buf-fod-cod-dpz    to   rf-ocr-cod-dpz         .
           move      spaces               to   rf-ocr-flg-rch         .
           move      d-buf-fod-tip-mag    to   rf-ocr-tip-mag         .
           move      d-buf-fod-num-mag    to   rf-ocr-num-pro         .
           move      zero                 to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to crz-buf-fod-600.
       crz-buf-fod-520.
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
______*    move      "IE"                 to   s-ope                  .
______*    call      "swd/mod/prg/obj/msegrt"
______*                                  using s                      .
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [ocr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to crz-buf-fod-600.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ocr-cod-dpz       not  = d-buf-fod-cod-dpz or
                     rf-ocr-flg-rch       not  = spaces            or
                     rf-ocr-tip-mag       not  = d-buf-fod-tip-mag or
                     rf-ocr-num-pro       not  = d-buf-fod-num-mag
                     go to crz-buf-fod-600.
      *                  *---------------------------------------------*
      *                  * Selezione sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo ordine                     *
      *                      *                                         *
      *                      * Non si considerano gli ordini pro-forma *
      *                      *-----------------------------------------*
           if        rf-ocr-tip-ord       =    "P"
                     go to crz-buf-fod-520.
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-ocr-dat-doc       >    d-buf-fod-dat-ela
                     go to crz-buf-fod-520.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo riga                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo riga in comodo ridefinito      *
      *                          *-------------------------------------*
           move      rf-ocr-tip-rig       to   w-crz-buf-fod-wtr      .
      *                          *-------------------------------------*
      *                          * Test su tipo funzionamento          *
      *                          *-------------------------------------*
           if        w-crz-buf-fod-wtf    not  = spaces
                     go to crz-buf-fod-520.
      *                          *-------------------------------------*
      *                          * Test su flag di ordine da verifica- *
      *                          * re                                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su personalizzazione       *
      *                              *---------------------------------*
           if        w-prs-snx-oav-snx    not = "S"
                     go to crz-buf-fod-530.
      *                              *---------------------------------*
      *                              * Test su parametro in input      *
      *                              *---------------------------------*
           if        d-buf-fod-snx-oav    not = 02
                     go to crz-buf-fod-530.
      *                              *---------------------------------*
      *                              * Test su flag                    *
      *                              *---------------------------------*
           if        rf-ocr-flg-nbx (1)   not = spaces
                     go to crz-buf-fod-520.
       crz-buf-fod-530.
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' da evadere ri- *
      *                      * ga ordine cliente                       *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su quantita' ordinata              *
      *                      *-----------------------------------------*
           if        d-qev-roc-qta-ord    not  > zero
                     go to crz-buf-fod-520.
      *                      *-----------------------------------------*
      *                      * Test su quantita' da evadere            *
      *                      *-----------------------------------------*
           if        d-qev-roc-qta-dev    =    zero
                     go to crz-buf-fod-520.
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' impegnata da clien- *
      *                  * ti                                          *
      *                  *---------------------------------------------*
           add       d-qev-roc-qta-dev    to   d-buf-fod-qta-idc      .
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi nel buffer, se   *
      *                  * raggiunto il limite, oltre                  *
      *                  *---------------------------------------------*
           if        d-buf-fod-num-ele    <    d-buf-fod-max-ele
                     add   1              to   d-buf-fod-num-ele
           else      go to crz-buf-fod-600.
       crz-buf-fod-535.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiave di ordinamento                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * In base alle personalizzazioni si   *
      *                          * stabilisce che data utilizzare      *
      *                          *-------------------------------------*
           if        w-prs-dcn-dsp-orc    =    "R"
                     move  rf-ocr-dcn-ric to   w-det-dat-ort-dwk
           else if   w-prs-dcn-dsp-orc    =    "C"
                     move  rf-ocr-dcn-cnf to   w-det-dat-ort-dwk
           else      move  rf-ocr-dcn-prv to   w-det-dat-ort-dwk      .
      *
           move      w-det-dat-ort-dwk    to   d-buf-fod-dcn-prv
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Tipo record per ordinamento         *
      *                          *-------------------------------------*
           if        w-det-dat-ort-dwk    >    d-buf-fod-dot-eff
                     move  50             to   d-buf-fod-tip-rec
                                              (d-buf-fod-num-ele)
           else      move  20             to   d-buf-fod-tip-rec
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Data documento                      *
      *                          *-------------------------------------*
           move      rf-ocr-dat-doc       to   d-buf-fod-dat-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero documento                    *
      *                          *-------------------------------------*
           move      rf-ocr-num-doc       to   d-buf-fod-num-doc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero progressivo riga ordine      *
      *                          *-------------------------------------*
           move      rf-ocr-num-prg       to   d-buf-fod-num-prg
                                              (d-buf-fod-num-ele)     .
      *                      *-----------------------------------------*
      *                      * Dati del buffer                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Data consegna richiesta             *
      *                          *-------------------------------------*
           move      rf-ocr-dcn-ric       to   d-buf-fod-dcn-ric
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di impegno/copertura      *
      *                          *-------------------------------------*
           move      d-qev-roc-qta-dev    to   d-buf-fod-qta-ioc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Quantita' di fabb./disponib.        *
      *                          *-------------------------------------*
           move      zero                 to   d-buf-fod-qta-fod
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Codice archivio                     *
      *                          *-------------------------------------*
           move      rf-ocr-cod-arc       to   d-buf-fod-cod-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Dipendenza archivio                 *
      *                          *-------------------------------------*
           move      rf-ocr-dpz-arc       to   d-buf-fod-dpz-arc
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Formato di stampa data consegna     *
      *                          * richiesta in ordini fornitori       *
      *                          *                                     *
      *                          * Utilizzata per 'tipo ordine'        *
      *                          *-------------------------------------*
           move      rf-ocr-tip-ord       to   d-buf-fod-fds-dcr
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Flag di conferma riga ordine        *
      *                          *-------------------------------------*
           move      rf-ocr-flg-cnf       to   d-buf-fod-flg-cnf
                                              (d-buf-fod-num-ele)     .
      *                          *-------------------------------------*
      *                          * Numero protocollo ordine            *
      *                          *-------------------------------------*
           move      rf-ocr-num-prt       to   d-buf-fod-num-prt
                                              (d-buf-fod-num-ele)     .
       crz-buf-fod-538.
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' fabbisogno/disponi- *
      *                  * bilita' degli elementi di disponibilita'    *
      *                  * entro l'orizzonte temporale                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo record 50, clienti oltre la     *
      *                      * data orizzonte temporale : oltre        *
      *                      *-----------------------------------------*
           if        d-buf-fod-tip-rec
                    (d-buf-fod-num-ele)   =    50
                     go to crz-buf-fod-580.
      *                      *-----------------------------------------*
      *                      * Inizializzazione quantita' da assegnare *
      *                      *-----------------------------------------*
           move      d-qev-roc-qta-dev    to   w-crz-buf-fod-wqa      .
      *                      *-----------------------------------------*
      *                      * Ricerca dell'elemento di disponibilita' *
      *                      * con data consegna prevista superiore    *
      *                      * alla data consegna richiesta dell'ordi- *
      *                      * ne cliente. Si parte dal secondo ele-   *
      *                      * mento in quanto il primo e' sicuramente *
      *                      * quello della giacenza                   *
      *                      *                                         *
      *                      * Il contatore 'c01' contiene il numero   *
      *                      * attuale di elementi nel buffer          *
      *                      *-----------------------------------------*
           move      1                    to   w-crz-buf-fod-c02      .
       crz-buf-fod-540.
           add       1                    to   w-crz-buf-fod-c02      .
      *                      *-----------------------------------------*
      *                      * Test se il contatore per la scansione   *
      *                      * 'c02' ha superato il numero di elementi *
      *                      * nel buffer                              *
      *                      *-----------------------------------------*
           if        w-crz-buf-fod-c02    >    w-crz-buf-fod-c01
                     go to crz-buf-fod-550.
      *                      *-----------------------------------------*
      *                      * Test se la data consegna prevista del-  *
      *                      * l'elemento in corso di trattamento e'   *
      *                      * inferiore alla data richiesta della     *
      *                      * riga ordine cliente                     *
      *                      *-----------------------------------------*
           if        d-buf-fod-dcn-prv
                    (w-crz-buf-fod-c02)   <    rf-ocr-dcn-ric
                     go to crz-buf-fod-540.
       crz-buf-fod-550.
      *                      *-----------------------------------------*
      *                      * Posizionamento sull'elemento di dispo-  *
      *                      * nibilita' precedente                    *
      *                      *-----------------------------------------*
           subtract  1                    from w-crz-buf-fod-c02      .
      *                      *-----------------------------------------*
      *                      * Se primo elemento : a trattamento primo *
      *                      * elemento                                *
      *                      *-----------------------------------------*
           if        w-crz-buf-fod-c02    =    1
                     go to crz-buf-fod-560.
      *                      *-----------------------------------------*
      *                      * Se saldo dell'elemento a zero, si passa *
      *                      * al precedente                           *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-crz-buf-fod-c02)   =    zero
                     go to crz-buf-fod-550.
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' utilizzabile   *
      *                      * dell'elemento                           *
      *                      *-----------------------------------------*
           if        w-crz-buf-fod-wqa    >    d-buf-fod-qta-fod
                                              (w-crz-buf-fod-c02)
                     move  d-buf-fod-qta-fod
                          (w-crz-buf-fod-c02)
                                          to   w-crz-buf-fod-wqu
           else      move  w-crz-buf-fod-wqa
                                          to   w-crz-buf-fod-wqu      .
      *                      *-----------------------------------------*
      *                      * Aggiornamento elemento                  *
      *                      *-----------------------------------------*
           subtract  w-crz-buf-fod-wqu    from d-buf-fod-qta-fod
                                              (w-crz-buf-fod-c02)     .
      *                      *-----------------------------------------*
      *                      * Aggiornamento quantita' da assegnare    *
      *                      *-----------------------------------------*
           subtract  w-crz-buf-fod-wqu    from w-crz-buf-fod-wqa      .
           if        w-crz-buf-fod-wqa    <    zero
                     move  zero           to   w-crz-buf-fod-wqa      .
      *                      *-----------------------------------------*
      *                      * Se quantita' da assegnare a zero : ol-  *
      *                      * tre                                     *
      *                      *-----------------------------------------*
           if        w-crz-buf-fod-wqa    =    zero
                     go to crz-buf-fod-580.
      *                      *-----------------------------------------*
      *                      * Altrimenti : riciclo su elemento prece- *
      *                      * dente                                   *
      *                      *-----------------------------------------*
           go to     crz-buf-fod-540.
       crz-buf-fod-560.
      *                      *-----------------------------------------*
      *                      * Trattamento primo elemento              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento della quantita' di    *
      *                          * fabbisogno/disponibilita' della     *
      *                          * giacenza                            *
      *                          *-------------------------------------*
           subtract  w-crz-buf-fod-wqa    from d-buf-fod-qta-fod (1)  .
      *                          *-------------------------------------*
      *                          * A riciclo                           *
      *                          *-------------------------------------*
           go to     crz-buf-fod-580.
       crz-buf-fod-580.
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [ocr] *
      *                  *---------------------------------------------*
           go to     crz-buf-fod-520.
       crz-buf-fod-600.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' in corso di spedizione *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione quantita' in corso di spe- *
      *                  * dizione                                     *
      *                  *---------------------------------------------*
           move      zero                 to   d-buf-fod-qta-ics      .
      *                  *---------------------------------------------*
      *                  * Start su file [osr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      d-buf-fod-cod-dpz    to   rf-osr-cod-dpz         .
           move      spaces               to   rf-osr-flg-rch         .
           move      d-buf-fod-tip-mag    to   rf-osr-tip-mag         .
           move      d-buf-fod-num-mag    to   rf-osr-num-pro         .
           move      zero                 to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to crz-buf-fod-700.
       crz-buf-fod-620.
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
______*    move      "IE"                 to   s-ope                  .
______*    call      "swd/mod/prg/obj/msegrt"
______*                                  using s                      .
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [osr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At end'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to crz-buf-fod-700.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-osr-cod-dpz       not  = d-buf-fod-cod-dpz  or
                     rf-osr-flg-rch       not  = spaces             or
                     rf-osr-tip-mag       not  = d-buf-fod-tip-mag  or
                     rf-osr-num-pro       not  = d-buf-fod-num-mag
                     go to crz-buf-fod-700.
      *                  *---------------------------------------------*
      *                  * Selezione sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su data registrazione              *
      *                      *-----------------------------------------*
           if        rf-osr-dat-doc       >    d-buf-fod-dat-ela
                     go to crz-buf-fod-620.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo riga                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo riga in comodo ridefinito      *
      *                          *-------------------------------------*
           move      rf-osr-tip-rig       to   w-crz-buf-fod-wtr      .
      *                          *-------------------------------------*
      *                          * Test su tipo funzionamento          *
      *                          *-------------------------------------*
           if        w-crz-buf-fod-wtf    not  = spaces
                     go to crz-buf-fod-620.
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' da spedire ri- *
      *                      * ga ordine di spedizione                 *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qds-ros-tip-ope      .
           perform   det-qds-ros-cll-000  thru det-qds-ros-cll-999    .
      *                      *-----------------------------------------*
      *                      * Se quantita' da spedire a zero: riciclo *
      *                      *-----------------------------------------*
           if        d-qds-ros-qta-dsp    not  > zero
                     go to crz-buf-fod-620.
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' in corso di spedi-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           add       d-qds-ros-qta-dsp    to   d-buf-fod-qta-ics      .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [osr] *
      *                  *---------------------------------------------*
           go to     crz-buf-fod-620.
       crz-buf-fod-700.
      *              *-------------------------------------------------*
      *              * Aggiornamento del saldo dell'elemento giacenza  *
      *              * con la quantita' totale in corso di spedizione  *
      *              *-------------------------------------------------*
           subtract  d-buf-fod-qta-ics    from d-buf-fod-qta-fod (1)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     crz-buf-fod-999.
       crz-buf-fod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione quantita' di fabbisogno/disponibilita'     *
      *    *-----------------------------------------------------------*
       det-qta-fod-000.
      *              *-------------------------------------------------*
      *              * Ordinamento buffer fabbisogni/disponibilita'    *
      *              *-------------------------------------------------*
           perform   ord-buf-fod-000      thru ord-buf-fod-999        .
       det-qta-fod-050.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di presenza di almeno  *
      *                  * una quantita' in fabbisogno entro l'oriz-   *
      *                  * zonte temporale                             *
      *                  *---------------------------------------------*
           move      spaces               to   d-buf-fod-fpf-eot      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di presenza di quan-   *
      *                  * tita' in fabbisogno alla data dell'orizzon- *
      *                  * te temporale                                *
      *                  *---------------------------------------------*
           move      spaces               to   d-buf-fod-fpf-aot      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di presenza di almeno  *
      *                  * una quantita' in fabbisogno oltre l'oriz-   *
      *                  * zonte temporale                             *
      *                  *---------------------------------------------*
           move      spaces               to   d-buf-fod-fpf-oot      .
       det-qta-fod-075.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione buffer delle quantita' di    *
      *              * fabbisogno/disponibilita'                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-qta-fod-c01      .
       det-qta-fod-100.
           add       1                    to   w-det-qta-fod-c01      .
           if        w-det-qta-fod-c01    >    d-buf-fod-num-ele
                     go to det-qta-fod-300.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo record        *
      *                  *---------------------------------------------*
           if        d-buf-fod-tip-rec
                    (w-det-qta-fod-c01)   =    00
                     go to det-qta-fod-120
           else if   d-buf-fod-tip-rec
                    (w-det-qta-fod-c01)   =    10
                     go to det-qta-fod-130
           else if   d-buf-fod-tip-rec
                    (w-det-qta-fod-c01)   =    20
                     go to det-qta-fod-140
           else if   d-buf-fod-tip-rec
                    (w-det-qta-fod-c01)   =    30
                     go to det-qta-fod-150
           else if   d-buf-fod-tip-rec
                    (w-det-qta-fod-c01)   =    40
                     go to det-qta-fod-160
           else if   d-buf-fod-tip-rec
                    (w-det-qta-fod-c01)   =    50
                     go to det-qta-fod-170.
       det-qta-fod-120.
      *                  *---------------------------------------------*
      *                  * Tipo record : 00 - Giacenza                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione saldo progressivo per  *
      *                      * gli elementi successivi                 *
      *                      *-----------------------------------------*
           move      d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   to   w-det-qta-fod-wsp      .
      *                      *-----------------------------------------*
      *                      * Eventuale attivazione flag di fabbi-    *
      *                      * sogno entro l'orizzonte temporale       *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   <    zero
                     move  "#"            to   d-buf-fod-fpf-eot      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-qta-fod-200.
       det-qta-fod-130.
      *                  *---------------------------------------------*
      *                  * Tipo record : 10 - Ordini fornitori entro   *
      *                  * l'orizzonte temporale                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento saldo progressivo con     *
      *                      * la disponibilita' dell'elemento         *
      *                      *-----------------------------------------*
           add       d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   to   w-det-qta-fod-wsp      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione saldo progressivo       *
      *                      *-----------------------------------------*
           move      w-det-qta-fod-wsp    to   d-buf-fod-qta-fod
                                              (w-det-qta-fod-c01)     .
      *                      *-----------------------------------------*
      *                      * Eventuale attivazione flag di fabbi-    *
      *                      * sogno entro l'orizzonte temporale       *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   <    zero
                     move  "#"            to   d-buf-fod-fpf-eot      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-qta-fod-200.
       det-qta-fod-140.
      *                  *---------------------------------------------*
      *                  * Tipo record : 20 - Ordini clienti entro     *
      *                  * l'orizzonte temporale                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione saldo progressivo cal-  *
      *                      * colato dall'elemento di disponibilita'  *
      *                      * precedente                              *
      *                      *-----------------------------------------*
           move      w-det-qta-fod-wsp    to   d-buf-fod-qta-fod
                                              (w-det-qta-fod-c01)     .
      *                      *-----------------------------------------*
      *                      * Eventuale attivazione flag di fabbi-    *
      *                      * sogno entro l'orizzonte temporale       *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   <    zero
                     move  "#"            to   d-buf-fod-fpf-eot      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-qta-fod-200.
       det-qta-fod-150.
      *                  *---------------------------------------------*
      *                  * Tipo record : 30 - Elemento della data o-   *
      *                  * rizzonte temporale                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento saldo progressivo con e-  *
      *                      * ventuale quantita' di sottoscorta       *
      *                      *-----------------------------------------*
           subtract  d-buf-fod-qta-ioc
                    (w-det-qta-fod-c01)   from w-det-qta-fod-wsp      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione saldo progressivo cal-  *
      *                      * colato                                  *
      *                      *-----------------------------------------*
           move      w-det-qta-fod-wsp    to   d-buf-fod-qta-fod
                                              (w-det-qta-fod-c01)     .
      *                      *-----------------------------------------*
      *                      * Eventuale attivazione flag di fabbiso-  *
      *                      * gno alla data dell'orizzonte temporale  *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   <    zero
                     move  "#"            to   d-buf-fod-fpf-aot      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-qta-fod-200.
       det-qta-fod-160.
      *                  *---------------------------------------------*
      *                  * Tipo record : 40 - Ordini fornitori oltre   *
      *                  * l'orizzonte temporale                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento saldo progressivo         *
      *                      *-----------------------------------------*
           add       d-buf-fod-qta-ioc
                    (w-det-qta-fod-c01)   to   w-det-qta-fod-wsp      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione saldo progressivo cal-  *
      *                      * colato                                  *
      *                      *-----------------------------------------*
           move      w-det-qta-fod-wsp    to   d-buf-fod-qta-fod
                                              (w-det-qta-fod-c01)     .
      *                      *-----------------------------------------*
      *                      * Eventuale attivazione flag di fabbi-    *
      *                      * sogno oltre l'orizzonte temporale       *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   <    zero
                     move  "#"            to   d-buf-fod-fpf-oot      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-qta-fod-200.
       det-qta-fod-170.
      *                  *---------------------------------------------*
      *                  * Tipo record : 50 - Ordini clienti oltre     *
      *                  * l'orizzonte temporale                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento saldo progressivo         *
      *                      *-----------------------------------------*
           subtract  d-buf-fod-qta-ioc
                    (w-det-qta-fod-c01)   from w-det-qta-fod-wsp      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione saldo progressivo cal-  *
      *                      * colato                                  *
      *                      *-----------------------------------------*
           move      w-det-qta-fod-wsp    to   d-buf-fod-qta-fod
                                              (w-det-qta-fod-c01)     .
      *                      *-----------------------------------------*
      *                      * Eventuale attivazione flag di fabbi-    *
      *                      * sogno oltre l'orizzonte temporale       *
      *                      *-----------------------------------------*
           if        d-buf-fod-qta-fod
                    (w-det-qta-fod-c01)   <    zero
                     move  "#"            to   d-buf-fod-fpf-oot      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-qta-fod-200.
       det-qta-fod-200.
      *                  *---------------------------------------------*
      *                  * Riciclo su scansione buffer                 *
      *                  *---------------------------------------------*
           go to     det-qta-fod-100.
       det-qta-fod-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-qta-fod-999.
       det-qta-fod-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento buffer delle disponibilita'                   *
      *    *-----------------------------------------------------------*
       ord-buf-fod-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        d-buf-fod-num-ele    <    2
                     go to ord-buf-fod-999.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-ord-buf-fod-c01      .
       ord-buf-fod-100.
           add       1                    to   w-ord-buf-fod-c01      .
           if        w-ord-buf-fod-c01    =    d-buf-fod-num-ele
                     go to ord-buf-fod-999.
           move      w-ord-buf-fod-c01    to   w-ord-buf-fod-c02
                                               w-ord-buf-fod-c03      .
           move      d-buf-fod-key-ord
                    (w-ord-buf-fod-c01)   to   w-ord-buf-fod-wsk      .
       ord-buf-fod-200.
           add       1                    to   w-ord-buf-fod-c02      .
           if        w-ord-buf-fod-c02    >    d-buf-fod-num-ele
                     go to ord-buf-fod-300.
           if        d-buf-fod-key-ord
                    (w-ord-buf-fod-c02)   >    w-ord-buf-fod-wsk
                     go to ord-buf-fod-200.
           move      w-ord-buf-fod-c02    to   w-ord-buf-fod-c03      .
           move      d-buf-fod-key-ord
                    (w-ord-buf-fod-c02)   to   w-ord-buf-fod-wsk      .
           go to     ord-buf-fod-200.
       ord-buf-fod-300.
           move      w-ord-buf-fod-c01    to   w-ord-buf-fod-c04      .          
           if        w-ord-buf-fod-wsk    >    d-buf-fod-key-ord
                                              (w-ord-buf-fod-c04)
                     go to ord-buf-fod-100.
           move      d-buf-fod-sng-ele
                    (w-ord-buf-fod-c03)   to   d-buf-fod-sng-ele
                                              (1000)                  .
           move      d-buf-fod-sng-ele
                    (w-ord-buf-fod-c04)   to   d-buf-fod-sng-ele
                                              (w-ord-buf-fod-c03)     .
           move      d-buf-fod-sng-ele
                    (1000)                to   d-buf-fod-sng-ele
                                              (w-ord-buf-fod-c04)     .
           go to     ord-buf-fod-100.
       ord-buf-fod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione quantitˆ in ordine da dipendenza a Sede    *
      *    *                                                           *
      *    * Si prendono in considerazione gli ordini memorizzati in   *
      *    * Sede, inevasi, che hanno 'D' come tipo archivio           *
      *    *-----------------------------------------------------------*
       det-ord-dpz-000.
      *              *-------------------------------------------------*
      *              * Test preliminare se codice in input non zero    *
      *              *-------------------------------------------------*
           if        d-buf-fod-num-mag    =    zero
                     go to det-ord-dpz-999.
       det-ord-dpz-100.
      *              *-------------------------------------------------*
      *              * Creazione elementi da righe ordini clienti      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione quantita' in uscita        *
      *                  *---------------------------------------------*
           move      zero                 to   d-buf-fod-qta-oaf      .
      *                  *---------------------------------------------*
      *                  * Start su file [ocr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      01                   to   rf-ocr-cod-dpz         .
           move      spaces               to   rf-ocr-flg-rch         .
           move      d-buf-fod-tip-mag    to   rf-ocr-tip-mag         .
           move      d-buf-fod-num-mag    to   rf-ocr-num-pro         .
           move      zero                 to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ord-dpz-900.
       det-ord-dpz-200.
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
______*    move      "IE"                 to   s-ope                  .
______*    call      "swd/mod/prg/obj/msegrt"
______*                                  using s                      .
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [ocr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ord-dpz-900.
       det-ord-dpz-300.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ocr-cod-dpz       not  = 01                or
                     rf-ocr-flg-rch       not  = spaces            or
                     rf-ocr-tip-mag       not  = d-buf-fod-tip-mag or
                     rf-ocr-num-pro       not  = d-buf-fod-num-mag
                     go to det-ord-dpz-900.
       det-ord-dpz-400.
      *                  *---------------------------------------------*
      *                  * Selezione sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo ordine                     *
      *                      *                                         *
      *                      * Si considerano solo gli ordini normali  *
      *                      *-----------------------------------------*
           if        rf-ocr-tip-ord       not  = spaces
                     go to det-ord-dpz-200.
      *                      *-----------------------------------------*
      *                      * Test su tipo archivio                   *
      *                      *                                         *
      *                      * Si considerano gli ordini a dipendenze  *
      *                      *-----------------------------------------*
           if        rf-ocr-tip-arc       not  = "D"
                     go to det-ord-dpz-200.
      *                      *-----------------------------------------*
      *                      * Test su codice archivio                 *
      *                      *-----------------------------------------*
           if        rf-ocr-cod-arc       not  = d-buf-fod-cod-dpz
                     go to det-ord-dpz-200.
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-ocr-dat-doc       >    d-buf-fod-dat-ela
                     go to det-ord-dpz-200.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo riga                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo riga in comodo ridefinito      *
      *                          *-------------------------------------*
           move      rf-ocr-tip-rig       to   w-crz-buf-fod-wtr      .
      *                          *-------------------------------------*
      *                          * Test su tipo funzionamento          *
      *                          *-------------------------------------*
           if        w-crz-buf-fod-wtf    not  = spaces
                     go to det-ord-dpz-200.
       det-ord-dpz-530.
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' da evadere ri- *
      *                      * ga ordine cliente                       *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su quantita' ordinata              *
      *                      *-----------------------------------------*
           if        d-qev-roc-qta-ord    not  > zero
                     go to det-ord-dpz-200.
      *                      *-----------------------------------------*
      *                      * Test su quantita' da evadere            *
      *                      *-----------------------------------------*
           if        d-qev-roc-qta-dev    =    zero
                     go to det-ord-dpz-200.
      *                  *---------------------------------------------*
      *                  * Aggiornamento quantita' in ordine a Sede    *
      *                  *---------------------------------------------*
           add       d-qev-roc-qta-dev    to   d-buf-fod-qta-oaf      .
       det-ord-dpz-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale record [ocr]     *
      *              *-------------------------------------------------*
           go to     det-ord-dpz-200.
       det-ord-dpz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ord-dpz-999.
       det-ord-dpz-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per operazioni sulle date                     *
      *    *                                                           *
      *    * 'det-dat-nrg-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'aumento di una data in giorni              *
      *    *                                                           *
      *    * Input  : w-det-dat-nrg-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-dat-nrg-ngi = nr. giorni di incremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-dat-nrg-dti = Data incrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'det-nrg-dat-000/999'                                     *
      *    *                                                           *
      *    * Routines per la diminuzione di una data in giorni         *
      *    *                                                           *
      *    * Input  : w-det-nrg-dat-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-nrg-dat-ngd = nr. giorni di decremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-nrg-dat-dtd = Data decrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione saldo di magazzino         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine cliente                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine fornitore                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine di spedizione cliente                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dts"                   .


