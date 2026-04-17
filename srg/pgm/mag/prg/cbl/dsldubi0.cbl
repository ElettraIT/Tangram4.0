       Identification Division.
       Program-Id.                                 dsldubi0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:    sld                 *
      *                                   Fase:    sldubi              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/12/23    *
      *                       Ultima revisione:    NdK del 23/05/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione delle giacenze per ubicazione di  *
      * un prodotto                                                    *
      *                                                                *
      *                ___ VALUTARE NORMALIZZAZIONE GLOBALE ARRAY ___  *
      *                ___ 'd-sld-ubi-dti-buf' ___                     *
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
      *        Input  : d-sld-ubi-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-sld-ubi-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "PU" - Determinazione buffer delle giacenze per ubicazione di  *
      *        un prodotto                                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "PU"                       *
      *                                                                *
      *                 d-sld-ubi-dat-sit = Data situazione            *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-sld-ubi-tip-mag = Tipo codice di magazzino   *
      *                                                                *
      *                 d-sld-ubi-num-mag = Codice numerico di magaz-  *
      *                                     zino                       *
      *                                                                *
      *                 d-sld-ubi-cod-ubi = Codice ubicazione          *
      *                                    (facoltativa)               *
      *                                                                *
      *                 d-sld-ubi-lit-ubi = Literal (flag)             *
      *                                    (facoltativa)               *
      *                                    (Se '#' = solo ubicazioni   *
      *                                     con giacenza e indice di   *
      *                                     percorso NON zero)         *
      *                                                                *
      *                                                                *
      *        Output : d-sld-ubi-buf-ele = Buffer delle giacenze per  *
      *                                     ubicazione del prodotto    *
      *                                                                *
      *                 d-sld-ubi-num-ele = Numero di ubicazioni in    *
      *                                     cui compare il prodotto    *
      *                                                                *
      *                 d-sld-ubi-lit-ubi = Literal delle ubicazioni   *
      *                                                                *
      *                 d-sld-ubi-cod-ubi = Codice ubicazione in testa *
      *                                     alla lista                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "UB" - Determinazione se ubicazione prevista per il prodotto   *
      *                                                                *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "UB"                       *
      *                                                                *
      *                 d-sld-ubi-dat-sit = Data situazione            *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-sld-ubi-tip-mag = Tipo codice di magazzino   *
      *                                                                *
      *                 d-sld-ubi-num-mag = Codice numerico di magaz-  *
      *                                     zino                       *
      *                                                                *
      *                                                                *
      *        Output : d-sld-ubi-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "UP" - Determinazione ubicazione principale per il prodotto    *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "UP"                       *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-sld-ubi-tip-mag = Tipo codice di magazzino   *
      *                                                                *
      *                 d-sld-ubi-num-mag = Codice numerico di magaz-  *
      *                                     zino                       *
      *                                                                *
      *        Output : d-sld-ubi-inx-ppr = Indice di percorso ubica-  *
      *                                     zione principale           *
      *                                                                *
      *                 d-sld-ubi-cod-ubi = Codice ubicazione          *
      *                                     principale                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "IP" - Determinazione indice di percorso ubicazione            *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "IP"                       *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-sld-ubi-cod-ubi = Codice ubicazione          *
      *                                                                *
      *        Output : d-sld-ubi-inx-ppr = Indice di percorso         *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "GU" - Determinazione buffer dei prodotti in giacenza per una  *
      *        ubicazione                                              *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "GU"                       *
      *                                                                *
      *                 d-sld-ubi-dat-sit = Data situazione            *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-sld-ubi-cod-ubi = Codice ubicazione          *
      *                                                                *
      *                 d-sld-ubi-tip-mag = Tipo codice di magazzino   *
      *                                                                *
      *                                                                *
      *        Output : d-sld-ubi-buf-ubi = Buffer delle giacenze per  *
      *                                     ubicazione                 *
      *                                                                *
      *                 d-sld-ubi-num-pro = Numero di prodotti nella   *
      *                                     ubicazione                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "TU" - Determinazione tipo ubicazione                          *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "TU"                       *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *                 d-sld-ubi-cod-ubi = Codice ubicazione          *
      *                                                                *
      *        Output : d-sld-ubi-exi-sts = Tipo ubicazione            *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "UV" - Determinazione ubicazioni virtuali (cioe' non normali)  *
      *                                                                *
      *        Input  : d-sld-ubi-tip-ope = "UV"                       *
      *                                                                *
      *                 d-sld-ubi-cod-dpz = Codice dipendenza in uso   *
      *                                                                *
      *        Output : d-sld-ubi-lit-ubi = Literal delle ubicazioni   *
      *                                     di transito separato da    *
      *                                     virgola                    *
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
      *        * [mau]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmau"                          .
      *        *-------------------------------------------------------*
      *        * [mms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmms"                          .
      *        *-------------------------------------------------------*
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

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
      *    * Work per routine det-gia-ubi-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-det-gia-ubi.
      *        *-------------------------------------------------------*
      *        * Anno di esercizio                                     *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi-ese          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per selezione ubicazione                       *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi-sub          pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di selezione per literal                         *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi-fsl          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Buffer elementi                                       *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi-ele.
      *            *---------------------------------------------------*
      *            * Numero elementi nel buffer                        *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-num      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero massimo elementi nel buffer                *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-max      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-sng occurs 99.
      *                *-----------------------------------------------*
      *                * Codice ubicazione                             *
      *                *-----------------------------------------------*
                   15  w-det-gia-ubi-ubi  pic  x(07)                  .
      *                *-----------------------------------------------*
      *                * Flag di saldo a zero non visibile             *
      *                *-----------------------------------------------*
                   15  w-det-gia-ubi-fsz  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zub]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zub.
               10  w-let-arc-zub-flg      pic  x(01)                  .
               10  w-let-arc-zub-dpz      pic  9(02)                  .
               10  w-let-arc-zub-cod      pic  x(07)                  .
               10  w-let-arc-zub-des      pic  x(30)                  .
               10  w-let-arc-zub-inx      pic  9(07)                  .
               10  w-let-arc-zub-tip      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per routine ord-tbl-ubi-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-ord-tbl-ubi.
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento buffer   *
      *        *-------------------------------------------------------*
           05  w-ord-tbl-ubi-wsk          pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 1                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-tbl-ubi-c01          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 2                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-tbl-ubi-c02          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 3                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-tbl-ubi-c03          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 4                          [Work] *
      *        *-------------------------------------------------------*
           05  w-ord-tbl-ubi-c04          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione giacenza per     *
      *    * ubicazione per un prodotto                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldubi0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-sld-ubi              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-sld-ubi-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-sld-ubi-tip-ope    =    "PU"
                     perform det-gia-ubi-000
                                          thru det-gia-ubi-999
      *                  *---------------------------------------------*
      *                  * Test se ubicazione prevista per prodotto    *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "UB"
                     perform det-ubi-pro-000
                                          thru det-ubi-pro-999
      *                  *---------------------------------------------*
      *                  * Determinazione ubicazione principale        *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "UP"
                     perform det-ubi-pri-000
                                          thru det-ubi-pri-999
      *                  *---------------------------------------------*
      *                  * Determinazione indice di percorso ubicazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "IP"
                     perform det-ipe-ubi-000
                                          thru det-ipe-ubi-999
      *                  *---------------------------------------------*
      *                  * Determinazione prodotti in ubicazione       *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "GU"
                     perform det-pro-ubi-000
                                          thru det-pro-ubi-999
      *                  *---------------------------------------------*
      *                  * Determinazione tipo ubicazione              *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "TU"
                     perform det-tip-ubi-000
                                          thru det-tip-ubi-999
      *                  *---------------------------------------------*
      *                  * Determinazione literal ubicazioni virtuali  *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "UV"
                     perform det-ubi-vrt-000
                                          thru det-ubi-vrt-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-sld-ubi-tip-ope    =    "C?"
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
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * [mms]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *              *-------------------------------------------------*
      *              * [zub]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magazz.  *
      *              *-------------------------------------------------*
           perform   det-sld-mag-opn-000  thru det-sld-mag-opn-999    .
       rou-opn-fls-999.
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
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * [mms]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *              *-------------------------------------------------*
      *              * [zub]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
       rou-cls-fls-100.
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
                     move  spaces         to   d-sld-ubi-exi-sts
           else      move  "#"            to   d-sld-ubi-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer giacenza per ubicazione per un pro- *
      *    * dotto                                                     *
      *    *-----------------------------------------------------------*
       det-gia-ubi-000.
      *              *-------------------------------------------------*
      *              * Test preliminare se codice in input non zero    *
      *              *-------------------------------------------------*
           if        d-sld-ubi-num-mag    =    zero
                     go to det-gia-ubi-900.
       det-gia-ubi-010.
      *              *-------------------------------------------------*
      *              * Trattamento literal                             *
      *              *-------------------------------------------------*
           if        d-sld-ubi-lit-ubi
                    (01 : 01)             =    "#"
                     move  "#"            to   w-det-gia-ubi-fsl
           else      move  spaces         to   w-det-gia-ubi-fsl      .   
      *
           move      spaces               to   d-sld-ubi-lit-ubi      .
       det-gia-ubi-030.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero elementi                *
      *              *-------------------------------------------------*
           move      zero                 to   d-sld-ubi-num-ele      .
      *              *-------------------------------------------------*
      *              * Definizione numero elementi massimo             *
      *              *                                                 *
      *              * N.B.: attualmente a 99 anziche' 999             *
      *              *-------------------------------------------------*
           move      99                   to   d-sld-ubi-max-ele      .
      *              *-------------------------------------------------*
      *              * Anno di esercizio da data di elaborazione       *
      *              *-------------------------------------------------*
           move      d-sld-ubi-dat-ela    to   s-dat                  .
           move      s-saa                to   w-det-gia-ubi-ese      .
       det-gia-ubi-050.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gia-ubi-num      .
           move      99                   to   w-det-gia-ubi-max      .
           move      zero                 to   w-det-gia-ubi-ctr      .
       det-gia-ubi-060.
           add       1                    to   w-det-gia-ubi-ctr      .
           if        w-det-gia-ubi-ctr    >    w-det-gia-ubi-max
                     go to det-gia-ubi-070.
           move      spaces               to   w-det-gia-ubi-ubi
                                              (w-det-gia-ubi-ctr)     .
           move      spaces               to   w-det-gia-ubi-fsz
                                              (w-det-gia-ubi-ctr)     .
           go to     det-gia-ubi-060.
       det-gia-ubi-070.
      *              *-------------------------------------------------*
      *              * Normalizzazione dell'elemento 1                 *
      *              *                                                 *
      *              * ___ VALUTARE NORMALIZZAZIONE GLOBALE ARRAY ___  *
      *              *-------------------------------------------------*
           move      zero                 to   d-sld-ubi-qta-gia (01)  .
       det-gia-ubi-100.
      *              *-------------------------------------------------*
      *              * Scansione [mms]                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-gia-ubi-sub      .
           perform   let-fil-mms-000      thru let-fil-mms-999        .
       det-gia-ubi-150.
      *              *-------------------------------------------------*
      *              * Scansione ubicazioni determinate                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-gia-ubi-ctr      .
       det-gia-ubi-200.
           add       1                    to   w-det-gia-ubi-ctr      .
           if        w-det-gia-ubi-ctr    >    w-det-gia-ubi-num
                     go to det-gia-ubi-800.
           if        w-det-gia-ubi-ctr    >    w-det-gia-ubi-max
                     go to det-gia-ubi-800.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione in input          *
      *                  *---------------------------------------------*
           if        d-sld-ubi-cod-ubi    =    spaces
                     go to det-gia-ubi-300.
           if        w-det-gia-ubi-ubi
                    (w-det-gia-ubi-ctr)   not  = d-sld-ubi-cod-ubi
                     go to det-gia-ubi-200.
       det-gia-ubi-300.
      *                  *---------------------------------------------*
      *                  * Determinazione singola giacenza             *
      *                  *---------------------------------------------*
           perform   det-gia-ubi-sld-000  thru det-gia-ubi-sld-999    .
      *                  *---------------------------------------------*
      *                  * Determinazione indice di percorso           *
      *                  *---------------------------------------------*
           move      d-sld-ubi-cod-dpz    to   w-let-arc-zub-dpz      .
           move      w-det-gia-ubi-ubi
                    (w-det-gia-ubi-ctr)   to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Se la giacenza e' a zero ed e' attivo il    *
      *                  * flag di saldo a zero non visibile, non si   *
      *                  * bufferizza l'elemento                       *
      *                  *---------------------------------------------*
           if        w-det-gia-ubi-fsz    
                    (w-det-gia-ubi-ctr)   =    spaces
                     go to det-gia-ubi-400.
           if        d-sld-mag-sld-mag    not  = zero
                     go to det-gia-ubi-400.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-gia-ubi-200.
       det-gia-ubi-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   d-sld-ubi-num-ele      .
           if        d-sld-ubi-num-ele    >    d-sld-ubi-max-ele
                     move  d-sld-ubi-max-ele
                                          to   d-sld-ubi-num-ele      .
      *                      *-----------------------------------------*
      *                      * Indice di percorso                      *
      *                      *-----------------------------------------*
           move      w-let-arc-zub-inx    to   d-sld-ubi-inx-per
                                              (d-sld-ubi-num-ele)     .
      *                      *-----------------------------------------*
      *                      * Giacenza                                *
      *                      *-----------------------------------------*
           move      d-sld-mag-sld-mag    to   d-sld-ubi-qta-gia
                                              (d-sld-ubi-num-ele)     .
      *                      *-----------------------------------------*
      *                      * Ubicazione                              *
      *                      *-----------------------------------------*
           move      w-det-gia-ubi-ubi
                    (w-det-gia-ubi-ctr)   to   d-sld-ubi-ubi-gia
                                              (d-sld-ubi-num-ele)     .
       det-gia-ubi-600.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-gia-ubi-200.
       det-gia-ubi-800.
      *              *-------------------------------------------------*
      *              * Ordinamento tabella in base all'indice di       *
      *              * percorso                                        *
      *              *-------------------------------------------------*
           perform   ord-tbl-ubi-000      thru ord-tbl-ubi-999        .
      *              *-------------------------------------------------*
      *              * Literal ubicazioni                              *
      *              *-------------------------------------------------*
           perform   det-ubi-lit-000      thru det-ubi-lit-999        .
       det-gia-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-gia-ubi-999.
       det-gia-ubi-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento tabella ubicazioni per indice di percorso     *
      *    *-----------------------------------------------------------*
       ord-tbl-ubi-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        d-sld-ubi-num-ele    <    2
                     go to ord-tbl-ubi-999.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-ord-tbl-ubi-c01      .
       ord-tbl-ubi-100.
           add       1                    to   w-ord-tbl-ubi-c01      .
           if        w-ord-tbl-ubi-c01    =    d-sld-ubi-num-ele
                     go to ord-tbl-ubi-999.
           move      w-ord-tbl-ubi-c01    to   w-ord-tbl-ubi-c02
                                               w-ord-tbl-ubi-c03      .
           move      d-sld-ubi-key-ord
                    (w-ord-tbl-ubi-c01)   to   w-ord-tbl-ubi-wsk      .
       ord-tbl-ubi-200.
           add       1                    to   w-ord-tbl-ubi-c02      .
           if        w-ord-tbl-ubi-c02    >    d-sld-ubi-num-ele
                     go to ord-tbl-ubi-300.
           if        d-sld-ubi-key-ord
                    (w-ord-tbl-ubi-c02)   >    w-ord-tbl-ubi-wsk
                     go to ord-tbl-ubi-200.
           move      w-ord-tbl-ubi-c02    to   w-ord-tbl-ubi-c03      .
           move      d-sld-ubi-key-ord
                    (w-ord-tbl-ubi-c02)   to   w-ord-tbl-ubi-wsk      .
           go to     ord-tbl-ubi-200.
       ord-tbl-ubi-300.
           move      w-ord-tbl-ubi-c01    to   w-ord-tbl-ubi-c04      .          
           if        w-ord-tbl-ubi-wsk    >    d-sld-ubi-key-ord
                                              (w-ord-tbl-ubi-c04)
                     go to ord-tbl-ubi-100.
           move      d-sld-ubi-sng-ele
                    (w-ord-tbl-ubi-c03)   to   d-sld-ubi-sng-ele
                                              (d-sld-ubi-max-ele)     .
           move      d-sld-ubi-sng-ele
                    (w-ord-tbl-ubi-c04)   to   d-sld-ubi-sng-ele
                                              (w-ord-tbl-ubi-c03)     .
           move      d-sld-ubi-sng-ele
                    (d-sld-ubi-max-ele)   to   d-sld-ubi-sng-ele
                                              (w-ord-tbl-ubi-c04)     .
           go to     ord-tbl-ubi-100.
       ord-tbl-ubi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer giacenza per ubicazione per un pro- *
      *    * dotto                                                     *
      *    *-----------------------------------------------------------*
       det-ubi-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gia-ubi-num      .
           move      spaces               to   d-sld-ubi-exi-sts      .
      *              *-------------------------------------------------*
      *              * Test preliminare su codici in input             *
      *              *-------------------------------------------------*
           if        d-sld-ubi-num-mag    =    zero
                     go to det-ubi-pro-900.
           if        d-sld-ubi-cod-ubi    =    spaces
                     go to det-ubi-pro-900.
       det-ubi-pro-200.
      *              *-------------------------------------------------*
      *              * Anno di esercizio da data di elaborazione       *
      *              *-------------------------------------------------*
           move      d-sld-ubi-dat-ela    to   s-dat                  .
           move      s-saa                to   w-det-gia-ubi-ese      .
      *              *-------------------------------------------------*
      *              * Scansione [mms]                                 *
      *              *-------------------------------------------------*
           move      d-sld-ubi-cod-ubi    to   w-det-gia-ubi-sub      .
           perform   let-fil-mms-000      thru let-fil-mms-999        .
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           if        w-det-gia-ubi-num    =    zero
                     move  "#"            to   d-sld-ubi-exi-sts      .
       det-ubi-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ubi-pro-999.
       det-ubi-pro-999.
           exit.

      *    *===========================================================*
      *    * Determinazione ubicazione principale di un prodotto       *
      *    *-----------------------------------------------------------*
       det-ubi-pri-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   d-sld-ubi-cod-ubi      .
           move      zero                 to   d-sld-ubi-inx-ppr      .
      *              *-------------------------------------------------*
      *              * Test preliminare su codici in input             *
      *              *-------------------------------------------------*
           if        d-sld-ubi-num-mag    =    zero
                     go to det-ubi-pri-900.
       det-ubi-pri-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione [mau]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Lettura [mau]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      d-sld-ubi-cod-dpz    to   rf-mau-cod-dpz         .
           move      d-sld-ubi-tip-mag    to   rf-mau-tip-mag         .
           move      d-sld-ubi-num-mag    to   rf-mau-num-mag         .
           move      spaces               to   rf-mau-var-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Ubicazione principale determinata               *
      *              *-------------------------------------------------*
           move      rf-mau-prm-ubi (1)   to   d-sld-ubi-cod-ubi      .
      *              *-------------------------------------------------*
      *              * Determinazione indice di percorso               *
      *              *-------------------------------------------------*
           move      d-sld-ubi-cod-dpz    to   w-let-arc-zub-dpz      .
           move      d-sld-ubi-cod-ubi    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           move      w-let-arc-zub-inx    to   d-sld-ubi-inx-ppr      .
       det-ubi-pri-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ubi-pri-999.
       det-ubi-pri-999.
           exit.

      *    *===========================================================*
      *    * Determinazione indice di percorso ubicazione              *
      *    *-----------------------------------------------------------*
       det-ipe-ubi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-sld-ubi-inx-ppr      .
       det-ipe-ubi-200.
      *              *-------------------------------------------------*
      *              * Determinazione indice di percorso               *
      *              *-------------------------------------------------*
           move      d-sld-ubi-cod-dpz    to   w-let-arc-zub-dpz      .
           move      d-sld-ubi-cod-ubi    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           move      w-let-arc-zub-inx    to   d-sld-ubi-inx-ppr      .
       det-ipe-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ipe-ubi-999.
       det-ipe-ubi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer prodotti presenti in una ubicazione *
      *    *-----------------------------------------------------------*
       det-pro-ubi-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero elementi                *
      *              *-------------------------------------------------*
           move      zero                 to   d-sld-ubi-num-pro      .
      *              *-------------------------------------------------*
      *              * Definizione numero elementi massimo             *
      *              *-------------------------------------------------*
           move      9999                 to   d-sld-ubi-max-pro      .
      *              *-------------------------------------------------*
      *              * Anno di esercizio da data di elaborazione       *
      *              *-------------------------------------------------*
           move      d-sld-ubi-dat-ela    to   s-dat                  .
           move      s-saa                to   w-det-gia-ubi-ese      .
       det-pro-ubi-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione file [mms]                   *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPDSMG    "         to   f-key                  .
           move      w-det-gia-ubi-ese    to   rf-mms-ann-ese         .
           move      d-sld-ubi-cod-dpz    to   rf-mms-cod-dpz         .
           move      d-sld-ubi-cod-ubi    to   rf-mms-cod-dsl         .
           move      d-sld-ubi-tip-mag    to   rf-mms-tip-mag         .
           move      zero                 to   rf-mms-num-mag         .
           move      spaces               to   rf-mms-var-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-pro-ubi-900.
       det-pro-ubi-200.
      *              *-------------------------------------------------*
      *              * Read Next [mms]                                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-pro-ubi-900.
       det-pro-ubi-300.
      *              *-------------------------------------------------*
      *              * Test max su [mms]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-mms-ann-ese       not  = w-det-gia-ubi-ese
                     go to det-pro-ubi-900.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-mms-cod-dpz       not  = d-sld-ubi-cod-dpz
                     go to det-pro-ubi-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo magazzino                      *
      *                  *---------------------------------------------*
           if        rf-mms-tip-mag       not  = d-sld-ubi-tip-mag
                     go to det-pro-ubi-900.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        rf-mms-cod-dsl       not  = d-sld-ubi-cod-ubi
                     go to det-pro-ubi-900.
       det-pro-ubi-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [mms]                              *
      *              *-------------------------------------------------*
       det-pro-ubi-500.
      *              *-------------------------------------------------*
      *              * Incremento tabella                              *
      *              *-------------------------------------------------*
           add       1                    to   d-sld-ubi-num-pro      .
           move      rf-mms-num-mag       to   d-sld-ubi-pro-ubi
                                              (d-sld-ubi-num-pro)     .
       det-pro-ubi-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-pro-ubi-200.
       det-pro-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-pro-ubi-999.
       det-pro-ubi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tipo ubicazione                            *
      *    *-----------------------------------------------------------*
       det-tip-ubi-000.
      *              *-------------------------------------------------*
      *              * Lettura tabella                                 *
      *              *-------------------------------------------------*
           move      d-sld-ubi-cod-dpz    to   w-let-arc-zub-dpz      .
           move      d-sld-ubi-cod-ubi    to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *              *-------------------------------------------------*
      *              * Tipo ubicazione letto                           *
      *              *-------------------------------------------------*
           move      w-let-arc-zub-tip    to   d-sld-ubi-exi-sts      .
       det-tip-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tip-ubi-999.
       det-tip-ubi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione literal ubicazioni virtuali                *
      *    *-----------------------------------------------------------*
       det-ubi-vrt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   d-sld-ubi-lit-ubi      .
       det-ubi-vrt-100.
      *              *-------------------------------------------------*
      *              * Start su [zub]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODUBI    "         to   f-key                  .
           move      d-sld-ubi-cod-dpz    to   rf-zub-cod-dpz         .
           move      spaces               to   rf-zub-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ubi-vrt-900.
       det-ubi-vrt-200.
      *              *-------------------------------------------------*
      *              * Next su [zub]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *                  *---------------------------------------------*
      *                  * Se fine file: ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ubi-vrt-900.
       det-ubi-vrt-300.
      *              *-------------------------------------------------*
      *              * Max su [zub]                                    *
      *              *-------------------------------------------------*
           if        rf-zub-cod-dpz       not  = d-sld-ubi-cod-dpz
                     go to det-ubi-vrt-900.
       det-ubi-vrt-400.
      *              *-------------------------------------------------*
      *              * Sel su [zub]                                    *
      *              *-------------------------------------------------*
           if        rf-zub-cod-ubi       =    spaces
                     go to det-ubi-vrt-200.
           if        rf-zub-tip-ubi       =    "N"
                     go to det-ubi-vrt-200.
       det-ubi-vrt-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento literal                           *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      d-sld-ubi-lit-ubi    to   w-all-str-cat (1)      .
           move      ","                  to   w-all-str-cat (2)      .
      *
           if        d-sld-ubi-lit-ubi    =    spaces
                     move  spaces         to   w-all-str-cat (2)      .
      *
           move      rf-zub-cod-ubi       to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   d-sld-ubi-lit-ubi      .
       det-ubi-vrt-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ubi-vrt-200.
       det-ubi-vrt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ubi-vrt-999.
       det-ubi-vrt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer giacenza per ubicazione per un pro- *
      *    * dotto                                                     *
      *    *                                                           *
      *    * Subroutine per assemblaggio literal                       *
      *    *-----------------------------------------------------------*
       det-ubi-lit-000.
      *              *-------------------------------------------------*
      *              * Scansione ubicazioni determinate                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gia-ubi-ctr      .
       det-ubi-lit-200.
           add       1                    to   w-det-gia-ubi-ctr      .
           if        w-det-gia-ubi-ctr    >    w-det-gia-ubi-num
                     go to det-ubi-lit-900.
           if        w-det-gia-ubi-ctr    >    w-det-gia-ubi-max
                     go to det-ubi-lit-900.
       det-ubi-lit-300.
      *              *-------------------------------------------------*
      *              * Test su eventuale flag che indica l'esclusione  *
      *              * delle ubicazioni con giacenza a zero o con      *
      *              * indice di percorso a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul flag                               *
      *                  *---------------------------------------------*
           if        w-det-gia-ubi-fsl    not  = "#"
                     go to det-ubi-lit-400.
      *                  *---------------------------------------------*
      *                  * Test su giacenza                            *
      *                  *---------------------------------------------*
           if        d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   =    zero
                     go to det-ubi-lit-200.
      *                  *---------------------------------------------*
      *                  * Test su indice di percorso                  *
      *                  *---------------------------------------------*
           if        d-sld-ubi-inx-per
                    (w-det-gia-ubi-ctr)   =    zero
                     go to det-ubi-lit-200.
       det-ubi-lit-400.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           move      d-sld-ubi-lit-ubi    to   w-all-str-cat (1)      .
      *
           if        d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   =    spaces
                     move  spaces         to   w-all-str-cat (2)
                     move  spaces         to   w-all-str-cat (3)
           else      move  ","            to   w-all-str-cat (2)
                     move  d-sld-ubi-ubi-gia
                          (w-det-gia-ubi-ctr)
                                          to   w-all-str-cat (3)      .
      *
           if        d-sld-ubi-lit-ubi    =    spaces
                     move  spaces         to   w-all-str-cat (2)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   d-sld-ubi-lit-ubi      .
       det-ubi-lit-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione primo elemento determinato      *
      *              *-------------------------------------------------*
           if        d-sld-ubi-cod-ubi    not  = spaces
                     go to det-ubi-lit-800.
           move      d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   to   d-sld-ubi-cod-ubi      .
       det-ubi-lit-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ubi-lit-200.
       det-ubi-lit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ubi-lit-999.
       det-ubi-lit-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer giacenza per ubicazione per un pro- *
      *    * dotto                                                     *
      *    *                                                           *
      *    * Subroutine di scansione [mms]                             *
      *    *-----------------------------------------------------------*
       let-fil-mms-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       let-fil-mms-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione file [mms]                   *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPMGDS    "         to   f-key                  .
           move      w-det-gia-ubi-ese    to   rf-mms-ann-ese         .
           move      d-sld-ubi-cod-dpz    to   rf-mms-cod-dpz         .
           move      d-sld-ubi-tip-mag    to   rf-mms-tip-mag         .
           move      d-sld-ubi-num-mag    to   rf-mms-num-mag         .
           move      spaces               to   rf-mms-var-mag         .
           move      spaces               to   rf-mms-cod-dsl         .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-fil-mms-900.
       let-fil-mms-200.
      *              *-------------------------------------------------*
      *              * Read Next [mms]                                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mms                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-fil-mms-900.
       let-fil-mms-300.
      *              *-------------------------------------------------*
      *              * Test max su [mms]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su anno di esercizio                   *
      *                  *---------------------------------------------*
           if        rf-mms-ann-ese       not  = w-det-gia-ubi-ese
                     go to let-fil-mms-900.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-mms-cod-dpz       not  = d-sld-ubi-cod-dpz
                     go to let-fil-mms-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo magazzino                      *
      *                  *---------------------------------------------*
           if        rf-mms-tip-mag       not  = d-sld-ubi-tip-mag
                     go to let-fil-mms-900.
      *                  *---------------------------------------------*
      *                  * Test su codice magazzino                    *
      *                  *---------------------------------------------*
           if        rf-mms-num-mag       not  = d-sld-ubi-num-mag
                     go to let-fil-mms-900.
       let-fil-mms-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [mms]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        w-det-gia-ubi-sub    =    spaces
                     go to let-fil-mms-500.
           if        rf-mms-cod-dsl       not  = w-det-gia-ubi-sub
                     go to let-fil-mms-200.
       let-fil-mms-500.
      *              *-------------------------------------------------*
      *              * Incremento tabella                              *
      *              *-------------------------------------------------*
           add       1                    to   w-det-gia-ubi-num      .
           move      rf-mms-cod-dsl       to   w-det-gia-ubi-ubi
                                              (w-det-gia-ubi-num)     .
           move      rf-mms-flg-zer       to   w-det-gia-ubi-fsz
                                              (w-det-gia-ubi-num)     .
       let-fil-mms-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     let-fil-mms-200.
       let-fil-mms-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-fil-mms-999.
       let-fil-mms-999.
           exit.

      *    *===========================================================*
      *    * Determinazione buffer giacenza per ubicazione per un pro- *
      *    * dotto                                                     *
      *    *                                                           *
      *    * Subroutine di determinazione saldo                        *
      *    *-----------------------------------------------------------*
       det-gia-ubi-sld-000.
      *              *-------------------------------------------------*
      *              * Preparazione link-area                          *
      *              *-------------------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0000                 to   d-sld-mag-tip-sld      .
           move      d-sld-ubi-dat-ela    to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      d-sld-ubi-cod-dpz    to   d-sld-mag-cod-dpz      .
           move      d-sld-ubi-tip-mag    to   d-sld-mag-tip-mag      .
           move      d-sld-ubi-num-mag    to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "U"                  to   d-sld-mag-uot-dsl      .
           move      w-det-gia-ubi-ubi
                    (w-det-gia-ubi-ctr)   to   d-sld-mag-cod-dsl      .
      *              *-------------------------------------------------*
      *              * Richiamo del sottoprogramma                     *
      *              *-------------------------------------------------*
           perform   det-sld-mag-cll-000  thru det-sld-mag-cll-999    .
       det-gia-ubi-sld-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-gia-ubi-sld-999.
       det-gia-ubi-sld-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zub]                         *
      *    *-----------------------------------------------------------*
       let-arc-zub-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zub-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice ubicazione a spaces              *
      *              *-------------------------------------------------*
           if        w-let-arc-zub-cod    =    spaces
                     go to let-arc-zub-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione [zub]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODUBI    "         to   f-key                  .
           move      w-let-arc-zub-dpz    to   rf-zub-cod-dpz         .
           move      w-let-arc-zub-cod    to   rf-zub-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zub-400.
       let-arc-zub-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zub-des-ubi       to   w-let-arc-zub-des      .
           move      rf-zub-inx-per       to   w-let-arc-zub-inx      .
           move      rf-zub-tip-ubi       to   w-let-arc-zub-tip      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zub-999.
       let-arc-zub-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zub-flg      .
           move      all   "."            to   w-let-arc-zub-des      .
           go to     let-arc-zub-600.
       let-arc-zub-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zub-des      .
           move      spaces               to   w-let-arc-zub-tip      .
       let-arc-zub-600.
           move      zero                 to   w-let-arc-zub-inx      .
       let-arc-zub-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione saldo di magazzino         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
