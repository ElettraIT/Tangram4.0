       Identification Division.
       Program-Id.                                 despcec0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:                        *
      *                                   Fase:    despcec0            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/10/12    *
      *                       Ultima revisione:    NdK del 07/02/14    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione, relativamente ad un cliente di : *
      *                                                                *
      * - Esposizione globale, in valuta base                          *
      *                                                                *
      * Versione su misura per ELETTRA con esposizione del fido        *
      * Payline del CERVED\                                            *
      *                                                                *
      *                                                                *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * N.B. : Il modulo si serve del file [cec] generato in modo      *
      *        'batch' dal programma 'gep580'                          *
      *                                                                *
      *        Attualmente utilizzato da 'orc300' e in 'ods450' SUM    *
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
      *        Input  : d-esp-cec-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-esp-cec-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-esp-cec-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-esp-cec-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "EC" - Esposizione cliente                                     *
      *                                                                *
      *                                                                *
      *        Input  : d-esp-cec-tip-ope = "EC"                       *
      *                                                                *
      *                 d-esp-cec-cod-cli = Codice cliente             *
      *                                                                *
      *                                                                *
      *        Output : d-esp-cec-esp-orc = Ordini inevasi             *
      *                 d-esp-cec-esp-ods = Ordini spedizione inevasi  *
      *                 d-esp-cec-esp-bol = Bolle da fatturare         *
      *                 d-esp-cec-esp-gep = Scadenze aperte            *
      *                 d-esp-cec-dat-agg = Data aggiornamento         *
      *                 d-esp-cec-fid-max = Fido massimo               *
      *                 d-esp-cec-snx-acr = Si/no copertura assic.     *
      *                 d-esp-cec-imp-acr = Importo copertura assic.   *
      *                 d-esp-cec-scd-acr = Scadenza copertura assic.  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "VE" - Visualizzazione esposizione cliente                     *
      *                                                                *
      *                                                                *
      *        Input  : d-esp-cec-tip-ope = "VE"                       *
      *                                                                *
      *                 d-esp-cec-cod-cli = Codice cliente             *
      *                                                                *
      *                 d-esp-cec-cod-coi = Correttivo ordini clienti  *
      *                 d-esp-cec-cod-csi = Correttivo ordini spediz.  *
      *                 d-esp-cec-cod-cbf = Correttivo bolle da fatt.  *
      *                 d-esp-cec-cod-csp = Correttivo scadenze        *
      *                 d-esp-cec-cod-cv1 = Correttivo libero 1        *
      *                 d-esp-cec-cod-cv2 = Correttivo libero 2        *
      *                                                                *
      *                                                                *
      *        Output : d-esp-cec-esp-orc = Ordini inevasi             *
      *                 d-esp-cec-esp-ods = Ordini spedizione inevasi  *
      *                 d-esp-cec-esp-bol = Bolle da fatturare         *
      *                 d-esp-cec-esp-gep = Scadenze aperte            *
      *                 d-esp-cec-dat-agg = Data aggiornamento         *
      *                 d-esp-cec-fid-max = Fido massimo               *
      *                 d-esp-cec-snx-acr = Si/no copertura assic.     *
      *                 d-esp-cec-imp-acr = Importo copertura assic.   *
      *                 d-esp-cec-scd-acr = Scadenza copertura assic.  *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "FC" - Fido massimo cliente                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-esp-cec-tip-ope = "FC"                       *
      *                                                                *
      *                 d-esp-cec-cod-cli = Codice cliente             *
      *                                                                *
      *                                                                *
      *        Output : d-esp-cec-fid-max = Fido massimo               *
      *                                                                *
      *                 d-esp-cec-dat-fid = Data riferimento fido      *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cec]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcec"                          .
      *        *-------------------------------------------------------*
      *        * [ccc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfccc"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/no gestione del fido                               *
      *        *-------------------------------------------------------*
           05  w-prs-snx-fid.
      *            *---------------------------------------------------*
      *            * Si/No gestione                                    *
      *            *---------------------------------------------------*
               10  w-prs-snx-fid-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Filler di separazione                             *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No cumulo Ordini Clienti                       *
      *            *---------------------------------------------------*
               10  w-prs-snx-fid-orc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No cumulo Ordini di Spedizione                 *
      *            *---------------------------------------------------*
               10  w-prs-snx-fid-ods      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No cumulo Bolle da fatturare                   *
      *            *---------------------------------------------------*
               10  w-prs-snx-fid-bol      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No cumulo Scadenze aperte                      *
      *            *---------------------------------------------------*
               10  w-prs-snx-fid-gep      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Filler di separazione                             *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No assicurazione del credito                   *
      *            *---------------------------------------------------*
               10  w-prs-snx-snx-acr      pic  x(01)                  .

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
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det esposizione mensile, in valuta base      *
      *        *-------------------------------------------------------*
           05  w-det-esp-cec.
      *            *---------------------------------------------------*
      *            * Anno di esercizio 1                               *
      *            *---------------------------------------------------*
               10  w-det-esp-cec-es1      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Anno di esercizio 2                               *
      *            *---------------------------------------------------*
               10  w-det-esp-cec-es2      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese di esercizio relativo alla data di riferi-   *
      *            * mento                                             *
      *            *---------------------------------------------------*
               10  w-det-esp-cec-mes      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero di esercizi da leggere                     *
      *            *---------------------------------------------------*
               10  w-det-esp-cec-esn      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Comodi per calcolo percentuali                    *
      *            *---------------------------------------------------*
               10  w-det-esp-cec-pr1      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-esp-cec-ct1      pic  9(02)                  .
               10  w-det-esp-cec-ct2      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det esposizione cliente                      *
      *        *-------------------------------------------------------*
           05  w-det-esp-cli.
               10  w-det-esp-cli-wfd      pic s9(13)                  .
               10  w-det-esp-cli-wac      pic s9(13)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione fatturato annuo  *
      *    * cliente, in valuta base                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/despcec0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-esp-cec              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-esp-cec-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-esp-cec-tip-ope    =    "EC"
                     perform det-esp-cec-000
                                          thru det-esp-cec-999
      *                  *---------------------------------------------*
      *                  * Visualizzazione esposizione                 *
      *                  *---------------------------------------------*
           else if   d-esp-cec-tip-ope    =    "VE"
                     perform vis-esp-cec-000
                                          thru vis-esp-cec-999
      *                  *---------------------------------------------*
      *                  * Fido cliente                                *
      *                  *---------------------------------------------*
           else if   d-esp-cec-tip-ope    =    "FC"
                     perform det-fid-cli-000
                                          thru det-fid-cli-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-esp-cec-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-esp-cec-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-esp-cec-tip-ope    =    "C?"
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
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no gestione del fido                     *
      *                  *---------------------------------------------*
           perform   prs-snx-fid-000      thru prs-snx-fid-999        .
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cec]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcec"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *                  *---------------------------------------------*
      *                  * [ccc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Personalizzazione relativa a si/no gestione del Fido      *
      *    *-----------------------------------------------------------*
       prs-snx-fid-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep[snx-fid]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-fid
           else      move  spaces         to   w-prs-snx-fid          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-fid-snx    not  = "N"
                     move  "S"            to   w-prs-snx-fid-snx      .
           if        w-prs-snx-fid-orc    not  = "N"
                     move  "S"            to   w-prs-snx-fid-orc      .
           if        w-prs-snx-fid-ods    not  = "N"
                     move  "S"            to   w-prs-snx-fid-ods      .
           if        w-prs-snx-fid-bol    not  = "N"
                     move  "S"            to   w-prs-snx-fid-bol      .
           if        w-prs-snx-fid-gep    not  = "N"
                     move  "S"            to   w-prs-snx-fid-gep      .
           if        w-prs-snx-snx-acr    not  = "N"
                     move  "S"            to   w-prs-snx-snx-acr      .
       prs-snx-fid-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
       rou-cls-fls-200.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cec]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcec"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *                  *---------------------------------------------*
      *                  * [ccc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
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
                     move  spaces         to   d-esp-cec-exi-sts
           else      move  "#"            to   d-esp-cec-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione esposizione cliente, in valuta base   "EC" *
      *    *-----------------------------------------------------------*
       det-esp-cec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [cec]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcec"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *              *-------------------------------------------------*
      *              * Lettura record [cec]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      d-esp-cec-cod-cli    to   rf-cec-cod-cli         .
           move      "pgm/gep/fls/ioc/obj/iofcec"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cec                 .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori letti                    *
      *              *-------------------------------------------------*
           move      rf-cec-prg-val (1)   to   d-esp-cec-esp-orc      .
           move      rf-cec-prg-val (2)   to   d-esp-cec-esp-ods      .
           move      rf-cec-prg-val (3)   to   d-esp-cec-esp-bol      .
           move      rf-cec-prg-val (4)   to   d-esp-cec-esp-gep      .
           move      rf-cec-prg-val (5)   to   d-esp-cec-esp-v01      .
           move      rf-cec-prg-val (6)   to   d-esp-cec-esp-v02      .
           move      rf-cec-dat-agg       to   d-esp-cec-dat-agg      .
      *              *-------------------------------------------------*
      *              * Filtraggio valori da personalizzazioni          *
      *              *-------------------------------------------------*
           if        w-prs-snx-fid-orc    not  = "S"
                     move  zero           to   d-esp-cec-esp-orc      .
      *
           if        w-prs-snx-fid-ods    not  = "S"
                     move  zero           to   d-esp-cec-esp-ods      .
      *
           if        w-prs-snx-fid-bol    not  = "S"
                     move  zero           to   d-esp-cec-esp-bol      .
      *
           if        w-prs-snx-fid-gep    not  = "S"
                     move  zero           to   d-esp-cec-esp-gep      .
      *              *-------------------------------------------------*
      *              * Determinazione fido                             *
      *              *-------------------------------------------------*
           perform   det-fid-cli-000      thru det-fid-cli-999        .
       det-esp-cec-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-esp-cec-999.
       det-esp-cec-999.
           exit.

      *    *===========================================================*
      *    * Determinazione Fido cliente                          "FC" *
      *    *-----------------------------------------------------------*
       det-fid-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-esp-cec-max-fid      .
           move      zero                 to   d-esp-cec-dat-fid      .
           move      spaces               to   d-esp-cec-snx-acr      .
           move      zero                 to   d-esp-cec-imp-acr      .
           move      zero                 to   d-esp-cec-scd-acr      .
       det-fid-cli-100.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       det-fid-cli-120.
      *                  *---------------------------------------------*
      *                  * Test su dati in input                       *
      *                  *---------------------------------------------*
           if        d-esp-cec-cod-cli    =    zero
                     go to det-fid-cli-900.
       det-fid-cli-200.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [ccc] per determinazione     *
      *                  * Fido massimo cliente                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record                  *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *                      *-----------------------------------------*
      *                      * Lettura record                          *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      d-esp-cec-cod-cli    to   rf-ccc-cod-cli         .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preventive                  *
      *                  *---------------------------------------------*
           if        rf-ccc-max-fid       not  numeric
                     move  zero           to   rf-ccc-max-fid         .
           if        rf-ccc-dat-fid       not  numeric
                     move  zero           to   rf-ccc-dat-fid         .
           if        rf-ccc-imp-acr       not  numeric
                     move  zero           to   rf-ccc-imp-acr         .
           if        rf-ccc-dat-acr       not  numeric
                     move  zero           to   rf-ccc-dat-acr         .
           if        rf-ccc-scd-acr       not  numeric
                     move  zero           to   rf-ccc-scd-acr         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori letti                *
      *                  *---------------------------------------------*
           move      rf-ccc-max-fid       to   d-esp-cec-max-fid      .
           move      rf-ccc-dat-fid       to   d-esp-cec-dat-fid      .
           move      rf-ccc-snx-acr       to   d-esp-cec-snx-acr      .
           move      rf-ccc-imp-acr       to   d-esp-cec-imp-acr      .
           move      rf-ccc-scd-acr       to   d-esp-cec-scd-acr      .
      *                  *---------------------------------------------*
      *                  * Filtraggio valori da personalizzazioni      *
      *                  *---------------------------------------------*
           if        w-prs-snx-snx-acr    not  = "S"
                     move  "N"            to   d-esp-cec-snx-acr
                     move  zero           to   d-esp-cec-imp-acr
                     move  zero           to   d-esp-cec-scd-acr      .
       det-fid-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-fid-cli-999.
       det-fid-cli-999.
           exit.

      *    *===========================================================*
      *    * Espansione per informazioni su esposizione cliente   "VE" *
      *    *-----------------------------------------------------------*
       vis-esp-cec-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
       vis-esp-cec-050.
      *              *-------------------------------------------------*
      *              * Determinazione esposizione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           perform   det-esp-cec-000      thru det-esp-cec-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito determinazione                *
      *                  *---------------------------------------------*
           if        d-esp-cec-esp-orc    =    zero  and
                     d-esp-cec-esp-ods    =    zero  and
                     d-esp-cec-esp-bol    =    zero  and
                     d-esp-cec-esp-gep    =    zero  and
                     d-esp-cec-max-fid    =    zero  and
                     d-esp-cec-imp-acr    =    zero
                     go to vis-esp-cec-900.
       vis-esp-cec-080.
      *              *-------------------------------------------------*
      *              * Calcoli preliminari                             *
      *              *-------------------------------------------------*
           move      d-esp-cec-max-fid    to   w-det-esp-cli-wfd      .
           move      d-esp-cec-imp-acr    to   w-det-esp-cli-wac      .
      *
           if        w-det-esp-cli-wfd    =    zero
                     go to vis-esp-cec-085.
      *
           if        c-dec                =    2
                     multiply  100        by   w-det-esp-cli-wfd
           else if   c-dec                =    1
                     multiply  10         by   w-det-esp-cli-wfd      .
       vis-esp-cec-085.
           if        w-det-esp-cli-wac    =    zero
                     go to vis-esp-cec-090.
      *
           if        c-dec                =    2
                     multiply  100        by   w-det-esp-cli-wac
           else if   c-dec                =    1
                     multiply  10         by   w-det-esp-cli-wac      .
       vis-esp-cec-090.
      *              *-------------------------------------------------*
      *              * Eventuale correttivo ordinato                   *
      *              *-------------------------------------------------*
           add       d-esp-cec-esp-coi    to   d-esp-cec-esp-orc      .   
       vis-esp-cec-100.
      *              *-------------------------------------------------*
      *              * Box di espansione                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-esp-cec-200.
      *                  *---------------------------------------------*
      *                  * Prompt per valori                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "                 CONTROLLO ESPOSIZIONE CLIENTE    
      -              "              "     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Data aggiornamento                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      58                   to   v-pos                  .
           move      "(Agg.         )"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ordini inevasi                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Ordini inevasi :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Ordini di sped.:"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Bolle da fatturare                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Bolle da fatt. :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Scadenze                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Scadenze       :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Fido interno                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "| Fido interno  :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Importo fido Payline                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      "| Fido Payline  :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "TOTALE         :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Oltre fido Payline                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Oltre Payline :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Oltre fido interno                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Oltre FIDO     :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-esp-cec-400.
      *                  *---------------------------------------------*
      *                  * Valori                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data aggiornamento                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      d-esp-cec-dat-agg    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale ordini inevasi                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      14                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-orc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale ordini di spedizione             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      15                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-ods    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale bolle da fatturare               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      16                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-bol    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale scadenze                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      17                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-gep    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Fido interno                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      14                   to   v-lin                  .
           move      58                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      w-det-esp-cli-wfd    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Importo fido Payline                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      16                   to   v-lin                  .
           move      58                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      w-det-esp-cli-wac    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Totale                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      19                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-orc    to   v-num                  .
           add       d-esp-cec-esp-ods    to   v-num                  .
           add       d-esp-cec-esp-bol    to   v-num                  .
           add       d-esp-cec-esp-gep    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Oltre Payline                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da visualizzare             *
      *                          *-------------------------------------*
           if        d-esp-cec-snx-acr    not  = "S"
                     go to vis-esp-cec-600.
           if        w-det-esp-cli-wac    =    zero
                     go to vis-esp-cec-600.
      *                          *-------------------------------------*
      *                          * Importo                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      19                   to   v-lin                  .
           move      58                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-orc    to   v-num                  .
           add       d-esp-cec-esp-ods    to   v-num                  .
           add       d-esp-cec-esp-bol    to   v-num                  .
           add       d-esp-cec-esp-gep    to   v-num                  .
           subtract  w-det-esp-cli-wac    from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-esp-cec-600.
      *                      *-----------------------------------------*
      *                      * Oltre fido interno                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      20                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "BG"                 to   v-edm                  .
           move      d-esp-cec-esp-orc    to   v-num                  .
           add       d-esp-cec-esp-ods    to   v-num                  .
           add       d-esp-cec-esp-bol    to   v-num                  .
           add       d-esp-cec-esp-gep    to   v-num                  .
           subtract  w-det-esp-cli-wfd    from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompt per presa visione                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione carattere di presa visione     *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      71                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-esp-cec-800.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-esp-cec-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-esp-cec-999.
       vis-esp-cec-999.
           exit.

