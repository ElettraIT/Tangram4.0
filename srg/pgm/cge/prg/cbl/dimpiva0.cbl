       Identification Division.
       Program-Id.                                 dimpiva0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    iva                 *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/10/07    *
      *                       Ultima revisione:    NdK del 13/06/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione di una imposta in base ad un      *
      * imponibile ed una aliquota e altre operazioni relative al      *
      * codice Iva                                                     *
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
      *        Input  : d-imp-iva-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-iva-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-iva-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-imp-iva-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DI" - Determinazione imposta iva di un importo                *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-iva-tip-ope = "DI"                       *
      *                                                                *
      *                 d-imp-iva-cod-iva = Codice Iva                 *
      *                                     Se zero, deve essere pre-  *
      *                                     sente l'aliquota           *
      *                                                                *
      *                 d-imp-iva-ibl-iva = Imponibile                 *
      *                                                                *
      *                                                                *
      *        Output : d-imp-iva-ims-iva = Imposta calcolata          *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "SI" - Scorporo iva di un importo                              *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-iva-tip-ope = "SI"                       *
      *                                                                *
      *                 d-imp-iva-cod-iva = Codice Iva                 *
      *                                     Se zero, deve essere pre-  *
      *                                     sente l'aliquota           *
      *                                                                *
      *                 d-imp-iva-ibl-iva = Importo da cui scorporare  *
      *                                                                *
      *                                                                *
      *        Output : d-imp-iva-ims-iva = Imposta scorporata         *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CT" - Controllo codice Iva                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-imp-iva-tip-ope = "CT"                       *
      *                                                                *
      *                 d-imp-iva-cod-iva = Codice Iva                 *
      *                                                                *
      *                 d-imp-iva-tip-iva = Tipo movimento Iva         *
      *                                    (opzionale)                 *
      *                                                                *
      *                 Puo' assumere i valori:                        *
      *                                                                *
      *                 - 1 : Dare cliente Italia o Estero non CEE     *
      *                 - 2 : Avere cliente Italia o Estero non CEE    *
      *                 - 3 : Corrispettivi Italia o Estero non CEE    *
      *                 - 4 : Dare fornitore Italia o Estero non CEE   *
      *                 - 5 : Avere fornitore Italia o Estero non CEE  *
      *                 - 6 : Bolla doganale Estero non CEE            *
      *                 - A : Dare cliente Estero CEE                  *
      *                 - B : Avere cliente Estero CEE                 *
      *                 - D : Dare fornitore Estero CEE                *
      *                 - E : Avere fornitore Estero CEE               *
      *                                                                *
      *                 d-imp-iva-tip-ctl = Tipo controllo ___ DA IMPL *
      *                                    (opzionale)                 *
      *                                                                *
      *                 Puo' assumere i valori:                        *
      *                                                                *
      *                 - I : Per controllo di soli imponibili         *
      *                 - X : Per controllo di soli esenti             *
      *                 - O : Per controllo di soli omaggi             *
      *                 - N : Per controllo di soli non detraibili     *
      *                                                                *
      *        Output : d-imp-iva-exi-sts = Flag di uscita             *
      *                                                                *
      *                 d-imp-iva-msg-exi = Messaggio di errore        *
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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work area per determinazioni varie                        *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det totale imposta                           *
      *        *-------------------------------------------------------*
           05  w-det-tot-imp.
      *            *---------------------------------------------------*
      *            * Decimali aliquota                                 *
      *            *---------------------------------------------------*
               10  w-det-tot-imp-dal      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Aliquota                                          *
      *            *---------------------------------------------------*
               10  w-det-tot-imp-ali      pic  9(03)v9(02)            .
               10  w-det-tot-imp-ali-r redefines
                   w-det-tot-imp-ali.
                   15  w-det-tot-imp-int  pic  9(03)                  .
                   15  w-det-tot-imp-dec  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per il calcolo                             *
      *            *---------------------------------------------------*
               10  w-det-tot-imp-ctr      pic  9(02)                  .
               10  w-det-tot-imp-wpa      pic s9(13)                  .
               10  w-det-tot-imp-s11      pic s9(11)                  .
               10  w-det-tot-imp-wpr      pic s9(13)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importi con Iva  *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-imp-iva              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-imp-iva-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-imp-iva-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-imp-iva-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione imposta Iva                  *
      *                  *---------------------------------------------*
           else if   d-imp-iva-tip-ope    =    "DI"
                     perform dim-000      thru dim-999
      *                  *---------------------------------------------*
      *                  * Determinazione scorporo Iva                 *
      *                  *---------------------------------------------*
           else if   d-imp-iva-tip-ope    =    "SI"
                     perform dsi-000      thru dsi-999
      *                  *---------------------------------------------*
      *                  * Controllo codice Iva                        *
      *                  *---------------------------------------------*
           else if   d-imp-iva-tip-ope    =    "CT"
                     perform ctl-000      thru ctl-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-imp-iva-exi-sts      .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       opn-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       opn-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zci]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zci]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   d-imp-iva-exi-sts
           else      move  "#"            to   d-imp-iva-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione imposta Iva                                *
      *    *-----------------------------------------------------------*
       dim-000.
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
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-imp-iva-ims-iva      .
       dim-010.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice a zero : ad uscita                *
      *                  *---------------------------------------------*
           if        d-imp-iva-cod-iva    =    zero
                     go to dim-900.
      *                  *---------------------------------------------*
      *                  * Se imponibile a zero : ad uscita            *
      *                  *---------------------------------------------*
           if        d-imp-iva-ibl-iva    =    zero
                     go to dim-900.
       dim-020.
      *              *-------------------------------------------------*
      *              * Test preliminare se aliquota non detraibile o   *
      *              * omaggio                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ridefinizione codice in input               *
      *                  *---------------------------------------------*
           move      d-imp-iva-cod-iva    to   w-edt-iva-cod          .
      *                  *---------------------------------------------*
      *                  * Test se non detraibili o omaggi             *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-003    =    7
                     go to dim-024
           else if   w-edt-iva-cod-003    =    9
                     go to dim-026
           else      go to dim-030.
       dim-024.
      *                  *---------------------------------------------*
      *                  * Non detraibili                              *
      *                  *---------------------------------------------*
           subtract  700                  from d-imp-iva-cod-iva      .
           go to     dim-030.
       dim-026.
      *                  *---------------------------------------------*
      *                  * Omaggi                                      *
      *                  *---------------------------------------------*
           subtract  900                  from d-imp-iva-cod-iva      .
           go to     dim-030.
       dim-030.
      *              *-------------------------------------------------*
      *              * Lettura preliminare [zci]                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODIVA    "         to   f-key                  .
           move      d-imp-iva-cod-iva    to   rf-zci-cod-iva         .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to dim-050.
       dim-040.
      *              *-------------------------------------------------*
      *              * Se record [zci] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione decimali aliquota           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tot-imp-dal      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione aliquota                    *
      *                  *---------------------------------------------*
           move      d-imp-iva-cod-iva    to   w-edt-iva-cod          .
           move      w-edt-iva-cod-045    to   w-det-tot-imp-ali      .
           move      w-edt-iva-cod-012    to   w-det-tot-imp-dec      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     dim-100.
       dim-050.
      *              *-------------------------------------------------*
      *              * Se record [zci] trovato                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione decimali aliquota           *
      *                  *---------------------------------------------*
           move      rf-zci-dec-ali       to   w-det-tot-imp-dal      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione aliquota                    *
      *                  *---------------------------------------------*
           if        w-det-tot-imp-dal    =    1
                     divide  10           into rf-zci-ali-iva
                                        giving w-det-tot-imp-ali
           else if   w-det-tot-imp-dal    =    2
                     divide  100          into rf-zci-ali-iva
                                        giving w-det-tot-imp-ali
           else      move    rf-zci-ali-iva
                                          to   w-det-tot-imp-ali      .
      *                  *---------------------------------------------*
      *                  * Se aliquota a zero in tabella, si pone il   *
      *                  * codice IVA stesso come aliquota, a meno che *
      *                  * non si tratti di un codice di esenzione     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ridefinizione codice Iva                *
      *                      *-----------------------------------------*
           move      d-imp-iva-cod-iva    to   w-edt-iva-cod          .
      *                      *-----------------------------------------*
      *                      * Test se imponibile                      *
      *                      *-----------------------------------------*
           if        w-edt-iva-cod-003    not  = zero
                     move  zero           to   w-det-tot-imp-ali
                     go to dim-900.
      *                      *-----------------------------------------*
      *                      * Se aliquota a zero                      *
      *                      *-----------------------------------------*
           if        w-det-tot-imp-ali    =    zero
                     move  d-imp-iva-cod-iva
                                          to   w-det-tot-imp-ali      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     dim-100.
       dim-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del regime di arrotonda- *
      *              * mento relativo alla valuta base                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente si desume dal codice della      *
      *                  * valuta base                                 *
      *                  *---------------------------------------------*
           if        c-sgl                =    "LIT"
                     go to dim-300
           else if   c-sgl                =    "EUR"
                     go to dim-500
           else      go to dim-300.
       dim-300.
      *              *-------------------------------------------------*
      *              * Regime tipo "LIRE"                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-det-tot-imp-ali    by   d-imp-iva-ibl-iva
                                        giving w-det-tot-imp-wpa      .
           divide    100                  into w-det-tot-imp-wpa
                                        giving w-det-tot-imp-s11
                                     remainder w-det-tot-imp-wpr      .
           move      w-det-tot-imp-s11    to   w-det-tot-imp-wpa      .
           if        w-det-tot-imp-wpr    >    zero
                     add      1           to   w-det-tot-imp-wpa
           else if   w-det-tot-imp-wpr    <    zero
                     subtract 1           from w-det-tot-imp-wpa      .
      *                  *---------------------------------------------*
      *                  * Valore in uscita                            *
      *                  *---------------------------------------------*
           move      w-det-tot-imp-wpa    to   d-imp-iva-ims-iva      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dim-900.
       dim-500.
      *              *-------------------------------------------------*
      *              * Regime tipo "EURO"                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-det-tot-imp-ali    by   d-imp-iva-ibl-iva
                                        giving w-det-tot-imp-wpa      .
           divide    100                  into w-det-tot-imp-wpa
                                        giving w-det-tot-imp-s11
                                       rounded                        .
           move      w-det-tot-imp-s11    to   w-det-tot-imp-wpa      .
      *                  *---------------------------------------------*
      *                  * Valore in uscita                            *
      *                  *---------------------------------------------*
           move      w-det-tot-imp-wpa    to   d-imp-iva-ims-iva      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dim-900.
       dim-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dim-999.
       dim-999.
           exit.

      *    *===========================================================*
      *    * Determinazione scorporo Iva                               *
      *    *-----------------------------------------------------------*
       dsi-000.
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
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-imp-iva-ims-iva      .
       dsi-010.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice a zero : ad uscita                *
      *                  *---------------------------------------------*
           if        d-imp-iva-cod-iva    =    zero
                     go to dsi-900.
      *                  *---------------------------------------------*
      *                  * Se Importo a zero : ad uscita               *
      *                  *---------------------------------------------*
           if        d-imp-iva-ibl-iva    =    zero
                     go to dsi-900.
       dsi-030.
      *              *-------------------------------------------------*
      *              * Lettura preliminare [zci]                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODIVA    "         to   f-key                  .
           move      d-imp-iva-cod-iva    to   rf-zci-cod-iva         .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to dsi-050.
       dsi-040.
      *              *-------------------------------------------------*
      *              * Se record [zci] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione decimali aliquota           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tot-imp-dal      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione aliquota                    *
      *                  *---------------------------------------------*
           move      d-imp-iva-cod-iva    to   w-det-tot-imp-ali      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     dsi-100.
       dsi-050.
      *              *-------------------------------------------------*
      *              * Se record [zci] trovato                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione decimali aliquota           *
      *                  *---------------------------------------------*
           move      rf-zci-dec-ali       to   w-det-tot-imp-dal      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione aliquota                    *
      *                  *---------------------------------------------*
           move      rf-zci-ali-iva       to   w-det-tot-imp-ali      .
      *                  *---------------------------------------------*
      *                  * Se aliquota a zero in tabella, si pone il   *
      *                  * codice IVA stesso come aliquota             *
      *                  *---------------------------------------------*
           if        w-det-tot-imp-ali    =    zero
                     move  d-imp-iva-cod-iva
                                          to   w-det-tot-imp-ali      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     dsi-100.
       dsi-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del regime di arrotonda- *
      *              * mento relativo alla valuta base                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente si desume dal codice della      *
      *                  * valuta base                                 *
      *                  *---------------------------------------------*
           if        c-sgl                =    "LIT"
                     go to dsi-300
           else if   c-sgl                =    "EUR"
                     go to dsi-500
           else      go to dsi-300.
       dsi-300.
      *              *-------------------------------------------------*
      *              * Regime tipo "LIRE"                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * ATTUALMENTE COME PER EURO                   *
      *                  *---------------------------------------------*
           go to     dsi-500.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dsi-900.
       dsi-500.
      *              *-------------------------------------------------*
      *              * Regime tipo "EURO"                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           move      d-imp-iva-ibl-iva    to   w-det-tot-imp-wpa      .
           multiply  100                  by   w-det-tot-imp-wpa      .
           divide    w-det-tot-imp-ali    into w-det-tot-imp-wpa
                                        giving w-det-tot-imp-wpr      .
           subtract  w-det-tot-imp-wpr    from d-imp-iva-ibl-iva
                                        giving w-det-tot-imp-s11      .
      *                  *---------------------------------------------*
      *                  * Valori in uscita                            *
      *                  *---------------------------------------------*
           move      w-det-tot-imp-s11    to   d-imp-iva-ims-iva      .
           move      w-det-tot-imp-wpr    to   d-imp-iva-ibl-iva      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dsi-900.
       dsi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dsi-999.
       dsi-999.
           exit.

      *    *===========================================================*
      *    * Controllo codice iva                                      *
      *    *-----------------------------------------------------------*
       ctl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo controllo                  *
      *              *                                                 *
      *              * N.B.: attualmente non gestito                   *
      *              *-------------------------------------------------*
           move      spaces               to   d-imp-iva-tip-ctl      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-imp-iva-exi-sts      .
      *              *-------------------------------------------------*
      *              * Normalizzazione messaggio di uscita             *
      *              *-------------------------------------------------*
           move      spaces               to   d-imp-iva-msg-exi      .
      *              *-------------------------------------------------*
      *              * Codice iva in area di comodo ridefinita         *
      *              *-------------------------------------------------*
           move      d-imp-iva-cod-iva    to   w-edt-iva-cod          .
      *              *-------------------------------------------------*
      *              * Se codice iva a zero : uscita                   *
      *              *-------------------------------------------------*
           if        w-edt-iva-cod-345    =    zero
                     go to ctl-999.
      *              *-------------------------------------------------*
      *              * Se codice iva > 999 : errore                    *
      *              *-------------------------------------------------*
           if        w-edt-iva-cod-345    not  > 999
                     go to ctl-100.
       ctl-050.
           move      "Codice iva formalmente scorretto !"
                                          to   d-imp-iva-msg-exi      .
           move      "#"                  to   d-imp-iva-exi-sts      .
           go to     ctl-999.
       ctl-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di codice iva   *
      *              *-------------------------------------------------*
           if        w-edt-iva-cod-003    =    0
                     go to ctl-150.
      *
           if        w-edt-iva-cod-003    =    1
                     go to ctl-200.
      *
           if        w-edt-iva-cod-003    =    3 or
                     w-edt-iva-cod-003    =    4 or
                     w-edt-iva-cod-003    =    5 or
                     w-edt-iva-cod-003    =    6
                     go to ctl-250.
      *
           if        w-edt-iva-cod-003    =    7
                     go to ctl-300.
      *
           if        w-edt-iva-cod-003    =    9
                     go to ctl-350.
      *              *-------------------------------------------------*
      *              * Se codice iva non previsto : errore             *
      *              *-------------------------------------------------*
           go to     ctl-050.
       ctl-150.
      *              *-------------------------------------------------*
      *              * Codice iva 001..099                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo di esistenza del codice           *
      *                  *---------------------------------------------*
           perform   ctl-900              thru ctl-909                .
           go to     ctl-999.
       ctl-200.
      *              *-------------------------------------------------*
      *              * Codice iva 101..199                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo di esistenza del codice           *
      *                  *---------------------------------------------*
           perform   ctl-900              thru ctl-909                .
           if        d-imp-iva-exi-sts    not  = spaces
                     go to ctl-999.
      *                  *---------------------------------------------*
      *                  * Il codice e' ammesso solo per un tipo movi- *
      *                  * mento di corrispettivi                      *
      *                  *---------------------------------------------*
           if        d-imp-iva-tip-iva    =    spaces
                     go to ctl-999.
           if        d-imp-iva-tip-iva    =    "3"
                     go to ctl-999.
           move      "Codice iva ammesso solo per corrispettivi !"
                                          to   d-imp-iva-msg-exi      .
           move      "#"                  to   d-imp-iva-exi-sts      .
           go to     ctl-999.
       ctl-250.
      *              *-------------------------------------------------*
      *              * Codice iva 300..399                             *
      *              *            400..499                             *
      *              *            500..599                             *
      *              *            600..699                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo di esistenza del codice           *
      *                  *---------------------------------------------*
           perform   ctl-900              thru ctl-909                .
           go to     ctl-999.
       ctl-300.
      *              *-------------------------------------------------*
      *              * Codice iva 700..799                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo di esistenza del codice           *
      *                  *---------------------------------------------*
           perform   ctl-900              thru ctl-909                .
           if        d-imp-iva-exi-sts    not  = spaces
                     go to ctl-999.
      *                  *---------------------------------------------*
      *                  * Il codice e' ammesso solo per un tipo movi- *
      *                  * mento per fornitori                         *
      *                  *---------------------------------------------*
           if        d-imp-iva-tip-iva    =    spaces
                     go to ctl-999.
           if        d-imp-iva-tip-iva    =    "4" or
                     d-imp-iva-tip-iva    =    "5" or
                     d-imp-iva-tip-iva    =    "6" or
                     d-imp-iva-tip-iva    =    "D" or
                     d-imp-iva-tip-iva    =    "E"
                     go to ctl-999.
           move      "Codice iva ammesso solo per fornitori !"
                                          to   d-imp-iva-msg-exi      .
           move      "#"                  to   d-imp-iva-exi-sts      .
           go to     ctl-999.
       ctl-350.
      *              *-------------------------------------------------*
      *              * Codice iva 900..999                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo di esistenza del codice           *
      *                  *---------------------------------------------*
           perform   ctl-900              thru ctl-909                .
           if        d-imp-iva-exi-sts    not  = spaces
                     go to ctl-999.
      *                  *---------------------------------------------*
      *                  * Il codice non e' ammesso per un tipo movi-  *
      *                  * mento di corrispettivi                      *
      *                  *---------------------------------------------*
           if        d-imp-iva-tip-iva    =    spaces
                     go to ctl-999.
           if        d-imp-iva-tip-iva    not  = "3"
                     go to ctl-999.
           move      "Codice iva non ammesso per corrispettivi !"
                                          to   d-imp-iva-msg-exi      .
           move      "#"                  to   d-imp-iva-exi-sts      .
           go to     ctl-999.
       ctl-900.
      *              *=================================================*
      *              * Subroutine interna di controllo esistenza del   *
      *              * codice iva                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ridefinizione codice in input               *
      *                  *---------------------------------------------*
           move      d-imp-iva-cod-iva    to   w-edt-iva-cod          .
      *                  *---------------------------------------------*
      *                  * Test se non detraibili o omaggi             *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-003    =    7 or
                     w-edt-iva-cod-003    =    9
                     move  zero           to   w-edt-iva-cod-003
                     move  w-edt-iva-cod  to   d-imp-iva-cod-iva      .
       ctl-905.
      *              *-------------------------------------------------*
      *              * Lettura di controllo [zci]                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODIVA    "         to   f-key                  .
           move      d-imp-iva-cod-iva    to   rf-zci-cod-iva         .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-908.
       ctl-906.
      *                  *---------------------------------------------*
      *                  * Test se imponibile                          *
      *                  *---------------------------------------------*
           if        w-edt-iva-cod-003    =    zero
                     move  spaces         to   d-imp-iva-msg-exi
           else      move  rf-zci-des-int to   d-imp-iva-msg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ctl-909.
       ctl-908.
      *              *-------------------------------------------------*
      *              * Uscita con errore                               *
      *              *-------------------------------------------------*
           move      "#"                  to   d-imp-iva-exi-sts      .
           move      "Codice iva non esistente !"
                                          to   d-imp-iva-msg-exi      .
       ctl-909.
           exit.
       ctl-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

