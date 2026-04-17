       Identification Division.
       Program-Id.                                 aindeml0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    azi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/05/04    *
      *                       Ultima revisione:    NdK del 17/04/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione indirizzo e-mail        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-ind-eml-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "AC"                 *
      *                                                                *
      *                       w-cod-ind-eml-ind : indirizzo e-mail     *
      *                                                                *
      *                       w-cod-ind-eml-lin : linea accettazione   *
      *                                                                *
      *                       w-cod-ind-eml-pos : pos.  accettazione   *
      *                                                                *
      *                       w-cod-ind-eml-dln : linea denominazione  *
      *                                           archivio             *
      *                                                                *
      *                       w-cod-ind-eml-dps : posizione denominaz. *
      *                                           archivio             *
      *                                                                *
      *                                                                *
      *              Output : w-cod-ind-eml-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-ind-eml-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-ind-eml-ind : indirizzo e-mail     *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-ind-eml-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-ind-eml-ind : indirizzo e-mail     *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-ind-eml-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-ind-eml-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-ind-eml-ind : indirizzo e-mail     *
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
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .

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
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux-ind-eml.
           05  w-aux-ind-eml-svv          pic  x(80)                  .
           05  w-aux-ind-eml-ec0.
               10  w-aux-ind-eml-e80      pic  x(80)                  .
           05  w-aux-ind-eml-num          pic  9(10)                  .
           05  w-aux-ind-eml-tpr          pic  x(01)                  .
           05  w-aux-ind-eml-nli          pic  9(02)                  .
           05  w-aux-ind-eml-crb          pic  9(04)                  .
           05  w-aux-ind-eml-crc          pic  9(04)                  .
           05  w-aux-ind-eml-cpb          pic  9(04)                  .
           05  w-aux-ind-eml-cpa          pic  9(04)                  .
           05  w-aux-ind-eml-bix          pic  9(04)                  .
           05  w-aux-ind-eml-buf
                               occurs 333.
               10  w-aux-ind-eml-tip      pic  9(02)                  .
               10  w-aux-ind-eml-cod      pic  9(07)                  .
               10  w-aux-ind-eml-ind      pic  x(80)                  .
               10  w-aux-ind-eml-den      pic  x(40)                  .
               10  w-aux-ind-eml-int      pic  x(40)                  .
           05  w-aux-ind-eml-ltp          pic  x(17)                  .
           05  w-aux-ind-eml-le1          pic  x(03)                  .
           05  w-aux-ind-eml-le2          pic  x(03)                  .
           05  w-aux-ind-eml-dup          pic  x(30)                  .
           05  w-aux-ind-eml-dmx.
               10  w-aux-ind-eml-dch
                               occurs 40  pic  x(01)                  .
           05  w-aux-ind-eml-c01          pic  9(04)                  .
           05  w-aux-ind-eml-c02          pic  9(04)                  .
           05  w-aux-ind-eml-c03          pic  9(04)                  .
           05  w-aux-ind-eml-c04          pic  9(04)                  .
           05  w-aux-ind-eml-c05          pic  9(04)                  .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-ble.
           05  w-ble-max                  pic  9(02)                  .
           05  w-ble-flg                  pic  x(01)                  .
           05  w-ble-str.
               10  w-ble-chr    occurs 40 pic  x(01)                  .
           05  w-ble-ctr                  pic  9(02)                  .

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
      *    * Link-area per accettazione indirizzo e-mail               *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/aindeml0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-ind-eml
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
           if        w-cod-ind-eml-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-ind-eml-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-ind-eml-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-ind-eml-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-ind-eml-ope    =    "A+" or
                     w-cod-ind-eml-ope    =    "I+" or
                     w-cod-ind-eml-ope    =    "F+"
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
      *                  * Open file [adc]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
       opn-999.
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
      *                  * Close file [adc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
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
                     move  spaces         to   w-cod-ind-eml-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk (04)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indirizzo e-mail                            *
      *                  *---------------------------------------------*
           move      w-cod-ind-eml-ind    to   w-cod-ind-eml-alf      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing originale               *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-ind-eml-s70      .
      *                  *---------------------------------------------*
      *                  * User function keys originali epurate        *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-ind-eml-s90      .
       acc-500.
      *              *-------------------------------------------------*
      *              * Preparazione valore editato per accettazione in *
      *              * formato alfanumerico                            *
      *              *-------------------------------------------------*
           perform   edt-000              thru edt-999                .
      *              *-------------------------------------------------*
      *              * Accettazione in formato alfanumerico            *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-ind-eml-s90    to   v-ufk                  .
           move      w-cod-ind-eml-lin    to   v-lin                  .
           move      w-cod-ind-eml-pos    to   v-pos                  .
           move      w-aux-ind-eml-e80    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-ind-eml-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-ind-eml-ope    =    "A+"
                     go to aco-400
           else if   w-cod-ind-eml-ope    =    "F+"
                     go to aco-200.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Insr                            *
      *              *-------------------------------------------------*
       aco-150.
      *                  *---------------------------------------------*
      *                  * Reimpostazione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione valore editato per accet-  *
      *                      * tazione in formato alfanumerico         *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Accettazione in formato alfanumerico    *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-ind-eml-s90    to   v-ufk                  .
           move      w-cod-ind-eml-lin    to   v-lin                  .
           move      w-cod-ind-eml-pos    to   v-pos                  .
           move      w-aux-ind-eml-e80    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-ind-eml-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro per Find                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore attuale di v-alf in area di salva-   *
      *                  * taggio                                      *
      *                  *---------------------------------------------*
           move      v-alf                to   w-aux-ind-eml-svv      .
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione minima             *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-ind-eml-dup      .
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione massima            *
      *                  *---------------------------------------------*
           move      all   "z"            to   w-aux-ind-eml-dmx      .
      *                  *---------------------------------------------*
      *                  * Preparazione tipo ricerca : per Find        *
      *                  *---------------------------------------------*
           move      "F"                  to   w-aux-ind-eml-tpr      .
      *                  *---------------------------------------------*
      *                  * Preparazione valore massimo di ricerca      *
      *                  *---------------------------------------------*
           move      w-aux-ind-eml-svv    to   w-aux-ind-eml-dmx      .
           move      40                   to   w-aux-ind-eml-c01      .
       aco-220.
           if        w-aux-ind-eml-c01    >    zero
                     if    w-aux-ind-eml-dch
                          (w-aux-ind-eml-c01)
                                          =    spaces
                           move     "z"   to   w-aux-ind-eml-dch
                                              (w-aux-ind-eml-c01)
                           subtract 1     from w-aux-ind-eml-c01
                           go to aco-220.
      *                  *---------------------------------------------*
      *                  * Ad interrogazione con box locale            *
      *                  *---------------------------------------------*
           go to     aco-650.
       aco-400.
      *              *-------------------------------------------------*
      *              * Se rientro dopo impostazione non terminata da   *
      *              * Find ne' da Insr                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt                              *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-425.
      *                      *-----------------------------------------*
      *                      * Ripristino valore originale             *
      *                      *-----------------------------------------*
           move      w-cod-ind-eml-alf    to   w-cod-ind-eml-ind      .
      *                      *-----------------------------------------*
      *                      * Preparazione e-mail editato             *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice e-mail originale *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-ind-eml-lin    to   v-lin                  .
           move      w-cod-ind-eml-pos    to   v-pos                  .
           move      w-aux-ind-eml-e80    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-ind-eml-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore impostato             *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cod-ind-eml-ind      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di comodo          *
      *                  *---------------------------------------------*
           move      w-cod-ind-eml-ind    to   w-cod-ind-eml-alf      .
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded, e se si a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      w-cod-ind-eml-alf    to   w-ble-str              .
           move      02                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            not  = spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore impostato *
      *                  *---------------------------------------------*
           if        w-cod-ind-eml-alf    =    "-"
                     go to aco-550.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se valore impostato normale                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-ind-eml-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-550.
      *                  *---------------------------------------------*
      *                  * Se valore impostato pari a '-', per esegui- *
      *                  * re la ricerca per descrizione               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test che esistano linea e posizione per *
      *                      * la descrizione, e se no a reimpostazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        w-cod-ind-eml-dln    =    zero or
                     w-cod-ind-eml-dps    =    zero
                     go to aco-100.
       aco-560.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-ind-eml-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-ind-eml-dln    to   v-lin                  .
           move      w-cod-ind-eml-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-570.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-ind-eml-dln    to   v-lin                  .
           move      w-cod-ind-eml-dps    to   v-pos                  .
           move      w-aux-ind-eml-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-ind-eml-dup      .
       aco-580.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-600.
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-ind-eml-dln    to   v-lin                  .
           move      w-cod-ind-eml-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-590.
      *                      *-----------------------------------------*
      *                      * Ripristino v-alf al valore salvato      *
      *                      *-----------------------------------------*
           move      w-aux-ind-eml-svv    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * A reimpostazione codice                 *
      *                      *-----------------------------------------*
           go to     aco-100.
       aco-600.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-610.
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-ind-eml-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-610.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a spaces : trattamento  *
      *                      * come per il tasto Up                    *
      *                      *-----------------------------------------*
           if        w-aux-ind-eml-dup    =    spaces
                     go to aco-590.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-ind-eml-dup    to   w-aux-ind-eml-dmx      .
           move      40                   to   w-aux-ind-eml-c01      .
       aco-620.
           if        w-aux-ind-eml-c01    >    zero
                     if    w-aux-ind-eml-dch
                          (w-aux-ind-eml-c01)
                                          =    spaces
                           move     "z"   to   w-aux-ind-eml-dch
                                              (w-aux-ind-eml-c01)
                           subtract 1     from w-aux-ind-eml-c01
                           go to aco-620.
      *                      *-----------------------------------------*
      *                      * Tipo ricerca : per descrizione          *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-ind-eml-tpr      .
       aco-650.
      *                      *-----------------------------------------*
      *                      * Lettura e bufferizzazione fino ad un    *
      *                      * massimo di 333 records con descrizio-   *
      *                      * ne compresa tra il minimo ed il mas-    *
      *                      * simo                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Azzeramento contatore records nel   *
      *                          * buffer                              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-ind-eml-crb      .
       aco-655.
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-aux-ind-eml-tpr    =    "F"
                     go to aco-656
           else if   w-aux-ind-eml-tpr    =    "D"
                     go to aco-657.
       aco-656.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Find     *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Start per indirizzo         *
      *                                  *-----------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMARC    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "EML"                to   rf-adc-tip-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      w-aux-ind-eml-svv    to   rf-adc-num-con         .
           move      zero                 to   rf-adc-tip-arc         .
           move      zero                 to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                                  *-----------------------------*
      *                                  * Se errore : a fine file,    *
      *                                  * altrimenti continuazione    *
      *                                  *-----------------------------*
           if        f-sts                =    e-not-err
                     go to aco-660
           else      go to aco-700.
       aco-657.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per descri-  *
      *                              * zione                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Start per denominazione di  *
      *                                  * ricerca                     *
      *                                  *-----------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CONARC    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "EML"                to   rf-adc-tip-con         .
           move      w-aux-ind-eml-dup    to   rf-adc-des-key         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      spaces               to   rf-adc-num-con         .
           move      zero                 to   rf-adc-tip-arc         .
           move      zero                 to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                                  *-----------------------------*
      *                                  * Se errore : a fine file,    *
      *                                  * altrimenti continuazione    *
      *                                  *-----------------------------*
           if        f-sts                =    e-not-err
                     go to aco-660
           else      go to aco-700.
       aco-660.
      *                          *-------------------------------------*
      *                          * Read next                           *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                          *-------------------------------------*
      *                          * Test se At End                      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-700.
       aco-665.
      *                          *-------------------------------------*
      *                          * Test sul max                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-aux-ind-eml-tpr    =    "F"
                     go to aco-666
           else if   w-aux-ind-eml-tpr    =    "D"
                     go to aco-667.
       aco-666.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Find     *
      *                              *---------------------------------*
           if        rf-adc-tip-con       not  = "EML"
                     go to aco-700.
           if        rf-adc-num-con       >    w-aux-ind-eml-dmx
                     go to aco-700
           else      go to aco-670.
       aco-667.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per descri-  *
      *                              * zione                           *
      *                              *---------------------------------*
           if        rf-adc-tip-con       not  = "EML"
                     go to aco-700.
           if        rf-adc-des-key       >    w-aux-ind-eml-dmx
                     go to aco-700
           else      go to aco-670.
       aco-670.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-ind-eml-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-ind-eml-crb    >    333
                     subtract  1          from w-aux-ind-eml-crb
                     go to aco-700.
       aco-675.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo archivio                   *
      *                              *---------------------------------*
           move      rf-adc-tip-arc       to   w-aux-ind-eml-tip
                                              (w-aux-ind-eml-crb)     .
      *                              *---------------------------------*
      *                              * Codice archivio                 *
      *                              *---------------------------------*
           move      rf-adc-cod-arc       to   w-aux-ind-eml-cod
                                              (w-aux-ind-eml-crb)     .
      *                              *---------------------------------*
      *                              * Indirizzo e-mail                *
      *                              *---------------------------------*
           move      rf-adc-num-con       to   w-aux-ind-eml-ind
                                              (w-aux-ind-eml-crb)     .
      *                              *---------------------------------*
      *                              * Denominazione archivio          *
      *                              *---------------------------------*
           move      rf-adc-des-key       to   w-aux-ind-eml-den
                                              (w-aux-ind-eml-crb)     .
      *                              *---------------------------------*
      *                              * Interlocutore                   *
      *                              *---------------------------------*
           move      rf-adc-int-con       to   w-aux-ind-eml-int
                                              (w-aux-ind-eml-crb)     .
      *                          *-------------------------------------*
      *                          * Riciclo nella lettura               *
      *                          *-------------------------------------*
           go to aco-660.
       aco-700.
      *                      *-----------------------------------------*
      *                      * Esame dei risultati della lettura con   *
      *                      * bufferizzazione dei max 333 records     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * di records bufferizzati             *
      *                          *-------------------------------------*
           if        w-aux-ind-eml-crb    =    zero
                     go to aco-710
           else  if  w-aux-ind-eml-crb    =    1
                     go to aco-730
           else      go to aco-750.
       aco-710.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a zero                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-aux-ind-eml-tpr    =    "F"
                     go to aco-712
           else      go to aco-714.
       aco-712.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Find     *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Salvataggio immagine video  *
      *                                  *-----------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Video in Off                *
      *                                  *-----------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione box vuoto   *
      *                                  *-----------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      13                   to   v-lto                  .
           move      64                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Messaggio entro il box      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      "Nessuno indirizzo trovato !             "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Parentesi quadre            *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Video in On                 *
      *                                  *-----------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Accettazione carattere      *
      *                                  *-----------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      61                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * E reimpostazione            *
      *                                  *-----------------------------*
           go to     aco-150.
       aco-714.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Descri-  *
      *                              * zione o per Mnemonico           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione puntini in  *
      *                                  * area di accettazione        *
      *                                  *-----------------------------*
           if        w-cod-ind-eml-dln    =    zero
                     go to aco-716.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-ind-eml-dln    to   v-lin                  .
           move      w-cod-ind-eml-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-716.
      *                                  *-----------------------------*
      *                                  * A reimpostazione codice     *
      *                                  *-----------------------------*
           go to     aco-590.
       aco-730.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a 1                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo ricerca per Find : al   *
      *                              * trattamento visualizzazione     *
      *                              *---------------------------------*
           if        w-aux-ind-eml-tpr    =    "F"
                     go to aco-750.
      *                              *---------------------------------*
      *                              * Preparazione indice sul primo   *
      *                              * elemento del buffer             *
      *                              *---------------------------------*
           move      1                    to   w-aux-ind-eml-bix      .
       aco-740.
      *                              *---------------------------------*
      *                              * Indirizzo e-mail in uscita      *
      *                              *---------------------------------*
           move      w-aux-ind-eml-ind
                    (w-aux-ind-eml-bix)   to   w-cod-ind-eml-ind      .
      *                              *---------------------------------*
      *                              * Preparazione Indirizzo e-mail   *
      *                              * tato                            *
      *                              *---------------------------------*
           perform   edt-000              thru edt-999                .
      *                              *---------------------------------*
      *                              * Visualizzazione e-mail          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-ind-eml-lin    to   v-lin                  .
           move      w-cod-ind-eml-pos    to   v-pos                  .
           move      w-aux-ind-eml-e80    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Normalizzazione function-key    *
      *                              *---------------------------------*
           move      spaces               to   v-key                  .
      *                              *---------------------------------*
      *                              * Tipo operazione a : non-conti-  *
      *                              * nuazione                        *
      *                              *---------------------------------*
           move      "AC"                 to   w-cod-ind-eml-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-750.
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-ind-eml-crb    to   w-aux-ind-eml-cpb      .
           subtract  1                    from w-aux-ind-eml-cpb      .
           divide    4                    into w-aux-ind-eml-cpb      .
           add       1                    to   w-aux-ind-eml-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-ind-eml-c01      .
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
           move      04                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      68                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Titolo                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      54                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      "                   INDIRIZZI E-MAIL               
      -              "    "               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Sopralineatura                  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      54                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      " -------------------------------------------------
      -              "--- "               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Sottolineatura                  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      54                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      " -------------------------------------------------
      -              "--- "               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina video    *
      *                              * contenente il record attual-    *
      *                              * mente trattato                  *
      *                              *---------------------------------*
           perform   aco-950              thru aco-959                .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-860.
      *                              *---------------------------------*
      *                              * Determinazione numero linea a   *
      *                              * video in funzione del numero    *
      *                              * elemento in tabella trattato,   *
      *                              * di indice (w-aux-ind-eml-c01)   *
      *                              *---------------------------------*
           divide    4                    into w-aux-ind-eml-c01
                                        giving w-aux-ind-eml-c05
                                     remainder w-aux-ind-eml-nli      .
           if        w-aux-ind-eml-nli    =    zero
                     move  4              to   w-aux-ind-eml-nli      .
           multiply  3                    by   w-aux-ind-eml-nli      .
           add       04                   to   w-aux-ind-eml-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-ind-eml-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-ind-eml-c01    <    w-aux-ind-eml-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-ind-eml-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-ind-eml-cpa    <    w-aux-ind-eml-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-ind-eml-nli    to   v-lin                  .
           move      15                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Deviazione a seconda della fun- *
      *                              * ction key impostata             *
      *                              *---------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-880
           else if   v-key                =    "UP  "
                     go to aco-890
           else if   v-key                =    "DOWN"
                     go to aco-900
           else if   v-key                =    "EXIT"
                     go to aco-910
           else if   v-key                =    "NXSC"
                     go to aco-920
           else if   v-key                =    "PRSC"
                     go to aco-930
           else      go to aco-870.
       aco-880.
      *                              *---------------------------------*
      *                              * Se Do oppure Slct               *
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
           move      w-aux-ind-eml-c01    to   w-aux-ind-eml-bix      .
      *                                  *-----------------------------*
      *                                  * Ad uscita dopo preparazione *
      *                                  * e visualizzazione del valo- *
      *                                  * re selezionato              *
      *                                  *-----------------------------*
           go to     aco-740.
       aco-890.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-ind-eml-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-nli    =    07
                     go to aco-940
           else      go to aco-860.
       aco-900.
      *                              *---------------------------------*
      *                              * Se Down o Return                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-c01    =    w-aux-ind-eml-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-ind-eml-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-nli    =    16
                     go to aco-940
           else      go to aco-860.
       aco-910.
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
      *                                  * A reimpostazione            *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-tpr    =    "F"
                     go to aco-590
           else      go to aco-560.
       aco-920.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-ind-eml-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-ind-eml-cpa    to   w-aux-ind-eml-c01      .
           multiply  4                    by   w-aux-ind-eml-c01      .
           subtract  3                    from w-aux-ind-eml-c01      .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-940.
       aco-930.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-ind-eml-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-ind-eml-cpa    to   w-aux-ind-eml-c01      .
           multiply  4                    by   w-aux-ind-eml-c01      .
           subtract  3                    from w-aux-ind-eml-c01      .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-940.
       aco-940.
      *                              *---------------------------------*
      *                              * Visualizzazione pagina video    *
      *                              * contenente il record attual-    *
      *                              * mente trattato                  *
      *                              *---------------------------------*
           perform   aco-950              thru aco-959                .
      *                              *---------------------------------*
      *                              * A reimpostazione function key   *
      *                              *---------------------------------*
           go to     aco-860.
       aco-950.
      *                              *---------------------------------*
      *                              * Subroutine interna di visualiz- *
      *                              * zazione pagina video contenente *
      *                              * il record attualmente trattato  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione numero pagi- *
      *                                  * na attualmente trattata     *
      *                                  *-----------------------------*
           move      w-aux-ind-eml-c01    to   w-aux-ind-eml-c02      .
           add       3                    to   w-aux-ind-eml-c02      .
           divide    4                    into w-aux-ind-eml-c02      .
           move      w-aux-ind-eml-c02    to   w-aux-ind-eml-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-ind-eml-c02      .
           multiply  4                    by   w-aux-ind-eml-c02      .
           add       1                    to   w-aux-ind-eml-c02      .
           add       3 
                     w-aux-ind-eml-c02  giving w-aux-ind-eml-c03      .
           move      w-aux-ind-eml-c03    to   w-aux-ind-eml-c04      .
           if        w-aux-ind-eml-c03    >    w-aux-ind-eml-crb
                     move  w-aux-ind-eml-crb
                                          to   w-aux-ind-eml-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-ind-eml-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Indirizzo e-mail        *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-aux-ind-eml-ind
                    (w-aux-ind-eml-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Codice archivio         *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Editing             *
      *                                          *---------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-ind-eml-cod
                    (w-aux-ind-eml-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Assemblaggio        *
      *                                          *---------------------*
           move      10                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
      *
           if        w-aux-ind-eml-tip
                    (w-aux-ind-eml-c02)   >    40
                     move  "V"            to   w-all-str-cat (1)
           else if   w-aux-ind-eml-tip
                    (w-aux-ind-eml-c02)   >    30
                     move  "A"            to   w-all-str-cat (1)
           else if   w-aux-ind-eml-tip
                    (w-aux-ind-eml-c02)   >    20
                     move  "D"            to   w-all-str-cat (1)
           else if   w-aux-ind-eml-tip
                    (w-aux-ind-eml-c02)   >    10
                     move  "F"            to   w-all-str-cat (1)
           else      move  "C"            to   w-all-str-cat (1)      .
      *
           move      "["                  to   w-all-str-cat (2)      .      
           move      v-edt                to   w-all-str-cat (3)      .      
           move      "]"                  to   w-all-str-cat (4)      .      
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                                          *---------------------*
      *                                          * Visualizzazione     *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Denominazione           *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-ind-eml-den
                    (w-aux-ind-eml-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Interlocutore           *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-ind-eml-int
                    (w-aux-ind-eml-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-ind-eml-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       3                    to   w-aux-ind-eml-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-c02    not  > w-aux-ind-eml-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-c02    >    w-aux-ind-eml-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-ind-eml-crb    not  > 4
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-ind-eml-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-ind-eml-c02      .
           add       3                    to   w-aux-ind-eml-c05      .
           go to     aco-952.
       aco-955.
      *                                  *-----------------------------*
      *                                  * Trattamento finale : visua- *
      *                                  * lizzazione del numero pagi- *
      *                                  * na attuale e del numero di  *
      *                                  * pagine totali               *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-ind-eml-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-ind-eml-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-ind-eml-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-ind-eml-le2      .
           move      spaces               to   w-aux-ind-eml-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-ind-eml-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-ind-eml-le2
                                delimited by   spaces
                                          into w-aux-ind-eml-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-aux-ind-eml-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Editing indirizzo e-mail                                  *
      *    *-----------------------------------------------------------*
       edt-000.
      *              *-------------------------------------------------*
      *              * Editing effettivo                               *
      *              *-------------------------------------------------*
           move      w-cod-ind-eml-ind    to   w-aux-ind-eml-e80      .
       edt-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


