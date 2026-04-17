       Identification Division.
       Program-Id.                                 acdeycp0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cdp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/10/92    *
      *                       Ultima revisione:    NdK del 14/10/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice tipo movimento   *
      *                    commesse di produzione                      *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-ycp-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "AC"                 *
      *                                                                *
      *                       w-cod-des-ycp-cod : codice tipo movim.   *
      *                                                                *
      *                       w-cod-des-ycp-lin : linea codice         *
      *                                                                *
      *                       w-cod-des-ycp-pos : posizione codice     *
      *                                                                *
      *                       w-cod-des-ycp-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-des-ycp-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-ycp-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-ycp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-des-ycp-cod : codice tipo movim.   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-ycp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-des-ycp-cod : codice tipo movim.   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-des-ycp-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-ycp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-des-ycp-cod : codice tipo movim.   *
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
      *        * [ycp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfycp"                          .

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
       01  w-aux-cde-ycp.
           05  w-aux-cde-ycp-nli          pic  9(02)                  .
           05  w-aux-cde-ycp-crb          pic  9(02)                  .
           05  w-aux-cde-ycp-cpb          pic  9(02)                  .
           05  w-aux-cde-ycp-cpa          pic  9(02)                  .
           05  w-aux-cde-ycp-bix          pic  9(02)                  .
           05  w-aux-cde-ycp-buf
                               occurs 30.
               10  w-aux-cde-ycp-bco      pic  x(05)                  .
               10  w-aux-cde-ycp-bde      pic  x(30)                  .
           05  w-aux-cde-ycp-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-cde-ycp-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-cde-ycp-lt2      pic  9(01)                  .
           05  w-aux-cde-ycp-dup          pic  x(30)                  .
           05  w-aux-cde-ycp-dmx.
               10  w-aux-cde-ycp-dch
                               occurs 30  pic  x(01)                  .
           05  w-aux-cde-ycp-c01          pic  9(02)                  .
           05  w-aux-cde-ycp-c02          pic  9(02)                  .
           05  w-aux-cde-ycp-c03          pic  9(02)                  .
           05  w-aux-cde-ycp-c04          pic  9(02)                  .
           05  w-aux-cde-ycp-c05          pic  9(02)                  .

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
      *    * Link-area per accettazione tipo movimento commesse di     *
      *    * produzione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cdp/prg/cpy/acdeycp0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-des-ycp
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
           if        w-cod-des-ycp-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-des-ycp-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-des-ycp-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-des-ycp-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-des-ycp-ope    =    "A+" or
                     w-cod-des-ycp-ope    =    "I+" or
                     w-cod-des-ycp-ope    =    "F+"
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
      *                  * Open file [ycp]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
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
      *                  * Close file [ycp]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
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
                     move  spaces         to   w-cod-des-ycp-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcdp0110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcdp0100"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-200.
           if        v-pfk (04)           =    "INSR"
                     move  spaces         to   v-pfk (04)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento di default            *
      *                  *---------------------------------------------*
           move      w-cod-des-ycp-cod    to   w-cod-des-ycp-s01      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing                         *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-des-ycp-s80      .
      *                  *---------------------------------------------*
      *                  * User function keys                          *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-des-ycp-s90      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-ycp-s90    to   v-ufk                  .
           move      w-cod-des-ycp-lin    to   v-lin                  .
           move      w-cod-des-ycp-pos    to   v-pos                  .
           move      w-cod-des-ycp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-des-ycp-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-des-ycp-ope    =    "A+"
                     go to aco-400
           else if   w-cod-des-ycp-ope    =    "F+"
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
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-ycp-s90    to   v-ufk                  .
           move      w-cod-des-ycp-lin    to   v-lin                  .
           move      w-cod-des-ycp-pos    to   v-pos                  .
           move      w-cod-des-ycp-cod    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-des-ycp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Find                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select eventuali       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Variabile 'cod-tcp'                     *
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-tcp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se e' stata effettuata *
      *                  * una selezione oppure no                     *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to aco-250.
       aco-225.
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-150.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Se selezione effettuata                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore selezionato in uscita            *
      *                      *-----------------------------------------*
           move      s-alf                to   w-cod-des-ycp-cod      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore selezionato      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-ycp-lin    to   v-lin                  .
           move      w-cod-des-ycp-pos    to   v-pos                  .
           move      w-cod-des-ycp-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-ycp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt                              *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-425.
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione valore originale      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-ycp-lin    to   v-lin                  .
           move      w-cod-des-ycp-pos    to   v-pos                  .
           move      w-cod-des-ycp-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-ycp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore impostato             *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cod-des-ycp-cod      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di comodo          *
      *                  *---------------------------------------------*
           move      w-cod-des-ycp-cod    to   w-cod-des-ycp-alf      .
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded, e se si a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      w-cod-des-ycp-alf    to   w-ble-str              .
           move      05                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            not  = spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore impostato *
      *                  *---------------------------------------------*
           if        w-cod-des-ycp-alf    =    "-"
                     go to aco-600.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se valore impostato normale                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-ycp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-600.
      *                  *---------------------------------------------*
      *                  * Se valore impostato pari a '-', per esegui- *
      *                  * re la ricerca per descrizione               *
      *                  *---------------------------------------------*
       aco-610.
      *                      *-----------------------------------------*
      *                      * Test che esistano linea e posizione per *
      *                      * la descrizione, e se no a reimpostazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        w-cod-des-ycp-dln    =    zero or
                     w-cod-des-ycp-dps    =    zero
                     go to aco-150.
       aco-620.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione deescri-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-cde-ycp-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-des-ycp-dln    to   v-lin                  .
           move      w-cod-des-ycp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-630.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-des-ycp-dln    to   v-lin                  .
           move      w-cod-des-ycp-dps    to   v-pos                  .
           move      w-aux-cde-ycp-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-cde-ycp-dup      .
       aco-640.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-des-ycp-dln    to   v-lin                  .
           move      w-cod-des-ycp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * A reimpostazione codice                 *
      *                      *-----------------------------------------*
           go to     aco-150.
       aco-650.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-660.
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-ycp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-660.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a spaces : a reimposta- *
      *                      * zione codice                            *
      *                      *-----------------------------------------*
           if        w-aux-cde-ycp-dup    =    spaces
                     go to aco-150.
       aco-670.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-cde-ycp-dup    to   w-aux-cde-ycp-dmx      .
           move      30                   to   w-aux-cde-ycp-c01      .
       aco-680.
           if        w-aux-cde-ycp-c01    >    zero
                     if    w-aux-cde-ycp-dch
                          (w-aux-cde-ycp-c01)
                                          =    spaces
                           move     "z"   to   w-aux-cde-ycp-dch
                                              (w-aux-cde-ycp-c01)
                           subtract 1     from w-aux-cde-ycp-c01
                           go to aco-680.
       aco-700.
      *                      *-----------------------------------------*
      *                      * Lettura e bufferizzazione fino ad un    *
      *                      * massimo di 30 records con descrizio-    *
      *                      * ne compresa tra il minimo ed il mas-    *
      *                      * simo                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Azzeramento contatore records nel   *
      *                          * buffer                              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-cde-ycp-crb      .
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cde-ycp-dup    to   rf-ycp-des-key         .
           move      spaces               to   rf-ycp-cod-tcp         .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
      *                          *-------------------------------------*
      *                          * Se start errata : a trattamento per *
      *                          * fine file                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-800.
       aco-710.
      *                          *-------------------------------------*
      *                          * Read next                           *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cdp/fls/ioc/obj/iofycp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ycp                 .
       aco-720.
      *                          *-------------------------------------*
      *                          * Test se At End                      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-800.
       aco-730.
      *                          *-------------------------------------*
      *                          * Test sul max                        *
      *                          *-------------------------------------*
           if        rf-ycp-des-key       >    w-aux-cde-ycp-dmx
                     go to aco-800.
       aco-740.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-cde-ycp-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-cde-ycp-crb    >    30
                     go to aco-800.
       aco-750.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice tipo movimento           *
      *                              *---------------------------------*
           move      rf-ycp-cod-tcp       to   w-aux-cde-ycp-bco
                                              (w-aux-cde-ycp-crb)     .
      *                              *---------------------------------*
      *                              * Descrizione tipo movimento      *
      *                              *---------------------------------*
           move      rf-ycp-des-tcp       to   w-aux-cde-ycp-bde
                                              (w-aux-cde-ycp-crb)     .
       aco-760.
      *                          *-------------------------------------*
      *                          * Riciclo nella lettura               *
      *                          *-------------------------------------*
           go to aco-710.
       aco-800.
      *                      *-----------------------------------------*
      *                      * Esame dei risultati della lettura con   *
      *                      * bufferizzazione dei max 30 records      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * di records bufferizzati             *
      *                          *-------------------------------------*
           if        w-aux-cde-ycp-crb    =    zero
                     go to aco-810
           else  if  w-aux-cde-ycp-crb    >    30
                     go to aco-820
           else  if  w-aux-cde-ycp-crb    =    1
                     go to aco-830
           else      go to aco-850.
       aco-810.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a zero                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione puntini in area *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-des-ycp-dln    to   v-lin                  .
           move      w-cod-des-ycp-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * A reimpostazione codice         *
      *                              *---------------------------------*
           go to     aco-150.
       aco-820.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati maggiore di 30              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se programma di interrogazione  *
      *                              * gia' attivo : a reimpostazione  *
      *                              * codice senza alcuna segnalazio- *
      *                              * ne                              *
      *                              *---------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcdp0110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-150.
      *                              *---------------------------------*
      *                              * Preparazione variabili di i.p.c.*
      *                              * per l'esecuzione della interro- *
      *                              * gazione con parametri precabla- *
      *                              * ti                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo interrogazione : 'D'   *
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
      *                                  * Descrizione di base         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "des-key"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      30                   to   s-car                  .
           move      w-aux-cde-ycp-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                              *---------------------------------*
      *                              * Preparazione uscita per Find    *
      *                              * precablato                      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo operazione a : esecu-  *
      *                                  * zione Find                  *
      *                                  *-----------------------------*
           move      "F+"                 to   w-cod-des-ycp-ope      .
      *                                  *-----------------------------*
      *                                  * Simulazione tasto Find      *
      *                                  *-----------------------------*
           move      "FIND"               to   v-key                  .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-830.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a 1                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione indice sul primo   *
      *                              * elemento del buffer             *
      *                              *---------------------------------*
           move      1                    to   w-aux-cde-ycp-bix      .
       aco-840.
      *                              *---------------------------------*
      *                              * Valore selezionato in uscita    *
      *                              *---------------------------------*
           move      w-aux-cde-ycp-bco
                    (w-aux-cde-ycp-bix)   to   w-cod-des-ycp-cod      .
      *                              *---------------------------------*
      *                              * Visualizzazione valore selezio- *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-ycp-lin    to   v-lin                  .
           move      w-cod-des-ycp-pos    to   v-pos                  .
           move      w-cod-des-ycp-cod    to   v-alf                  .
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
           move      "AC"                 to   w-cod-des-ycp-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-850.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati compreso tra 1 e 30, si e-  *
      *                          * segue la selezione nel box locale   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-cde-ycp-crb    to   w-aux-cde-ycp-cpb      .
           subtract  1                    from w-aux-cde-ycp-cpb      .
           divide    6                    into w-aux-cde-ycp-cpb      .
           add       1                    to   w-aux-cde-ycp-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-cde-ycp-c01      .
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
           move      07                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      66                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "   Selezionare il tipo movimento desiderato    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "-----------------------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "-----------------------------------------------"
                                          to   v-alf                  .
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
      *                              * di indice (w-aux-cde-ycp-c01)   *
      *                              *---------------------------------*
           move      w-aux-cde-ycp-c01    to   w-aux-cde-ycp-nli      .
       aco-865.
           if        w-aux-cde-ycp-nli    >    6
                     subtract  6          from w-aux-cde-ycp-nli
                     go to aco-865.
           add       09                   to   w-aux-cde-ycp-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-cde-ycp-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-cde-ycp-c01    <    w-aux-cde-ycp-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-cde-ycp-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-cde-ycp-cpa    <    w-aux-cde-ycp-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-cde-ycp-nli    to   v-lin                  .
           move      30                   to   v-pos                  .
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
           move      w-aux-cde-ycp-c01    to   w-aux-cde-ycp-bix      .
      *                                  *-----------------------------*
      *                                  * Ad uscita dopo preparazione *
      *                                  * e visualizzazione del valo- *
      *                                  * re selezionato              *
      *                                  *-----------------------------*
           go to     aco-840.
       aco-890.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-ycp-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-ycp-nli    =    10
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
           if        w-aux-cde-ycp-c01    =    w-aux-cde-ycp-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-ycp-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-ycp-nli    =    15
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
      *                                  * A reimpostazione codice     *
      *                                  *-----------------------------*
           go to     aco-150.
       aco-920.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-ycp-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-ycp-cpa    to   w-aux-cde-ycp-c01      .
           multiply  6                    by   w-aux-cde-ycp-c01      .
           subtract  5                    from w-aux-cde-ycp-c01      .
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
           subtract  1                    from w-aux-cde-ycp-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-ycp-cpa    to   w-aux-cde-ycp-c01      .
           multiply  6                    by   w-aux-cde-ycp-c01      .
           subtract  5                    from w-aux-cde-ycp-c01      .
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
           move      w-aux-cde-ycp-c01    to   w-aux-cde-ycp-c02      .
           add       5                    to   w-aux-cde-ycp-c02      .
           divide    6                    into w-aux-cde-ycp-c02      .
           move      w-aux-cde-ycp-c02    to   w-aux-cde-ycp-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-ycp-c02      .
           multiply  6                    by   w-aux-cde-ycp-c02      .
           add       1                    to   w-aux-cde-ycp-c02      .
           add       5
                     w-aux-cde-ycp-c02  giving w-aux-cde-ycp-c03      .
           move      w-aux-cde-ycp-c03    to   w-aux-cde-ycp-c04      .
           if        w-aux-cde-ycp-c03    >    w-aux-cde-ycp-crb
                     move  w-aux-cde-ycp-crb
                                          to   w-aux-cde-ycp-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      10                   to   w-aux-cde-ycp-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione elemento    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Codice tipo movimento   *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-aux-cde-ycp-c05    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-aux-cde-ycp-bco
                    (w-aux-cde-ycp-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Descrizione tipo movim. *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-cde-ycp-c05    to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-cde-ycp-bde
                    (w-aux-cde-ycp-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-ycp-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-ycp-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-cde-ycp-c02    not  > w-aux-cde-ycp-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-cde-ycp-c02    >    w-aux-cde-ycp-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-cde-ycp-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      w-aux-cde-ycp-c05    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-cde-ycp-c02      .
           add       1                    to   w-aux-cde-ycp-c05      .
           go to     aco-952.
       aco-955.
      *                                  *-----------------------------*
      *                                  * Trattamento finale : visua- *
      *                                  * lizzazione del numero pagi- *
      *                                  * na attuale e del numero di  *
      *                                  * pagine totali               *
      *                                  *-----------------------------*
           move      w-aux-cde-ycp-cpa    to   w-aux-cde-ycp-lt1      .
           move      w-aux-cde-ycp-cpb    to   w-aux-cde-ycp-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-cde-ycp-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
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

