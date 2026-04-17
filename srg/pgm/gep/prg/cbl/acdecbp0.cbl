       Identification Division.
       Program-Id.                                 acdecbp0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/09/91    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice nostra cassa, o  *
      *                    nostra banca, o nostro c/c postale          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-cbp-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "AC"                 *
      *                                                                *
      *                       w-cod-des-cbp-tip : tipo record          *
      *                                           - 01 : Cassa         *
      *                                           - 02 : Banca         *
      *                                           - 03 : C/C postale   *
      *                                                                *
      *                       w-cod-des-cbp-cod : codice               *
      *                                                                *
      *                       w-cod-des-cbp-lin : linea codice         *
      *                                                                *
      *                       w-cod-des-cbp-pos : posizione codice     *
      *                                                                *
      *                       w-cod-des-cbp-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-des-cbp-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-cbp-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-cbp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-des-cbp-cod : codice               *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-cbp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-des-cbp-cod : codice               *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-des-cbp-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-des-cbp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-des-cbp-cod : codice               *
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
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .

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
       01  w-aux-cde-cbp.
           05  w-aux-cde-cbp-nli          pic  9(02)                  .
           05  w-aux-cde-cbp-crb          pic  9(02)                  .
           05  w-aux-cde-cbp-cbp          pic  9(02)                  .
           05  w-aux-cde-cbp-cpa          pic  9(02)                  .
           05  w-aux-cde-cbp-bix          pic  9(02)                  .
           05  w-aux-cde-cbp-buf
                               occurs 30.
               10  w-aux-cde-cbp-tip      pic  9(02)                  .
               10  w-aux-cde-cbp-cod      pic  x(10)                  .
               10  w-aux-cde-cbp-des      pic  x(40)                  .
           05  w-aux-cde-cbp-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-cde-cbp-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-cde-cbp-lt2      pic  9(01)                  .
           05  w-aux-cde-cbp-dup          pic  x(40)                  .
           05  w-aux-cde-cbp-dmx.
               10  w-aux-cde-cbp-dch
                               occurs 40  pic  x(01)                  .
           05  w-aux-cde-cbp-c01          pic  9(02)                  .
           05  w-aux-cde-cbp-c02          pic  9(02)                  .
           05  w-aux-cde-cbp-c03          pic  9(02)                  .
           05  w-aux-cde-cbp-c04          pic  9(02)                  .
           05  w-aux-cde-cbp-c05          pic  9(02)                  .

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
      *    * Link-area per accettazione codice nostra cassa, banca, o  *
      *    * c/c postale                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-des-cbp
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
           if        w-cod-des-cbp-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-des-cbp-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-des-cbp-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-des-cbp-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-des-cbp-ope    =    "A+" or
                     w-cod-des-cbp-ope    =    "I+" or
                     w-cod-des-cbp-ope    =    "F+"
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
      *                  * Open file [cbp]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
      *                  * Close file [cbp]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
                     move  spaces         to   w-cod-des-cbp-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo record        *
      *                  *---------------------------------------------*
           if        w-cod-des-cbp-tip    =    01
                     go to acc-010
           else if   w-cod-des-cbp-tip    =    02
                     go to acc-020
           else if   w-cod-des-cbp-tip    =    03
                     go to acc-030.
       acc-010.
      *                  *---------------------------------------------*
      *                  * Se tipo record 01 : Cassa                   *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep0110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                >    zero
                     if    v-pfk (03)     =    "FIND"
                           move  spaces   to   v-pfk (03)             .
           go to     acc-100.
       acc-020.
      *                  *---------------------------------------------*
      *                  * Se tipo record 02 : Banca                   *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep0160"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                >    zero
                     if    v-pfk (03)     =    "FIND"
                           move  spaces   to   v-pfk (03)             .
           go to     acc-100.
       acc-030.
      *                  *---------------------------------------------*
      *                  * Se tipo record 03 : C/C postale             *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep0210"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                >    zero
                     if    v-pfk (03)     =    "FIND"
                           move  spaces   to   v-pfk (03)             .
           go to     acc-100.
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo record        *
      *                  *---------------------------------------------*
           if        w-cod-des-cbp-tip    =    01
                     go to acc-110
           else if   w-cod-des-cbp-tip    =    02
                     go to acc-120
           else if   w-cod-des-cbp-tip    =    03
                     go to acc-130.
       acc-110.
      *                  *---------------------------------------------*
      *                  * Se tipo record 01 : Cassa                   *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep0100"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                >    zero
                     if    v-pfk (04)     =    "INSR"
                           move  spaces   to   v-pfk (04)             .
           go to     acc-200.
       acc-120.
      *                  *---------------------------------------------*
      *                  * Se tipo record 02 : Banca                   *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep0150"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                >    zero
                     if    v-pfk (04)     =    "INSR"
                           move  spaces   to   v-pfk (04)             .
           go to     acc-200.
       acc-130.
      *                  *---------------------------------------------*
      *                  * Se tipo record 03 : C/C postale             *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep0200"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                >    zero
                     if    v-pfk (04)     =    "INSR"
                           move  spaces   to   v-pfk (04)             .
           go to     acc-200.
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      w-cod-des-cbp-tip    to   w-cod-des-cbp-s01      .
      *                  *---------------------------------------------*
      *                  * Codice di default                           *
      *                  *---------------------------------------------*
           move      w-cod-des-cbp-cod    to   w-cod-des-cbp-s02      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing                         *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-des-cbp-s80      .
      *                  *---------------------------------------------*
      *                  * User function keys                          *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-des-cbp-s90      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-cbp-s90    to   v-ufk                  .
           move      w-cod-des-cbp-lin    to   v-lin                  .
           move      w-cod-des-cbp-pos    to   v-pos                  .
           move      w-cod-des-cbp-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-des-cbp-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-des-cbp-ope    =    "A+"
                     go to aco-400
           else if   w-cod-des-cbp-ope    =    "F+"
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
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-cbp-s90    to   v-ufk                  .
           move      w-cod-des-cbp-lin    to   v-lin                  .
           move      w-cod-des-cbp-pos    to   v-pos                  .
           move      w-cod-des-cbp-cod    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-des-cbp-ope      .
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
      *                      * Variabile 'cod-cbp'                     *
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cbp"            to   s-var                  .
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
           move      s-alf                to   w-cod-des-cbp-cod      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore selezionato      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-cbp-lin    to   v-lin                  .
           move      w-cod-des-cbp-pos    to   v-pos                  .
           move      w-cod-des-cbp-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-cbp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
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
      *                      * Rivisualizzazione valore originale      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-cbp-lin    to   v-lin                  .
           move      w-cod-des-cbp-pos    to   v-pos                  .
           move      w-cod-des-cbp-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-cbp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore impostato             *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cod-des-cbp-cod      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di comodo          *
      *                  *---------------------------------------------*
           move      w-cod-des-cbp-cod    to   w-cod-des-cbp-alf      .
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded, e se si a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      w-cod-des-cbp-alf    to   w-ble-str              .
           move      10                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            not  = spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore impostato *
      *                  *---------------------------------------------*
           if        w-cod-des-cbp-alf    =    "-"
                     go to aco-600.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se valore impostato normale                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-des-cbp-ope      .
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
           if        w-cod-des-cbp-dln    =    zero or
                     w-cod-des-cbp-dps    =    zero
                     go to aco-150.
       aco-620.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-cde-cbp-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-des-cbp-dln    to   v-lin                  .
           move      w-cod-des-cbp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-630.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-des-cbp-dln    to   v-lin                  .
           move      w-cod-des-cbp-dps    to   v-pos                  .
           move      w-aux-cde-cbp-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-cde-cbp-dup      .
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
           move      40                   to   v-car                  .
           move      w-cod-des-cbp-dln    to   v-lin                  .
           move      w-cod-des-cbp-dps    to   v-pos                  .
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
           move      "AC"                 to   w-cod-des-cbp-ope      .
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
           if        w-aux-cde-cbp-dup    =    spaces
                     go to aco-150.
       aco-670.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-cde-cbp-dup    to   w-aux-cde-cbp-dmx      .
           move      40                   to   w-aux-cde-cbp-c01      .
       aco-680.
           if        w-aux-cde-cbp-c01    >    zero
                     if    w-aux-cde-cbp-dch
                          (w-aux-cde-cbp-c01)
                                          =    spaces
                           move     "z"   to   w-aux-cde-cbp-dch
                                              (w-aux-cde-cbp-c01)
                           subtract 1     from w-aux-cde-cbp-c01
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
           move      zero                 to   w-aux-cde-cbp-crb      .
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-des-cbp-s01    to   rf-cbp-tip-cbp         .
           move      w-aux-cde-cbp-dup    to   rf-cbp-des-key         .
           move      spaces               to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
           if        rf-cbp-tip-cbp       not  = w-cod-des-cbp-s01
                     go to aco-800.
           if        rf-cbp-des-key       >    w-aux-cde-cbp-dmx
                     go to aco-800.
       aco-740.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-cde-cbp-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-cde-cbp-crb    >    30
                     go to aco-800.
       aco-750.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo record                     *
      *                              *---------------------------------*
           move      rf-cbp-tip-cbp       to   w-aux-cde-cbp-tip
                                              (w-aux-cde-cbp-crb)     .
      *                              *---------------------------------*
      *                              * Codice                          *
      *                              *---------------------------------*
           move      rf-cbp-cod-cbp       to   w-aux-cde-cbp-cod
                                              (w-aux-cde-cbp-crb)     .
      *                              *---------------------------------*
      *                              * Descrizione                     *
      *                              *---------------------------------*
           move      rf-cbp-des-cbp       to   w-aux-cde-cbp-des
                                              (w-aux-cde-cbp-crb)     .
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
           if        w-aux-cde-cbp-crb    =    zero
                     go to aco-810
           else  if  w-aux-cde-cbp-crb    >    30
                     go to aco-820
           else  if  w-aux-cde-cbp-crb    =    1
                     go to aco-830
           else      go to aco-845.
       aco-810.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a zero                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione puntini in area *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-des-cbp-dln    =    zero
                     go to aco-812.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-des-cbp-dln    to   v-lin                  .
           move      w-cod-des-cbp-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-812.
      *                              *---------------------------------*
      *                              * A reimpostazione codice         *
      *                              *---------------------------------*
           go to     aco-150.
       aco-820.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati maggiore di 30              *
      *                          *-------------------------------------*
       aco-822.
      *                              *---------------------------------*
      *                              * Se programma di interrogazione  *
      *                              * gia' attivo : a reimpostazione  *
      *                              * codice senza alcuna segnalazio- *
      *                              * ne                              *
      *                              *---------------------------------*
           if        w-cod-des-cbp-s01    =    01
                     go to aco-823
           else if   w-cod-des-cbp-s01    =    02
                     go to aco-824
           else if   w-cod-des-cbp-s01    =    03
                     go to aco-825.
       aco-823.
           move      "P?"                 to   s-ope                  .
           move      "pgep0110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to aco-828
           else      go to aco-150.
       aco-824.
           move      "P?"                 to   s-ope                  .
           move      "pgep0160"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to aco-828
           else      go to aco-150.
       aco-825.
           move      "P?"                 to   s-ope                  .
           move      "pgep0210"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to aco-828
           else      go to aco-150.
       aco-828.
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
           move      40                   to   s-car                  .
           move      w-aux-cde-cbp-dup    to   s-alf                  .
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
           move      "F+"                 to   w-cod-des-cbp-ope      .
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
           move      1                    to   w-aux-cde-cbp-bix      .
       aco-840.
      *                              *---------------------------------*
      *                              * Valore selezionato in uscita    *
      *                              *---------------------------------*
           move      w-aux-cde-cbp-cod
                    (w-aux-cde-cbp-bix)   to   w-cod-des-cbp-cod      .
      *                              *---------------------------------*
      *                              * Visualizzazione valore selezio- *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-des-cbp-lin    to   v-lin                  .
           move      w-cod-des-cbp-pos    to   v-pos                  .
           move      w-cod-des-cbp-cod    to   v-alf                  .
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
           move      "AC"                 to   w-cod-des-cbp-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-845.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati compreso tra 1 e 30, si e-  *
      *                          * segue la selezione nel box locale   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-cde-cbp-crb    to   w-aux-cde-cbp-cbp      .
           subtract  1                    from w-aux-cde-cbp-cbp      .
           divide    6                    into w-aux-cde-cbp-cbp      .
           add       1                    to   w-aux-cde-cbp-cbp      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-cde-cbp-c01      .
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
           move      12                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      69                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-850.
           if        w-cod-des-cbp-s01    =    01
                     go to aco-851
           else if   w-cod-des-cbp-s01    =    02
                     go to aco-852
           else if   w-cod-des-cbp-s01    =    03
                     go to aco-853.
       aco-851.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "           Selezionare la cassa desiderata        
      -              "  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-858.
       aco-852.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "           Selezionare la banca desiderata        
      -              "  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-858.
       aco-853.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "        Selezionare il c/c postale desiderato     
      -              "  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-858.
       aco-858.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "--"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "--"
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
      *                              * di indice (w-aux-cde-cbp-c01)   *
      *                              *---------------------------------*
           move      w-aux-cde-cbp-c01    to   w-aux-cde-cbp-nli      .
       aco-865.
           if        w-aux-cde-cbp-nli    >    6
                     subtract  6          from w-aux-cde-cbp-nli
                     go to aco-865.
           add       09                   to   w-aux-cde-cbp-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-cde-cbp-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-cde-cbp-c01    <    w-aux-cde-cbp-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-cde-cbp-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-cde-cbp-cpa    <    w-aux-cde-cbp-cbp
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-cde-cbp-nli    to   v-lin                  .
           move      27                   to   v-pos                  .
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
           move      w-aux-cde-cbp-c01    to   w-aux-cde-cbp-bix      .
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
           subtract  1                    from w-aux-cde-cbp-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-cbp-nli    =    10
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
           if        w-aux-cde-cbp-c01    =    w-aux-cde-cbp-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-cbp-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-cbp-nli    =    15
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
      *                                  * A reimpostazione descrizio- *
      *                                  * ne                          *
      *                                  *-----------------------------*
           go to     aco-630.
       aco-920.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-cbp-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-cbp-cpa    to   w-aux-cde-cbp-c01      .
           multiply  6                    by   w-aux-cde-cbp-c01      .
           subtract  5                    from w-aux-cde-cbp-c01      .
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
           subtract  1                    from w-aux-cde-cbp-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-cbp-cpa    to   w-aux-cde-cbp-c01      .
           multiply  6                    by   w-aux-cde-cbp-c01      .
           subtract  5                    from w-aux-cde-cbp-c01      .
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
           move      w-aux-cde-cbp-c01    to   w-aux-cde-cbp-c02      .
           add       5                    to   w-aux-cde-cbp-c02      .
           divide    6                    into w-aux-cde-cbp-c02      .
           move      w-aux-cde-cbp-c02    to   w-aux-cde-cbp-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-cbp-c02      .
           multiply  6                    by   w-aux-cde-cbp-c02      .
           add       1                    to   w-aux-cde-cbp-c02      .
           add       5
                     w-aux-cde-cbp-c02  giving w-aux-cde-cbp-c03      .
           move      w-aux-cde-cbp-c03    to   w-aux-cde-cbp-c04      .
           if        w-aux-cde-cbp-c03    >    w-aux-cde-cbp-crb
                     move  w-aux-cde-cbp-crb
                                          to   w-aux-cde-cbp-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      10                   to   w-aux-cde-cbp-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione elemento    *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Codice                  *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-aux-cde-cbp-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-aux-cde-cbp-cod
                    (w-aux-cde-cbp-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Descrizione             *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-cbp-c05    to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-aux-cde-cbp-des
                    (w-aux-cde-cbp-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-cbp-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-cbp-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-cde-cbp-c02    not  > w-aux-cde-cbp-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-cde-cbp-c02    >    w-aux-cde-cbp-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-cde-cbp-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-cbp-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-cde-cbp-c02      .
           add       1                    to   w-aux-cde-cbp-c05      .
           go to     aco-952.
       aco-955.
      *                                  *-----------------------------*
      *                                  * Trattamento finale : visua- *
      *                                  * lizzazione del numero pagi- *
      *                                  * na attuale e del numero di  *
      *                                  * pagine totali               *
      *                                  *-----------------------------*
           move      w-aux-cde-cbp-cpa    to   w-aux-cde-cbp-lt1      .
           move      w-aux-cde-cbp-cbp    to   w-aux-cde-cbp-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-cde-cbp-ltp    to   v-alf                  .
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

