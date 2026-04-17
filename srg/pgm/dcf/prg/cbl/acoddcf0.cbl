       Identification Division.
       Program-Id.                                 acoddcf0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 08/06/92    *
      *                       Ultima revisione:    NdK del 01/02/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice dipendenza del   *
      *                    fornitore                                   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcf-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-dcf-fnt : codice fornitore     *
      *                                                                *
      *                       w-cod-cod-dcf-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dcf-lin : linea codice         *
      *                                                                *
      *                       w-cod-cod-dcf-pos : posizione codice     *
      *                                                                *
      *                       w-cod-cod-dcf-rln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dcf-rps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcf-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A*"  Inizio accettazione, con ammissibilita' del codice di-   *
      *       pendenza "*   "                                          *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "A*"                 *
      *                                                                *
      *                       w-cod-cod-dcf-fnt : codice fornitore     *
      *                                                                *
      *                       w-cod-cod-dcf-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dcf-lin : linea codice         *
      *                                                                *
      *                       w-cod-cod-dcf-pos : posizione codice     *
      *                                                                *
      *                       w-cod-cod-dcf-rln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dcf-rps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcf-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcf-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcf-fnt : codice fornitore     *
      *                                                                *
      *                       w-cod-cod-dcf-cod : codice dipendenza    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcf-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcf-fnt : codice fornitore     *
      *                                                                *
      *                       w-cod-cod-dcf-cod : codice dipendenza    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcf-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcf-fnt : codice fornitore     *
      *                                                                *
      *                       w-cod-cod-dcf-cod : codice dipendenza    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "NT"  Richiesta eventuali note associate al fornitore          *
      *                                                                *
      *              Input  : w-cod-cod-dcf-ope : "NT"                 *
      *                       w-cod-cod-dcf-prg : nome del programma   *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
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
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [dfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdfx"                          .

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
       01  w-aux-cde-dcf.
           05  w-aux-cde-dcf-nli          pic  9(02)                  .
           05  w-aux-cde-dcf-crb          pic  9(02)                  .
           05  w-aux-cde-dcf-cpb          pic  9(02)                  .
           05  w-aux-cde-dcf-cpa          pic  9(02)                  .
           05  w-aux-cde-dcf-bix          pic  9(02)                  .
           05  w-aux-cde-dcf-buf
                               occurs 30.
               10  w-aux-cde-dcf-bco      pic  x(04)                  .
               10  w-aux-cde-dcf-brs      pic  x(40)                  .
               10  w-aux-cde-dcf-bvd      pic  x(40)                  .
               10  w-aux-cde-dcf-bld      pic  x(40)                  .
           05  w-aux-cde-dcf-ltp          pic  x(17)                  .
           05  w-aux-cde-dcf-le1          pic  x(03)                  .
           05  w-aux-cde-dcf-le2          pic  x(03)                  .
           05  w-aux-cde-dcf-dup          pic  x(40)                  .
           05  w-aux-cde-dcf-dmx.
               10  w-aux-cde-dcf-dch
                               occurs 40  pic  x(01)                  .
           05  w-aux-cde-dcf-c01          pic  9(02)                  .
           05  w-aux-cde-dcf-c02          pic  9(02)                  .
           05  w-aux-cde-dcf-c03          pic  9(02)                  .
           05  w-aux-cde-dcf-c04          pic  9(02)                  .
           05  w-aux-cde-dcf-c05          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutine relativa alle note                    *
      *    *-----------------------------------------------------------*
       01  w-not-fnt.
      *        *-------------------------------------------------------*
      *        * Numero di linee necessarie                            *
      *        *-------------------------------------------------------*
           05  w-not-fnt-num-lin          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice fornitore salvato                              *
      *        *-------------------------------------------------------*
           05  w-not-fnt-cod-fnt          pic  9(07)                  .
           05  w-not-fnt-dpz-fnt          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza fornitore per la start              *
      *        *-------------------------------------------------------*
           05  w-not-fnt-dpz-str          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodi                                                *
      *        *-------------------------------------------------------*
           05  w-not-fnt-ctr-001          pic  9(02)                  .
           05  w-not-fnt-ctr-002          pic  9(05)                  .
           
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
      *    * Link-area per accettazione codice dipendenza fornitore    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acoddcf0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-dcf
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
           if        w-cod-cod-dcf-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-dcf-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-dcf-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-dcf-ope    =    "AC" or
                     w-cod-cod-dcf-ope    =    "A*"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-dcf-ope    =    "A+" or
                     w-cod-cod-dcf-ope    =    "I+" or
                     w-cod-cod-dcf-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-cod-dcf-ope    =    "NT"
                     perform   not-000    thru not-999                .
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
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-cod-dcf-prg      .
           move      zero                 to   w-not-fnt-cod-fnt      .
           move      spaces               to   w-not-fnt-dpz-fnt      .
      *                  *---------------------------------------------*
      *                  * Open file [dcf]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Open file [dfx]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
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
      *                  * Close file [dcf]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Close file [dfx]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
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
                     move  spaces         to   w-cod-cod-dcf-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione segnale se accettazione con am-  *
      *              * missibilita' del codice dipendenza "*   " oppu- *
      *              * re no                                           *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcf-ope    =    "A*"
                     move  "#"            to   w-cod-cod-dcf-fla
           else      move  spaces         to   w-cod-cod-dcf-fla      .
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcf4011"           to   s-pro                  .
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
           move      "pdcf4000"           to   s-pro                  .
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
      *                  * Codice fornitore di default                 *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcf-fnt    to   w-cod-cod-dcf-s02      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza di default                *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcf-cod    to   w-cod-cod-dcf-s01      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing                         *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-cod-dcf-s80      .
      *                  *---------------------------------------------*
      *                  * User function keys                          *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-cod-dcf-s90      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcf-s90    to   v-ufk                  .
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           move      w-cod-cod-dcf-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-cod-dcf-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcf-ope    =    "A+"
                     go to aco-400
           else if   w-cod-cod-dcf-ope    =    "F+"
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
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcf-s90    to   v-ufk                  .
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           move      w-cod-cod-dcf-cod    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-dcf-ope      .
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
      *                      * Variabile 'dpz-fnt'                     *
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dpz-fnt"            to   s-var                  .
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
           move      s-alf                to   w-cod-cod-dcf-cod      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore selezionato      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           move      w-cod-cod-dcf-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcf-ope      .
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
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           move      w-cod-cod-dcf-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcf-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore impostato             *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cod-cod-dcf-cod      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di comodo          *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcf-cod    to   w-cod-cod-dcf-alf      .
      *                  *---------------------------------------------*
      *                  * Test se valore pari a "*   " e valore non   *
      *                  * ammissibile                                 *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-alf    =    "*   " and
                     w-cod-cod-dcf-fla    =    spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded, e se si a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcf-alf    to   w-ble-str              .
           move      04                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            not  = spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore impostato *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-alf    =    "-"
                     go to aco-600.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se valore impostato normale                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcf-ope      .
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
           if        w-cod-cod-dcf-rln    =    zero or
                     w-cod-cod-dcf-rps    =    zero
                     go to aco-150.
       aco-620.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-cde-dcf-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcf-rln    to   v-lin                  .
           move      w-cod-cod-dcf-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-623.
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area per indiriz- *
      *                  * zo                                          *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-vln    =    zero
                     go to aco-626.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcf-vln    to   v-lin                  .
           move      w-cod-cod-dcf-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-626.
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area per locali-  *
      *                  * ta'                                         *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-lln    =    zero
                     go to aco-630.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcf-lln    to   v-lin                  .
           move      w-cod-cod-dcf-lps    to   v-pos                  .
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
           move      w-cod-cod-dcf-rln    to   v-lin                  .
           move      w-cod-cod-dcf-rps    to   v-pos                  .
           move      w-aux-cde-dcf-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-cde-dcf-dup      .
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
           move      w-cod-cod-dcf-rln    to   v-lin                  .
           move      w-cod-cod-dcf-rps    to   v-pos                  .
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
           move      "AC"                 to   w-cod-cod-dcf-ope      .
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
           if        w-aux-cde-dcf-dup    =    spaces
                     go to aco-150.
       aco-670.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-cde-dcf-dup    to   w-aux-cde-dcf-dmx      .
           move      40                   to   w-aux-cde-dcf-c01      .
       aco-680.
           if        w-aux-cde-dcf-c01    >    zero
                     if    w-aux-cde-dcf-dch
                          (w-aux-cde-dcf-c01)
                                          =    spaces
                           move     "z"   to   w-aux-cde-dcf-dch
                                              (w-aux-cde-dcf-c01)
                           subtract 1     from w-aux-cde-dcf-c01
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
           move      zero                 to   w-aux-cde-dcf-crb      .
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "RAGKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cde-dcf-dup    to   rf-dcf-rag-key         .
           move      w-cod-cod-dcf-fnt    to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
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
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
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
           if        rf-dcf-rag-key       >    w-aux-cde-dcf-dmx
                     go to aco-800.
      *                          *-------------------------------------*
      *                          * Selezione su codice fornitore : se  *
      *                          * non e' uguale si ricicla            *
      *                          *-------------------------------------*
           if        rf-dcf-cod-fnt       not  = w-cod-cod-dcf-fnt
                     go to aco-710.
      *                          *-------------------------------------*
      *                          * Selezione su codice dipendenza : se *
      *                          * a spaces si ricicla                 *
      *                          *-------------------------------------*
           if        rf-dcf-dpz-fnt       =    spaces
                     go to aco-710.
       aco-740.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-cde-dcf-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-cde-dcf-crb    >    30
                     go to aco-800.
       aco-750.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice dipendenza               *
      *                              *---------------------------------*
           move      rf-dcf-dpz-fnt       to   w-aux-cde-dcf-bco
                                              (w-aux-cde-dcf-crb)     .
      *                              *---------------------------------*
      *                              * Ragione sociale                 *
      *                              *---------------------------------*
           move      rf-dcf-rag-soc       to   w-aux-cde-dcf-brs
                                              (w-aux-cde-dcf-crb)     .
      *                              *---------------------------------*
      *                              * Indirizzo                       *
      *                              *---------------------------------*
           move      rf-dcf-via-dcf       to   w-aux-cde-dcf-bvd
                                              (w-aux-cde-dcf-crb)     .
      *                              *---------------------------------*
      *                              * Localita'                       *
      *                              *---------------------------------*
           move      rf-dcf-loc-dcf       to   w-aux-cde-dcf-bld
                                              (w-aux-cde-dcf-crb)     .
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
           if        w-aux-cde-dcf-crb    =    zero
                     go to aco-810
           else  if  w-aux-cde-dcf-crb    >    30
                     go to aco-820
           else  if  w-aux-cde-dcf-crb    =    1
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
           if        w-cod-cod-dcf-rln    =    zero
                     go to aco-812.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcf-rln    to   v-lin                  .
           move      w-cod-cod-dcf-rps    to   v-pos                  .
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
      *                              *---------------------------------*
      *                              * Se programma di interrogazione  *
      *                              * gia' attivo : a reimpostazione  *
      *                              * codice senza alcuna segnalazio- *
      *                              * ne                              *
      *                              *---------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcf4011"           to   s-pro                  .
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
      *                                  * Codice fornitore            *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-cod-dcf-fnt    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Ragione sociale             *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "rag-key"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      30                   to   s-car                  .
           move      w-aux-cde-dcf-dup    to   s-alf                  .
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
           move      "F+"                 to   w-cod-cod-dcf-ope      .
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
           move      1                    to   w-aux-cde-dcf-bix      .
       aco-840.
      *                              *---------------------------------*
      *                              * Valore selezionato in uscita    *
      *                              *---------------------------------*
           move      w-aux-cde-dcf-bco
                    (w-aux-cde-dcf-bix)   to   w-cod-cod-dcf-cod      .
      *                              *---------------------------------*
      *                              * Visualizzazione valore selezio- *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           move      w-cod-cod-dcf-cod    to   v-alf                  .
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
           move      "AC"                 to   w-cod-cod-dcf-ope      .
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
           move      w-aux-cde-dcf-crb    to   w-aux-cde-dcf-cpb      .
           subtract  1                    from w-aux-cde-dcf-cpb      .
           divide    4                    into w-aux-cde-dcf-cpb      .
           add       1                    to   w-aux-cde-dcf-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-cde-dcf-c01      .
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
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "        Selezionare la dipendenza desiderata      
      -              "  "                 to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
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
      *                              * di indice (w-aux-cde-dcf-c01)   *
      *                              *---------------------------------*
           divide    4                    into w-aux-cde-dcf-c01
                                        giving w-aux-cde-dcf-c05
                                     remainder w-aux-cde-dcf-nli      .
           if        w-aux-cde-dcf-nli    =    zero
                     move  4              to   w-aux-cde-dcf-nli      .
           multiply  3                    by   w-aux-cde-dcf-nli      .
           add       04                   to   w-aux-cde-dcf-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-cde-dcf-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-cde-dcf-c01    <    w-aux-cde-dcf-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-cde-dcf-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-cde-dcf-cpa    <    w-aux-cde-dcf-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-cde-dcf-nli    to   v-lin                  .
           move      22                   to   v-pos                  .
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
           move      w-aux-cde-dcf-c01    to   w-aux-cde-dcf-bix      .
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
           subtract  1                    from w-aux-cde-dcf-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-dcf-nli    =    07
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
           if        w-aux-cde-dcf-c01    =    w-aux-cde-dcf-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-dcf-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-dcf-nli    =    16
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
      *                                  * A reimpostazione ragione    *
      *                                  * sociale                     *
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
           add       1                    to   w-aux-cde-dcf-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-dcf-cpa    to   w-aux-cde-dcf-c01      .
           multiply  4                    by   w-aux-cde-dcf-c01      .
           subtract  3                    from w-aux-cde-dcf-c01      .
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
           subtract  1                    from w-aux-cde-dcf-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-dcf-cpa    to   w-aux-cde-dcf-c01      .
           multiply  4                    by   w-aux-cde-dcf-c01      .
           subtract  3                    from w-aux-cde-dcf-c01      .
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
           move      w-aux-cde-dcf-c01    to   w-aux-cde-dcf-c02      .
           add       3                    to   w-aux-cde-dcf-c02      .
           divide    4                    into w-aux-cde-dcf-c02      .
           move      w-aux-cde-dcf-c02    to   w-aux-cde-dcf-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-dcf-c02      .
           multiply  4                    by   w-aux-cde-dcf-c02      .
           add       1                    to   w-aux-cde-dcf-c02      .
           add       3 
                     w-aux-cde-dcf-c02  giving w-aux-cde-dcf-c03      .
           move      w-aux-cde-dcf-c03    to   w-aux-cde-dcf-c04      .
           if        w-aux-cde-dcf-c03    >    w-aux-cde-dcf-crb
                     move  w-aux-cde-dcf-crb
                                          to   w-aux-cde-dcf-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-cde-dcf-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Codice dipendenza       *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-aux-cde-dcf-bco
                    (w-aux-cde-dcf-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Ragione sociale         *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-aux-cde-dcf-brs
                    (w-aux-cde-dcf-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Via                     *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-cde-dcf-bvd
                    (w-aux-cde-dcf-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-cde-dcf-bld
                    (w-aux-cde-dcf-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-dcf-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       3                    to   w-aux-cde-dcf-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-cde-dcf-c02    not  > w-aux-cde-dcf-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-cde-dcf-c02    >    w-aux-cde-dcf-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-cde-dcf-crb    not  > 4
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-dcf-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-cde-dcf-c02      .
           add       3                    to   w-aux-cde-dcf-c05      .
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
           move      w-aux-cde-dcf-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cde-dcf-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-cde-dcf-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cde-dcf-le2      .
           move      spaces               to   w-aux-cde-dcf-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-cde-dcf-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-cde-dcf-le2
                                delimited by   spaces
                                          into w-aux-cde-dcf-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-aux-cde-dcf-ltp    to   v-alf                  .
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

      *    *===========================================================*
      *    * Annotazioni fornitore                                     *
      *    *-----------------------------------------------------------*
       not-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      w-cod-cod-dcf-cod    to   w-not-fnt-dpz-str      .
           move      zero                 to   w-not-fnt-ctr-002      .
       not-050.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-cod    =    spaces
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Test se presente codice programma           *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-prg    =    spaces
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Test se stesso fornitore                    *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcf-fnt    =    w-not-fnt-cod-fnt and
                     w-cod-cod-dcf-cod    =    w-not-fnt-dpz-fnt
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione fornitore in corso di trat- *
      *                  * tamento                                     *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcf-fnt    to   w-not-fnt-cod-fnt      .
           move      w-cod-cod-dcf-cod    to   w-not-fnt-dpz-fnt      .
       not-100.
      *              *-------------------------------------------------*
      *              * Start su [dfx] per ricerca eventuali note       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "FNTPRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-cod-dcf-fnt    to   rf-dfx-cod-fnt         .
           move      w-not-fnt-dpz-str    to   rf-dfx-dpz-fnt         .
           move      w-cod-cod-dcf-prg    to   rf-dfx-cod-prg         .
           move      zero                 to   rf-dfx-cod-ann         .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to not-800.
       not-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [dfx]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dfx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to not-800.
       not-300.
      *              *-------------------------------------------------*
      *              * Test max su [dfx]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice fornitore                    *
      *                  *---------------------------------------------*
           if        rf-dfx-cod-fnt       not  = w-cod-cod-dcf-fnt
                     go to not-800.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza fornitore         *
      *                  *---------------------------------------------*
           if        rf-dfx-dpz-fnt       not  = w-not-fnt-dpz-str
                     go to not-800.
      *                  *---------------------------------------------*
      *                  * Test su codice programma                    *
      *                  *---------------------------------------------*
           if        rf-dfx-cod-prg       not  = w-cod-cod-dcf-prg
                     go to not-800.
       not-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [dfx]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione data attuale                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su eventuale data iniziale             *
      *                  *---------------------------------------------*
           if        rf-dfx-dat-ini       =    zero
                     go to not-420.
           if        rf-dfx-dat-ini       not  < s-dat
                     go to not-200.
       not-420.
      *                  *---------------------------------------------*
      *                  * Test su eventuale data finale               *
      *                  *---------------------------------------------*
           if        rf-dfx-dat-fin       =    zero
                     go to not-500.
           if        rf-dfx-dat-fin       <    s-dat
                     go to not-200.
       not-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione note                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incermento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-not-fnt-ctr-002      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero linee necessarie      *
      *                  *---------------------------------------------*
           move      11                   to   w-not-fnt-ctr-001      .
           move      zero                 to   w-not-fnt-num-lin      .
       not-520.
           subtract  1                    from w-not-fnt-ctr-001      .
           if        w-not-fnt-ctr-001    <    1
                     go to not-540.
           if        rf-dfx-ann-rig
                    (w-not-fnt-ctr-001)   =    spaces
                     go to not-520.
           move      w-not-fnt-ctr-001    to   w-not-fnt-num-lin      .
       not-540.
      *                  *---------------------------------------------*
      *                  * Test sul numero di linee determinate        *
      *                  *---------------------------------------------*
           if        w-not-fnt-num-lin    =    zero
                     go to not-900.
       not-600.
      *                  *---------------------------------------------*
      *                  * Costruzione box                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
      *
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
      *
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           subtract  2                    from v-pos                  .
      *
           move      w-cod-cod-dcf-lin    to   v-lto                  .
           add       4                    to   v-lto                  .
           add       w-not-fnt-num-lin    to   v-lto                  .
      *
           move      w-cod-cod-dcf-pos    to   v-pto                  .
           add       40                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
      *
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-not-fnt-num-lin    to   v-lin                  .
      *
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           add       5                    to   v-pos                  .
      *
           move      "Digitare 'S' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-610.
      *                  *---------------------------------------------*
      *                  * Visualizzazione righe annotazione           *
      *                  *---------------------------------------------*
           move      zero                 to   w-not-fnt-ctr-001      .
       not-620.
           add       1                    to   w-not-fnt-ctr-001      .
           if        w-not-fnt-ctr-001    >    w-not-fnt-num-lin
                     go to not-630.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           add       w-not-fnt-ctr-001    to   v-lin                  .
      *
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           subtract  1                    from v-pos                  .
           move      rf-dfx-ann-rig
                    (w-not-fnt-ctr-001)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     not-620.
       not-630.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-640.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione pre presa visione                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
      *
           move      w-cod-cod-dcf-lin    to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-not-fnt-num-lin    to   v-lin                  .
      *
           move      w-cod-cod-dcf-pos    to   v-pos                  .
           add       38                   to   v-pos                  .
      *
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione pre presa visione                  *
      *              *-------------------------------------------------*
           if        v-alf                not  = "S"
                     go to not-640.
       not-700.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-800.
      *              *-------------------------------------------------*
      *              * Test se necessaria una seconda start            *
      *              *-------------------------------------------------*
           if        w-not-fnt-dpz-str    =    "*   "
                     go to not-900.
           if        w-not-fnt-ctr-002    =    zero
                     move  "*   "         to   w-not-fnt-dpz-str
                     go to not-100.
       not-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-999.
       not-999.
           exit.
