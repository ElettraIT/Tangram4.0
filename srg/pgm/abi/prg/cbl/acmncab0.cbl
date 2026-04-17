       Identification Division.
       Program-Id.                                 acmncab0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    abi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/07/91    *
      *                       Ultima revisione:    NdK del 16/02/02    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice C.A.B.           *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cab-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-cab-abi : codice A.B.I.        *
      *                                                                *
      *                       w-cod-mne-cab-cab : codice C.A.B.        *
      *                                                                *
      *                       w-cod-mne-cab-lin : linea codice C.A.B.  *
      *                                                                *
      *                       w-cod-mne-cab-pos : pos.  codice C.A.B.  *
      *                                                                *
      *                       w-cod-mne-cab-dln : linea denominazione  *
      *                                           C.A.B.               *
      *                                                                *
      *                       w-cod-mne-cab-dps : posizione denominaz. *
      *                                           C.A.B.               *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cab-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cab-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-cab-cab : codice C.A.B.        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cab-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-cab-cab : codice C.A.B.        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-cab-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cab-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-cab-cab : codice C.A.B.        *
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
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .

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
       01  w-aux-mne-cab.
           05  w-aux-mne-cab-svv          pic  x(10)                  .
           05  w-aux-mne-cab-ec0.
               10  w-aux-mne-cab-ec9      pic  x(10)                  .
           05  w-aux-mne-cab-anu.
               10  w-aux-mne-cab-a00.
                   15  w-aux-mne-cab-a0a
                               occurs 10  pic  x(01)                  .
               10  w-aux-mne-cab-a10 redefines
                   w-aux-mne-cab-a00.
                   15  w-aux-mne-cab-a1n
                               occurs 10  pic  9(01)                  .
           05  w-aux-mne-cab-num          pic  9(10)                  .
           05  w-aux-mne-cab-tpr          pic  x(01)                  .
           05  w-aux-mne-cab-nli          pic  9(02)                  .
           05  w-aux-mne-cab-crb          pic  9(04)                  .
           05  w-aux-mne-cab-crc          pic  9(04)                  .
           05  w-aux-mne-cab-cpb          pic  9(04)                  .
           05  w-aux-mne-cab-cpa          pic  9(04)                  .
           05  w-aux-mne-cab-bix          pic  9(04)                  .
           05  w-aux-mne-cab-buf
                               occurs 333.
               10  w-aux-mne-cab-cab      pic  9(05)                  .
               10  w-aux-mne-cab-den      pic  x(40)                  .
               10  w-aux-mne-cab-via      pic  x(40)                  .
               10  w-aux-mne-cab-loc      pic  x(40)                  .
           05  w-aux-mne-cab-ltp          pic  x(17)                  .
           05  w-aux-mne-cab-le1          pic  x(03)                  .
           05  w-aux-mne-cab-le2          pic  x(03)                  .
           05  w-aux-mne-cab-dup          pic  x(30)                  .
           05  w-aux-mne-cab-dmx.
               10  w-aux-mne-cab-dch
                               occurs 40  pic  x(01)                  .
           05  w-aux-mne-cab-c01          pic  9(04)                  .
           05  w-aux-mne-cab-c02          pic  9(04)                  .
           05  w-aux-mne-cab-c03          pic  9(04)                  .
           05  w-aux-mne-cab-c04          pic  9(04)                  .
           05  w-aux-mne-cab-c05          pic  9(04)                  .

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
      *    * Link-area per accettazione codice C.A.B.                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmncab0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-cab
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
           if        w-cod-mne-cab-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-cab-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-cab-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-cab-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-cab-ope    =    "A+" or
                     w-cod-mne-cab-ope    =    "I+" or
                     w-cod-mne-cab-ope    =    "F+"
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
      *                  * Open file [axi]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *                  *---------------------------------------------*
      *                  * Open file [axs]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
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
      *                  * Close file [axs]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *                  *---------------------------------------------*
      *                  * Close file [axs]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
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
                     move  spaces         to   w-cod-mne-cab-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pabi2000"           to   s-pro                  .
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
      *                  * Codice A.B.I.                               *
      *                  *---------------------------------------------*
           move      w-cod-mne-cab-abi    to   w-cod-mne-cab-s01      .
      *                  *---------------------------------------------*
      *                  * Codice C.A.B.                               *
      *                  *---------------------------------------------*
           move      w-cod-mne-cab-cab    to   w-cod-mne-cab-s02      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing originale               *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-mne-cab-s70      .
      *                  *---------------------------------------------*
      *                  * User function keys originali epurate        *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-mne-cab-s90      .
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
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-s90    to   v-ufk                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
           move      w-aux-mne-cab-ec9    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-cab-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-mne-cab-ope    =    "A+"
                     go to aco-400
           else if   w-cod-mne-cab-ope    =    "F+"
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
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-s90    to   v-ufk                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
           move      w-aux-mne-cab-ec9    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-mne-cab-ope      .
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
           move      v-alf                to   w-aux-mne-cab-svv      .
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione minima             *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-cab-dup      .
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione massima            *
      *                  *---------------------------------------------*
           move      all   "z"            to   w-aux-mne-cab-dmx      .
      *                  *---------------------------------------------*
      *                  * Preparazione tipo ricerca : per Find        *
      *                  *---------------------------------------------*
           move      "F"                  to   w-aux-mne-cab-tpr      .
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
      *                      * Ripristino valore originale per il co-  *
      *                      * dice C.A.B.                             *
      *                      *-----------------------------------------*
           move      w-cod-mne-cab-s02    to   w-cod-mne-cab-cab      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice C.A.B. editato      *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice C.A.B. originale *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
           move      w-aux-mne-cab-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-cab-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Valore attuale di v-alf in area di salva-   *
      *                  * taggio                                      *
      *                  *---------------------------------------------*
           move      v-alf                to   w-aux-mne-cab-svv      .
      *                  *---------------------------------------------*
      *                  * Se il valore e' completamente a spaces      *
      *                  *---------------------------------------------*
           if        v-alf                not  = spaces
                     go to aco-435.
      *                      *-----------------------------------------*
      *                      * Codice C.A.B. a zero                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-cod-mne-cab-cab      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice C.A.B. editato      *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice C.A.B.           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
           move      w-aux-mne-cab-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-cab-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-435.
      *                  *---------------------------------------------*
      *                  * Test se il valore impostato contiene blanks *
      *                  * embedded, e se si : reimpostazione          *
      *                  *---------------------------------------------*
           move      10                   to   w-ble-max              .
           move      v-alf                to   w-ble-str              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            =    spaces
                     go to aco-475.
       aco-450.
      *                  *---------------------------------------------*
      *                  * Reimpostazione su v-alf                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-s90    to   v-ufk                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-mne-cab-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-475.
      *                  *---------------------------------------------*
      *                  * Se valore a '-' : ad impostazione della de- *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        v-alf                =    "-"
                     go to aco-550.
       aco-480.
      *                  *---------------------------------------------*
      *                  * Conversione del valore impostato da alfanu- *
      *                  * merico a numerico. Se ci sono anomalie : a  *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore impostato in area per scansione  *
      *                      *-----------------------------------------*
           move      v-alf                to   w-aux-mne-cab-anu      .
      *                      *-----------------------------------------*
      *                      * Azzeramenti preliminari                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-cab-num      .
           move      zero                 to   w-aux-mne-cab-c01      .
       aco-482.
      *                      *-----------------------------------------*
      *                      * Scansione                               *
      *                      *-----------------------------------------*
       aco-484.
           add       1                    to   w-aux-mne-cab-c01      .
           if        w-aux-mne-cab-c01    >    10
                     go to aco-485.
           if        w-aux-mne-cab-a0a
                    (w-aux-mne-cab-c01)   =    spaces
                     go to aco-485.
           if        w-aux-mne-cab-a0a
                    (w-aux-mne-cab-c01)   <    "0" or
                     w-aux-mne-cab-a0a
                    (w-aux-mne-cab-c01)   >    "9"
                     go to aco-500.
           multiply  10                   by   w-aux-mne-cab-num      .
           add       w-aux-mne-cab-a1n
                    (w-aux-mne-cab-c01)   to   w-aux-mne-cab-num      .
           go to     aco-484.
       aco-485.
           if        w-aux-mne-cab-num    >    99999
                     go to aco-450
           else      go to aco-525.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se formato non-numerico                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mnemonico minimo           *
      *                      *-----------------------------------------*
           move      w-aux-mne-cab-anu    to   w-aux-mne-cab-dup      .
      *                      *-----------------------------------------*
      *                      * Preparazione mnemonico massimo          *
      *                      *-----------------------------------------*
           move      w-aux-mne-cab-dup    to   w-aux-mne-cab-dmx      .
           move      10                   to   w-aux-mne-cab-c01      .
       aco-510.
           if        w-aux-mne-cab-c01    >    zero
                     if    w-aux-mne-cab-dch
                          (w-aux-mne-cab-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-cab-dch
                                              (w-aux-mne-cab-c01)
                           subtract 1     from w-aux-mne-cab-c01
                           go to aco-510.
      *                      *-----------------------------------------*
      *                      * Preparazione tipo ricerca : per Mnemo-  *
      *                      * nico                                    *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-cab-tpr      .
      *                      *-----------------------------------------*
      *                      * Ad interrogazione con box locale        *
      *                      *-----------------------------------------*
           go to     aco-650.
       aco-525.
      *                  *---------------------------------------------*
      *                  * Se formato veramente numerico               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice C.A.B. in area di uscita         *
      *                      *-----------------------------------------*
           move      w-aux-mne-cab-num    to   w-cod-mne-cab-cab      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice C.A.B. editato      *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice C.A.B. editato   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
           move      w-aux-mne-cab-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-cab-ope      .
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
           if        w-cod-mne-cab-dln    =    zero or
                     w-cod-mne-cab-dps    =    zero
                     go to aco-450.
       aco-560.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-cab-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cab-dln    to   v-lin                  .
           move      w-cod-mne-cab-dps    to   v-pos                  .
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
           move      w-cod-mne-cab-dln    to   v-lin                  .
           move      w-cod-mne-cab-dps    to   v-pos                  .
           move      w-aux-mne-cab-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-cab-dup      .
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
           move      w-cod-mne-cab-dln    to   v-lin                  .
           move      w-cod-mne-cab-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-590.
      *                      *-----------------------------------------*
      *                      * Ripristino v-alf al valore salvato      *
      *                      *-----------------------------------------*
           move      w-aux-mne-cab-svv    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * A reimpostazione codice                 *
      *                      *-----------------------------------------*
           go to     aco-450.
       aco-600.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-610.
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-cab-ope      .
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
           if        w-aux-mne-cab-dup    =    spaces
                     go to aco-590.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-mne-cab-dup    to   w-aux-mne-cab-dmx      .
           move      40                   to   w-aux-mne-cab-c01      .
       aco-620.
           if        w-aux-mne-cab-c01    >    zero
                     if    w-aux-mne-cab-dch
                          (w-aux-mne-cab-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-cab-dch
                                              (w-aux-mne-cab-c01)
                           subtract 1     from w-aux-mne-cab-c01
                           go to aco-620.
      *                      *-----------------------------------------*
      *                      * Tipo ricerca : per descrizione          *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-mne-cab-tpr      .
       aco-650.
      *                      *-----------------------------------------*
      *                      * Lettura e bufferizzazione fino ad un    *
      *                      * massimo di 333 records con descrizio-   *
      *                      * ne compresa tra il minimo ed il mas-    *
      *                      * simo, relativamente al codice A.B.I.    *
      *                      * passato come parametro                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Azzeramento contatore records nel   *
      *                          * buffer                              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-mne-cab-crb      .
       aco-655.
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-aux-mne-cab-tpr    =    "F" or
                     w-aux-mne-cab-tpr    =    "D"
                     go to aco-656
           else if   w-aux-mne-cab-tpr    =    "M"
                     go to aco-657.
       aco-656.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Find o   *
      *                              * per Descrizione                 *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Start per denominazione di  *
      *                                  * ricerca                     *
      *                                  *-----------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DENRIC    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-cab-s01    to   rf-axs-cod-abi         .
           move      w-aux-mne-cab-dup    to   rf-axs-den-ric         .
           move      zero                 to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *                                  *-----------------------------*
      *                                  * Se errore : a fine file,    *
      *                                  * altrimenti continuazione    *
      *                                  *-----------------------------*
           if        f-sts                =    e-not-err
                     go to aco-660
           else      go to aco-700.
       aco-657.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Mnemoni- *
      *                              * co                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Start per mnemonico         *
      *                                  *-----------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-cab-s01    to   rf-axs-cod-abi         .
           move      w-aux-mne-cab-dup    to   rf-axs-cod-mne         .
           move      zero                 to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
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
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *                          *-------------------------------------*
      *                          * Test se At End                      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-700.
       aco-665.
      *                          *-------------------------------------*
      *                          * Test sul max                        *
      *                          *-------------------------------------*
           if        rf-axs-cod-abi       not  = w-cod-mne-cab-s01
                     go to aco-700.
           if        w-aux-mne-cab-tpr    =    "F" or
                     w-aux-mne-cab-tpr    =    "D"
                     go to aco-666
           else if   w-aux-mne-cab-tpr    =    "M"
                     go to aco-667.
       aco-666.
           if        rf-axs-den-ric       >    w-aux-mne-cab-dmx
                     go to aco-700
           else      go to aco-670.
       aco-667.
           if        rf-axs-cod-mne       >    w-aux-mne-cab-dmx
                     go to aco-700
           else      go to aco-670.
       aco-670.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-mne-cab-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-mne-cab-crb    >    333
                     subtract  1          from w-aux-mne-cab-crb
                     go to aco-700.
       aco-675.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice C.A.B.                   *
      *                              *---------------------------------*
           move      rf-axs-cod-cab       to   w-aux-mne-cab-cab
                                              (w-aux-mne-cab-crb)     .
      *                              *---------------------------------*
      *                              * Denominazione sportello         *
      *                              *---------------------------------*
           move      rf-axs-den-spt       to   w-aux-mne-cab-den
                                              (w-aux-mne-cab-crb)     .
      *                              *---------------------------------*
      *                              * Via per lo sportello            *
      *                              *---------------------------------*
           move      rf-axs-via-spt       to   w-aux-mne-cab-via
                                              (w-aux-mne-cab-crb)     .
      *                              *---------------------------------*
      *                              * Localita' per lo sportello      *
      *                              *---------------------------------*
           move      rf-axs-loc-spt       to   w-aux-mne-cab-loc
                                              (w-aux-mne-cab-crb)     .
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
           if        w-aux-mne-cab-crb    =    zero
                     go to aco-710
           else  if  w-aux-mne-cab-crb    =    1
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
           if        w-aux-mne-cab-tpr    =    "F"
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
           move      "Nessuno sportello trovato !             "
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
           if        w-cod-mne-cab-dln    =    zero
                     go to aco-716.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cab-dln    to   v-lin                  .
           move      w-cod-mne-cab-dps    to   v-pos                  .
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
           if        w-aux-mne-cab-tpr    =    "F"
                     go to aco-750.
      *                              *---------------------------------*
      *                              * Preparazione indice sul primo   *
      *                              * elemento del buffer             *
      *                              *---------------------------------*
           move      1                    to   w-aux-mne-cab-bix      .
       aco-740.
      *                              *---------------------------------*
      *                              * Codice C.A.B. in uscita         *
      *                              *---------------------------------*
           move      w-aux-mne-cab-cab
                    (w-aux-mne-cab-bix)   to   w-cod-mne-cab-cab      .
      *                              *---------------------------------*
      *                              * Preparazione codice C.A.B. edi- *
      *                              * tato                            *
      *                              *---------------------------------*
           perform   edt-000              thru edt-999                .
      *                              *---------------------------------*
      *                              * Visualizzazione codice C.A.B.   *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cab-lin    to   v-lin                  .
           move      w-cod-mne-cab-pos    to   v-pos                  .
           move      w-aux-mne-cab-ec9    to   v-alf                  .
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
           move      "AC"                 to   w-cod-mne-cab-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-750.
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-mne-cab-crb    to   w-aux-mne-cab-cpb      .
           subtract  1                    from w-aux-mne-cab-cpb      .
           divide    4                    into w-aux-mne-cab-cpb      .
           add       1                    to   w-aux-mne-cab-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-mne-cab-c01      .
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
      *                              * Nome Istituto                   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Normalizzazione [axi]       *
      *                                  *-----------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *                                  *-----------------------------*
      *                                  * Lettura [axi]               *
      *                                  *-----------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI    "         to   f-key                  .
           move      w-cod-mne-cab-abi    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *                                  *-----------------------------*
      *                                  * Codice ABI Istituto         *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      05                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-cod-mne-cab-abi    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Denominazione Istituto      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rf-axi-den-abi       to   v-alf                  .
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
      *                              * di indice (w-aux-mne-cab-c01)   *
      *                              *---------------------------------*
           divide    4                    into w-aux-mne-cab-c01
                                        giving w-aux-mne-cab-c05
                                     remainder w-aux-mne-cab-nli      .
           if        w-aux-mne-cab-nli    =    zero
                     move  4              to   w-aux-mne-cab-nli      .
           multiply  3                    by   w-aux-mne-cab-nli      .
           add       04                   to   w-aux-mne-cab-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-cab-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-cab-c01    <    w-aux-mne-cab-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-cab-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-cab-cpa    <    w-aux-mne-cab-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-cab-nli    to   v-lin                  .
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
           move      w-aux-mne-cab-c01    to   w-aux-mne-cab-bix      .
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
           subtract  1                    from w-aux-mne-cab-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-mne-cab-nli    =    07
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
           if        w-aux-mne-cab-c01    =    w-aux-mne-cab-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-mne-cab-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-mne-cab-nli    =    16
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
           if        w-aux-mne-cab-tpr    =    "F"
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
           add       1                    to   w-aux-mne-cab-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-mne-cab-cpa    to   w-aux-mne-cab-c01      .
           multiply  4                    by   w-aux-mne-cab-c01      .
           subtract  3                    from w-aux-mne-cab-c01      .
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
           subtract  1                    from w-aux-mne-cab-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-mne-cab-cpa    to   w-aux-mne-cab-c01      .
           multiply  4                    by   w-aux-mne-cab-c01      .
           subtract  3                    from w-aux-mne-cab-c01      .
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
           move      w-aux-mne-cab-c01    to   w-aux-mne-cab-c02      .
           add       3                    to   w-aux-mne-cab-c02      .
           divide    4                    into w-aux-mne-cab-c02      .
           move      w-aux-mne-cab-c02    to   w-aux-mne-cab-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-mne-cab-c02      .
           multiply  4                    by   w-aux-mne-cab-c02      .
           add       1                    to   w-aux-mne-cab-c02      .
           add       3 
                     w-aux-mne-cab-c02  giving w-aux-mne-cab-c03      .
           move      w-aux-mne-cab-c03    to   w-aux-mne-cab-c04      .
           if        w-aux-mne-cab-c03    >    w-aux-mne-cab-crb
                     move  w-aux-mne-cab-crb
                                          to   w-aux-mne-cab-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-mne-cab-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Codice C.A.B.           *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-aux-mne-cab-cab
                    (w-aux-mne-cab-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Denominazione           *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-aux-mne-cab-den
                    (w-aux-mne-cab-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Via                     *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-mne-cab-via
                    (w-aux-mne-cab-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-mne-cab-loc
                    (w-aux-mne-cab-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-mne-cab-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       3                    to   w-aux-mne-cab-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-mne-cab-c02    not  > w-aux-mne-cab-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-mne-cab-c02    >    w-aux-mne-cab-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-mne-cab-crb    not  > 4
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-mne-cab-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-cab-c02      .
           add       3                    to   w-aux-mne-cab-c05      .
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
           move      w-aux-mne-cab-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-mne-cab-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-mne-cab-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-mne-cab-le2      .
           move      spaces               to   w-aux-mne-cab-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-mne-cab-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-mne-cab-le2
                                delimited by   spaces
                                          into w-aux-mne-cab-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-aux-mne-cab-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Editing Codice C.A.B.                                     *
      *    *-----------------------------------------------------------*
       edt-000.
      *              *-------------------------------------------------*
      *              * Editing effettivo                               *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-cab-s70    to   v-edm                  .
           move      w-cod-mne-cab-cab    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione valore editato                   *
      *              *-------------------------------------------------*
           move      v-edt                to   w-aux-mne-cab-ec9      .
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
