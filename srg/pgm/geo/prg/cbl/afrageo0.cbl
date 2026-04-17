       Identification Division.
       Program-Id.                                 afrageo0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    geo                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/07/91    *
      *                       Ultima revisione:    NdK del 16/02/02    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice frazione         *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-fra-geo-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "AC"                 *
      *                                                                *
      *                       w-cod-fra-geo-cmn : codice comune        *
      *                                                                *
      *                       w-cod-fra-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-cod-fra-geo-lin : linea codice frazio- *
      *                                           ne                   *
      *                                                                *
      *                       w-cod-fra-geo-pos : posizione codice     *
      *                                           frazione             *
      *                                                                *
      *                       w-cod-fra-geo-dln : linea descrizione    *
      *                                           frazione             *
      *                                                                *
      *                       w-cod-fra-geo-dps : posizione descrizio- *
      *                                           ne frazione          *
      *                                                                *
      *                                                                *
      *              Output : w-cod-fra-geo-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-fra-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-fra-geo-fzn : codice frazione      *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-fra-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-fra-geo-fzn : codice frazione      *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-fra-geo-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-fra-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-fra-geo-fzn : codice frazione      *
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
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .

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
       01  w-aux-fra-geo.
           05  w-aux-fra-geo-ec0.
               10  w-aux-fra-geo-ec9      pic  x(03)                  .
           05  w-aux-fra-geo-anu.
               10  w-aux-fra-geo-a00.
                   15  w-aux-fra-geo-a0a
                               occurs 03  pic  x(01)                  .
               10  w-aux-fra-geo-a10 redefines
                   w-aux-fra-geo-a00.
                   15  w-aux-fra-geo-a1n
                               occurs 03  pic  9(01)                  .
           05  w-aux-fra-geo-num          pic  9(03)                  .
           05  w-aux-fra-geo-tpr          pic  x(01)                  .
           05  w-aux-fra-geo-nli          pic  9(02)                  .
           05  w-aux-fra-geo-crb          pic  9(04)                  .
           05  w-aux-fra-geo-crc          pic  9(04)                  .
           05  w-aux-fra-geo-cpb          pic  9(04)                  .
           05  w-aux-fra-geo-cpa          pic  9(04)                  .
           05  w-aux-fra-geo-bix          pic  9(04)                  .
           05  w-aux-fra-geo-buf
                               occurs 999.
               10  w-aux-fra-geo-fzn      pic  9(03)                  .
               10  w-aux-fra-geo-dfr      pic  x(30)                  .
           05  w-aux-fra-geo-bxy          pic  x(33)                  .
           05  w-aux-fra-geo-ltp          pic  x(17)                  .
           05  w-aux-fra-geo-le1          pic  x(03)                  .
           05  w-aux-fra-geo-le2          pic  x(03)                  .
           05  w-aux-fra-geo-dup          pic  x(30)                  .
           05  w-aux-fra-geo-dmx.
               10  w-aux-fra-geo-dch
                               occurs 30  pic  x(01)                  .
           05  w-aux-fra-geo-c01          pic  9(04)                  .
           05  w-aux-fra-geo-c02          pic  9(04)                  .
           05  w-aux-fra-geo-c03          pic  9(04)                  .
           05  w-aux-fra-geo-c04          pic  9(04)                  .
           05  w-aux-fra-geo-c05          pic  9(04)                  .

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
      *    * Link-area per accettazione frazione                       *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/afrageo0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-fra-geo
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
           if        w-cod-fra-geo-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-fra-geo-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-fra-geo-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-fra-geo-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-fra-geo-ope    =    "A+" or
                     w-cod-fra-geo-ope    =    "I+" or
                     w-cod-fra-geo-ope    =    "F+"
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
      *                  * Open file [gxc]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
      *                  * Close file [gxc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
                     move  spaces         to   w-cod-fra-geo-ope      .
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
           move      "pgeo4000"           to   s-pro                  .
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
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      w-cod-fra-geo-cmn    to   w-cod-fra-geo-s01      .
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           move      w-cod-fra-geo-fzn    to   w-cod-fra-geo-s02      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing originale               *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-fra-geo-s70      .
      *                  *---------------------------------------------*
      *                  * User function keys originali epurate        *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-fra-geo-s90      .
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
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-s90    to   v-ufk                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-fra-geo-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-fra-geo-ope    =    "A+"
                     go to aco-400
           else if   w-cod-fra-geo-ope    =    "F+"
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
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-s90    to   v-ufk                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-fra-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro per Find                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione minima             *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-fra-geo-dup      .
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione massima            *
      *                  *---------------------------------------------*
           move      all   "z"            to   w-aux-fra-geo-dmx      .
      *                  *---------------------------------------------*
      *                  * Preparazione tipo ricerca : per Find        *
      *                  *---------------------------------------------*
           move      "F"                  to   w-aux-fra-geo-tpr      .
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
      *                      * dice frazione                           *
      *                      *-----------------------------------------*
           move      w-cod-fra-geo-s02    to   w-cod-fra-geo-fzn      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice frazione editato    *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice frazione origi-  *
      *                      * nale                                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-fra-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Se il valore e' completamente a spaces      *
      *                  *---------------------------------------------*
           if        v-alf                not  = spaces
                     go to aco-435.
      *                      *-----------------------------------------*
      *                      * Codice frazione a zero                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-cod-fra-geo-fzn      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice frazione editato    *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice frazione         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-fra-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-435.
      *                  *---------------------------------------------*
      *                  * Test se il valore impostato contiene blanks *
      *                  * embedded, e se si : reimpostazione          *
      *                  *---------------------------------------------*
           move      03                   to   w-ble-max              .
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
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-s90    to   v-ufk                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-fra-geo-ope      .
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
           move      v-alf                to   w-aux-fra-geo-anu      .
      *                      *-----------------------------------------*
      *                      * Azzeramenti preliminari                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fra-geo-num      .
           move      zero                 to   w-aux-fra-geo-c01      .
       aco-482.
      *                      *-----------------------------------------*
      *                      * Scansione                               *
      *                      *-----------------------------------------*
       aco-484.
           add       1                    to   w-aux-fra-geo-c01      .
           if        w-aux-fra-geo-c01    >    03
                     go to aco-500.
           if        w-aux-fra-geo-a0a
                    (w-aux-fra-geo-c01)   =    spaces
                     go to aco-500.
           if        w-aux-fra-geo-a0a
                    (w-aux-fra-geo-c01)   <    "0" or
                     w-aux-fra-geo-a0a
                    (w-aux-fra-geo-c01)   >    "9"
                     go to aco-450.
           multiply  10                   by   w-aux-fra-geo-num      .
           add       w-aux-fra-geo-a1n
                    (w-aux-fra-geo-c01)   to   w-aux-fra-geo-num      .
           go to     aco-484.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se formato veramente numerico               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice frazione in area di uscita       *
      *                      *-----------------------------------------*
           move      w-aux-fra-geo-num    to   w-cod-fra-geo-fzn      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice frazione editato    *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice frazione editato *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-fra-geo-ope      .
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
           if        w-cod-fra-geo-dln    =    zero or
                     w-cod-fra-geo-dps    =    zero
                     go to aco-450.
       aco-560.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-fra-geo-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-fra-geo-dln    to   v-lin                  .
           move      w-cod-fra-geo-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-570.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-fra-geo-dln    to   v-lin                  .
           move      w-cod-fra-geo-dps    to   v-pos                  .
           move      w-aux-fra-geo-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-fra-geo-dup      .
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
           move      30                   to   v-car                  .
           move      w-cod-fra-geo-dln    to   v-lin                  .
           move      w-cod-fra-geo-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-590.
      *                      *-----------------------------------------*
      *                      * Preparazione v-alf al valore originale  *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
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
           move      "AC"                 to   w-cod-fra-geo-ope      .
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
           if        w-aux-fra-geo-dup    =    spaces
                     go to aco-590.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-fra-geo-dup    to   w-aux-fra-geo-dmx      .
           move      30                   to   w-aux-fra-geo-c01      .
       aco-620.
           if        w-aux-fra-geo-c01    >    zero
                     if    w-aux-fra-geo-dch
                          (w-aux-fra-geo-c01)
                                          =    spaces
                           move     "z"   to   w-aux-fra-geo-dch
                                              (w-aux-fra-geo-c01)
                           subtract 1     from w-aux-fra-geo-c01
                           go to aco-620.
      *                      *-----------------------------------------*
      *                      * Tipo ricerca : per descrizione          *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-fra-geo-tpr      .
       aco-650.
      *                      *-----------------------------------------*
      *                      * Lettura e bufferizzazione fino ad un    *
      *                      * massimo di 999 records con descrizio-   *
      *                      * ne compresa tra il minimo ed il mas-    *
      *                      * simo, relativamente al codice comune    *
      *                      * passato come parametro                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Azzeramento contatore records nel   *
      *                          * buffer                              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-fra-geo-crb      .
      *                          *-------------------------------------*
      *                          * Preparazione valori per Start       *
      *                          *-------------------------------------*
           move      w-cod-fra-geo-s01    to   rf-gxc-cod-cmn         .
           move      001                  to   rf-gxc-cod-fzn         .
           move      000                  to   rf-gxc-cod-lct         .
       aco-655.
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                          *-------------------------------------*
      *                          * Se start errata : a trattamento per *
      *                          * fine file                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-700.
       aco-660.
      *                          *-------------------------------------*
      *                          * Read next                           *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                          *-------------------------------------*
      *                          * Test se At End                      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-700.
      *                          *-------------------------------------*
      *                          * Test sul max                        *
      *                          *-------------------------------------*
           if        rf-gxc-cod-cmn       not  = w-cod-fra-geo-s01
                     go to aco-700.
      *                          *-------------------------------------*
      *                          * Se letta una localita' si superano  *
      *                          * con una re-start tutte le localita' *
      *                          * consecutive                         *
      *                          *-------------------------------------*
           if        rf-gxc-cod-lct       =    zero
                     go to aco-665.
           if        rf-gxc-cod-fzn       =    999
                     go to aco-700.
           add       001                  to   rf-gxc-cod-fzn         .
           move      000                  to   rf-gxc-cod-lct         .
           go to     aco-655.
       aco-665.
      *                          *-------------------------------------*
      *                          * Selezione                           *
      *                          *-------------------------------------*
           if        rf-gxc-cod-fzn       =    zero
                     go to aco-660.
           if        rf-gxc-des-ord       <    w-aux-fra-geo-dup or
                     rf-gxc-des-ord       >    w-aux-fra-geo-dmx
                     go to aco-660.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-fra-geo-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-fra-geo-crb    >    999
                     subtract  1          from w-aux-fra-geo-crb
                     go to aco-700.
       aco-670.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice frazione                 *
      *                              *---------------------------------*
           move      rf-gxc-cod-fzn       to   w-aux-fra-geo-fzn
                                              (w-aux-fra-geo-crb)     .
      *                              *---------------------------------*
      *                              * Descrizione frazione            *
      *                              *---------------------------------*
           move      rf-gxc-des-cfl       to   w-aux-fra-geo-dfr
                                              (w-aux-fra-geo-crb)     .
      *                          *-------------------------------------*
      *                          * Riciclo nella lettura               *
      *                          *-------------------------------------*
           go to aco-660.
       aco-700.
      *                      *-----------------------------------------*
      *                      * Esame dei risultati della lettura con   *
      *                      * bufferizzazione dei max 999 records     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * di records bufferizzati             *
      *                          *-------------------------------------*
           if        w-aux-fra-geo-crb    =    zero
                     go to aco-710
           else  if  w-aux-fra-geo-crb    =    1
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
           if        w-aux-fra-geo-tpr    =    "F"
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
           move      "Nessuna frazione memorizzata !          "
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
      *                              * Se tipo di ricerca per descri-  *
      *                              * zione                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione puntini in  *
      *                                  * area di accettazione        *
      *                                  *-----------------------------*
           if        w-cod-fra-geo-dln    =    zero
                     go to aco-716.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-fra-geo-dln    to   v-lin                  .
           move      w-cod-fra-geo-dps    to   v-pos                  .
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
           if        w-aux-fra-geo-tpr    =    "F"
                     go to aco-750.
      *                              *---------------------------------*
      *                              * Preparazione indice sul primo   *
      *                              * elemento del buffer             *
      *                              *---------------------------------*
           move      1                    to   w-aux-fra-geo-bix      .
       aco-740.
      *                              *---------------------------------*
      *                              * Codice frazione in uscita       *
      *                              *---------------------------------*
           move      w-aux-fra-geo-fzn
                    (w-aux-fra-geo-bix)   to   w-cod-fra-geo-fzn      .
      *                              *---------------------------------*
      *                              * Preparazione codice frazione e- *
      *                              * ditato                          *
      *                              *---------------------------------*
           perform   edt-000              thru edt-999                .
      *                              *---------------------------------*
      *                              * Visualizzazione della frazione  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-fra-geo-lin    to   v-lin                  .
           move      w-cod-fra-geo-pos    to   v-pos                  .
           move      w-aux-fra-geo-ec9    to   v-alf                  .
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
           move      "AC"                 to   w-cod-fra-geo-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-750.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati compreso tra 1 e 999 si e-  *
      *                          * segue un ordinamento dei records    *
      *                          * bufferizzati in ordine di descri-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-fra-geo-c01      .
       aco-752.
           add       1                    to   w-aux-fra-geo-c01      .
           if        w-aux-fra-geo-c01    =    w-aux-fra-geo-crb
                     go to aco-800.
           move      w-aux-fra-geo-c01    to   w-aux-fra-geo-c02      .
           move      w-aux-fra-geo-c01    to   w-aux-fra-geo-c03      .
       aco-754.
           add       1                    to   w-aux-fra-geo-c03      .
           if        w-aux-fra-geo-c03    >    w-aux-fra-geo-crb
                     go to aco-756.
           if        w-aux-fra-geo-dfr
                    (w-aux-fra-geo-c03)   <    w-aux-fra-geo-dfr
                                              (w-aux-fra-geo-c02)
                     move   w-aux-fra-geo-c03
                                          to   w-aux-fra-geo-c02      .
           go to     aco-754.
       aco-756.
           if        w-aux-fra-geo-c02    =    w-aux-fra-geo-c01
                     go to aco-752.
           move      w-aux-fra-geo-buf
                    (w-aux-fra-geo-c01)   to   w-aux-fra-geo-bxy      .
           move      w-aux-fra-geo-buf
                    (w-aux-fra-geo-c02)   to   w-aux-fra-geo-buf
                                              (w-aux-fra-geo-c01)     .
           move      w-aux-fra-geo-bxy    to   w-aux-fra-geo-buf
                                              (w-aux-fra-geo-c02)     .
           go to     aco-752.
       aco-800.
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-fra-geo-crb    to   w-aux-fra-geo-cpb      .
           subtract  1                    from w-aux-fra-geo-cpb      .
           divide    12                   into w-aux-fra-geo-cpb      .
           add       1                    to   w-aux-fra-geo-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-fra-geo-c01      .
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
           move      21                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      59                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      " Selezionare la frazione desiderata  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      " ----------------------------------- "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      " ----------------------------------- "
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
      *                              * di indice (w-aux-fra-geo-c01)   *
      *                              *---------------------------------*
           divide    12                   into w-aux-fra-geo-c01
                                        giving w-aux-fra-geo-c05
                                     remainder w-aux-fra-geo-nli      .
           if        w-aux-fra-geo-nli    =    zero
                     move  12             to   w-aux-fra-geo-nli      .
           add       06                   to   w-aux-fra-geo-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-fra-geo-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-fra-geo-c01    <    w-aux-fra-geo-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-fra-geo-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-fra-geo-cpa    <    w-aux-fra-geo-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-fra-geo-nli    to   v-lin                  .
           move      28                   to   v-pos                  .
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
           move      w-aux-fra-geo-c01    to   w-aux-fra-geo-bix      .
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
           subtract  1                    from w-aux-fra-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-fra-geo-nli    =    07
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
           if        w-aux-fra-geo-c01    =    w-aux-fra-geo-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-fra-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-fra-geo-nli    =    18
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
           go to     aco-560.
       aco-920.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-fra-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-fra-geo-cpa    to   w-aux-fra-geo-c01      .
           multiply  12                   by   w-aux-fra-geo-c01      .
           subtract  11                   from w-aux-fra-geo-c01      .
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
           subtract  1                    from w-aux-fra-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-fra-geo-cpa    to   w-aux-fra-geo-c01      .
           multiply  12                   by   w-aux-fra-geo-c01      .
           subtract  11                   from w-aux-fra-geo-c01      .
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
           move      w-aux-fra-geo-c01    to   w-aux-fra-geo-c02      .
           add       11                   to   w-aux-fra-geo-c02      .
           divide    12                   into w-aux-fra-geo-c02      .
           move      w-aux-fra-geo-c02    to   w-aux-fra-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-fra-geo-c02      .
           multiply  12                   by   w-aux-fra-geo-c02      .
           add       1                    to   w-aux-fra-geo-c02      .
           add       11
                     w-aux-fra-geo-c02  giving w-aux-fra-geo-c03      .
           move      w-aux-fra-geo-c03    to   w-aux-fra-geo-c04      .
           if        w-aux-fra-geo-c03    >    w-aux-fra-geo-crb
                     move  w-aux-fra-geo-crb
                                          to   w-aux-fra-geo-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-fra-geo-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione codice      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      w-aux-fra-geo-c05    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-aux-fra-geo-fzn
                    (w-aux-fra-geo-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione descrizione *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-fra-geo-c05    to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-fra-geo-dfr
                    (w-aux-fra-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-fra-geo-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       1                    to   w-aux-fra-geo-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-fra-geo-c02    not  > w-aux-fra-geo-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-fra-geo-c02    >    w-aux-fra-geo-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-fra-geo-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      w-aux-fra-geo-c05    to   v-lin                  .
           move      22                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-fra-geo-c02      .
           add       1                    to   w-aux-fra-geo-c05      .
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
           move      w-aux-fra-geo-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-fra-geo-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-fra-geo-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-fra-geo-le2      .
           move      spaces               to   w-aux-fra-geo-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-fra-geo-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-fra-geo-le2
                                delimited by   spaces
                                          into w-aux-fra-geo-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-aux-fra-geo-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Editing Codice frazione                                   *
      *    *-----------------------------------------------------------*
       edt-000.
      *              *-------------------------------------------------*
      *              * Editing effettivo                               *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-fra-geo-s70    to   v-edm                  .
           move      w-cod-fra-geo-fzn    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione valore editato                   *
      *              *-------------------------------------------------*
           move      v-edt                to   w-aux-fra-geo-ec9      .
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

