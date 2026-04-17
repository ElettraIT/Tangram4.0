       Identification Division.
       Program-Id.                                 acomgeo0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    geo                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/07/91    *
      *                       Ultima revisione:    NdK del 13/09/13    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice comune           *
      *                                                                *
      *                    -------------------------------------       *
      *                                                                *
      *                    Metodi di ricerca :                         *
      *                                                                *
      *                    '-'  seguito da return = Ricerca per        *
      *                                             descrizione        *
      *                                                                *
      *                    '-'  seguito da return = Ricerca dicotomica *
      *                               e da 'find'   per descrizione    *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-com-geo-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "AC"                 *
      *                                                                *
      *                       w-cod-com-geo-cmn : codice comune        *
      *                                                                *
      *                       w-cod-com-geo-lin : linea codice comune  *
      *                                                                *
      *                       w-cod-com-geo-pos : posizione codice co- *
      *                                           mune                 *
      *                                                                *
      *                       w-cod-com-geo-dln : linea descrizione    *
      *                                           comune               *
      *                                                                *
      *                       w-cod-com-geo-dps : posizione descrizio- *
      *                                           ne comune            *
      *                                                                *
      *                                                                *
      *              Output : w-cod-com-geo-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-com-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-com-geo-cmn : codice comune        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-com-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-com-geo-cmn : codice comune        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-com-geo-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-com-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-com-geo-cmn : codice comune        *
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
       01  w-aux-com-geo.
           05  w-aux-com-geo-ec0.
               10  w-aux-com-geo-ec9      pic  x(05)                  .
           05  w-aux-com-geo-anu.
               10  w-aux-com-geo-a00.
                   15  w-aux-com-geo-a0a
                               occurs 05  pic  x(01)                  .
               10  w-aux-com-geo-a10 redefines
                   w-aux-com-geo-a00.
                   15  w-aux-com-geo-a1n
                               occurs 05  pic  9(01)                  .
           05  w-aux-com-geo-num          pic  9(05)                  .
           05  w-aux-com-geo-tpr          pic  x(01)                  .
           05  w-aux-com-geo-nli          pic  9(02)                  .
           05  w-aux-com-geo-crb          pic  9(04)                  .
           05  w-aux-com-geo-crc          pic  9(04)                  .
           05  w-aux-com-geo-cpb          pic  9(04)                  .
           05  w-aux-com-geo-cpa          pic  9(04)                  .
           05  w-aux-com-geo-bix          pic  9(04)                  .
           05  w-aux-com-geo-max          pic  9(03) value 999        .
           05  w-aux-com-geo-buf
                               occurs 999.
               10  w-aux-com-geo-cmn      pic  9(05)                  .
               10  w-aux-com-geo-cco      pic  x(05)                  .
               10  w-aux-com-geo-dco      pic  x(30)                  .
               10  w-aux-com-geo-pco      pic  x(03)                  .
           05  w-aux-com-geo-bxy          pic  x(35)                  .
           05  w-aux-com-geo-ltp          pic  x(17)                  .
           05  w-aux-com-geo-le1          pic  x(03)                  .
           05  w-aux-com-geo-le2          pic  x(03)                  .
           05  w-aux-com-geo-dup          pic  x(30)                  .
           05  w-aux-com-geo-vnr          pic  x(30)                  .
           05  w-aux-com-geo-d50          pic  x(05)                  .
           05  w-aux-com-geo-d51 redefines
               w-aux-com-geo-d50          pic  9(05)                  .
           05  w-aux-com-geo-dmx.
               10  w-aux-com-geo-dch
                               occurs 30  pic  x(01)                  .
           05  w-aux-com-geo-c01          pic  9(04)                  .
           05  w-aux-com-geo-c02          pic  9(04)                  .
           05  w-aux-com-geo-c03          pic  9(04)                  .
           05  w-aux-com-geo-c04          pic  9(04)                  .
           05  w-aux-com-geo-c05          pic  9(04)                  .

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
      *    * Link-area per accettazione comune                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acomgeo0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-com-geo
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
           if        w-cod-com-geo-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-com-geo-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-com-geo-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-com-geo-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-com-geo-ope    =    "A+" or
                     w-cod-com-geo-ope    =    "I+" or
                     w-cod-com-geo-ope    =    "F+"
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
                     move  spaces         to   w-cod-com-geo-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find, se non   *
      *              * e' prevista linea e posizione per descrizione   *
      *              *-------------------------------------------------*
           if        w-cod-com-geo-dln    not  = zero and
                     w-cod-com-geo-dps    not  = zero
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
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
           move      w-cod-com-geo-cmn    to   w-cod-com-geo-s01      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing originale               *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-com-geo-s70      .
      *                  *---------------------------------------------*
      *                  * User function keys originali epurate        *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-com-geo-s90      .
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
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-s90    to   v-ufk                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
           move      w-aux-com-geo-ec9    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-com-geo-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-com-geo-ope    =    "A+"
                     go to aco-400
           else if   w-cod-com-geo-ope    =    "F+"
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
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-s90    to   v-ufk                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
           move      w-aux-com-geo-ec9    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-com-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro per Find                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Come per impostazione del carattere '-'     *
      *                  *---------------------------------------------*
           go to     aco-550.
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
      *                      * dice comune                             *
      *                      *-----------------------------------------*
           move      w-cod-com-geo-s01    to   w-cod-com-geo-cmn      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice comune editato     *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice comune originale *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
           move      w-aux-com-geo-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-com-geo-ope      .
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
      *                      * Codice comune a zero                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-cod-com-geo-cmn      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice comune editato      *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice comune           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
           move      w-aux-com-geo-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-com-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-435.
      *                  *---------------------------------------------*
      *                  * Test se il valore impostato contiene blanks *
      *                  * embedded, e se si : reimpostazione          *
      *                  *---------------------------------------------*
           move      v-alf                to   w-all-str-alf          .
           move      03                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        =    spaces
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
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-s90    to   v-ufk                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-com-geo-ope      .
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
           move      v-alf                to   w-aux-com-geo-anu      .
      *                      *-----------------------------------------*
      *                      * Azzeramenti preliminari                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-com-geo-num      .
           move      zero                 to   w-aux-com-geo-c01      .
       aco-482.
      *                      *-----------------------------------------*
      *                      * Scansione                               *
      *                      *-----------------------------------------*
       aco-484.
           add       1                    to   w-aux-com-geo-c01      .
           if        w-aux-com-geo-c01    >    05
                     go to aco-500.
           if        w-aux-com-geo-a0a
                    (w-aux-com-geo-c01)   =    spaces
                     go to aco-500.
           if        w-aux-com-geo-a0a
                    (w-aux-com-geo-c01)   <    "0" or
                     w-aux-com-geo-a0a
                    (w-aux-com-geo-c01)   >    "9"
                     go to aco-450.
           multiply  10                   by   w-aux-com-geo-num      .
           add       w-aux-com-geo-a1n
                    (w-aux-com-geo-c01)   to   w-aux-com-geo-num      .
           go to     aco-484.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se formato veramente numerico               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice comune in area di uscita         *
      *                      *-----------------------------------------*
           move      w-aux-com-geo-num    to   w-cod-com-geo-cmn      .
      *                      *-----------------------------------------*
      *                      * Preparazione codice comune editato      *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice comune editato   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
           move      w-aux-com-geo-ec9    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-com-geo-ope      .
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
           if        w-cod-com-geo-dln    =    zero or
                     w-cod-com-geo-dps    =    zero
                     go to aco-450.
       aco-560.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-com-geo-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-com-geo-dln    to   v-lin                  .
           move      w-cod-com-geo-dps    to   v-pos                  .
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
           move      "FIND"               to   v-pfk (03)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-com-geo-dln    to   v-lin                  .
           move      w-cod-com-geo-dps    to   v-pos                  .
           move      w-aux-com-geo-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-com-geo-dup      .
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
           move      w-cod-com-geo-dln    to   v-lin                  .
           move      w-cod-com-geo-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-590.
      *                      *-----------------------------------------*
      *                      * Preparazione v-alf al valore originale  *
      *                      *-----------------------------------------*
           perform   edt-000              thru edt-999                .
           move      w-aux-com-geo-ec9    to   v-alf                  .
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
           move      "AC"                 to   w-cod-com-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-610.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to aco-614.
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : rientro ad   *
      *                      * accettazione descrizione in uppercase   *
      *                      * case                                    *
      *                      *-----------------------------------------*
           if        w-aux-com-geo-dup    =    spaces
                     go to aco-560.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore impostato in *
      *                      * formato privo di spaces e di caratteri  *
      *                      * diversi da A..Z - 0..9, e salvataggio   *
      *                      * del risultato                           *
      *                      *-----------------------------------------*
           move      w-aux-com-geo-dup    to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-aux-com-geo-vnr      .
      *                      *-----------------------------------------*
      *                      * Se il valore normalizzato e' a spaces : *
      *                      * rientro ad accettazione descrizione     *
      *                      * in uppercase                            *
      *                      *-----------------------------------------*
           if        w-aux-com-geo-vnr    =    spaces
                     go to aco-560.
       aco-612.
      *                      *-----------------------------------------*
      *                      * Esecuzione ricerca totale               *
      *                      *-----------------------------------------*
           perform   aco-fnd-000          thru aco-fnd-999            .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con la stessa descrizione               *
      *                      *-----------------------------------------*
           if        w-aux-com-geo-crb    >    w-aux-com-geo-max
                     move  w-aux-com-geo-max
                                          to   w-aux-com-geo-crb
                     go to aco-800.
      *                      *-----------------------------------------*
      *                      * A visualizzazione                       *
      *                      *-----------------------------------------*
           go to     aco-750.
       aco-614.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a spaces : trattamento  *
      *                      * come per il tasto Up                    *
      *                      *-----------------------------------------*
           if        w-aux-com-geo-dup    =    spaces
                     go to aco-590.
      *                      *-----------------------------------------*
      *                      * Determinazione del tipo ricerca, se per *
      *                      * C.a.p. o per descrizione                *
      *                      *-----------------------------------------*
           move      w-aux-com-geo-dup    to   w-aux-com-geo-d50      .
           if        w-aux-com-geo-dup    not  = w-aux-com-geo-d50
                     move  "D"            to   w-aux-com-geo-tpr
                     go to aco-615.
           if        w-aux-com-geo-d51    not  numeric
                     move  "D"            to   w-aux-com-geo-tpr
           else      move  "C"            to   w-aux-com-geo-tpr      .
       aco-615.
      *                      *-----------------------------------------*
      *                      * Se tipo ricerca per descrizione si pre- *
      *                      * para la descrizione in uppercase con il *
      *                      * padding per il max                      *
      *                      *-----------------------------------------*
           if        w-aux-com-geo-tpr    =    "C"
                     go to aco-630.
           move      w-aux-com-geo-dup    to   w-aux-com-geo-dmx      .
           move      30                   to   w-aux-com-geo-c01      .
       aco-620.
           if        w-aux-com-geo-c01    >    zero
                     if    w-aux-com-geo-dch
                          (w-aux-com-geo-c01)
                                          =    spaces
                           move     "z"   to   w-aux-com-geo-dch
                                              (w-aux-com-geo-c01)
                           subtract 1     from w-aux-com-geo-c01
                           go to aco-620.
       aco-630.
      *                      *-----------------------------------------*
      *                      * Lettura e bufferizzazione fino ad un    *
      *                      * massimo di 'max' records                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Azzeramento contatore records nel   *
      *                          * buffer                              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-com-geo-crb      .
       aco-640.
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione secondo il tipo di   *
      *                              * ricerca                         *
      *                              *---------------------------------*
           if        w-aux-com-geo-tpr    =    "C"
                     go to aco-645
           else      go to aco-650.
       aco-645.
      *                              *---------------------------------*
      *                              * Se per C.a.p.                   *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CAPCFL    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-com-geo-d51    to   rf-gxc-cap-avp         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
           go to     aco-655.
       aco-650.
      *                              *---------------------------------*
      *                              * Se per descrizione              *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESORD    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-com-geo-dup    to   rf-gxc-des-ord         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
           go to     aco-655.
       aco-655.
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
      *                          * Deviazione a seconda del tipo di    *
      *                          * ricerca                             *
      *                          *-------------------------------------*
           if        w-aux-com-geo-tpr    =    "C"
                     go to aco-665
           else      go to aco-670.
       aco-665.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per C.a.p.       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test sul max                    *
      *                              *---------------------------------*
           if        rf-gxc-cap-avp       not  = w-aux-com-geo-d51
                     go to aco-700.
      *                              *---------------------------------*
      *                              * Se l'elemento letto non e' un   *
      *                              * comune si superano con una re-  *
      *                              * start tutte le frazioni e le    *
      *                              * localita' consecutive; se in-   *
      *                              * vece e' un comune, e' sicura-   *
      *                              * mente incluso nella selezione   *
      *                              *---------------------------------*
           if        rf-gxc-cod-fzn       =    zero and
                     rf-gxc-cod-lct       =    zero
                     go to aco-680.
           if        rf-gxc-cod-cmn       =    99999
                     go to aco-700.
           move      "SK"                 to   f-ope                  .
           move      "CAPCFL    "         to   f-key                  .
           move      "GT"                 to   f-cfr                  .
           move      999                  to   rf-gxc-cod-fzn         .
           move      999                  to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
           if        f-sts                not  = e-not-err
                     go to aco-700
           else      go to aco-660.
       aco-670.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per descrizione  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test sul max                    *
      *                              *---------------------------------*
           if        rf-gxc-des-ord       >    w-aux-com-geo-dmx
                     go to aco-700.
      *                              *---------------------------------*
      *                              * Selezione                       *
      *                              *---------------------------------*
           if        rf-gxc-cod-fzn       not  = zero or
                     rf-gxc-cod-lct       not  = zero
                     go to aco-660.
       aco-680.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-com-geo-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-com-geo-crb    >    w-aux-com-geo-max
                     subtract  1          from w-aux-com-geo-crb
                     go to aco-700.
       aco-690.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice comune                   *
      *                              *---------------------------------*
           move      rf-gxc-cod-cmn       to   w-aux-com-geo-cmn
                                              (w-aux-com-geo-crb)     .
      *                              *---------------------------------*
      *                              * Descrizione comune              *
      *                              *---------------------------------*
           move      rf-gxc-des-cfl       to   w-aux-com-geo-dco
                                              (w-aux-com-geo-crb)     .
      *                              *---------------------------------*
      *                              * CAP del comune                  *
      *                              *---------------------------------*
           move      rf-gxc-cap-avp       to   w-aux-com-geo-cco
                                              (w-aux-com-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice provincia del comune     *
      *                              *---------------------------------*
           move      rf-gxc-cod-prv       to   w-aux-com-geo-pco
                                              (w-aux-com-geo-crb)     .
      *                          *-------------------------------------*
      *                          * Riciclo nella lettura               *
      *                          *-------------------------------------*
           go to aco-660.
       aco-700.
      *                      *-----------------------------------------*
      *                      * Esame dei risultati della lettura con   *
      *                      * bufferizzazione dei 'max' records       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * di records bufferizzati             *
      *                          *-------------------------------------*
           if        w-aux-com-geo-crb    =    zero
                     go to aco-710
           else  if  w-aux-com-geo-crb    =    1
                     go to aco-730
           else      go to aco-750.
       aco-710.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a zero                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione puntini in area *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-com-geo-dln    =    zero
                     go to aco-712.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-com-geo-dln    to   v-lin                  .
           move      w-cod-com-geo-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-712.
      *                              *---------------------------------*
      *                              * A reimpostazione codice         *
      *                              *---------------------------------*
           go to     aco-590.
       aco-730.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a 1                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione indice sul primo   *
      *                              * elemento del buffer             *
      *                              *---------------------------------*
           move      1                    to   w-aux-com-geo-bix      .
       aco-740.
      *                              *---------------------------------*
      *                              * Codice comune in uscita         *
      *                              *---------------------------------*
           move      w-aux-com-geo-cmn
                    (w-aux-com-geo-bix)   to   w-cod-com-geo-cmn      .
      *                              *---------------------------------*
      *                              * Preparazione codice comune edi- *
      *                              * tato                            *
      *                              *---------------------------------*
           perform   edt-000              thru edt-999                .
      *                              *---------------------------------*
      *                              * Visualizzazione del comune      *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-com-geo-lin    to   v-lin                  .
           move      w-cod-com-geo-pos    to   v-pos                  .
           move      w-aux-com-geo-ec9    to   v-alf                  .
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
           move      "AC"                 to   w-cod-com-geo-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-750.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati compreso tra 1 e 999        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il tipo ricerca e' per de-   *
      *                              * scrizione, gli elementi sono    *
      *                              * gia' stati caricati in ordine   *
      *                              * di descrizione, e non e' neces- *
      *                              * sario alcun ordinamento         *
      *                              *---------------------------------*
           if        w-aux-com-geo-tpr    =    "D"
                     go to aco-800.
      *                              *---------------------------------*
      *                              * Se invece il tipo ricerca e'    *
      *                              * per C.a.p. si esegue un ordi-   *
      *                              * namento dei records bufferiz-   *
      *                              * zati per descrizione            *
      *                              *---------------------------------*
           move      zero                 to   w-aux-com-geo-c01      .
       aco-752.
           add       1                    to   w-aux-com-geo-c01      .
           if        w-aux-com-geo-c01    =    w-aux-com-geo-crb
                     go to aco-800.
           move      w-aux-com-geo-c01    to   w-aux-com-geo-c02      .
           move      w-aux-com-geo-c01    to   w-aux-com-geo-c03      .
       aco-754.
           add       1                    to   w-aux-com-geo-c03      .
           if        w-aux-com-geo-c03    >    w-aux-com-geo-crb
                     go to aco-756.
           if        w-aux-com-geo-dco
                    (w-aux-com-geo-c03)   <    w-aux-com-geo-dco
                                              (w-aux-com-geo-c02)
                     move   w-aux-com-geo-c03
                                          to   w-aux-com-geo-c02      .
           go to     aco-754.
       aco-756.
           if        w-aux-com-geo-c02    =    w-aux-com-geo-c01
                     go to aco-752.
           move      w-aux-com-geo-buf
                    (w-aux-com-geo-c01)   to   w-aux-com-geo-bxy      .
           move      w-aux-com-geo-buf
                    (w-aux-com-geo-c02)   to   w-aux-com-geo-buf
                                              (w-aux-com-geo-c01)     .
           move      w-aux-com-geo-bxy    to   w-aux-com-geo-buf
                                              (w-aux-com-geo-c02)     .
           go to     aco-752.
       aco-800.
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-com-geo-crb    to   w-aux-com-geo-cpb      .
           subtract  1                    from w-aux-com-geo-cpb      .
           divide    12                   into w-aux-com-geo-cpb      .
           add       1                    to   w-aux-com-geo-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-com-geo-c01      .
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
           move      18                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      63                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "      Selezionare il comune desiderato      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      " ------------------------------------------ "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      " ------------------------------------------ "
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
      *                              * di indice (w-aux-com-geo-c01)   *
      *                              *---------------------------------*
           divide    12                   into w-aux-com-geo-c01
                                        giving w-aux-com-geo-c05
                                     remainder w-aux-com-geo-nli      .
           if        w-aux-com-geo-nli    =    zero
                     move  12             to   w-aux-com-geo-nli      .
           add       06                   to   w-aux-com-geo-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-com-geo-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-com-geo-c01    <    w-aux-com-geo-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-com-geo-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-com-geo-cpa    <    w-aux-com-geo-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-com-geo-nli    to   v-lin                  .
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
           move      w-aux-com-geo-c01    to   w-aux-com-geo-bix      .
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
           subtract  1                    from w-aux-com-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-com-geo-nli    =    07
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
           if        w-aux-com-geo-c01    =    w-aux-com-geo-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-com-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-com-geo-nli    =    18
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
           add       1                    to   w-aux-com-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-com-geo-cpa    to   w-aux-com-geo-c01      .
           multiply  12                   by   w-aux-com-geo-c01      .
           subtract  11                   from w-aux-com-geo-c01      .
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
           subtract  1                    from w-aux-com-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-com-geo-cpa    to   w-aux-com-geo-c01      .
           multiply  12                   by   w-aux-com-geo-c01      .
           subtract  11                   from w-aux-com-geo-c01      .
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
           move      w-aux-com-geo-c01    to   w-aux-com-geo-c02      .
           add       11                   to   w-aux-com-geo-c02      .
           divide    12                   into w-aux-com-geo-c02      .
           move      w-aux-com-geo-c02    to   w-aux-com-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-com-geo-c02      .
           multiply  12                   by   w-aux-com-geo-c02      .
           add       1                    to   w-aux-com-geo-c02      .
           add       11
                     w-aux-com-geo-c02  giving w-aux-com-geo-c03      .
           move      w-aux-com-geo-c03    to   w-aux-com-geo-c04      .
           if        w-aux-com-geo-c03    >    w-aux-com-geo-crb
                     move  w-aux-com-geo-crb
                                          to   w-aux-com-geo-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-com-geo-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Cap del comune          *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-aux-com-geo-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-aux-com-geo-cco
                    (w-aux-com-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Descrizione comune      *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-com-geo-c05    to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-aux-com-geo-dco
                    (w-aux-com-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Codice provincia        *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-aux-com-geo-c05    to   v-lin                  .
           move      59                   to   v-pos                  .
           move      w-aux-com-geo-pco
                    (w-aux-com-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-com-geo-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       1                    to   w-aux-com-geo-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-com-geo-c02    not  > w-aux-com-geo-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-com-geo-c02    >    w-aux-com-geo-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-com-geo-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      w-aux-com-geo-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-com-geo-c02      .
           add       1                    to   w-aux-com-geo-c05      .
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
           move      w-aux-com-geo-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-com-geo-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-com-geo-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-com-geo-le2      .
           move      spaces               to   w-aux-com-geo-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-com-geo-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-com-geo-le2
                                delimited by   spaces
                                          into w-aux-com-geo-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-aux-com-geo-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione ricerca totale                                 *
      *    *-----------------------------------------------------------*
       aco-fnd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "d"                  to   w-aux-com-geo-tpr      .
           move      zero                 to   w-aux-com-geo-crb      .
       aco-fnd-100.
      *              *-------------------------------------------------*
      *              * Start per descrizione                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESORD    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      spaces               to   rf-gxc-des-ord         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnd-900.
       aco-fnd-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale per descrizione             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnd-900.
       aco-fnd-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
       aco-fnd-400.
      *              *-------------------------------------------------*
      *              * Selezione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che la descrizione non sia a Spaces    *
      *                  *---------------------------------------------*
           if        rf-gxc-des-ord       =    spaces
                     go to aco-fnd-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9, e preparazione *
      *                  * 2. valore per il match                      *
      *                  *---------------------------------------------*
           move      rf-gxc-des-ord       to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-aux-com-geo-vnr    to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                  *---------------------------------------------*
      *                  * Se c'e' stato un match : a bufferizzazione  *
      *                  *---------------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to aco-fnd-600.
       aco-fnd-500.
      *                  *---------------------------------------------*
      *                  * Se non c'e' stato alcun match : si ricicla  *
      *                  *---------------------------------------------*
           go to     aco-fnd-200.
       aco-fnd-600.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-com-geo-crb      .
      *              *-------------------------------------------------*
      *              * Test se piu' di max records letti               *
      *              *-------------------------------------------------*
           if        w-aux-com-geo-crb    >    w-aux-com-geo-max
                     go to aco-fnd-900.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      rf-gxc-cod-cmn       to   w-aux-com-geo-cmn
                                              (w-aux-com-geo-crb)     .
           move      rf-gxc-des-cfl       to   w-aux-com-geo-dco
                                              (w-aux-com-geo-crb)     .
           move      rf-gxc-cap-avp       to   w-aux-com-geo-cco
                                              (w-aux-com-geo-crb)     .
           move      rf-gxc-cod-prv       to   w-aux-com-geo-pco
                                              (w-aux-com-geo-crb)     .
       aco-fnd-800.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     aco-fnd-200.
       aco-fnd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-fnd-999.
       aco-fnd-999.
           exit.

      *    *===========================================================*
      *    * Editing Codice comune                                     *
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
           move      w-cod-com-geo-s70    to   v-edm                  .
           move      w-cod-com-geo-cmn    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione valore editato                   *
      *              *-------------------------------------------------*
           move      v-edt                to   w-aux-com-geo-ec9      .
       edt-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

