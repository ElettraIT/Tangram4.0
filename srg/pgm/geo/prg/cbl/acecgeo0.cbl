       Identification Division.
       Program-Id.                                 acecgeo0           .
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
      * Descrizione pgm:   Modulo accettazione C.a.p. e citta' per le  *
      *                    anagrafiche che utilizzano il 'geo'.        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cap-cit-geo-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cap-cit-geo-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cap-cit-geo-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cap-cit-geo-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cap-cit-geo-ope : "AC"                 *
      *                                                                *
      *                       w-cap-cit-geo-naz : codice nazione       *
      *                                                                *
      *                       w-cap-cit-geo-cec : C.a.p. e citta'      *
      *                                                                *
      *                       w-cap-cit-geo-lin : "" linea             *
      *                                                                *
      *                       w-cap-cit-geo-pos : "" posizione         *
      *                                                                *
      *                       w-cap-cit-geo-cmn : codice comune        *
      *                                                                *
      *                       w-cap-cit-geo-dco : descrizione comune   *
      *                                                                *
      *                       w-cap-cit-geo-prv : sigla provincia      *
      *                                                                *
      *                       w-cap-cit-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-cap-cit-geo-dfr : descrizione frazione *
      *                                                                *
      *                       w-cap-cit-geo-lct : codice localita'     *
      *                                                                *
      *                       w-cap-cit-geo-dlo : descrizione localita'*
      *                                                                *
      *                                                                *
      *              Output : w-cap-cit-geo-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cap-cit-geo-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cap-cit-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cap-cit-geo-aut : automatismo eseguito *
      *                                           - Spaces : No        *
      *                                           - S      : Si        *
      *                                                                *
      *                       w-cap-cit-geo-naz : codice nazione       *
      *                                                                *
      *                       w-cap-cit-geo-cec : C.a.p. e citta'      *
      *                                                                *
      *                       w-cap-cit-geo-cmn : codice comune        *
      *                                                                *
      *                       w-cap-cit-geo-dco : descrizione comune   *
      *                                                                *
      *                       w-cap-cit-geo-prv : sigla provincia      *
      *                                                                *
      *                       w-cap-cit-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-cap-cit-geo-dfr : descrizione frazione *
      *                                                                *
      *                       w-cap-cit-geo-lct : codice localita'     *
      *                                                                *
      *                       w-cap-cit-geo-dlo : descrizione localita'*
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cap-cit-geo-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cap-cit-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *              Output : w-cap-cit-geo-ope : "A+" = continuare    *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cap-cit-geo-aut : automatismo eseguito *
      *                                           - Spaces : No        *
      *                                           - S      : Si        *
      *                                                                *
      *                       w-cap-cit-geo-naz : codice nazione       *
      *                                                                *
      *                       w-cap-cit-geo-cec : C.a.p. e citta'      *
      *                                                                *
      *                       w-cap-cit-geo-cmn : codice comune        *
      *                                                                *
      *                       w-cap-cit-geo-dco : descrizione comune   *
      *                                                                *
      *                       w-cap-cit-geo-prv : sigla provincia      *
      *                                                                *
      *                       w-cap-cit-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-cap-cit-geo-dfr : descrizione frazione *
      *                                                                *
      *                       w-cap-cit-geo-lct : codice localita'     *
      *                                                                *
      *                       w-cap-cit-geo-dlo : descrizione localita'*
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
      *        *-------------------------------------------------------*
      *        * [gxp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxp"                          .

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
       01  w-aux-cit-geo.
           05  w-aux-cit-geo-w40.
               10  w-aux-cit-geo-w4a.
                   15  w-aux-cit-geo-w4x
                               occurs 40  pic  x(01)                  .
               10  w-aux-cit-geo-w4b redefines 
                   w-aux-cit-geo-w4a.
                   15  w-aux-cit-geo-w4c.
                       20  w-aux-cit-geo-w4n
                                          pic  9(05)                  .
                   15  w-aux-cit-geo-w4d.
                       20  w-aux-cit-geo-w4p.
                           25  w-aux-cit-geo-w4q
                                          pic  x(01)                  .
                           25  w-aux-cit-geo-w4r
                                          pic  x(24)                  .
                       20  w-aux-cit-geo-w4u
                                          pic  x(10)                  .
           05  w-aux-cit-geo-24p          pic  x(24) value all "."    .
           05  w-aux-cit-geo-upp.
               10  w-aux-cit-geo-uuc      pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
               10  w-aux-cit-geo-uur redefines
                   w-aux-cit-geo-uuc.
                   15  w-aux-cit-geo-uux
                               occurs 26  pic  x(01)                  .
               10  w-aux-cit-geo-ulc      pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
               10  w-aux-cit-geo-ulr redefines
                   w-aux-cit-geo-ulc.
                   15  w-aux-cit-geo-ulx
                               occurs 26  pic  x(01)                  .
           05  w-aux-cit-geo-tpr          pic  x(01)                  .
           05  w-aux-cit-geo-rpr          pic  x(03)                  .
           05  w-aux-cit-geo-cap          pic  x(05)                  .
           05  w-aux-cit-geo-dup          pic  x(30)                  .
           05  w-aux-cit-geo-dmx.
               10  w-aux-cit-geo-dch
                               occurs 30  pic  x(01)                  .
           05  w-aux-cit-geo-nli          pic  9(02)                  .
           05  w-aux-cit-geo-crb          pic  9(04)                  .
           05  w-aux-cit-geo-crc          pic  9(04)                  .
           05  w-aux-cit-geo-cpb          pic  9(04)                  .
           05  w-aux-cit-geo-cpa          pic  9(04)                  .
           05  w-aux-cit-geo-bix          pic  9(04)                  .
           05  w-aux-cit-geo-buf
                               occurs 999.
               10  w-aux-cit-geo-tel      pic  x(01)                  .
               10  w-aux-cit-geo-cmn      pic  9(05)                  .
               10  w-aux-cit-geo-fzn      pic  9(03)                  .
               10  w-aux-cit-geo-lct      pic  9(03)                  .
               10  w-aux-cit-geo-del      pic  x(30)                  .
               10  w-aux-cit-geo-avp      pic  x(05)                  .
               10  w-aux-cit-geo-prv      pic  x(03)                  .
           05  w-aux-cit-geo-bxy          pic  x(47)                  .
           05  w-aux-cit-geo-ltp          pic  x(17)                  .
           05  w-aux-cit-geo-le1          pic  x(03)                  .
           05  w-aux-cit-geo-le2          pic  x(03)                  .
           05  w-aux-cit-geo-c01          pic  9(04)                  .
           05  w-aux-cit-geo-c02          pic  9(04)                  .
           05  w-aux-cit-geo-c03          pic  9(04)                  .
           05  w-aux-cit-geo-c04          pic  9(04)                  .
           05  w-aux-cit-geo-c05          pic  9(04)                  .
           05  w-aux-cit-geo-ele.
               10  w-aux-cit-geo-eix      pic  9(03)                  .
               10  w-aux-cit-geo-ect      pic  x(01)                  .
               10  w-aux-cit-geo-edt      pic  x(10)                  .
               10  w-aux-cit-geo-ecc      pic  9(05)                  .
               10  w-aux-cit-geo-edc      pic  x(30)                  .
               10  w-aux-cit-geo-ecf      pic  9(03)                  .
               10  w-aux-cit-geo-edf      pic  x(30)                  .
               10  w-aux-cit-geo-ecl      pic  9(03)                  .
               10  w-aux-cit-geo-edl      pic  x(30)                  .
               10  w-aux-cit-geo-esp      pic  x(03)                  .
               10  w-aux-cit-geo-edp      pic  x(25)                  .
               10  w-aux-cit-geo-ecp      pic  x(05)                  .
               10  w-aux-cit-geo-edx      pic  x(30)                  .
               10  w-aux-cit-geo-ede.
                   15  w-aux-cit-geo-edy
                               occurs 40  pic  x(01)                  .
               10  w-aux-cit-geo-ei0      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione C.a.p. e citta'                *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/acecgeo0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cap-cit-geo
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
           if        w-cap-cit-geo-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cap-cit-geo-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cap-cit-geo-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cap-cit-geo-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cap-cit-geo-ope    =    "A+" or
                     w-cap-cit-geo-ope    =    "F+" or
                     w-cap-cit-geo-ope    =    "I+"
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
      *                  *---------------------------------------------*
      *                  * Open file [gxp]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
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
      *                  *---------------------------------------------*
      *                  * Close file [gxp]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
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
                     move  spaces         to   w-cap-cit-geo-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Flag in uscita di automatismo eseguito a : No   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cap-cit-geo-aut      .
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find, se co-   *
      *              * dice nazione non-Italia                         *
      *              *-------------------------------------------------*
           if        w-cap-cit-geo-naz    =    "IT "
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
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-naz    to   w-cap-cit-geo-s01      .
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-cec    to   w-cap-cit-geo-s02      .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-cmn    to   w-cap-cit-geo-s03      .
      *                  *---------------------------------------------*
      *                  * Descrizione comune                          *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-dco    to   w-cap-cit-geo-s04      .
      *                  *---------------------------------------------*
      *                  * Sigla provincia                             *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-prv    to   w-cap-cit-geo-s05      .
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-fzn    to   w-cap-cit-geo-s06      .
      *                  *---------------------------------------------*
      *                  * Descrizione frazione                        *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-dfr    to   w-cap-cit-geo-s07      .
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-lct    to   w-cap-cit-geo-s08      .
      *                  *---------------------------------------------*
      *                  * Descrizione localita'                        *
      *                  *---------------------------------------------*
           move      w-cap-cit-geo-dlo    to   w-cap-cit-geo-s09      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing originale               *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cap-cit-geo-s70      .
      *                  *---------------------------------------------*
      *                  * User function keys originali epurate        *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cap-cit-geo-s90      .
       acc-500.
      *              *-------------------------------------------------*
      *              * Accettazione in formato alfanumerico            *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cap-cit-geo-s90    to   v-ufk                  .
           move      w-cap-cit-geo-lin    to   v-lin                  .
           move      w-cap-cit-geo-pos    to   v-pos                  .
           move      w-cap-cit-geo-cec    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cap-cit-geo-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cap-cit-geo-ope    =    "I+"
                     go to aco-100
           else      go to aco-200.
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
      *                      * Accettazione in formato alfanumerico    *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cap-cit-geo-s90    to   v-ufk                  .
           move      w-cap-cit-geo-lin    to   v-lin                  .
           move      w-cap-cit-geo-pos    to   v-pos                  .
           move      w-cap-cit-geo-cec    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cap-cit-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro dopo impostazione non terminata da   *
      *              * Insr                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore impostato             *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cap-cit-geo-cec      .
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt                              *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-225.
      *                      *-----------------------------------------*
      *                      * Ripristino valore originale per C.a.p.  *
      *                      * e citta'                                *
      *                      *-----------------------------------------*
           move      w-cap-cit-geo-s02    to   w-cap-cit-geo-cec      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione C.a.p. e citta' ori-  *
      *                      * ginale                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cap-cit-geo-lin    to   v-lin                  .
           move      w-cap-cit-geo-pos    to   v-pos                  .
           move      w-cap-cit-geo-cec    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cap-cit-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-225.
      *                  *---------------------------------------------*
      *                  * Se codice nazione diverso da 'IT '          *
      *                  *---------------------------------------------*
           if        w-cap-cit-geo-naz    =    "IT "
                     go to aco-275.
       aco-250.
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cap-cit-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-275.
      *                  *---------------------------------------------*
      *                  * Se terminazione non avvenuta con Find si e- *
      *                  * sce senza alcun automatismo                 *
      *                  *---------------------------------------------*
           if        v-key                not  = "FIND"
                     go to aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo se il valore impostato corrispon- *
      *                  * de ad uno dei formati semiautomatici :      *
      *                  *                                             *
      *                  * - Formato 1 : Solo il C.a.p.                *
      *                  *                                             *
      *                  * - Formato 2 : Descrizione senza C.a.p.      *
      *                  *                                             *
      *                  * - Formato 3 : Solo il codice provincia      *
      *                  *                                             *
      *                  * Se non e' uno dei formati previsti si va'   *
      *                  * alla reimpostazione.                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Impostazione in area di ridefinizione   *
      *                      *-----------------------------------------*
           move      w-cap-cit-geo-cec    to   w-aux-cit-geo-w4a      .
      *                      *-----------------------------------------*
      *                      * Se primo carattere a spaces : no semi-  *
      *                      * automatico, a reimpostazione            *
      *                      *-----------------------------------------*
           if        w-aux-cit-geo-w4x (1)
                                          =    spaces
                     go to aco-150.
      *                      *-----------------------------------------*
      *                      * Se primo carattere '+' ed inoltre il    *
      *                      * secondo ed il terzo sono diversi da     *
      *                      * spaces : a ricerca per codice provincia *
      *                      *-----------------------------------------*
           if        w-aux-cit-geo-w4x (1)
                                          =    "+"      and
                     w-aux-cit-geo-w4x (2)
                                          not  = spaces and
                     w-aux-cit-geo-w4x (3)
                                          not  = spaces and
                     w-aux-cit-geo-w4x (4)
                                          =    spaces
                     go to aco-350.
      *                      *-----------------------------------------*
      *                      * Se ultimi dieci caratteri a non spaces  *
      *                      * : no semiautomatico, a reimpostazione   *
      *                      *-----------------------------------------*
           if        w-aux-cit-geo-w4u    not  = spaces
                     go to aco-150.
      *                      *-----------------------------------------*
      *                      * Test sui primi cinque caratteri         *
      *                      *-----------------------------------------*
           if        w-aux-cit-geo-w4n    not  numeric
                     go to aco-325.
       aco-300.
      *                      *-----------------------------------------*
      *                      * Se primi cinque caratteri : numerici    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se i caratteri residui non sono a   *
      *                          * spaces, o a puntini : no semiauto-  *
      *                          * matico, reimpostazione              *
      *                          *-------------------------------------*
           if        w-aux-cit-geo-w4d    =    spaces
                     go to aco-305.
           if        w-aux-cit-geo-w4q    =    spaces        and
                     w-aux-cit-geo-w4r    =    w-aux-cit-geo-24p
                     go to aco-305.
           go to     aco-150.
       aco-305.
      *                          *-------------------------------------*
      *                          * Se i caratteri residui sono a spa-  *
      *                          * ces o a puntini                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo ricerca 'C', ovvero per il *
      *                              * C.a.p., ovvero Formato-1        *
      *                              *---------------------------------*
           move      "C"                  to   w-aux-cit-geo-tpr      .
      *                              *---------------------------------*
      *                              * Preparazione C.a.p. cercato     *
      *                              *---------------------------------*
           move      w-aux-cit-geo-w4c    to   w-aux-cit-geo-cap      .
      *                              *---------------------------------*
      *                              * A ricerca e selezione           *
      *                              *---------------------------------*
           go to     aco-400.
       aco-325.
      *                      *-----------------------------------------*
      *                      * Se primi cinque caratteri : nonnumerici *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo ricerca 'D', ovvero per la de- *
      *                          * scrizione, ovvero Formato-2         *
      *                          *-------------------------------------*
           move      "D"                  to   w-aux-cit-geo-tpr      .
       aco-330.
      *                          *-------------------------------------*
      *                          * Preparazione descrizione di ricer-  *
      *                          * ca minima in uppercase              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-cit-geo-c01      .
       aco-331.
           add       1                    to   w-aux-cit-geo-c01      .
           if        w-aux-cit-geo-c01    >    30
                     go to aco-332.
           move      zero                 to   w-aux-cit-geo-c02      .
           inspect   w-aux-cit-geo-ulc
                                      tallying w-aux-cit-geo-c02
                                for characters
                                before initial w-aux-cit-geo-w4x
                                              (w-aux-cit-geo-c01)     .
           if        w-aux-cit-geo-c02    not  < 26
                     go to aco-331.
           add       1                    to   w-aux-cit-geo-c02      .
           move      w-aux-cit-geo-uux
                    (w-aux-cit-geo-c02)   to   w-aux-cit-geo-w4x
                                              (w-aux-cit-geo-c01)     .
           go to     aco-331.
       aco-332.
           move      w-aux-cit-geo-w4a    to   w-aux-cit-geo-dup      .
       aco-335.
      *                          *-------------------------------------*
      *                          * Preparazione descrizione di ricer-  *
      *                          * ca massima con padding di 'z' sul-  *
      *                          * la descrizione minima               *
      *                          *-------------------------------------*
           move      w-aux-cit-geo-dup    to   w-aux-cit-geo-dmx      .
           move      30                   to   w-aux-cit-geo-c01      .
       aco-336.
           if        w-aux-cit-geo-c01    >    zero
                     if    w-aux-cit-geo-dch
                          (w-aux-cit-geo-c01)
                                          =    spaces
                           move     "z"   to   w-aux-cit-geo-dch
                                              (w-aux-cit-geo-c01)
                           subtract 1     from w-aux-cit-geo-c01
                           go to aco-336.
      *                          *-------------------------------------*
      *                          * A ricerca e selezione               *
      *                          *-------------------------------------*
           go to     aco-400.
       aco-350.
      *                      *-----------------------------------------*
      *                      * Se primo carattere : '+'                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione codice provincia di    *
      *                          * ricerca in uppercase                *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-cit-geo-c01      .
       aco-351.
           add       1                    to   w-aux-cit-geo-c01      .
           if        w-aux-cit-geo-c01    >    3
                     go to aco-352.
           move      zero                 to   w-aux-cit-geo-c02      .
           inspect   w-aux-cit-geo-ulc
                                      tallying w-aux-cit-geo-c02
                                for characters
                                before initial w-aux-cit-geo-w4x
                                              (w-aux-cit-geo-c01)     .
           if        w-aux-cit-geo-c02    not  < 26
                     go to aco-351.
           add       1                    to   w-aux-cit-geo-c02      .
           move      w-aux-cit-geo-uux
                    (w-aux-cit-geo-c02)   to   w-aux-cit-geo-w4x
                                              (w-aux-cit-geo-c01)     .
           go to     aco-351.
       aco-352.
      *                          *-------------------------------------*
      *                          * Assemblaggio del secondo e terzo    *
      *                          * carattere per creare un codice      *
      *                          * provincia                           *
      *                          *-------------------------------------*
           move      w-aux-cit-geo-w4x (2)
                                          to   w-aux-cit-geo-rpr (1:1).
           move      w-aux-cit-geo-w4x (3)
                                          to   w-aux-cit-geo-rpr (2:1).
      *                          *-------------------------------------*
      *                          * Tipo ricerca 'P', ovvero per codice *
      *                          * provincia, ovvero Formato-3         *
      *                          *-------------------------------------*
           move      "P"                  to   w-aux-cit-geo-tpr      .
      *                          *-------------------------------------*
      *                          * A ricerca e selezione               *
      *                          *-------------------------------------*
           go to     aco-400.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Ricerca e selezione su box locale, con ca-  *
      *                  * ricamento di max 999 records                *
      *                  *---------------------------------------------*
       aco-425.
      *                      *-----------------------------------------*
      *                      * Preparazioni preliminari                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero records bufferizzati : zero  *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-cit-geo-crb      .
       aco-450.
      *                      *-----------------------------------------*
      *                      * Start                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * ricerca                             *
      *                          *-------------------------------------*
           if        w-aux-cit-geo-tpr    =    "C"
                     go to aco-455
           else if   w-aux-cit-geo-tpr    =    "D"
                     go to aco-460
           else      go to aco-465.
       aco-455.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per C.a.p.       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione start                *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CAPCFL    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cit-geo-cap    to   rf-gxc-cap-avp         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                              *---------------------------------*
      *                              * Se start errata : a trattamento *
      *                              * per fine file                   *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-625.
      *                              *---------------------------------*
      *                              * ALtrimenti : a read next        *
      *                              *---------------------------------*
           go to     aco-475.
       aco-460.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per descrizione  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione start                *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESORD    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cit-geo-dup    to   rf-gxc-des-ord         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                              *---------------------------------*
      *                              * Se start errata : a trattamento *
      *                              * per fine file                   *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-625.
      *                              *---------------------------------*
      *                              * ALtrimenti : a read next        *
      *                              *---------------------------------*
           go to     aco-475.
       aco-465.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per Provincia    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione start                *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "PRVCFL    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cit-geo-rpr    to   rf-gxc-cod-prv         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                              *---------------------------------*
      *                              * Se start errata : a trattamento *
      *                              * per fine file                   *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-625.
      *                              *---------------------------------*
      *                              * ALtrimenti : a read next        *
      *                              *---------------------------------*
           go to     aco-475.
       aco-475.
      *                      *-----------------------------------------*
      *                      * Read next                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Esecuzione read next                *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                          *-------------------------------------*
      *                          * Se at end : a trattamento per fine  *
      *                          * file                                *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-625.
       aco-500.
      *                      *-----------------------------------------*
      *                      * Test max                                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * ricerca                             *
      *                          *-------------------------------------*
           if        w-aux-cit-geo-tpr    =    "C"
                     go to aco-505
           else if   w-aux-cit-geo-tpr    =    "D"
                     go to aco-510
           else      go to aco-515.
       aco-505.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per C.a.p.       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione test, se non supera- *
      *                              * to : a trattamento per fine fi- *
      *                              * le                              *
      *                              *---------------------------------*
           if        rf-gxc-cap-avp       not  = w-aux-cit-geo-cap
                     go to aco-625.
      *                              *---------------------------------*
      *                              * Altrimenti : a selezione        *
      *                              *---------------------------------*
           go to     aco-550.
       aco-510.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per descrizione  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione test, se non supera- *
      *                              * to : a trattamento per fine fi- *
      *                              * le                              *
      *                              *---------------------------------*
           if        rf-gxc-des-ord       >    w-aux-cit-geo-dmx
                     go to aco-625.
      *                              *---------------------------------*
      *                              * Altrimenti : a selezione        *
      *                              *---------------------------------*
           go to     aco-550.
       aco-515.
      *                          *-------------------------------------*
      *                          * Se tipo di ricerca per provincia    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione test, se non supera- *
      *                              * to : a trattamento per fine fi- *
      *                              * le                              *
      *                              *---------------------------------*
           if        rf-gxc-cod-prv       not  = w-aux-cit-geo-rpr
                     go to aco-625.
      *                              *---------------------------------*
      *                              * Altrimenti : a selezione        *
      *                              *---------------------------------*
           go to     aco-550.
       aco-550.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record letto            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero records bufferiz- *
      *                          * zati                                *
      *                          *-------------------------------------*
           add       1                    to   w-aux-cit-geo-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre la massima capacita' del   *
      *                          * buffer : a trattamento per fine     *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-cit-geo-crb    >    999
                     subtract  1          from w-aux-cit-geo-crb
                     go to     aco-625.
       aco-575.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo elemento                   *
      *                              *---------------------------------*
           if        rf-gxc-cod-lct       not  = zero
                     move  "L"            to   w-aux-cit-geo-tel
                                              (w-aux-cit-geo-crb)
           else if   rf-gxc-cod-fzn       not  = zero
                     move  "F"            to   w-aux-cit-geo-tel
                                              (w-aux-cit-geo-crb)
           else      move  "C"            to   w-aux-cit-geo-tel
                                              (w-aux-cit-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice comune                   *
      *                              *---------------------------------*
           move      rf-gxc-cod-cmn       to   w-aux-cit-geo-cmn
                                              (w-aux-cit-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice frazione                 *
      *                              *---------------------------------*
           move      rf-gxc-cod-fzn       to   w-aux-cit-geo-fzn
                                              (w-aux-cit-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice localita'                *
      *                              *---------------------------------*
           move      rf-gxc-cod-lct       to   w-aux-cit-geo-lct
                                              (w-aux-cit-geo-crb)     .
      *                              *---------------------------------*
      *                              * Descrizione elemento            *
      *                              *---------------------------------*
           move      rf-gxc-des-cfl       to   w-aux-cit-geo-del
                                              (w-aux-cit-geo-crb)     .
      *                              *---------------------------------*
      *                              * C.a.p. elemento                 *
      *                              *---------------------------------*
           move      rf-gxc-cap-avp       to   w-aux-cit-geo-avp
                                              (w-aux-cit-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice provincia elemento       *
      *                              *---------------------------------*
           move      rf-gxc-cod-prv       to   w-aux-cit-geo-prv
                                              (w-aux-cit-geo-crb)     .
       aco-600.
      *                          *-------------------------------------*
      *                          * Riciclo a read next                 *
      *                          *-------------------------------------*
           go to     aco-475.
       aco-625.
      *                      *-----------------------------------------*
      *                      * Trattamento per fine file               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * di records caricati nel buffer      *
      *                          *-------------------------------------*
           if        w-aux-cit-geo-crb    =    zero
                     go to aco-650
           else if   w-aux-cit-geo-crb    =    1
                     go to aco-675
           else      go to aco-750.
       aco-650.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer pari a zero                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     aco-150.
       aco-675.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer pari a 1                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Indice elemento nel buffer a 1  *
      *                              *---------------------------------*
           move      1                    to   w-aux-cit-geo-bix      .
       aco-700.
      *                              *---------------------------------*
      *                              * Ritorno con automatismo dell'e- *
      *                              * lemento caricato nel buffer con *
      *                              * indice (w-aux-cit-geo-bix)      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Letture addizionali relati- *
      *                                  * ve all'elemento del buffer  *
      *                                  * indice (w-aux-cit-geo-eix)  *
      *                                  * con composizione dell'area  *
      *                                  * w-aux-cit-geo-ele.          *
      *                                  *-----------------------------*
           move      w-aux-cit-geo-bix    to   w-aux-cit-geo-eix      .
           perform   aco-960              thru aco-989                .
      *                                  *-----------------------------*
      *                                  * Valori determinati in usci- *
      *                                  * ta                          *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * C.a.p. e citta'         *
      *                                      *-------------------------*
           move      w-aux-cit-geo-ede    to   w-cap-cit-geo-cec      .
      *                                      *-------------------------*
      *                                      * Codice Comune           *
      *                                      *-------------------------*
           move      w-aux-cit-geo-ecc    to   w-cap-cit-geo-cmn      .
      *                                      *-------------------------*
      *                                      * Descrizione Comune      *
      *                                      *-------------------------*
           move      w-aux-cit-geo-edc    to   w-cap-cit-geo-dco      .
      *                                      *-------------------------*
      *                                      * Sigla Provincia         *
      *                                      *-------------------------*
           move      w-aux-cit-geo-esp    to   w-cap-cit-geo-prv      .
      *                                      *-------------------------*
      *                                      * Codice Frazione         *
      *                                      *-------------------------*
           move      w-aux-cit-geo-ecf    to   w-cap-cit-geo-fzn      .
      *                                      *-------------------------*
      *                                      * Descrizione Frazione    *
      *                                      *-------------------------*
           move      w-aux-cit-geo-edf    to   w-cap-cit-geo-dfr      .
      *                                      *-------------------------*
      *                                      * Codice Localita'        *
      *                                      *-------------------------*
           move      w-aux-cit-geo-ecl    to   w-cap-cit-geo-lct      .
      *                                      *-------------------------*
      *                                      * Descrizione Localita'   *
      *                                      *-------------------------*
           move      w-aux-cit-geo-edl    to   w-cap-cit-geo-dlo      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione C.a.p. e    *
      *                                  * citta' selezionati          *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cap-cit-geo-lin    to   v-lin                  .
           move      w-cap-cit-geo-pos    to   v-pos                  .
           move      w-cap-cit-geo-cec    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Segnale di automatismo ese- *
      *                                  * guito                       *
      *                                  *-----------------------------*
           move      "S"                  to   w-cap-cit-geo-aut      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione a : non     *
      *                                  * continuazione               *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cap-cit-geo-ope      .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-750.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer superiori a 1                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-aux-cit-geo-tpr    =    "C" or
                     w-aux-cit-geo-tpr    =    "P"
                     go to aco-760
           else      go to aco-770.
       aco-760.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per C.a.p.   *
      *                              * o per il codice della provincia *
      *                              * si esegue un ordinamento dei    *
      *                              * records bufferizzati in ordine  *
      *                              * di descrizione                  *
      *                              *---------------------------------*
           move      zero                 to   w-aux-cit-geo-c01      .
       aco-762.
           add       1                    to   w-aux-cit-geo-c01      .
           if        w-aux-cit-geo-c01    =    w-aux-cit-geo-crb
                     go to aco-800.
           move      w-aux-cit-geo-c01    to   w-aux-cit-geo-c02      .
           move      w-aux-cit-geo-c01    to   w-aux-cit-geo-c03      .
       aco-764.
           add       1                    to   w-aux-cit-geo-c03      .
           if        w-aux-cit-geo-c03    >    w-aux-cit-geo-crb
                     go to aco-766.
           if        w-aux-cit-geo-del
                    (w-aux-cit-geo-c03)   <    w-aux-cit-geo-del
                                              (w-aux-cit-geo-c02)
                     move   w-aux-cit-geo-c03
                                          to   w-aux-cit-geo-c02      .
           go to     aco-764.
       aco-766.
           if        w-aux-cit-geo-c02    =    w-aux-cit-geo-c01
                     go to aco-762.
           move      w-aux-cit-geo-buf
                    (w-aux-cit-geo-c01)   to   w-aux-cit-geo-bxy      .
           move      w-aux-cit-geo-buf
                    (w-aux-cit-geo-c02)   to   w-aux-cit-geo-buf
                                              (w-aux-cit-geo-c01)     .
           move      w-aux-cit-geo-bxy    to   w-aux-cit-geo-buf
                                              (w-aux-cit-geo-c02)     .
           go to     aco-762.
       aco-770.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per C.a.p.   *
      *                              * non si esegue alcun ordinamento *
      *                              * dei records bufferizzati, per-  *
      *                              * che' sono gia' stati letti in   *
      *                              * ordine di descrizione           *
      *                              *---------------------------------*
           go to     aco-800.
       aco-800.
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-cit-geo-crb    to   w-aux-cit-geo-cpb      .
           subtract  1                    from w-aux-cit-geo-cpb      .
           divide    12                   into w-aux-cit-geo-cpb      .
           add       1                    to   w-aux-cit-geo-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-cit-geo-c01      .
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
           move      "      Selezionare l'elemento desiderato     "
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
      *                              * di indice (w-aux-cit-geo-c01)   *
      *                              *---------------------------------*
           divide    12                   into w-aux-cit-geo-c01
                                        giving w-aux-cit-geo-c05
                                     remainder w-aux-cit-geo-nli      .
           if        w-aux-cit-geo-nli    =    zero
                     move  12             to   w-aux-cit-geo-nli      .
           add       06                   to   w-aux-cit-geo-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-cit-geo-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-cit-geo-c01    <    w-aux-cit-geo-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-cit-geo-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-cit-geo-cpa    <    w-aux-cit-geo-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXPD"               to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-cit-geo-nli    to   v-lin                  .
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
           else if   v-key                =    "EXPD"
                     go to aco-875
           else      go to aco-870.
       aco-875.
      *                              *---------------------------------*
      *                              * Se Expd                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Espansione record           *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Salvataggio video       *
      *                                      *-------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Video in Off            *
      *                                      *-------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Box per espansione      *
      *                                      *-------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      71                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Literals nel box        *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Tipo elemento :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Provincia     :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Comune        :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Frazione      :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Localita'     :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Letture addizionali re- *
      *                                      * lative all'elemento in- *
      *                                      * dice (w-aux-cit-geo-eix)*
      *                                      * nel buffer, con compo-  *
      *                                      * sizione dell'area work  *
      *                                      * w-aux-cit-geo-ele.      *
      *                                      *-------------------------*
           move      w-aux-cit-geo-c01    to   w-aux-cit-geo-eix      .
           perform   aco-960              thru aco-989                .
      *                                      *-------------------------*
      *                                      * Visualizzazioni         *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Tipo elemento       *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-cit-geo-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Sigla Provincia     *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-cit-geo-esp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Descr. Provincia    *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      25                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-cit-geo-edp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Codice Comune       *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      12                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-cit-geo-ecc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Descr. Comune       *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-cit-geo-edc    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Codice Frazione     *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-cit-geo-ecf    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Descr. Frazione     *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-cit-geo-edf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Codice Localita'    *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-cit-geo-ecl    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Descr. Localita'    *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-cit-geo-edl    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                          *---------------------*
      *                                          * Parentesi quadre    *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Video in On             *
      *                                      *-------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Presa visione           *
      *                                      *-------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           move      16                   to   v-lin                  .
           move      68                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Ripristino video        *
      *                                      *-------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Rientro ad accettazio-  *
      *                                      * ne nel box principale   *
      *                                      *-------------------------*
           go to     aco-860.
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
           move      w-aux-cit-geo-c01    to   w-aux-cit-geo-bix      .
      *                                  *-----------------------------*
      *                                  * Ad uscita dopo preparazione *
      *                                  * e visualizzazione del valo- *
      *                                  * re selezionato              *
      *                                  *-----------------------------*
           go to     aco-700.
       aco-890.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cit-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cit-geo-nli    =    07
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
           if        w-aux-cit-geo-c01    =    w-aux-cit-geo-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cit-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cit-geo-nli    =    18
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
           go to     aco-150.
       aco-920.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cit-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cit-geo-cpa    to   w-aux-cit-geo-c01      .
           multiply  12                   by   w-aux-cit-geo-c01      .
           subtract  11                   from w-aux-cit-geo-c01      .
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
           subtract  1                    from w-aux-cit-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cit-geo-cpa    to   w-aux-cit-geo-c01      .
           multiply  12                   by   w-aux-cit-geo-c01      .
           subtract  11                   from w-aux-cit-geo-c01      .
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
           move      w-aux-cit-geo-c01    to   w-aux-cit-geo-c02      .
           add       11                   to   w-aux-cit-geo-c02      .
           divide    12                   into w-aux-cit-geo-c02      .
           move      w-aux-cit-geo-c02    to   w-aux-cit-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cit-geo-c02      .
           multiply  12                   by   w-aux-cit-geo-c02      .
           add       1                    to   w-aux-cit-geo-c02      .
           add       11
                     w-aux-cit-geo-c02  giving w-aux-cit-geo-c03      .
           move      w-aux-cit-geo-c03    to   w-aux-cit-geo-c04      .
           if        w-aux-cit-geo-c03    >    w-aux-cit-geo-crb
                     move  w-aux-cit-geo-crb
                                          to   w-aux-cit-geo-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-cit-geo-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Cap elemento            *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-aux-cit-geo-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-aux-cit-geo-avp
                    (w-aux-cit-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Descrizione elemento    *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-cit-geo-c05    to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-aux-cit-geo-del
                    (w-aux-cit-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Codice provincia        *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-aux-cit-geo-c05    to   v-lin                  .
           move      59                   to   v-pos                  .
           move      w-aux-cit-geo-prv
                    (w-aux-cit-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cit-geo-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cit-geo-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-cit-geo-c02    not  > w-aux-cit-geo-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-cit-geo-c02    >    w-aux-cit-geo-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-cit-geo-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      w-aux-cit-geo-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-cit-geo-c02      .
           add       1                    to   w-aux-cit-geo-c05      .
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
           move      w-aux-cit-geo-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cit-geo-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-cit-geo-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cit-geo-le2      .
           move      spaces               to   w-aux-cit-geo-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-cit-geo-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-cit-geo-le2
                                delimited by   spaces
                                          into w-aux-cit-geo-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-aux-cit-geo-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-960.
      *                              *---------------------------------*
      *                              * Subroutine interna per letture  *
      *                              * addizionali relative ad elemen- *
      *                              * to indice (w-aux-cit-geo-eix),  *
      *                              * con composizione dell'area di   *
      *                              * work w-aux-cit-geo-ele.         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazioni iniziali gene- *
      *                                  * rali                        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Tipo elemento           *
      *                                      *-------------------------*
           move      w-aux-cit-geo-tel
                    (w-aux-cit-geo-eix)   to   w-aux-cit-geo-ect      .
      *                                      *-------------------------*
      *                                      * Descrizione tipo ele-   *
      *                                      * mento                   *
      *                                      *-------------------------*
           if        w-aux-cit-geo-ect    =    "L"
                     move  "Localita' "   to   w-aux-cit-geo-edt
           else if   w-aux-cit-geo-ect    =    "F"
                     move  "Frazione  "   to   w-aux-cit-geo-edt
           else      move  "Comune    "   to   w-aux-cit-geo-edt      .
      *                                      *-------------------------*
      *                                      * Codice comune           *
      *                                      *-------------------------*
           move      w-aux-cit-geo-cmn
                    (w-aux-cit-geo-eix)   to   w-aux-cit-geo-ecc      .
      *                                      *-------------------------*
      *                                      * Codice frazione         *
      *                                      *-------------------------*
           move      w-aux-cit-geo-fzn
                    (w-aux-cit-geo-eix)   to   w-aux-cit-geo-ecf      .
      *                                      *-------------------------*
      *                                      * Codice localita'        *
      *                                      *-------------------------*
           move      w-aux-cit-geo-lct
                    (w-aux-cit-geo-eix)   to   w-aux-cit-geo-ecl      .
      *                                      *-------------------------*
      *                                      * C.a.p. elemento         *
      *                                      *-------------------------*
           move      w-aux-cit-geo-avp
                    (w-aux-cit-geo-eix)   to   w-aux-cit-geo-ecp      .
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del    *
      *                                  * tipo elemento               *
      *                                  *-----------------------------*
           if        w-aux-cit-geo-ect    =    "L"
                     go to aco-975
           else if   w-aux-cit-geo-ect    =    "F"
                     go to aco-970
           else      go to aco-965.
       aco-965.
      *                                  *-----------------------------*
      *                                  * Se il tipo elemento indica  *
      *                                  * un Comune                   *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Normalizzazione della   *
      *                                      * Frazione                *
      *                                      *-------------------------*
           move      zero                 to   w-aux-cit-geo-ecf      .
           move      spaces               to   w-aux-cit-geo-edf      .
      *                                      *-------------------------*
      *                                      * Normalizzazione della   *
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      zero                 to   w-aux-cit-geo-ecl      .
           move      spaces               to   w-aux-cit-geo-edl      .
       aco-967.
      *                                      *-------------------------*
      *                                      * Lettura record per il   *
      *                                      * Comune                  *
      *                                      *-------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-aux-cit-geo-ecc    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                                      *-------------------------*
      *                                      * Se record non trovato : *
      *                                      * normalizzazioni         *
      *                                      *-------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-gxc-des-cfl
                     move  spaces         to   rf-gxc-cod-prv         .
      *                                      *-------------------------*
      *                                      * Descrizione Comune      *
      *                                      *-------------------------*
           move      rf-gxc-des-cfl       to   w-aux-cit-geo-edc      .
      *                                      *-------------------------*
      *                                      * Sigla provincia         *
      *                                      *-------------------------*
           move      rf-gxc-cod-prv       to   w-aux-cit-geo-esp      .
      *                                      *-------------------------*
      *                                      * A preparazioni finali   *
      *                                      * generali                *
      *                                      *-------------------------*
           go to     aco-980.
       aco-970.
      *                                  *-----------------------------*
      *                                  * Se il tipo elemento indica  *
      *                                  * una Frazione                *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Normalizzazione della   *
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      zero                 to   w-aux-cit-geo-ecl      .
           move      spaces               to   w-aux-cit-geo-edl      .
       aco-972.
      *                                      *-------------------------*
      *                                      * Lettura record per la   *
      *                                      * Frazione                *
      *                                      *-------------------------*
           if        w-aux-cit-geo-ecf    =    zero
                     move  spaces         to   rf-gxc-des-cfl
                     go to aco-973.
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-aux-cit-geo-ecc    to   rf-gxc-cod-cmn         .
           move      w-aux-cit-geo-ecf    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                                      *-------------------------*
      *                                      * Se record non trovato : *
      *                                      * normalizzazioni         *
      *                                      *-------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-gxc-des-cfl         .
       aco-973.
      *                                      *-------------------------*
      *                                      * Descrizione Frazione    *
      *                                      *-------------------------*
           move      rf-gxc-des-cfl       to   w-aux-cit-geo-edf      .
      *                                      *-------------------------*
      *                                      * A lettura record per il *
      *                                      * Comune                  *
      *                                      *-------------------------*
           go to     aco-967.
       aco-975.
      *                                  *-----------------------------*
      *                                  * Se il tipo elemento indica  *
      *                                  * una Localita'               *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Lettura record per la   *
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-aux-cit-geo-ecc    to   rf-gxc-cod-cmn         .
           move      w-aux-cit-geo-ecf    to   rf-gxc-cod-fzn         .
           move      w-aux-cit-geo-ecl    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                                      *-------------------------*
      *                                      * Se record non trovato : *
      *                                      * normalizzazioni         *
      *                                      *-------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-gxc-des-cfl         .
      *                                      *-------------------------*
      *                                      * Descrizione Localita'   *
      *                                      *-------------------------*
           move      rf-gxc-des-cfl       to   w-aux-cit-geo-edl      .
      *                                      *-------------------------*
      *                                      * A lettura record per la *
      *                                      * Frazione                *
      *                                      *-------------------------*
           go to     aco-972.
       aco-980.
      *                                  *-----------------------------*
      *                                  * Preparazioni finali genera- *
      *                                  * li                          *
      *                                  *-----------------------------*
       aco-981.
      *                                      *-------------------------*
      *                                      * Lettura provincia       *
      *                                      *-------------------------*
           if        w-aux-cit-geo-esp    =    spaces
                     move  spaces         to   rf-gxp-des-prv
                     go to aco-982.
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-aux-cit-geo-esp    to   rf-gxp-cod-prv         .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
      *                                      *-------------------------*
      *                                      * Se record non trovato : *
      *                                      * normalizzazioni         *
      *                                      *-------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-gxp-des-prv         .
       aco-982.
      *                                      *-------------------------*
      *                                      * Descrizione provincia   *
      *                                      *-------------------------*
           move      rf-gxp-des-prv       to   w-aux-cit-geo-edp      .
       aco-983.
      *                                      *-------------------------*
      *                                      * Descrizione C.a.p. e    *
      *                                      * citta'                  *
      *                                      *-------------------------*
           if        w-aux-cit-geo-ect    =    "F"
                     move  w-aux-cit-geo-edf
                                          to   w-aux-cit-geo-edx
           else if   w-aux-cit-geo-ect    =    "L"
                     if    w-aux-cit-geo-edf
                                          not  = spaces
                           move  w-aux-cit-geo-edf
                                          to   w-aux-cit-geo-edx
                     else  move  w-aux-cit-geo-edc
                                          to   w-aux-cit-geo-edx
           else      move  w-aux-cit-geo-edc
                                          to   w-aux-cit-geo-edx      .
           if        w-aux-cit-geo-ecp    =    spaces
                     move  w-aux-cit-geo-edx
                                          to   w-aux-cit-geo-ede
                     go to aco-984.
           move      spaces               to   w-aux-cit-geo-ede      .
           string    w-aux-cit-geo-ecp
                                delimited by   size
                     " "        delimited by   size
                     w-aux-cit-geo-edx
                                delimited by   size
                                          into w-aux-cit-geo-ede      .
       aco-984.
           move      40                   to   w-aux-cit-geo-ei0      .
       aco-985.
           if        w-aux-cit-geo-edy
                    (w-aux-cit-geo-ei0)   not  = spaces
                     go to aco-986.
           subtract  1                    from w-aux-cit-geo-ei0      .
           go to     aco-985.
       aco-986.
           add       2                    to   w-aux-cit-geo-ei0      .
           string    w-aux-cit-geo-esp    
                                delimited by   size
                                          into w-aux-cit-geo-ede
                                  with pointer w-aux-cit-geo-ei0      .
       aco-989.
           exit.
       aco-999.
           exit.
