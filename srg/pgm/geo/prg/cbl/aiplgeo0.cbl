       Identification Division.
       Program-Id.                                 aiplgeo0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    geo                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/07/91    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione Indirizzo + C.a.p. e    *
      *                    citta' per le anagrafiche che utilizzano    *
      *                    il 'geo'.                                   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-ind-cec-geo-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "AC"                 *
      *                                                                *
      *                       w-ind-cec-geo-naz : codice nazione       *
      *                                                                *
      *                       w-ind-cec-geo-ind : indirizzo            *
      *                                                                *
      *                       w-ind-cec-geo-lin : "" linea             *
      *                                                                *
      *                       w-ind-cec-geo-pos : "" posizione         *
      *                                                                *
      *                       w-ind-cec-geo-cec : c.a.p. e citta'      *
      *                                                                *
      *                       w-ind-cec-geo-cmn : codice comune        *
      *                                                                *
      *                       w-ind-cec-geo-dco : descrizione comune   *
      *                                                                *
      *                       w-ind-cec-geo-prv : sigla provincia      *
      *                                                                *
      *                       w-ind-cec-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-ind-cec-geo-dfr : descrizione frazione *
      *                                                                *
      *                       w-ind-cec-geo-lct : codice localita'     *
      *                                                                *
      *                       w-ind-cec-geo-dlo : descrizione localita'*
      *                                                                *
      *                                                                *
      *              Output : w-ind-cec-geo-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-ind-cec-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-ind-cec-geo-aut : automatismo eseguito *
      *                                           - Spaces : No        *
      *                                           - S      : Si        *
      *                                                                *
      *                       w-ind-cec-geo-naz : codice nazione       *
      *                                                                *
      *                       w-ind-cec-geo-ind : indirizzo            *
      *                                                                *
      *                       w-ind-cec-geo-cec : c.a.p. e citta'      *
      *                                                                *
      *                       w-ind-cec-geo-cmn : codice comune        *
      *                                                                *
      *                       w-ind-cec-geo-dco : descrizione comune   *
      *                                                                *
      *                       w-ind-cec-geo-prv : sigla provincia      *
      *                                                                *
      *                       w-ind-cec-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-ind-cec-geo-dfr : descrizione frazione *
      *                                                                *
      *                       w-ind-cec-geo-lct : codice localita'     *
      *                                                                *
      *                       w-ind-cec-geo-dlo : descrizione localita'*
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-ind-cec-geo-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-ind-cec-geo-aut : automatismo eseguito *
      *                                           - Spaces : No        *
      *                                           - S      : Si        *
      *                                                                *
      *                       w-ind-cec-geo-naz : codice nazione       *
      *                                                                *
      *                       w-ind-cec-geo-ind : indirizzo            *
      *                                                                *
      *                       w-ind-cec-geo-cec : c.a.p. e citta'      *
      *                                                                *
      *                       w-ind-cec-geo-cmn : codice comune        *
      *                                                                *
      *                       w-ind-cec-geo-dco : descrizione comune   *
      *                                                                *
      *                       w-ind-cec-geo-prv : sigla provincia      *
      *                                                                *
      *                       w-ind-cec-geo-fzn : codice frazione      *
      *                                                                *
      *                       w-ind-cec-geo-dfr : descrizione frazione *
      *                                                                *
      *                       w-ind-cec-geo-lct : codice localita'     *
      *                                                                *
      *                       w-ind-cec-geo-dlo : descrizione localita'*
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "IL"  Composizione dell'indirizzo, dato l'indirizzo precedente *
      *       e la descrizione della localita'                         *
      *                                                                *
      *              Input  : w-ind-cec-geo-ope : "IL"                 *
      *                                                                *
      *                       w-ind-cec-geo-ind : indirizzo vecchio    *
      *                                                                *
      *                       w-ind-cec-geo-dlo : descrizione localita'*
      *                                                                *
      *                                                                *
      *              Output : w-ind-cec-geo-ind : indirizzo nuovo      *
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
      *        * [gxs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxs"                          .
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
       01  w-aux-cec-geo.
           05  w-aux-cec-geo-w40.
               10  w-aux-cec-geo-w4a.
                   15  w-aux-cec-geo-w4x
                               occurs 40  pic  x(01)                  .
               10  w-aux-cec-geo-w4b      pic  x(40)                  .
           05  w-aux-cec-geo-upp.
               10  w-aux-cec-geo-uuc      pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
               10  w-aux-cec-geo-uur redefines
                   w-aux-cec-geo-uuc.
                   15  w-aux-cec-geo-uux
                               occurs 26  pic  x(01)                  .
               10  w-aux-cec-geo-ulc      pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
               10  w-aux-cec-geo-ulr redefines
                   w-aux-cec-geo-ulc.
                   15  w-aux-cec-geo-ulx
                               occurs 26  pic  x(01)                  .
           05  w-aux-cec-geo-sgs          pic  x(05)                  .
           05  w-aux-cec-geo-via          pic  x(30)                  .
           05  w-aux-cec-geo-civ          pic  x(20)                  .
           05  w-aux-cec-geo-dup          pic  x(30)                  .
           05  w-aux-cec-geo-dmx.
               10  w-aux-cec-geo-dch
                               occurs 30  pic  x(01)                  .
           05  w-aux-cec-geo-nli          pic  9(02)                  .
           05  w-aux-cec-geo-crb          pic  9(04)                  .
           05  w-aux-cec-geo-crc          pic  9(04)                  .
           05  w-aux-cec-geo-cpb          pic  9(04)                  .
           05  w-aux-cec-geo-cpa          pic  9(04)                  .
           05  w-aux-cec-geo-bix          pic  9(04)                  .
           05  w-aux-cec-geo-bxy          pic  x(77)                  .
           05  w-aux-cec-geo-ltp          pic  x(17)                  .
           05  w-aux-cec-geo-le1          pic  x(03)                  .
           05  w-aux-cec-geo-le2          pic  x(03)                  .
           05  w-aux-cec-geo-c01          pic  9(04)                  .
           05  w-aux-cec-geo-c02          pic  9(04)                  .
           05  w-aux-cec-geo-c03          pic  9(04)                  .
           05  w-aux-cec-geo-c04          pic  9(04)                  .
           05  w-aux-cec-geo-c05          pic  9(04)                  .
           05  w-aux-cec-geo-c06          pic  9(02)                  .
           05  w-aux-cec-geo-c07          pic  9(02)                  .
           05  w-aux-cec-geo-ele.
               10  w-aux-cec-geo-eix      pic  9(03)                  .
               10  w-aux-cec-geo-ect      pic  x(01)                  .
               10  w-aux-cec-geo-evi      pic  x(30)                  .
               10  w-aux-cec-geo-ecc      pic  9(05)                  .
               10  w-aux-cec-geo-edc      pic  x(30)                  .
               10  w-aux-cec-geo-ecf      pic  9(03)                  .
               10  w-aux-cec-geo-edf      pic  x(30)                  .
               10  w-aux-cec-geo-ecl      pic  9(03)                  .
               10  w-aux-cec-geo-edl      pic  x(30)                  .
               10  w-aux-cec-geo-esp      pic  x(03)                  .
               10  w-aux-cec-geo-edp      pic  x(25)                  .
               10  w-aux-cec-geo-ecp      pic  x(05)                  .
               10  w-aux-cec-geo-edx      pic  x(30)                  .
               10  w-aux-cec-geo-ein.
                   15  w-aux-cec-geo-eiy
                               occurs 40  pic  x(01)                  .
               10  w-aux-cec-geo-eci.
                   15  w-aux-cec-geo-ecy
                               occurs 40  pic  x(01)                  .
               10  w-aux-cec-geo-exy      pic  x(40)                  .

      *    *===========================================================*
      *    * Continuazione della work per subroutine di accettazione   *
      *    * estrapolata unicamente per far si' che il gruppo di dati  *
      *    * non superi i 64 Kb                                        *
      *    *-----------------------------------------------------------*
       01  w-aux-cec-geo-ex1.
           05  w-aux-cec-geo-tel
                          occurs 999      pic  x(01)                  .
       01  w-aux-cec-geo-ex2.
           05  w-aux-cec-geo-cmn
                          occurs 999      pic  9(05)                  .
       01  w-aux-cec-geo-ex3.
           05  w-aux-cec-geo-fzn
                          occurs 999      pic  9(03)                  .
       01  w-aux-cec-geo-ex4.
           05  w-aux-cec-geo-lct
                          occurs 999      pic  9(03)                  .
       01  w-aux-cec-geo-ex5.
           05  w-aux-cec-geo-des
                          occurs 999      pic  x(30)                  .
       01  w-aux-cec-geo-ex6.
           05  w-aux-cec-geo-spc
                          occurs 999      pic  x(30)                  .
       01  w-aux-cec-geo-ex7.
           05  w-aux-cec-geo-avp
                          occurs 999      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per normalizzazione di un campo alfabetico in un va- *
      *    * lore privo di spaces e di caratteri non compresi tra i    *
      *    * limiti A..Z - 0..9                                        *
      *    *                                                           *
      *    * Work per match tra due valori cosi' normalizzati          *
      *    *-----------------------------------------------------------*
       01  w-atz-1t9.
      *        *-------------------------------------------------------*
      *        * Valore da normalizzare, o primo valore per il match   *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-vdn.
               10  w-atz-1t9-vdn-chr
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore normalizzato, o secondo valore per il match    *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-vno.
               10  w-atz-1t9-vno-chr
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di match in uscita                               *
      *        *  - Spaces : Ok, match                                 *
      *        *  - N      : Ko, no match                              *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-flg-mch          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici locali                            *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-inx-vdn          pic  9(02)                  .
           05  w-atz-1t9-inx-vno          pic  9(02)                  .
           05  w-atz-1t9-ctr-mch          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per messaggio di errore                          *
      *        *-------------------------------------------------------*
           05  w-err-msg-err              pic  x(65)                  .

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
      *    * Link-area per accettazione Indirizzo, C.a.p. e citta'     *
      *    *-----------------------------------------------------------*
           copy      "pgm/geo/prg/cpy/aiplgeo0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-ind-cec-geo
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
           if        w-ind-cec-geo-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-ind-cec-geo-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-ind-cec-geo-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-ind-cec-geo-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-ind-cec-geo-ope    =    "A+" or
                     w-ind-cec-geo-ope    =    "F+" or
                     w-ind-cec-geo-ope    =    "I+"
                     perform   aco-000    thru aco-999
           else if   w-ind-cec-geo-ope    =    "IL"
                     perform   ilo-000    thru ilo-999                .
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
      *                  * Open file [gxs]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
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
      *                  *---------------------------------------------*
      *                  * Close file [gxsp]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
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
                     move  spaces         to   w-ind-cec-geo-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Flag in uscita di automatismo eseguito a : No   *
      *              *-------------------------------------------------*
           move      spaces               to   w-ind-cec-geo-aut      .
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find, se co-   *
      *              * dice nazione non-Italia                         *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-naz    =    "IT "
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgeo5000"           to   s-pro                  .
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
           move      w-ind-cec-geo-naz    to   w-ind-cec-geo-s01      .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-ind    to   w-ind-cec-geo-s02      .
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cec    to   w-ind-cec-geo-s03      .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-cmn    to   w-ind-cec-geo-s04      .
      *                  *---------------------------------------------*
      *                  * Descrizione comune                          *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-dco    to   w-ind-cec-geo-s05      .
      *                  *---------------------------------------------*
      *                  * Sigla provincia                             *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-prv    to   w-ind-cec-geo-s06      .
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-fzn    to   w-ind-cec-geo-s07      .
      *                  *---------------------------------------------*
      *                  * Descrizione frazione                        *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-dfr    to   w-ind-cec-geo-s08      .
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-lct    to   w-ind-cec-geo-s09      .
      *                  *---------------------------------------------*
      *                  * Descrizione localita'                       *
      *                  *---------------------------------------------*
           move      w-ind-cec-geo-dlo    to   w-ind-cec-geo-s10      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing originale               *
      *                  *---------------------------------------------*
           move      v-edm                to   w-ind-cec-geo-s70      .
      *                  *---------------------------------------------*
      *                  * User function keys originali epurate        *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-ind-cec-geo-s90      .
       acc-500.
      *              *-------------------------------------------------*
      *              * Accettazione in formato alfanumerico            *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-ind-cec-geo-s90    to   v-ufk                  .
           move      w-ind-cec-geo-lin    to   v-lin                  .
           move      w-ind-cec-geo-pos    to   v-pos                  .
           move      w-ind-cec-geo-ind    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-ind-cec-geo-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-ope    =    "I+"
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
           move      w-ind-cec-geo-s90    to   v-ufk                  .
           move      w-ind-cec-geo-lin    to   v-lin                  .
           move      w-ind-cec-geo-pos    to   v-pos                  .
           move      w-ind-cec-geo-ind    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-ind-cec-geo-ope      .
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
           move      v-alf                to   w-ind-cec-geo-ind      .
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt                              *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-225.
      *                      *-----------------------------------------*
      *                      * Ripristino valore originale per Indi-   *
      *                      * rizzo                                   *
      *                      *-----------------------------------------*
           move      w-ind-cec-geo-s02    to   w-ind-cec-geo-ind      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione Indirizzo originale   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-ind-cec-geo-lin    to   v-lin                  .
           move      w-ind-cec-geo-pos    to   v-pos                  .
           move      w-ind-cec-geo-ind    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-ind-cec-geo-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-225.
      *                  *---------------------------------------------*
      *                  * Se codice nazione diverso da 'IT '          *
      *                  *---------------------------------------------*
           if        w-ind-cec-geo-naz    =    "IT "
                     go to aco-275.
       aco-250.
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-ind-cec-geo-ope      .
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
      *                  * de al formato semiautomatico :              *
      *                  *                                             *
      *                  * - ss... vv..., nn...                        *
      *                  *                                             *
      *                  *   ss... : sigla dello stradario             *
      *                  *   vv... : chiave per la via                 *
      *                  *   nn... : numero civico (opzionale)         *
      *                  *                                             *
      *                  * Se non e' di formato semiautomatico si va'  *
      *                  * a reimpostazione                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Impostazione in area di ridefinizione   *
      *                      *-----------------------------------------*
           move      w-ind-cec-geo-ind    to   w-aux-cec-geo-w4a      .
       aco-280.
      *                      *-----------------------------------------*
      *                      * Estrazione del numero civico, conside-  *
      *                      * rando tale la porzione del valore im-   *
      *                      * postato successiva al carattere ','     *
      *                      * eliminando la porzione estratta dal va- *
      *                      * lore impostato; se numero civico oltre  *
      *                      * i venti caratteri : a reimpostazione    *
      *                      *-----------------------------------------*
           move      spaces               to   w-aux-cec-geo-w4b      .
           move      zero                 to   w-aux-cec-geo-c01      .
           inspect   w-aux-cec-geo-w4a
                                      tallying w-aux-cec-geo-c01
                                for characters
                                before initial ","                    .
           if        w-aux-cec-geo-c01    =    40
                     go to aco-283.
           add       1                    to   w-aux-cec-geo-c01      .
       aco-281.
           add       1                    to   w-aux-cec-geo-c01      .
           if        w-aux-cec-geo-c01    >    40
                     go to aco-282.
           if        w-aux-cec-geo-w4x
                    (w-aux-cec-geo-c01)   =    spaces
                     go to aco-281.
           unstring  w-aux-cec-geo-w4a    into w-aux-cec-geo-w4b
                                  with pointer w-aux-cec-geo-c01      .
       aco-282.
           inspect   w-aux-cec-geo-w4a
                     replacing characters by   spaces
                                         after initial       ","      .
           inspect   w-aux-cec-geo-w4a
                                replacing all  ","        by spaces   .
       aco-283.
           move      w-aux-cec-geo-w4b    to   w-aux-cec-geo-civ      .
           if        w-aux-cec-geo-civ    =    w-aux-cec-geo-w4b
                     go to aco-285.
           move      "Formato di impostazione errato !                  
      -              "               "    to   w-err-msg-err          .
           perform   err-000              thru err-999                .
           go to     aco-150.
       aco-285.
      *                      *-----------------------------------------*
      *                      * Area di ridefinizione in uppercase      *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-cec-geo-c01      .
       aco-286.
           add       1                    to   w-aux-cec-geo-c01      .
           if        w-aux-cec-geo-c01    >    30
                     go to aco-290.
           move      zero                 to   w-aux-cec-geo-c02      .
           inspect   w-aux-cec-geo-ulc
                                      tallying w-aux-cec-geo-c02
                                for characters
                                before initial w-aux-cec-geo-w4x
                                              (w-aux-cec-geo-c01)     .
           if        w-aux-cec-geo-c02    not  < 26
                     go to aco-286.
           add       1                    to   w-aux-cec-geo-c02      .
           move      w-aux-cec-geo-uux
                    (w-aux-cec-geo-c02)   to   w-aux-cec-geo-w4x
                                              (w-aux-cec-geo-c01)     .
           go to     aco-286.
       aco-290.
      *                      *-----------------------------------------*
      *                      * Estrazione della sigla dello stradario  *
      *                      * fino ad un massimo di cinque caratteri; *
      *                      * se oltre cinque caratteri : a reimpo-   *
      *                      * stazione                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-aux-cec-geo-w4b      .
           string    w-aux-cec-geo-w4a
                                delimited by   spaces
                                          into w-aux-cec-geo-w4b      .
           move      w-aux-cec-geo-w4b    to   w-aux-cec-geo-sgs      .
           if        w-aux-cec-geo-sgs    =    w-aux-cec-geo-w4b
                     go to aco-292.
           move      "Formato di impostazione errato !                  
      -              "               "    to   w-err-msg-err          .
           perform   err-000              thru err-999                .
           go to     aco-150.
       aco-292.
      *                      *-----------------------------------------*
      *                      * Test che sia stata impostata la sigla   *
      *                      * dello stradario                         *
      *                      *-----------------------------------------*
           if        w-aux-cec-geo-sgs    not  = spaces
                     go to aco-295.
           move      "Manca la sigla dello stradario per la ricerca !"
                                          to   w-err-msg-err          .
           perform   err-000              thru err-999                .
           go to     aco-150.
       aco-295.
      *                      *-----------------------------------------*
      *                      * Test che esista uno stradario con la    *
      *                      * sigla pari a quella dedotta, se non e-  *
      *                      * siste segnalazione e reimpostazione     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start su stradari                   *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SGLDES    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cec-geo-sgs    to   rf-gxs-sgl-std         .
           move      spaces               to   rf-gxs-des-key         .
           move      spaces               to   rf-gxs-spc-key         .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *                          *-------------------------------------*
      *                          * Se Start Ok : a read next           *
      *                          *-------------------------------------*
           if        f-sts                =    e-not-err
                     go to aco-297.
       aco-296.
      *                          *-------------------------------------*
      *                          * Messaggio se stradario non esisten- *
      *                          * te                                  *
      *                          *-------------------------------------*
           move      spaces               to   w-err-msg-err          .
           string    "Non esiste uno stradario con sigla '"
                                delimited by   size
                     w-aux-cec-geo-sgs
                                delimited by   spaces
                     "' !"      delimited by   size
                                          into w-err-msg-err          .
           perform   err-000              thru err-999                .
           go to     aco-150.
       aco-297.
      *                          *-------------------------------------*
      *                          * Read next su stradari               *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *                          *-------------------------------------*
      *                          * Se At End : a segnalazione di stra- *
      *                          * dario non esistente                 *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-296.
      *                          *-------------------------------------*
      *                          * Se non e' lo stradario cercato : a  *
      *                          * segnalazione di stradario non esi-  *
      *                          * stente, altrimenti lo stradario e-  *
      *                          * siste                               *
      *                          *-------------------------------------*
           if        rf-gxs-sgl-std       not  = w-aux-cec-geo-sgs
                     go to aco-296.
       aco-300.
      *                      *-----------------------------------------*
      *                      * Eliminazione della sigla dello strada-  *
      *                      * rio dal valore impostato messo in area  *
      *                      * di ridefinizione                        *
      *                      *-----------------------------------------*
           inspect   w-aux-cec-geo-w4a
                     replacing characters by   spaces
                                before initial spaces                 .
       aco-305.
      *                      *-----------------------------------------*
      *                      * Estrazione della via per un massimo di  *
      *                      * trenta caratteri; se oltre trenta ca-   *
      *                      * ratteri a reimpostazione                *
      *                      *-----------------------------------------*
           move      spaces               to   w-aux-cec-geo-w4b      .
           move      zero                 to   w-aux-cec-geo-c01      .
           inspect   w-aux-cec-geo-w4a
                                      tallying w-aux-cec-geo-c01
                                   for leading spaces                 .
           add       1                    to   w-aux-cec-geo-c01      .
           unstring  w-aux-cec-geo-w4a
                                          into w-aux-cec-geo-w4b
                                  with pointer w-aux-cec-geo-c01      .
           move      w-aux-cec-geo-w4b    to   w-aux-cec-geo-via      .
           if        w-aux-cec-geo-via    not  = w-aux-cec-geo-w4b
                     go to aco-150.
      *                      *-----------------------------------------*
      *                      * Se manca la via : a reimpostazione      *
      *                      *-----------------------------------------*
           if        w-aux-cec-geo-via    not  = spaces
                     go to aco-310.
           move      "Formato di impostazione errato !                  
      -              "               "    to   w-err-msg-err          .
           perform   err-000              thru err-999                .
           go to     aco-150.
       aco-310.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore impostato in *
      *                      * formato privo di spaces e di caratteri  *
      *                      * diversi da A..Z - 0..9, e salvataggio   *
      *                      * del risultato                           *
      *                      *-----------------------------------------*
           move      w-aux-cec-geo-via    to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-aux-cec-geo-dup      .
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
           move      zero                 to   w-aux-cec-geo-crb      .
       aco-450.
      *                      *-----------------------------------------*
      *                      * Start                                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SGLDES    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cec-geo-sgs    to   rf-gxs-sgl-std         .
           move      spaces               to   rf-gxs-des-key         .
           move      spaces               to   rf-gxs-spc-key         .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
      *                      *-----------------------------------------*
      *                      * Se start errata : a trattamento per fi- *
      *                      * ne file                                 *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-625.
       aco-475.
      *                      *-----------------------------------------*
      *                      * Read next                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Esecuzione read next                *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxs                 .
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
           if        rf-gxs-sgl-std       not  = w-aux-cec-geo-sgs
                     go to aco-625.
       aco-525.
      *                      *-----------------------------------------*
      *                      * Selezione                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su descrizione                 *
      *                          *-------------------------------------*
           if        rf-gxs-des-key       =    spaces and
                     rf-gxs-spc-key       =    spaces
                     go to aco-475.
      *                          *-------------------------------------*
      *                          * Normalizzazione A..Z - 0..9 e pre-  *
      *                          * parazione 2. valore per il match    *
      *                          *-------------------------------------*
           move      rf-gxs-des-key       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                          *-------------------------------------*
      *                          * Preparazione 1. valore per il match *
      *                          *-------------------------------------*
           move      w-aux-cec-geo-dup    to   w-atz-1t9-vdn          .
      *                          *-------------------------------------*
      *                          * Match tra i due valori              *
      *                          *-------------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                          *-------------------------------------*
      *                          * Se c'e' stato un match : a buffe-   *
      *                          * rizzazione con indice match a 11    *
      *                          *-------------------------------------*
           if        w-atz-1t9-flg-mch    =    spaces
                     go to aco-550
           else      go to aco-475.
       aco-550.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record letto            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero records bufferiz- *
      *                          * zati                                *
      *                          *-------------------------------------*
           add       1                    to   w-aux-cec-geo-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre la massima capacita' del   *
      *                          * buffer : a trattamento per fine     *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-cec-geo-crb    >    999
                     subtract  1          from w-aux-cec-geo-crb
                     go to     aco-625.
       aco-575.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo elemento                   *
      *                              *---------------------------------*
           if        rf-gxs-cod-lct       not  = zero
                     move  "L"            to   w-aux-cec-geo-tel
                                              (w-aux-cec-geo-crb)
           else if   rf-gxs-cod-fzn       not  = zero
                     move  "F"            to   w-aux-cec-geo-tel
                                              (w-aux-cec-geo-crb)
           else      move  "C"            to   w-aux-cec-geo-tel
                                              (w-aux-cec-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice comune                   *
      *                              *---------------------------------*
           move      rf-gxs-cod-cmn       to   w-aux-cec-geo-cmn
                                              (w-aux-cec-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice frazione                 *
      *                              *---------------------------------*
           move      rf-gxs-cod-fzn       to   w-aux-cec-geo-fzn
                                              (w-aux-cec-geo-crb)     .
      *                              *---------------------------------*
      *                              * Codice localita'                *
      *                              *---------------------------------*
           move      rf-gxs-cod-lct       to   w-aux-cec-geo-lct
                                              (w-aux-cec-geo-crb)     .
      *                              *---------------------------------*
      *                              * Descrizione reale per la strada *
      *                              *---------------------------------*
           move      rf-gxs-des-str       to   w-aux-cec-geo-des
                                              (w-aux-cec-geo-crb)     .
      *                              *---------------------------------*
      *                              * Ulteriore specifica per la      *
      *                              * chiave                          *
      *                              *---------------------------------*
           move      zero                 to   w-aux-cec-geo-c07      .
           inspect   rf-gxs-spc-key   tallying w-aux-cec-geo-c07
                     for   characters   before initial   ","          .
           add       2                    to   w-aux-cec-geo-c07      .
           move      spaces               to   w-aux-cec-geo-spc
                                              (w-aux-cec-geo-crb)     .
           unstring  rf-gxs-spc-key       into w-aux-cec-geo-spc
                                              (w-aux-cec-geo-crb)
                                  with pointer w-aux-cec-geo-c07      .
      *                              *---------------------------------*
      *                              * C.a.p. elemento                 *
      *                              *---------------------------------*
           move      rf-gxs-cap-avp       to   w-aux-cec-geo-avp
                                              (w-aux-cec-geo-crb)     .
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
           if        w-aux-cec-geo-crb    =    zero
                     go to aco-650
           else if   w-aux-cec-geo-crb    =    1
                     go to aco-675
           else      go to aco-750.
       aco-650.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer pari a zero                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Messaggio per ricerca fallita   *
      *                              *---------------------------------*
           move      spaces               to   w-err-msg-err          .
           string    "Nessuna via pari a quella impostata nello stradari
      -              "o '"      delimited by   size
                     w-aux-cec-geo-sgs
                                delimited by   spaces
                     "' !"      delimited by   size
                                          into w-err-msg-err          .
           perform   err-000              thru err-999                .
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
           move      1                    to   w-aux-cec-geo-bix      .
       aco-700.
      *                              *---------------------------------*
      *                              * Ritorno con automatismo dell'e- *
      *                              * lemento caricato nel buffer con *
      *                              * indice (w-aux-cec-geo-bix)      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Letture addizionali relati- *
      *                                  * ve all'elemento del buffer  *
      *                                  * indice (w-aux-cec-geo-eix)  *
      *                                  * con composizione dell'area  *
      *                                  * w-aux-cec-geo-ele.          *
      *                                  *-----------------------------*
           move      w-aux-cec-geo-bix    to   w-aux-cec-geo-eix      .
           perform   aco-960              thru aco-989                .
      *                                  *-----------------------------*
      *                                  * Valori determinati in usci- *
      *                                  * ta                          *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Indirizzo               *
      *                                      *-------------------------*
           move      w-aux-cec-geo-ein    to   w-ind-cec-geo-ind      .
      *                                      *-------------------------*
      *                                      * C.a.p. e citta'         *
      *                                      *-------------------------*
           move      w-aux-cec-geo-eci    to   w-ind-cec-geo-cec      .
      *                                      *-------------------------*
      *                                      * Codice Comune           *
      *                                      *-------------------------*
           move      w-aux-cec-geo-ecc    to   w-ind-cec-geo-cmn      .
      *                                      *-------------------------*
      *                                      * Descrizione Comune      *
      *                                      *-------------------------*
           move      w-aux-cec-geo-edc    to   w-ind-cec-geo-dco      .
      *                                      *-------------------------*
      *                                      * Sigla Provincia         *
      *                                      *-------------------------*
           move      w-aux-cec-geo-esp    to   w-ind-cec-geo-prv      .
      *                                      *-------------------------*
      *                                      * Codice Frazione         *
      *                                      *-------------------------*
           move      w-aux-cec-geo-ecf    to   w-ind-cec-geo-fzn      .
      *                                      *-------------------------*
      *                                      * Descrizione Frazione    *
      *                                      *-------------------------*
           move      w-aux-cec-geo-edf    to   w-ind-cec-geo-dfr      .
      *                                      *-------------------------*
      *                                      * Codice Localita'        *
      *                                      *-------------------------*
           move      w-aux-cec-geo-ecl    to   w-ind-cec-geo-lct      .
      *                                      *-------------------------*
      *                                      * Descrizione Localita'   *
      *                                      *-------------------------*
           move      w-aux-cec-geo-edl    to   w-ind-cec-geo-dlo      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione Indirizzo   *
      *                                  * selezionato                 *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-ind-cec-geo-lin    to   v-lin                  .
           move      w-ind-cec-geo-pos    to   v-pos                  .
           move      w-ind-cec-geo-ind    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Segnale di automatismo ese- *
      *                                  * guito                       *
      *                                  *-----------------------------*
           move      "S"                  to   w-ind-cec-geo-aut      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione a : non     *
      *                                  * continuazione               *
      *                                  *-----------------------------*
           move      "AC"                 to   w-ind-cec-geo-ope      .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-750.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer superiori a 1                *
      *                          *-------------------------------------*
       aco-800.
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-cec-geo-crb    to   w-aux-cec-geo-cpb      .
           subtract  1                    from w-aux-cec-geo-cpb      .
           divide    12                   into w-aux-cec-geo-cpb      .
           add       1                    to   w-aux-cec-geo-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-cec-geo-c01      .
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
           move      04                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      76                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "                  Selezionare l'elemento desiderat
      -              "o                  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "-------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "-------------------"
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
      *                              * di indice (w-aux-cec-geo-c01)   *
      *                              *---------------------------------*
           divide    12                   into w-aux-cec-geo-c01
                                        giving w-aux-cec-geo-c05
                                     remainder w-aux-cec-geo-nli      .
           if        w-aux-cec-geo-nli    =    zero
                     move  12             to   w-aux-cec-geo-nli      .
           add       06                   to   w-aux-cec-geo-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-cec-geo-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-cec-geo-c01    <    w-aux-cec-geo-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-cec-geo-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-cec-geo-cpa    <    w-aux-cec-geo-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXPD"               to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-cec-geo-nli    to   v-lin                  .
           move      13                   to   v-pos                  .
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
           move      "Via           :"    to   v-alf                  .
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
      *                                      * dice (w-aux-cec-geo-eix)*
      *                                      * nel buffer, con compo-  *
      *                                      * sizione dell'area work  *
      *                                      * w-aux-cec-geo-ele.      *
      *                                      *-------------------------*
           move      w-aux-cec-geo-c01    to   w-aux-cec-geo-eix      .
           perform   aco-960              thru aco-989                .
      *                                      *-------------------------*
      *                                      * Visualizzazioni         *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Via                 *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-cec-geo-evi    to   v-alf                  .
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
           move      w-aux-cec-geo-esp    to   v-alf                  .
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
           move      w-aux-cec-geo-edp    to   v-alf                  .
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
           move      w-aux-cec-geo-ecc    to   v-num                  .
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
           move      w-aux-cec-geo-edp    to   v-alf                  .
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
           move      w-aux-cec-geo-ecf    to   v-num                  .
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
           move      w-aux-cec-geo-edf    to   v-alf                  .
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
           move      w-aux-cec-geo-ecl    to   v-num                  .
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
           move      w-aux-cec-geo-edl    to   v-alf                  .
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
           move      w-aux-cec-geo-c01    to   w-aux-cec-geo-bix      .
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
           subtract  1                    from w-aux-cec-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cec-geo-nli    =    07
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
           if        w-aux-cec-geo-c01    =    w-aux-cec-geo-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cec-geo-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cec-geo-nli    =    18
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
           add       1                    to   w-aux-cec-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cec-geo-cpa    to   w-aux-cec-geo-c01      .
           multiply  12                   by   w-aux-cec-geo-c01      .
           subtract  11                   from w-aux-cec-geo-c01      .
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
           subtract  1                    from w-aux-cec-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cec-geo-cpa    to   w-aux-cec-geo-c01      .
           multiply  12                   by   w-aux-cec-geo-c01      .
           subtract  11                   from w-aux-cec-geo-c01      .
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
           move      w-aux-cec-geo-c01    to   w-aux-cec-geo-c02      .
           add       11                   to   w-aux-cec-geo-c02      .
           divide    12                   into w-aux-cec-geo-c02      .
           move      w-aux-cec-geo-c02    to   w-aux-cec-geo-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cec-geo-c02      .
           multiply  12                   by   w-aux-cec-geo-c02      .
           add       1                    to   w-aux-cec-geo-c02      .
           add       11
                     w-aux-cec-geo-c02  giving w-aux-cec-geo-c03      .
           move      w-aux-cec-geo-c03    to   w-aux-cec-geo-c04      .
           if        w-aux-cec-geo-c03    >    w-aux-cec-geo-crb
                     move  w-aux-cec-geo-crb
                                          to   w-aux-cec-geo-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-cec-geo-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * C.a.p.                  *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-aux-cec-geo-c05    to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-aux-cec-geo-avp
                    (w-aux-cec-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Descrizione reale per   *
      *                                      * la strada               *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-cec-geo-c05    to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-aux-cec-geo-des
                    (w-aux-cec-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Ulteriore specifica per *
      *                                      * la chiave della strada  *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-cec-geo-c05    to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-aux-cec-geo-spc
                    (w-aux-cec-geo-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cec-geo-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cec-geo-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-cec-geo-c02    not  > w-aux-cec-geo-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-cec-geo-c02    >    w-aux-cec-geo-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-cec-geo-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      69                   to   v-car                  .
           move      w-aux-cec-geo-c05    to   v-lin                  .
           move      06                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-cec-geo-c02      .
           add       1                    to   w-aux-cec-geo-c05      .
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
           move      w-aux-cec-geo-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cec-geo-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-cec-geo-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cec-geo-le2      .
           move      spaces               to   w-aux-cec-geo-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-cec-geo-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-cec-geo-le2
                                delimited by   spaces
                                          into w-aux-cec-geo-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-aux-cec-geo-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-960.
      *                              *---------------------------------*
      *                              * Subroutine interna per letture  *
      *                              * addizionali relative ad elemen- *
      *                              * to indice (w-aux-cec-geo-eix),  *
      *                              * con composizione dell'area di   *
      *                              * work w-aux-cec-geo-ele.         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazioni iniziali gene- *
      *                                  * rali                        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Tipo elemento           *
      *                                      *-------------------------*
           move      w-aux-cec-geo-tel
                    (w-aux-cec-geo-eix)   to   w-aux-cec-geo-ect      .
      *                                      *-------------------------*
      *                                      * Descrizione reale per   *
      *                                      * la strada               *
      *                                      *-------------------------*
           move      w-aux-cec-geo-des
                    (w-aux-cec-geo-eix)   to   w-aux-cec-geo-evi      .
      *                                      *-------------------------*
      *                                      * Codice comune           *
      *                                      *-------------------------*
           move      w-aux-cec-geo-cmn
                    (w-aux-cec-geo-eix)   to   w-aux-cec-geo-ecc      .
      *                                      *-------------------------*
      *                                      * Codice frazione         *
      *                                      *-------------------------*
           move      w-aux-cec-geo-fzn
                    (w-aux-cec-geo-eix)   to   w-aux-cec-geo-ecf      .
      *                                      *-------------------------*
      *                                      * Codice localita'        *
      *                                      *-------------------------*
           move      w-aux-cec-geo-lct
                    (w-aux-cec-geo-eix)   to   w-aux-cec-geo-ecl      .
      *                                      *-------------------------*
      *                                      * C.a.p. elemento         *
      *                                      *-------------------------*
           move      w-aux-cec-geo-avp
                    (w-aux-cec-geo-eix)   to   w-aux-cec-geo-ecp      .
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del    *
      *                                  * tipo elemento               *
      *                                  *-----------------------------*
           if        w-aux-cec-geo-ect    =    "L"
                     go to aco-975
           else if   w-aux-cec-geo-ect    =    "F"
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
           move      zero                 to   w-aux-cec-geo-ecf      .
           move      spaces               to   w-aux-cec-geo-edf      .
      *                                      *-------------------------*
      *                                      * Normalizzazione della   *
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      zero                 to   w-aux-cec-geo-ecl      .
           move      spaces               to   w-aux-cec-geo-edl      .
       aco-967.
      *                                      *-------------------------*
      *                                      * Lettura record per il   *
      *                                      * Comune                  *
      *                                      *-------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-aux-cec-geo-ecc    to   rf-gxc-cod-cmn         .
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
           move      rf-gxc-des-cfl       to   w-aux-cec-geo-edc      .
      *                                      *-------------------------*
      *                                      * Sigla provincia         *
      *                                      *-------------------------*
           move      rf-gxc-cod-prv       to   w-aux-cec-geo-esp      .
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
           move      zero                 to   w-aux-cec-geo-ecl      .
           move      spaces               to   w-aux-cec-geo-edl      .
       aco-972.
      *                                      *-------------------------*
      *                                      * Lettura record per la   *
      *                                      * Frazione                *
      *                                      *-------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-aux-cec-geo-ecc    to   rf-gxc-cod-cmn         .
           move      w-aux-cec-geo-ecf    to   rf-gxc-cod-fzn         .
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
      *                                      *-------------------------*
      *                                      * Descrizione Frazione    *
      *                                      *-------------------------*
           move      rf-gxc-des-cfl       to   w-aux-cec-geo-edf      .
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
           move      w-aux-cec-geo-ecc    to   rf-gxc-cod-cmn         .
           move      w-aux-cec-geo-ecf    to   rf-gxc-cod-fzn         .
           move      w-aux-cec-geo-ecl    to   rf-gxc-cod-lct         .
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
           move      rf-gxc-des-cfl       to   w-aux-cec-geo-edl      .
      *                                      *-------------------------*
      *                                      * A lettura record per la *
      *                                      * Frazione se codice fra- *
      *                                      * zione diverso da zero   *
      *                                      * altrimenti a lettura    *
      *                                      * record per il comune    *
      *                                      *-------------------------*
           if        w-aux-cec-geo-ecf    =    zero
                     go to aco-967
           else      go to aco-972.
       aco-980.
      *                                  *-----------------------------*
      *                                  * Preparazioni finali genera- *
      *                                  * li                          *
      *                                  *-----------------------------*
       aco-981.
      *                                      *-------------------------*
      *                                      * Lettura provincia       *
      *                                      *-------------------------*
           if        w-aux-cec-geo-esp    =    spaces
                     move  spaces         to   rf-gxp-des-prv
                     go to aco-982.
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-aux-cec-geo-esp    to   rf-gxp-cod-prv         .
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
           move      rf-gxp-des-prv       to   w-aux-cec-geo-edp      .
       aco-983.
      *                                      *-------------------------*
      *                                      * Descrizione Indirizzo   *
      *                                      *-------------------------*
           perform   ind-000              thru ind-999                .
      *                                      *-------------------------*
      *                                      * Descrizione C.a.p. e    *
      *                                      * citta'                  *
      *                                      *-------------------------*
           perform   cec-000              thru cec-999                .
       aco-989.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Composizione nuovo indirizzo da vecchio indirizzo e de-   *
      *    * scrizione localita'                                       *
      *    *-----------------------------------------------------------*
       ilo-000.
      *              *-------------------------------------------------*
      *              * Se la descrizione della localita' e' a Spaces   *
      *              * non si esegue alcuna azione                     *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-dlo    =    spaces
                     go to ilo-999.
      *              *-------------------------------------------------*
      *              * Se l'indirizzo attuale e' a spaces lo si pone   *
      *              * semplicemente pari alla descrizione della lo-   *
      *              * calita'                                         *
      *              *-------------------------------------------------*
           if        w-ind-cec-geo-ind    =    spaces
                     move  w-ind-cec-geo-dlo
                                          to   w-ind-cec-geo-ind
                     go to ilo-999.
       ilo-200.
      *              *-------------------------------------------------*
      *              * Concatenamento Descrizione localita' ed Indi-   *
      *              * rizzo, separati da un trattino, in area di co-  *
      *              * modo                                            *
      *              *-------------------------------------------------*
           move      w-ind-cec-geo-dlo    to   w-aux-cec-geo-eci      .
           move      w-ind-cec-geo-ind    to   w-aux-cec-geo-ein      .
           move      30                   to   w-aux-cec-geo-c07      .
       ilo-225.
           if        w-aux-cec-geo-ecy
                    (w-aux-cec-geo-c07)   =    spaces
                     subtract  1          from w-aux-cec-geo-c07
                     go to     ilo-225.
       ilo-250.
           add       02                   to   w-aux-cec-geo-c07      .
           move      "-"                  to   w-aux-cec-geo-ecy
                                              (w-aux-cec-geo-c07)     .
           add       01                   to   w-aux-cec-geo-c07      .
       ilo-275.
           move      zero                 to   w-aux-cec-geo-c06      .
       ilo-300.
           add       01                   to   w-aux-cec-geo-c07      .
           if        w-aux-cec-geo-c07    >    40
                     go to ilo-400.
           add       01                   to   w-aux-cec-geo-c06      .
           move      w-aux-cec-geo-eiy
                    (w-aux-cec-geo-c06)   to   w-aux-cec-geo-ecy
                                              (w-aux-cec-geo-c07)     .
           go to     ilo-300.
       ilo-400.
      *              *-------------------------------------------------*
      *              * Area di comodo concatenata in uscita            *
      *              *-------------------------------------------------*
           move      w-aux-cec-geo-eci    to   w-ind-cec-geo-ind      .
       ilo-999.
           exit.

      *    *===========================================================*
      *    * Composizione indirizzo da area w-aux-cec-geo-ele.         *
      *    *-----------------------------------------------------------*
       ind-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo elemento          *
      *              *-------------------------------------------------*
           if        w-aux-cec-geo-ect    =    "L"
                     go to ind-400.
       ind-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento Comune o Frazione              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indirizzo da descrizione reale per la stra- *
      *                  * da                                          *
      *                  *---------------------------------------------*
           move      w-aux-cec-geo-evi    to   w-aux-cec-geo-ein      .
      *                  *---------------------------------------------*
      *                  * A trattamento numero civico                 *
      *                  *---------------------------------------------*
           go to     ind-600.
       ind-400.
      *              *-------------------------------------------------*
      *              * Se tipo elemento Localita'                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indirizzo da descrizione localita' piu' de- *
      *                  * scrizione reale per la strada               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se descrizione localita' a spaces : so- *
      *                      * lo descrizione reale per la strada      *
      *                      *-----------------------------------------*
           if        w-aux-cec-geo-edl    =    spaces
                     go to ind-200.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione localita'      *
      *                      *-----------------------------------------*
           move      w-aux-cec-geo-edl    to   w-aux-cec-geo-ein      .
      *                      *-----------------------------------------*
      *                      * Se descrizione reale per la strada a    *
      *                      * spaces : a trattamento numero civico    *
      *                      *-----------------------------------------*
           if        w-aux-cec-geo-evi    =    spaces
                     go to ind-600.
       ind-450.
      *                      *-----------------------------------------*
      *                      * Concatenamento Localita' e Strada sepa- *
      *                      * rati da un trattino                     *
      *                      *-----------------------------------------*
           move      30                   to   w-aux-cec-geo-c06      .
       ind-460.
           if        w-aux-cec-geo-eiy
                    (w-aux-cec-geo-c06)   not  = spaces
                     go to ind-470.
           subtract  1                    from w-aux-cec-geo-c06      .
           go to     ind-460.
       ind-470.
           add       2                    to   w-aux-cec-geo-c06      .
           move      "-"                  to   w-aux-cec-geo-eiy
                                              (w-aux-cec-geo-c06)     .
           add       2                    to   w-aux-cec-geo-c06      .
           string    w-aux-cec-geo-evi
                                delimited by   size
                                          into w-aux-cec-geo-ein
                                  with pointer w-aux-cec-geo-c06      .
       ind-600.
      *              *-------------------------------------------------*
      *              * Trattamento numero civico                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non c'e' numero civico : no trattamento  *
      *                  * numero civico                               *
      *                  *---------------------------------------------*
           if        w-aux-cec-geo-civ    =    spaces
                     go to ind-999.
      *                  *---------------------------------------------*
      *                  * Se valore attuale a spaces : no trattamento *
      *                  * numero civico                               *
      *                  *---------------------------------------------*
           if        w-aux-cec-geo-ein    =    spaces
                     go to ind-999.
      *                  *---------------------------------------------*
      *                  * Se non almeno tre caratteri a spaces finali *
      *                  * : no trattamento numero civico              *
      *                  *---------------------------------------------*
           if        w-aux-cec-geo-eiy (40)
                                          not  = spaces or
                     w-aux-cec-geo-eiy (39)
                                          not  = spaces or
                     w-aux-cec-geo-eiy (38)
                                          not  = spaces
                     go to ind-999.
       ind-650.
      *                  *---------------------------------------------*
      *                  * Concatenamento numero civico                *
      *                  *---------------------------------------------*
           move      40                   to   w-aux-cec-geo-c06      .
       ind-660.
           if        w-aux-cec-geo-eiy
                    (w-aux-cec-geo-c06)   not  = spaces
                     go to ind-670.
           subtract  1                    from w-aux-cec-geo-c06      .
           go to     ind-660.
       ind-670.
           add       1                    to   w-aux-cec-geo-c06      .
______*    move      ","                  to   w-aux-cec-geo-eiy
______*                                       (w-aux-cec-geo-c06)     .
      *
           move      " "                  to   w-aux-cec-geo-eiy
                                              (w-aux-cec-geo-c06)     .
      *
           add       1                    to   w-aux-cec-geo-c06      .
           string    w-aux-cec-geo-civ
                                delimited by   size
                                          into w-aux-cec-geo-ein
                                  with pointer w-aux-cec-geo-c06      .
       ind-999.
           exit.

      *    *===========================================================*
      *    * Composizione C.a.p. e localita' da area w-aux-cec-geo-ele.*
      *    *-----------------------------------------------------------*
       cec-000.
      *              *-------------------------------------------------*
      *              * Descrizione elemento in area di comodo          *
      *              *-------------------------------------------------*
           if        w-aux-cec-geo-ect    =    "L"
                     go to cec-050
           else if   w-aux-cec-geo-ect    =    "F"
                     move  w-aux-cec-geo-edf
                                          to   w-aux-cec-geo-exy
                     go to cec-100
           else      move  w-aux-cec-geo-edc
                                          to   w-aux-cec-geo-exy
                     go to cec-100.
       cec-050.
           if        w-aux-cec-geo-ecf    not  = zero
                     move  w-aux-cec-geo-edf
                                          to   w-aux-cec-geo-exy
           else      move  w-aux-cec-geo-edc
                                          to   w-aux-cec-geo-exy      .
       cec-100.
      *              *-------------------------------------------------*
      *              * Spaces preliminari in area di destinazione      *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-cec-geo-eci      .
      *              *-------------------------------------------------*
      *              * Se descrizione elemento a spaces : uscita       *
      *              *-------------------------------------------------*
           if        w-aux-cec-geo-exy    =    spaces
                     go to cec-999.
       cec-200.
      *              *-------------------------------------------------*
      *              * Se C.a.p. a spaces : solo descrizione elemento, *
      *              * altrimenti sia C.a.p. che descrizione           *
      *              *-------------------------------------------------*
           if        w-aux-cec-geo-ecp    =    spaces
                     go to cec-250.
           string    w-aux-cec-geo-ecp
                                delimited by   size
                     " "        delimited by   size
                     w-aux-cec-geo-exy
                                delimited by   size
                                          into w-aux-cec-geo-eci      .
           go to     cec-400.
       cec-250.
           move      w-aux-cec-geo-exy    to   w-aux-cec-geo-eci      .
       cec-400.
      *              *-------------------------------------------------*
      *              * Trattamento sigla provincia                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non c'e' sigla provincia : no trattamen- *
      *                  * to sigla provincia                          *
      *                  *---------------------------------------------*
           if        w-aux-cec-geo-esp    =    spaces
                     go to cec-999.
      *                  *---------------------------------------------*
      *                  * Se valore attuale a spaces : no trattamento *
      *                  * numero civico                               *
      *                  *---------------------------------------------*
           if        w-aux-cec-geo-eci    =    spaces
                     go to cec-999.
      *                  *---------------------------------------------*
      *                  * Se non almeno quattro caratteri a spaces    *
      *                  * finali : no trattamento sigla provincia     *
      *                  *---------------------------------------------*
           if        w-aux-cec-geo-ecy (40)
                                          not  = spaces or
                     w-aux-cec-geo-ecy (39)
                                          not  = spaces or
                     w-aux-cec-geo-ecy (38)
                                          not  = spaces or
                     w-aux-cec-geo-ecy (37)
                                          not  = spaces
                     go to cec-999.
       cec-450.
      *                  *---------------------------------------------*
      *                  * Concatenamento sigla provincia              *
      *                  *---------------------------------------------*
           move      40                   to   w-aux-cec-geo-c06      .
       cec-460.
           if        w-aux-cec-geo-ecy
                    (w-aux-cec-geo-c06)   not  = spaces
                     go to cec-470.
           subtract  1                    from w-aux-cec-geo-c06      .
           go to     cec-460.
       cec-470.
           add       2                    to   w-aux-cec-geo-c06      .
           string    w-aux-cec-geo-esp
                                delimited by   size
                                          into w-aux-cec-geo-eci
                                  with pointer w-aux-cec-geo-c06      .
       cec-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      13                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-msg-err        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       err-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione di un campo alfabetico in un valore privo *
      *    * di spaces e di caratteri non compresi tra i limiti A..Z   *
      *    * oppure 0..9                                               *
      *    *-----------------------------------------------------------*
       nor-atz-1t9-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del valore di ori-  *
      *              * gine                                            *
      *              *-------------------------------------------------*
           move      w-atz-1t9-vdn        to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-atz-1t9-vdn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare valore di destina-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-atz-1t9-vno          .
      *              *-------------------------------------------------*
      *              * Indice su valore di origine a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-atz-1t9-inx-vdn      .
      *              *-------------------------------------------------*
      *              * Indice su valore di destinazione a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   w-atz-1t9-inx-vno      .
       nor-atz-1t9-100.
      *              *-------------------------------------------------*
      *              * Incremento indice su valore di origine          *
      *              *-------------------------------------------------*
           add       1                    to   w-atz-1t9-inx-vdn      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-atz-1t9-inx-vdn    >    40
                     go to nor-atz-1t9-999.
      *              *-------------------------------------------------*
      *              * Se il carattere di origine in esame e' compreso *
      *              * tra A..Z, lo si sposta nella destinazione e si  *
      *              * ricicla sul carattere di origine successivo     *
      *              *-------------------------------------------------*
           if        w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  < "A" and
                     w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  > "Z"
                     add   1              to   w-atz-1t9-inx-vno
                     move  w-atz-1t9-vdn-chr
                          (w-atz-1t9-inx-vdn)
                                          to   w-atz-1t9-vno-chr
                                              (w-atz-1t9-inx-vno)
                     go to nor-atz-1t9-100.
      *              *-------------------------------------------------*
      *              * Se il carattere di origine in esame e' compreso *
      *              * tra 0..9, lo si sposta nella destinazione e si  *
      *              * ricicla sul carattere di origine successivo     *
      *              *-------------------------------------------------*
           if        w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  < "0" and
                     w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  > "9"
                     add   1              to   w-atz-1t9-inx-vno
                     move  w-atz-1t9-vdn-chr
                          (w-atz-1t9-inx-vdn)
                                          to   w-atz-1t9-vno-chr
                                              (w-atz-1t9-inx-vno)
                     go to nor-atz-1t9-100.
      *              *-------------------------------------------------*
      *              * In ogni altro caso si ignora il carattere di o- *
      *              * rigine e si ricicla sul carattere di origine    *
      *              * successivo                                      *
      *              *-------------------------------------------------*
           go to     nor-atz-1t9-100.
       nor-atz-1t9-999.
           exit.

      *    *===========================================================*
      *    * Match tra due valori normalizzati A..Z - 0..9             *
      *    *-----------------------------------------------------------*
       mch-atz-1t9-000.
           move      zero                 to   w-atz-1t9-inx-vno      .
           inspect   w-atz-1t9-vdn    tallying w-atz-1t9-inx-vno
                                  for trailing spaces                 .
           move      40                   to   w-atz-1t9-inx-vdn      .
           subtract  w-atz-1t9-inx-vno    from w-atz-1t9-inx-vdn      .
           move      zero                 to   w-atz-1t9-ctr-mch      .
           inspect   w-atz-1t9-vno    tallying w-atz-1t9-ctr-mch
                                       for all w-atz-1t9-vdn
                                              (1 : w-atz-1t9-inx-vdn) .
           if        w-atz-1t9-ctr-mch    =    zero
                     move  "N"            to   w-atz-1t9-flg-mch
           else      move  spaces         to   w-atz-1t9-flg-mch      .
       mch-atz-1t9-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
