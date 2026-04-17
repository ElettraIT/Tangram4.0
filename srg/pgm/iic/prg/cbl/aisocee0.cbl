       Identification Division.
       Program-Id.                                 aisocee0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    iic                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 19/01/93    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice ISO stato CEE    *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cde-iso-cee-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cde-iso-cee-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cde-iso-cee-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cde-iso-cee-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Accettazione                                             *
      *                                                                *
      *              Input  : w-cde-iso-cee-ope : "AC"                 *
      *                                                                *
      *                       w-cde-iso-cee-iso : codice ISO stato CEE *
      *                                                                *
      *                       w-cde-iso-cee-lin : linea per il codice  *
      *                                                                *
      *                       w-cde-iso-cee-pos : posizione per il co- *
      *                                           dice                 *
      *                                                                *
      *                       w-cde-iso-cee-dln : linea per la descri- *
      *                                           zione                *
      *                                                                *
      *                       w-cde-iso-cee-dps : posizione per la de- *
      *                                           scrizione            *
      *                                                                *
      *                                                                *
      *              Output : w-cde-iso-cee-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cde-iso-cee-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cde-iso-cee-ope : "A+" = Accettazione  *
      *                                                  da rieseguire *
      *                                           "AC" = Accettazione  *
      *                                                  completata    *
      *                                                                *
      *                       w-cde-iso-cee-iso : codice ISO stato CEE *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .

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
       01  w-aux-iso-cee.
           05  w-aux-iso-cee-c01          pic  9(02)                  .
           05  w-aux-iso-cee-c02          pic  9(02)                  .
           05  w-aux-iso-cee-c03          pic  9(02)                  .
           05  w-aux-iso-cee-c04          pic  9(02)                  .
           05  w-aux-iso-cee-c05          pic  9(02)                  .
           05  w-aux-iso-cee-nli          pic  9(02)                  .
           05  w-aux-iso-cee-d20          pic  x(20)                  .
           05  w-aux-iso-cee-tpf          pic  x(01)                  .
           05  w-aux-iso-cee-crb          pic  9(02)                  .
           05  w-aux-iso-cee-cpb          pic  9(02)                  .
           05  w-aux-iso-cee-cpa          pic  9(02)                  .
           05  w-aux-iso-cee-buf
                               occurs 30.
               10  w-aux-iso-cee-cbu      pic  x(03)                  .
               10  w-aux-iso-cee-dbu.
                   15  w-aux-iso-cee-fbu  pic  x(01)                  .
                   15  filler             pic  x(19)                  .
           05  w-aux-iso-cee-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-iso-cee-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-iso-cee-lt2      pic  9(01)                  .
           05  w-aux-iso-cee-dup          pic  x(20)                  .
           05  w-aux-iso-cee-dmx.
               10  w-aux-iso-cee-dch
                               occurs 20  pic  x(01)                  .

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
      *    * Link-area per accettazione codice ISO stato CEE           *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/aisocee0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cde-iso-cee
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
           if        w-cde-iso-cee-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cde-iso-cee-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cde-iso-cee-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cde-iso-cee-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cde-iso-cee-ope    =    "A+" or
                     w-cde-iso-cee-ope    =    "F+"
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
      *              *-------------------------------------------------*
      *              * Open file [gxn]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
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
      *              *-------------------------------------------------*
      *              * Close file [gxn]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
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
                     move  spaces         to   w-cde-iso-cee-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           if        v-pfk (04)           not  = "INSR"
                     go to acc-100.
           move      spaces               to   v-pfk (04)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore originale del codice                 *
      *                  *---------------------------------------------*
           move      w-cde-iso-cee-iso    to   w-cde-iso-cee-svo      .
      *                  *---------------------------------------------*
      *                  * User function keys                          *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cde-iso-cee-ufk      .
       acc-200.
      *              *-------------------------------------------------*
      *              * Preparazione accettazione alfanumerica          *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-ufk    to   v-ufk                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-svo    to   v-alf                  .
       acc-400.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cde-iso-cee-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro con function key Find           *
      *              *-------------------------------------------------*
           if        w-cde-iso-cee-ope    =    "F+"
                     go to aco-400.
      *              *-------------------------------------------------*
      *              * Salvataggio valore impostato                    *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cde-iso-cee-alf      .
       aco-025.
      *              *-------------------------------------------------*
      *              * Se Exit o Delt                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-050.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione valore originale          *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-svo    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione completata       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cde-iso-cee-ope      .
      *                  *---------------------------------------------*
      *                  * Valore originale in uscita                  *
      *                  *---------------------------------------------*
           move      w-cde-iso-cee-svo    to   w-cde-iso-cee-iso      .
      *                  *---------------------------------------------*
      *                  * Linea e posizione per descrizione a zero    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cde-iso-cee-dln      .
           move      zero                 to   w-cde-iso-cee-dps      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-050.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico accettato a spaces       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-cde-iso-cee-alf    not  = spaces
                     go to aco-075.
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione completata       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cde-iso-cee-ope      .
      *                  *---------------------------------------------*
      *                  * Valore in uscita : a spaces                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cde-iso-cee-iso      .
      *                  *---------------------------------------------*
      *                  * Linea e posizione per descrizione a zero    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cde-iso-cee-dln      .
           move      zero                 to   w-cde-iso-cee-dps      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-075.
      *              *-------------------------------------------------*
      *              * Se blanks embedded nel valore alfanumerico ac-  *
      *              * cettato                                         *
      *              *-------------------------------------------------*
       aco-077.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           move      1                    to   w-aux-iso-cee-c01      .
       aco-079.
           add       1                    to   w-aux-iso-cee-c01      .
           if        w-aux-iso-cee-c01    >    03
                     go to aco-100.
           if        w-cde-iso-cee-cha
                    (w-aux-iso-cee-c01)   not  = spaces
                     go to aco-079.
       aco-081.
           add       1                    to   w-aux-iso-cee-c01      .
           if        w-aux-iso-cee-c01    >    03
                     go to aco-100.
           if        w-cde-iso-cee-cha
                    (w-aux-iso-cee-c01)   =    spaces
                     go to aco-081.
       aco-083.
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione da rieseguire    *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cde-iso-cee-ope      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione alfanumerica      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-ufk    to   v-ufk                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se impostazione normale, e non per ricerca per  *
      *              * descrizione                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-cde-iso-cee-alf    =    "-" and
                     w-cde-iso-cee-dln    not  = zero
                     go to aco-300.
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione completata       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cde-iso-cee-ope      .
      *                  *---------------------------------------------*
      *                  * Valore in uscita : pari al valore accettato *
      *                  *---------------------------------------------*
           move      w-cde-iso-cee-alf    to   w-cde-iso-cee-iso      .
      *                  *---------------------------------------------*
      *                  * Linea e posizione per descrizione a zero    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cde-iso-cee-dln      .
           move      zero                 to   w-cde-iso-cee-dps      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-300.
      *              *-------------------------------------------------*
      *              * Se ricerca per descrizione                      *
      *              *-------------------------------------------------*
       aco-305.
      *                  *---------------------------------------------*
      *                  * Comodo di accettazione per la descrizione   *
      *                  * a spaces                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-iso-cee-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione a spaces        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cde-iso-cee-dln    to   v-lin                  .
           move      w-cde-iso-cee-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-310.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cde-iso-cee-dln    to   v-lin                  .
           move      w-cde-iso-cee-dps    to   v-pos                  .
           move      w-aux-iso-cee-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-iso-cee-dup      .
       aco-315.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-320.
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione a spaces    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cde-iso-cee-dln    to   v-lin                  .
           move      w-cde-iso-cee-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione da riesegui- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cde-iso-cee-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione alfanumerica  *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-ufk    to   v-ufk                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-320.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-325.
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione completata   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cde-iso-cee-ope      .
      *                      *-----------------------------------------*
      *                      * Valore originale in uscita              *
      *                      *-----------------------------------------*
           move      w-cde-iso-cee-svo    to   w-cde-iso-cee-iso      .
      *                      *-----------------------------------------*
      *                      * Linea e posizione per descrizione a ze- *
      *                      * ro                                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-cde-iso-cee-dln      .
           move      zero                 to   w-cde-iso-cee-dps      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-325.
      *                  *---------------------------------------------*
      *                  * Se impostazione a spaces                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-aux-iso-cee-dup    not  = spaces
                     go to aco-330.
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione da riesegui- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cde-iso-cee-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione alfanumerica  *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-ufk    to   v-ufk                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-330.
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione massima            *
      *                  *---------------------------------------------*
       aco-331.
           move      w-aux-iso-cee-dup    to   w-aux-iso-cee-dmx      .
           move      20                   to   w-aux-iso-cee-c01      .
       aco-332.
           if        w-aux-iso-cee-c01    >    zero
                     if    w-aux-iso-cee-dch
                          (w-aux-iso-cee-c01)
                                          =    spaces
                           move     "z"   to   w-aux-iso-cee-dch
                                              (w-aux-iso-cee-c01)
                           subtract 1     from w-aux-iso-cee-c01
                           go to    aco-332.
       aco-335.
      *                  *---------------------------------------------*
      *                  * Tipo ricerca in corso : per descrizione     *
      *                  *---------------------------------------------*
           move      "D"                  to   w-aux-iso-cee-tpf      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records con la stessa *
      *                  * descrizione nel buffer                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-iso-cee-crb      .
       aco-340.
      *                  *---------------------------------------------*
      *                  * Start su [gxn]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESUPP    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-iso-cee-dup    to   rf-gxn-des-upp         .
           move      spaces               to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-600.
       aco-345.
      *                  *---------------------------------------------*
      *                  * Read Next su [gxn]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-600.
       aco-350.
      *                  *---------------------------------------------*
      *                  * Se nazione non CEE : la si ignora           *
      *                  *---------------------------------------------*
           if        rf-gxn-snx-cee       not  = "S"
                     go to aco-345.
       aco-355.
      *                  *---------------------------------------------*
      *                  * Descrizione letta in area di lavoro di 20   *
      *                  * caratteri                                   *
      *                  *---------------------------------------------*
           move      rf-gxn-des-naz       to   w-aux-iso-cee-d20      .
      *                  *---------------------------------------------*
      *                  * Trasformazione in uppercase della descri-   *
      *                  * zione nel comodo di lavoro                  *
      *                  *---------------------------------------------*
           move      w-aux-iso-cee-d20    to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-aux-iso-cee-d20      .
       aco-360.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : fine lettura          *
      *                  *---------------------------------------------*
           if        w-aux-iso-cee-d20    <    w-aux-iso-cee-dup or
                     w-aux-iso-cee-d20    >    w-aux-iso-cee-dmx
                     go to aco-600.
       aco-365.
      *                  *---------------------------------------------*
      *                  * Incremento numero records nel buffer        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-iso-cee-crb      .
       aco-370.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice nazione CEE                      *
      *                      *-----------------------------------------*
           move      rf-gxn-cod-naz       to   w-aux-iso-cee-cbu
                                              (w-aux-iso-cee-crb)     .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      rf-gxn-des-naz       to   w-aux-iso-cee-dbu
                                              (w-aux-iso-cee-crb)     .
       aco-375.
      *                  *---------------------------------------------*
      *                  * Se raggiunta la massima capacita' del buf-  *
      *                  * fer : come per fine file                    *
      *                  *---------------------------------------------*
           if        w-aux-iso-cee-crb    =    30
                     go to aco-600.
      *                  *---------------------------------------------*
      *                  * Altrimenti : riciclo a lettura record suc-  *
      *                  * cessivo                                     *
      *                  *---------------------------------------------*
           go to     aco-345.
       aco-400.
      *              *-------------------------------------------------*
      *              * Se ricerca per function key Find                *
      *              *-------------------------------------------------*
       aco-405.
      *                  *---------------------------------------------*
      *                  * Tipo ricerca in corso : per tasto Find      *
      *                  *---------------------------------------------*
           move      "F"                  to   w-aux-iso-cee-tpf      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-iso-cee-crb      .
       aco-410.
      *                  *---------------------------------------------*
      *                  * Start su [gxn]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODNAZ    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      spaces               to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-600.
       aco-415.
      *                  *---------------------------------------------*
      *                  * Read Next su [gxn]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-600.
       aco-420.
      *                  *---------------------------------------------*
      *                  * Se nazione non CEE : la si ignora           *
      *                  *---------------------------------------------*
           if        rf-gxn-snx-cee       not  = "S"
                     go to aco-415.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Incremento numero records nel buffer        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-iso-cee-crb      .
       aco-430.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice nazione CEE                      *
      *                      *-----------------------------------------*
           move      rf-gxn-cod-naz       to   w-aux-iso-cee-cbu
                                              (w-aux-iso-cee-crb)     .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      rf-gxn-des-naz       to   w-aux-iso-cee-dbu
                                              (w-aux-iso-cee-crb)     .
       aco-435.
      *                  *---------------------------------------------*
      *                  * Se raggiunta la massima capacita' del buf-  *
      *                  * fer : come per fine file                    *
      *                  *---------------------------------------------*
           if        w-aux-iso-cee-crb    =    30
                     go to aco-600.
      *                  *---------------------------------------------*
      *                  * Altrimenti : riciclo a lettura record suc-  *
      *                  * cessivo                                     *
      *                  *---------------------------------------------*
           go to     aco-415.
       aco-600.
      *              *-------------------------------------------------*
      *              * Se raggiunta la fine file in ricerche           *
      *              *-------------------------------------------------*
       aco-625.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero records   *
      *                  * trovati                                     *
      *                  *---------------------------------------------*
           if        w-aux-iso-cee-crb    =    zero
                     go to aco-650
           else if   w-aux-iso-cee-crb    =    1
                     go to aco-675
           else      go to aco-700.
       aco-650.
      *                  *---------------------------------------------*
      *                  * Se trovati zero records                     *
      *                  *---------------------------------------------*
       aco-655.
      *                      *-----------------------------------------*
      *                      * Visualizzazione del segnale di record   *
      *                      * non trovato                             *
      *                      *-----------------------------------------*
           if        w-cde-iso-cee-dln    =    zero
                     go to aco-660.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cde-iso-cee-dln    to   v-lin                  .
           move      w-cde-iso-cee-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-660.
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione da riesegui- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cde-iso-cee-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-ufk    to   v-ufk                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-675.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo record                   *
      *                  *---------------------------------------------*
       aco-680.
      *                      *-----------------------------------------*
      *                      * Codice numerico in valore accettato     *
      *                      *-----------------------------------------*
           move      w-aux-iso-cee-cbu (1)
                                          to   w-cde-iso-cee-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore del codice       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione completata   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cde-iso-cee-ope      .
      *                      *-----------------------------------------*
      *                      * Valore in uscita : pari al valore sele- *
      *                      * zionato                                 *
      *                      *-----------------------------------------*
           move      w-cde-iso-cee-alf    to   w-cde-iso-cee-iso      .
      *                      *-----------------------------------------*
      *                      * Linea e posizione per descrizione a ze- *
      *                      * ro                                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-cde-iso-cee-dln      .
           move      zero                 to   w-cde-iso-cee-dps      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-700.
      *                  *---------------------------------------------*
      *                  * Se trovati piu' records                     *
      *                  *---------------------------------------------*
       aco-705.
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-aux-iso-cee-crb    to   w-aux-iso-cee-cpb      .
           subtract  1                    from w-aux-iso-cee-cpb      .
           divide    6                    into w-aux-iso-cee-cpb      .
           add       1                    to   w-aux-iso-cee-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-iso-cee-c01      .
       aco-710.
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
           move      09                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      73                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "     Selezionare il codice ISO dello stato CEE des
      -              "iderato     "       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   aco-950              thru aco-959                .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-715.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-aux-iso-cee-c01    to   w-aux-iso-cee-nli      .
       aco-720.
           if        w-aux-iso-cee-nli    >    6
                     subtract  6          from w-aux-iso-cee-nli
                     go to aco-720.
           add       11                   to   w-aux-iso-cee-nli      .
       aco-725.
      *                      *-----------------------------------------*
      *                      * Accettazione function key               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-iso-cee-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-iso-cee-c01    <    w-aux-iso-cee-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-iso-cee-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-iso-cee-cpa    <    w-aux-iso-cee-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-iso-cee-nli    to   v-lin                  .
           move      28                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-730.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-735
           else if   v-key                =    "UP  "
                     go to aco-740
           else if   v-key                =    "DOWN"
                     go to aco-745
           else if   v-key                =    "NXSC"
                     go to aco-750
           else if   v-key                =    "PRSC"
                     go to aco-755
           else if   v-key                =    "EXIT"
                     go to aco-760
           else      go to aco-725.
       aco-735.
      *                      *-----------------------------------------*
      *                      * Se Return o Slct o Do                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ripristino immagine video           *
      *                          *-------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice selezionato in valore accet- *
      *                          * tato                                *
      *                          *-------------------------------------*
           move      w-aux-iso-cee-cbu
                    (w-aux-iso-cee-c01)   to   w-cde-iso-cee-alf      .
      *                          *-------------------------------------*
      *                          * Rivisualizzazione valore seleziona- *
      *                          * to                                  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo uscita : accettazione comple-  *
      *                          * tata                                *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cde-iso-cee-ope      .
      *                          *-------------------------------------*
      *                          * Valore in uscita : pari al valore   *
      *                          * selezionato                         *
      *                          *-------------------------------------*
           move      w-cde-iso-cee-alf    to   w-cde-iso-cee-iso      .
      *                          *-------------------------------------*
      *                          * Linea e posizione per descrizione a *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-cde-iso-cee-dln      .
           move      zero                 to   w-cde-iso-cee-dps      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-740.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Decremento contatore record attual- *
      *                          * mente trattato nel buffer           *
      *                          *-------------------------------------*
           subtract  1                    from w-aux-iso-cee-c01      .
      *                          *-------------------------------------*
      *                          * Se non si era alla prima riga della *
      *                          * pagina : a reimpostazione           *
      *                          *-------------------------------------*
           if        w-aux-iso-cee-nli    not  = 12
                     go to aco-715.
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina contenente   *
      *                          * il record attualmente trattato      *
      *                          *-------------------------------------*
           perform   aco-950              thru aco-959                .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     aco-715.
       aco-745.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore record attual- *
      *                          * mente trattato nel buffer           *
      *                          *-------------------------------------*
           add       1                    to   w-aux-iso-cee-c01      .
      *                          *-------------------------------------*
      *                          * Se non si era all'ultima riga della *
      *                          * pagina : a reimpostazione           *
      *                          *-------------------------------------*
           if        w-aux-iso-cee-nli    not  = 17
                     go to aco-715.
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina contenente   *
      *                          * il record attualmente trattato      *
      *                          *-------------------------------------*
           perform   aco-950              thru aco-959                .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     aco-715.
       aco-750.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero record per la *
      *                          * prima riga del video successivo     *
      *                          *-------------------------------------*
           add       1                    to   w-aux-iso-cee-cpa      .
           move      w-aux-iso-cee-cpa    to   w-aux-iso-cee-c01      .
           multiply  6                    by   w-aux-iso-cee-c01      .
           subtract  5                    from w-aux-iso-cee-c01      .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina contenente   *
      *                          * il record attualmente trattato      *
      *                          *-------------------------------------*
           perform   aco-950              thru aco-959                .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     aco-715.
       aco-755.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero record per la *
      *                          * prima riga del video precedente     *
      *                          *-------------------------------------*
           subtract  1                    from w-aux-iso-cee-cpa      .
           move      w-aux-iso-cee-cpa    to   w-aux-iso-cee-c01      .
           multiply  6                    by   w-aux-iso-cee-c01      .
           subtract  5                    from w-aux-iso-cee-c01      .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina contenente   *
      *                          * il record attualmente trattato      *
      *                          *-------------------------------------*
           perform   aco-950              thru aco-959                .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     aco-715.
       aco-760.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
       aco-765.
      *                          *-------------------------------------*
      *                          * Ripristino immagine video           *
      *                          *-------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * ricerca in corso                    *
      *                          *-------------------------------------*
           if        w-aux-iso-cee-tpf    =    "D"
                     go to aco-775
           else      go to aco-780.
       aco-775.
      *                          *-------------------------------------*
      *                          * Se era in corso una ricerca per la  *
      *                          * descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione descri- *
      *                              * zione in uppercase              *
      *                              *---------------------------------*
           go to     aco-310.
       aco-780.
      *                          *-------------------------------------*
      *                          * Se era in corso una ricerca per il  *
      *                          * tasti funzione Find                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo uscita : accettazione da   *
      *                              * rieseguire                      *
      *                              *---------------------------------*
           move      "A+"                 to   w-cde-iso-cee-ope      .
      *                              *---------------------------------*
      *                              * Normalizzazione function key    *
      *                              *---------------------------------*
           move      spaces               to   v-key                  .
      *                              *---------------------------------*
      *                              * Preparazione accettazione alfa- *
      *                              * numerica                        *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cde-iso-cee-ufk    to   v-ufk                  .
           move      w-cde-iso-cee-lin    to   v-lin                  .
           move      w-cde-iso-cee-pos    to   v-pos                  .
           move      w-cde-iso-cee-alf    to   v-alf                  .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-950.
      *              *-------------------------------------------------*
      *              * Subroutine per la visualizzazione della pagina  *
      *              * video contenente il record attualmente trattato *
      *              *-------------------------------------------------*
       aco-951.
      *                  *---------------------------------------------*
      *                  * Preparazioni preliminari                    *
      *                  *---------------------------------------------*
           move      w-aux-iso-cee-c01    to   w-aux-iso-cee-c02      .
           add       5                    to   w-aux-iso-cee-c02      .
           divide    6                    into w-aux-iso-cee-c02      .
           move      w-aux-iso-cee-c02    to   w-aux-iso-cee-cpa      .
           subtract  1                    from w-aux-iso-cee-c02      .
           multiply  6                    by   w-aux-iso-cee-c02      .
           add       1                    to   w-aux-iso-cee-c02      .
           add       5
                     w-aux-iso-cee-c02  giving w-aux-iso-cee-c03      .
           move      w-aux-iso-cee-c03    to   w-aux-iso-cee-c04      .
           if        w-aux-iso-cee-c03    >    w-aux-iso-cee-crb
                     move  w-aux-iso-cee-crb
                                          to   w-aux-iso-cee-c03      .
           move      12                   to   w-aux-iso-cee-c05      .
       aco-953.
      *                  *---------------------------------------------*
      *                  * Visualizzazioni relative alla riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codici ISO dello stato CEE              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-aux-iso-cee-c05    to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-iso-cee-cbu
                    (w-aux-iso-cee-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-aux-iso-cee-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-aux-iso-cee-dbu
                    (w-aux-iso-cee-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-iso-cee-c02      .
           add       1                    to   w-aux-iso-cee-c05      .
           if        w-aux-iso-cee-c02    not  > w-aux-iso-cee-c03
                     go to aco-953.
       aco-955.
      *                  *---------------------------------------------*
      *                  * Visualizzazione righe residue a spaces      *
      *                  *---------------------------------------------*
           if        w-aux-iso-cee-c02    >    w-aux-iso-cee-c04
                     go to aco-957.
           if        w-aux-iso-cee-crb    not  > 6
                     go to aco-957.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      w-aux-iso-cee-c05    to   v-lin                  .
           move      10                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-iso-cee-c02      .
           add       1                    to   w-aux-iso-cee-c05      .
           go to     aco-955.
       aco-957.
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina               *
      *                  *---------------------------------------------*
           move      w-aux-iso-cee-cpa    to   w-aux-iso-cee-lt1      .
           move      w-aux-iso-cee-cpb    to   w-aux-iso-cee-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-iso-cee-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
