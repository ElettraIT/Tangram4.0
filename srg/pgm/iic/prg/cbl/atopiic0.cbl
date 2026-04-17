       Identification Division.
       Program-Id.                                 atopiic0           .
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
      * Descrizione pgm:   Modulo accettazione codice tipo operazione  *
      *                    per gestione movimenti Iva intracomunitaria *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cmn-top-iic-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cmn-top-iic-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cmn-top-iic-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-top-iic-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Accettazione                                             *
      *                                                                *
      *              Input  : w-cmn-top-iic-ope : "AC"                 *
      *                                                                *
      *                       w-cmn-top-iic-top : codice tipo opera-   *
      *                                           zione                *
      *                                                                *
      *                       w-cmn-top-iic-lin : linea per il codice  *
      *                                                                *
      *                       w-cmn-top-iic-pos : posizione per il co- *
      *                                           dice                 *
      *                                                                *
      *                       w-cmn-top-iic-dln : linea per la descri- *
      *                                           zione                *
      *                                                                *
      *                       w-cmn-top-iic-dps : posizione per la de- *
      *                                           scrizione            *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-top-iic-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cmn-top-iic-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-top-iic-ope : "A+" = Accettazione  *
      *                                                  da rieseguire *
      *                                           "AC" = Accettazione  *
      *                                                  completata    *
      *                                                                *
      *                       w-cmn-top-iic-top : codice tipo opera-   *
      *                                           zione                *
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
      *    * Tabella tipi operazione per gestione Iva intracomunitaria *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.tab"                   .

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
       01  w-aux-top-iic.
           05  w-aux-top-iic-c01          pic  9(02)                  .
           05  w-aux-top-iic-c02          pic  9(02)                  .
           05  w-aux-top-iic-c03          pic  9(02)                  .
           05  w-aux-top-iic-c04          pic  9(02)                  .
           05  w-aux-top-iic-c05          pic  9(02)                  .
           05  w-aux-top-iic-nli          pic  9(02)                  .
           05  w-aux-top-iic-m04          pic  x(04)                  .
           05  w-aux-top-iic-d50          pic  x(50)                  .
           05  w-aux-top-iic-tpf          pic  x(01)                  .
           05  w-aux-top-iic-crb          pic  9(02)                  .
           05  w-aux-top-iic-cpb          pic  9(02)                  .
           05  w-aux-top-iic-cpa          pic  9(02)                  .
           05  w-aux-top-iic-buf
                               occurs 30.
               10  w-aux-top-iic-cbu      pic  9(04)                  .
               10  w-aux-top-iic-mbu      pic  x(04)                  .
               10  w-aux-top-iic-dbu.
                   15  w-aux-top-iic-fbu  pic  x(01)                  .
                   15  filler             pic  x(49)                  .
           05  w-aux-top-iic-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-top-iic-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-top-iic-lt2      pic  9(01)                  .
           05  w-aux-top-iic-dup          pic  x(50)                  .
           05  w-aux-top-iic-dmx.
               10  w-aux-top-iic-dch
                               occurs 50  pic  x(01)                  .

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
      *    * Link-area per accettazione codice tipo operazione per la  *
      *    * gestione movimenti Iva intracomunitaria                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cmn-top-iic
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
           if        w-cmn-top-iic-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cmn-top-iic-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cmn-top-iic-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cmn-top-iic-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cmn-top-iic-ope    =    "A+" or
                     w-cmn-top-iic-ope    =    "F+"
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
                     move  spaces         to   w-cmn-top-iic-ope      .
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
      *                  * Valore numerico del codice                  *
      *                  *---------------------------------------------*
           move      w-cmn-top-iic-top    to   w-cmn-top-iic-num      .
      *                  *---------------------------------------------*
      *                  * Edit mask per il codice                     *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cmn-top-iic-edm      .
      *                  *---------------------------------------------*
      *                  * User function keys                          *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cmn-top-iic-ufk      .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing per conversione in alfanumerico     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-top-iic-edm    to   v-edm                  .
           move      w-cmn-top-iic-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore convertito               *
      *                  *---------------------------------------------*
           move      v-edt                to   w-cmn-top-iic-alf      .
       acc-300.
      *              *-------------------------------------------------*
      *              * Preparazione accettazione alfanumerica          *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
       acc-400.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cmn-top-iic-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro con function key Find           *
      *              *-------------------------------------------------*
           if        w-cmn-top-iic-ope    =    "F+"
                     go to aco-400.
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cmn-top-iic-alf      .
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
      *                  * Rivisualizzazione valore originale numerico *
      *                  * editato alfanumericamente                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione completata       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cmn-top-iic-ope      .
      *                  *---------------------------------------------*
      *                  * Valore numerico originale in uscita         *
      *                  *---------------------------------------------*
           move      w-cmn-top-iic-num    to   w-cmn-top-iic-top      .
      *                  *---------------------------------------------*
      *                  * Linea e posizione per descrizione a zero    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cmn-top-iic-dln      .
           move      zero                 to   w-cmn-top-iic-dps      .
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
           if        w-cmn-top-iic-alf    not  = spaces
                     go to aco-075.
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione completata       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cmn-top-iic-ope      .
      *                  *---------------------------------------------*
      *                  * Valore numerico in uscita : a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cmn-top-iic-top      .
      *                  *---------------------------------------------*
      *                  * Linea e posizione per descrizione a zero    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cmn-top-iic-dln      .
           move      zero                 to   w-cmn-top-iic-dps      .
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
           move      1                    to   w-aux-top-iic-c01      .
       aco-079.
           add       1                    to   w-aux-top-iic-c01      .
           if        w-aux-top-iic-c01    >    04
                     go to aco-100.
           if        w-cmn-top-iic-cha
                    (w-aux-top-iic-c01)   not  = spaces
                     go to aco-079.
       aco-081.
           add       1                    to   w-aux-top-iic-c01      .
           if        w-aux-top-iic-c01    >    04
                     go to aco-100.
           if        w-cmn-top-iic-cha
                    (w-aux-top-iic-c01)   =    spaces
                     go to aco-081.
       aco-083.
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione da rieseguire    *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cmn-top-iic-ope      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione alfanumerica      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se il valore alfanumerico accettato e' comple-  *
      *              * tamente numerico                                *
      *              *-------------------------------------------------*
       aco-102.
      *                  *---------------------------------------------*
      *                  * Test con conversione da alfanumerico a nu-  *
      *                  * merico                                      *
      *                  *---------------------------------------------*
       aco-104.
           move      zero                 to   w-cmn-top-iic-num      .
           move      zero                 to   w-aux-top-iic-c01      .
       aco-106.
           add       1                    to   w-aux-top-iic-c01      .
           if        w-aux-top-iic-c01    >    04
                     go to aco-108.
           if        w-cmn-top-iic-cha
                    (w-aux-top-iic-c01)   =    spaces
                     go to aco-108.
           if        w-cmn-top-iic-cha
                    (w-aux-top-iic-c01)   <    "0" or
                     w-cmn-top-iic-cha
                    (w-aux-top-iic-c01)   >    "9"
                     go to aco-125.
           multiply  10                   by   w-cmn-top-iic-num      .
           move      w-cmn-top-iic-cha
                    (w-aux-top-iic-c01)   to   w-cmn-top-iic-chn (04) .
           go to     aco-106.
       aco-108.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-top-iic-edm    to   v-edm                  .
           move      w-cmn-top-iic-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione valore numerico editato   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Tipo uscita : accettazione completata       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cmn-top-iic-ope      .
      *                  *---------------------------------------------*
      *                  * Valore numerico in uscita : pari al valore  *
      *                  * accettato convertito in numerico            *
      *                  *---------------------------------------------*
           move      w-cmn-top-iic-num    to   w-cmn-top-iic-top      .
      *                  *---------------------------------------------*
      *                  * Linea e posizione per descrizione a zero    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cmn-top-iic-dln      .
           move      zero                 to   w-cmn-top-iic-dps      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-125.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se il valore non numerico  *
      *              * impostato indica una ricerca per mnemonico op-  *
      *              * pure per descrizione                            *
      *              *-------------------------------------------------*
           if        w-cmn-top-iic-alf    =    "-" and
                     w-cmn-top-iic-dln    not  = zero
                     go to aco-300
           else      go to aco-200.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se ricerca per mnemonico                        *
      *              *-------------------------------------------------*
       aco-205.
      *                  *---------------------------------------------*
      *                  * Tipo ricerca in corso : per mnemonico       *
      *                  *---------------------------------------------*
           move      "M"                  to   w-aux-top-iic-tpf      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records con lo stesso *
      *                  * mnemonico nel buffer                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-top-iic-crb      .
       aco-210.
      *                  *---------------------------------------------*
      *                  * Start su tabella in memoria                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to aco-600.
       aco-215.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale tabella in memoria      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to aco-600.
       aco-220.
      *                  *---------------------------------------------*
      *                  * Mnemonico letto in area di lavoro di 4 ca-  *
      *                  * ratteri                                     *
      *                  *---------------------------------------------*
           move      w-top-iic-tab-mne    to   w-aux-top-iic-m04      .
      *                  *---------------------------------------------*
      *                  * Se il mnemonico letto non corrisponde al    *
      *                  * mnemonico da ricercare : si ignora il re-   *
      *                  * cord e si ricicla per il successivo         *
      *                  *---------------------------------------------*
           if        w-aux-top-iic-m04    not  = w-cmn-top-iic-alf
                     go to aco-215.
       aco-225.
      *                  *---------------------------------------------*
      *                  * Incremento numero records nel buffer        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-top-iic-crb      .
       aco-230.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico                         *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-top    to   w-aux-top-iic-cbu
                                              (w-aux-top-iic-crb)     .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-mne    to   w-aux-top-iic-mbu
                                              (w-aux-top-iic-crb)     .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-des    to   w-aux-top-iic-dbu
                                              (w-aux-top-iic-crb)     .
       aco-235.
      *                  *---------------------------------------------*
      *                  * Se raggiunta la massima capacita' del buf-  *
      *                  * fer : come per fine file                    *
      *                  *---------------------------------------------*
           if        w-aux-top-iic-crb    =    30
                     go to aco-600.
      *                  *---------------------------------------------*
      *                  * Altrimenti : riciclo a lettura record suc-  *
      *                  * cessivo                                     *
      *                  *---------------------------------------------*
           go to     aco-215.
       aco-300.
      *              *-------------------------------------------------*
      *              * Se ricerca per descrizione                      *
      *              *-------------------------------------------------*
       aco-305.
      *                  *---------------------------------------------*
      *                  * Comodo di accettazione per la descrizione   *
      *                  * a spaces                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-top-iic-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione a spaces        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-cmn-top-iic-dln    to   v-lin                  .
           move      w-cmn-top-iic-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-310.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cmn-top-iic-dln    to   v-lin                  .
           move      w-cmn-top-iic-dps    to   v-pos                  .
           move      w-aux-top-iic-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-top-iic-dup      .
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
           move      50                   to   v-car                  .
           move      w-cmn-top-iic-dln    to   v-lin                  .
           move      w-cmn-top-iic-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione da riesegui- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cmn-top-iic-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione alfanumerica  *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
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
           move      "AC"                 to   w-cmn-top-iic-ope      .
      *                      *-----------------------------------------*
      *                      * Valore numerico originale in uscita     *
      *                      *-----------------------------------------*
           move      w-cmn-top-iic-num    to   w-cmn-top-iic-top      .
      *                      *-----------------------------------------*
      *                      * Linea e posizione per descrizione a ze- *
      *                      * ro                                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-cmn-top-iic-dln      .
           move      zero                 to   w-cmn-top-iic-dps      .
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
           if        w-aux-top-iic-dup    not  = spaces
                     go to aco-330.
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione da riesegui- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cmn-top-iic-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione alfanumerica  *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-330.
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione massima            *
      *                  *---------------------------------------------*
       aco-331.
           move      w-aux-top-iic-dup    to   w-aux-top-iic-dmx      .
           move      50                   to   w-aux-top-iic-c01      .
       aco-332.
           if        w-aux-top-iic-c01    >    zero
                     if    w-aux-top-iic-dch
                          (w-aux-top-iic-c01)
                                          =    spaces
                           move     "z"   to   w-aux-top-iic-dch
                                              (w-aux-top-iic-c01)
                           subtract 1     from w-aux-top-iic-c01
                           go to    aco-332.
       aco-335.
      *                  *---------------------------------------------*
      *                  * Tipo ricerca in corso : per descrizione     *
      *                  *---------------------------------------------*
           move      "D"                  to   w-aux-top-iic-tpf      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records con la stessa *
      *                  * descrizione nel buffer                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-top-iic-crb      .
       aco-340.
      *                  *---------------------------------------------*
      *                  * Start su tabella in memoria                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to aco-600.
       aco-345.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale tabella in memoria      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to aco-600.
       aco-350.
      *                  *---------------------------------------------*
      *                  * Descrizione letta in area di lavoro di 50   *
      *                  * caratteri                                   *
      *                  *---------------------------------------------*
           move      w-top-iic-tab-des    to   w-aux-top-iic-d50      .
       aco-355.
      *                  *---------------------------------------------*
      *                  * Trasformazione in uppercase della descri-   *
      *                  * zione nel comodo di lavoro                  *
      *                  *---------------------------------------------*
           move      w-aux-top-iic-d50    to   w-all-str-alf          .
           move      50                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-aux-top-iic-d50      .
       aco-360.
      *                  *---------------------------------------------*
      *                  * Se la descrizione letta non e' compresa tra *
      *                  * il limite minimo e massimo : si ignora il   *
      *                  * record e si ricicla per il successivo       *
      *                  *---------------------------------------------*
           if        w-aux-top-iic-d50    <    w-aux-top-iic-dup or
                     w-aux-top-iic-d50    >    w-aux-top-iic-dmx
                     go to aco-345.
       aco-365.
      *                  *---------------------------------------------*
      *                  * Incremento numero records nel buffer        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-top-iic-crb      .
       aco-370.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico                         *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-top    to   w-aux-top-iic-cbu
                                              (w-aux-top-iic-crb)     .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-mne    to   w-aux-top-iic-mbu
                                              (w-aux-top-iic-crb)     .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-des    to   w-aux-top-iic-dbu
                                              (w-aux-top-iic-crb)     .
       aco-375.
      *                  *---------------------------------------------*
      *                  * Se raggiunta la massima capacita' del buf-  *
      *                  * fer : come per fine file                    *
      *                  *---------------------------------------------*
           if        w-aux-top-iic-crb    =    30
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
           move      "F"                  to   w-aux-top-iic-tpf      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-top-iic-crb      .
       aco-410.
      *                  *---------------------------------------------*
      *                  * Start su tabella in memoria                 *
      *                  *---------------------------------------------*
           move      "S"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to aco-600.
       aco-415.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale tabella in memoria      *
      *                  *---------------------------------------------*
           move      "N"                  to   w-top-iic-tab-tle      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        w-top-iic-tab-flg    not  = spaces
                     go to aco-600.
       aco-420.
      *                  *---------------------------------------------*
      *                  * Incremento numero records nel buffer        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-top-iic-crb      .
       aco-425.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico                         *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-top    to   w-aux-top-iic-cbu
                                              (w-aux-top-iic-crb)     .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-mne    to   w-aux-top-iic-mbu
                                              (w-aux-top-iic-crb)     .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-top-iic-tab-des    to   w-aux-top-iic-dbu
                                              (w-aux-top-iic-crb)     .
       aco-430.
      *                  *---------------------------------------------*
      *                  * Se raggiunta la massima capacita' del buf-  *
      *                  * fer : come per fine file                    *
      *                  *---------------------------------------------*
           if        w-aux-top-iic-crb    =    30
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
           if        w-aux-top-iic-crb    =    zero
                     go to aco-650
           else if   w-aux-top-iic-crb    =    1
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
           if        w-cmn-top-iic-dln    =    zero
                     go to aco-660.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-cmn-top-iic-dln    to   v-lin                  .
           move      w-cmn-top-iic-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-660.
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione da riesegui- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cmn-top-iic-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione alfanumerica  *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
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
      *                      * Codice numerico in valore numerico      *
      *                      *-----------------------------------------*
           move      w-aux-top-iic-cbu (1)
                                          to   w-cmn-top-iic-num      .
       aco-685.
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-top-iic-edm    to   v-edm                  .
           move      w-cmn-top-iic-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cmn-top-iic-alf      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione valore numerico edi-  *
      *                      * tato                                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo uscita : accettazione completata   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cmn-top-iic-ope      .
      *                      *-----------------------------------------*
      *                      * Valore numerico in uscita : pari al va- *
      *                      * lore accettato convertito in numerico   *
      *                      *-----------------------------------------*
           move      w-cmn-top-iic-num    to   w-cmn-top-iic-top      .
      *                      *-----------------------------------------*
      *                      * Linea e posizione per descrizione a ze- *
      *                      * ro                                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-cmn-top-iic-dln      .
           move      zero                 to   w-cmn-top-iic-dps      .
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
           move      w-aux-top-iic-crb    to   w-aux-top-iic-cpb      .
           subtract  1                    from w-aux-top-iic-cpb      .
           divide    6                    into w-aux-top-iic-cpb      .
           add       1                    to   w-aux-top-iic-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-top-iic-c01      .
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
           move      "          Selezionare il tipo operazione desiderat
      -              "o           "       to   v-alf                  .
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
           move      w-aux-top-iic-c01    to   w-aux-top-iic-nli      .
       aco-720.
           if        w-aux-top-iic-nli    >    6
                     subtract  6          from w-aux-top-iic-nli
                     go to aco-720.
           add       11                   to   w-aux-top-iic-nli      .
       aco-725.
      *                      *-----------------------------------------*
      *                      * Accettazione function key               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-top-iic-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-top-iic-c01    <    w-aux-top-iic-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-top-iic-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-top-iic-cpa    <    w-aux-top-iic-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-top-iic-nli    to   v-lin                  .
           move      22                   to   v-pos                  .
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
      *                          * Codice selezionato in valore nume-  *
      *                          * rico                                *
      *                          *-------------------------------------*
           move      w-aux-top-iic-cbu
                    (w-aux-top-iic-c01)   to   w-cmn-top-iic-num      .
      *                          *-------------------------------------*
      *                          * Editing valore numerico             *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-top-iic-edm    to   v-edm                  .
           move      w-cmn-top-iic-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cmn-top-iic-alf      .
      *                          *-------------------------------------*
      *                          * Rivisualizzazione valore numerico   *
      *                          * editato                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo uscita : accettazione comple-  *
      *                          * tata                                *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cmn-top-iic-ope      .
      *                          *-------------------------------------*
      *                          * Valore numerico in uscita : pari al *
      *                          * lore numerico selezionato           *
      *                          *-------------------------------------*
           move      w-cmn-top-iic-num    to   w-cmn-top-iic-top      .
      *                          *-------------------------------------*
      *                          * Linea e posizione per descrizione a *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-cmn-top-iic-dln      .
           move      zero                 to   w-cmn-top-iic-dps      .
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
           subtract  1                    from w-aux-top-iic-c01      .
      *                          *-------------------------------------*
      *                          * Se non si era alla prima riga della *
      *                          * pagina : a reimpostazione           *
      *                          *-------------------------------------*
           if        w-aux-top-iic-nli    not  = 12
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
           add       1                    to   w-aux-top-iic-c01      .
      *                          *-------------------------------------*
      *                          * Se non si era all'ultima riga della *
      *                          * pagina : a reimpostazione           *
      *                          *-------------------------------------*
           if        w-aux-top-iic-nli    not  = 17
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
           add       1                    to   w-aux-top-iic-cpa      .
           move      w-aux-top-iic-cpa    to   w-aux-top-iic-c01      .
           multiply  6                    by   w-aux-top-iic-c01      .
           subtract  5                    from w-aux-top-iic-c01      .
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
           subtract  1                    from w-aux-top-iic-cpa      .
           move      w-aux-top-iic-cpa    to   w-aux-top-iic-c01      .
           multiply  6                    by   w-aux-top-iic-c01      .
           subtract  5                    from w-aux-top-iic-c01      .
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
           if        w-aux-top-iic-tpf    =    "M"
                     go to aco-770
           else if   w-aux-top-iic-tpf    =    "D"
                     go to aco-775
           else      go to aco-780.
       aco-770.
      *                          *-------------------------------------*
      *                          * Se era in corso una ricerca per il  *
      *                          * mnemonico                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo uscita : accettazione da   *
      *                              * rieseguire                      *
      *                              *---------------------------------*
           move      "A+"                 to   w-cmn-top-iic-ope      .
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
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
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
           move      "A+"                 to   w-cmn-top-iic-ope      .
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
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-top-iic-ufk    to   v-ufk                  .
           move      w-cmn-top-iic-lin    to   v-lin                  .
           move      w-cmn-top-iic-pos    to   v-pos                  .
           move      w-cmn-top-iic-alf    to   v-alf                  .
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
           move      w-aux-top-iic-c01    to   w-aux-top-iic-c02      .
           add       5                    to   w-aux-top-iic-c02      .
           divide    6                    into w-aux-top-iic-c02      .
           move      w-aux-top-iic-c02    to   w-aux-top-iic-cpa      .
           subtract  1                    from w-aux-top-iic-c02      .
           multiply  6                    by   w-aux-top-iic-c02      .
           add       1                    to   w-aux-top-iic-c02      .
           add       5
                     w-aux-top-iic-c02  giving w-aux-top-iic-c03      .
           move      w-aux-top-iic-c03    to   w-aux-top-iic-c04      .
           if        w-aux-top-iic-c03    >    w-aux-top-iic-crb
                     move  w-aux-top-iic-crb
                                          to   w-aux-top-iic-c03      .
           move      12                   to   w-aux-top-iic-c05      .
       aco-953.
      *                  *---------------------------------------------*
      *                  * Visualizzazioni relative alla riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-aux-top-iic-c05    to   v-lin                  .
           move      10                   to   v-pos                  .
           move      w-aux-top-iic-mbu
                    (w-aux-top-iic-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-top-iic-c05    to   v-lin                  .
           move      16                   to   v-pos                  .
           move      w-aux-top-iic-cbu
                    (w-aux-top-iic-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-aux-top-iic-c05    to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-aux-top-iic-dbu
                    (w-aux-top-iic-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-top-iic-c02      .
           add       1                    to   w-aux-top-iic-c05      .
           if        w-aux-top-iic-c02    not  > w-aux-top-iic-c03
                     go to aco-953.
       aco-955.
      *                  *---------------------------------------------*
      *                  * Visualizzazione righe residue a spaces      *
      *                  *---------------------------------------------*
           if        w-aux-top-iic-c02    >    w-aux-top-iic-c04
                     go to aco-957.
           if        w-aux-top-iic-crb    not  > 6
                     go to aco-957.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      62                   to   v-car                  .
           move      w-aux-top-iic-c05    to   v-lin                  .
           move      10                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-top-iic-c02      .
           add       1                    to   w-aux-top-iic-c05      .
           go to     aco-955.
       aco-957.
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina               *
      *                  *---------------------------------------------*
           move      w-aux-top-iic-cpa    to   w-aux-top-iic-lt1      .
           move      w-aux-top-iic-cpb    to   w-aux-top-iic-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-top-iic-ltp    to   v-alf                  .
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

      *    *===========================================================*
      *    * Lettura tabella tipi operazione per gestione Iva intra-   *
      *    * comunitaria                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.rtn"                   .

