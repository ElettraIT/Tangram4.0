       Identification Division.
       Program-Id.                                 acoecmb0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/11/94    *
      *                       Ultima revisione:    NdK del 02/08/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione coefficiente di cambio  *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-coe-cmb-vlt-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "AC"                 *
      *                                                                *
      *                       w-coe-cmb-vlt-sdv : sigla valuta         *
      *                                                                *
      *                       w-coe-cmb-vlt-tdc : tipo coefficiente    *
      *                                            - *                 *
      *                                            - /                 *
      *                                           serve per limitare   *
      *                                           la selezione ai so-  *
      *                                           li valori memorizza- *
      *                                           ti con questo tipo   *
      *                                           di coefficiente      *
      *                                                                *
      *                       w-coe-cmb-vlt-drc : data riferimento per *
      *                                           il cambio            *
      *                                                                *
      *                       w-coe-cmb-vlt-quc : quale coefficiente   *
      *                                           di cambio            *
      *                                           - 00 : Cambio uffi-  *
      *                                                  ciale         *
      *                                           - 01 : Cambio alter- *
      *                                                  nativo 1      *
      *                                           - .. : Cambio alter- *
      *                                                  nativo ..     *
      *                                           - 09 : Cambio alter- *
      *                                                  nativo 9      *
      *                                                                *
      *                       w-coe-cmb-vlt-snd : calcolo preventivo   *
      *                                           del valore di de-    *
      *                                           falut riferito alla  *
      *                                           data riferimento     *
      *                                           - Spaces : No        *
      *                                           - N      : No        *
      *                                           - S      : Si        *
      *                                                                *
      *                       w-coe-cmb-vlt-cdc : coefficiente di cam- *
      *                                           bio; se non viene    *
      *                                           trovato un richiesto *
      *                                           valore di default    *
      *                                           rimane questo        *
      *                                                                *
      *                       w-coe-cmb-vlt-lin : linea coefficiente   *
      *                                           di cambio            *
      *                                                                *
      *                       w-coe-cmb-vlt-pos : posizione coeffi-    *
      *                                           ciente di cambio     *
      *                                                                *
      *                       w-coe-cmb-vlt-dln : linea coefficiente   *
      *                                           di default, solo se  *
      *                                           richiesto un de-     *
      *                                           fault                *
      *                                                                *
      *                       w-coe-cmb-vlt-dps : posizione coeffi-    *
      *                                           ciente di cambio,    *
      *                                           solo se richiesto    *
      *                                           un default           *
      *                                                                *
      *                       v-edm             : maschera editing     *
      *                                                                *
      *                                                                *
      *              Output : w-coe-cmb-vlt-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-coe-cmb-vlt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-coe-cmb-vlt-cdc : coefficiente di cam- *
      *                                           bio                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-coe-cmb-vlt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-coe-cmb-vlt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CC"  Calcolo del coefficiente di cambio per una certa valuta  *
      *       ad una certa data                                        *
      *                                                                *
      *              Input  : w-coe-cmb-vlt-ope : "CC"                 *
      *                                                                *
      *                       w-coe-cmb-vlt-sdv : sigla valuta         *
      *                                                                *
      *                       w-coe-cmb-vlt-tdc : tipo coefficiente    *
      *                                            - *                 *
      *                                            - /                 *
      *                                           serve per limitare   *
      *                                           la selezione ai so-  *
      *                                           li valori memorizza- *
      *                                           ti con questo tipo   *
      *                                           di coefficiente      *
      *                                                                *
      *                       w-coe-cmb-vlt-drc : data riferimento per *
      *                                           il cambio            *
      *                                                                *
      *                       w-coe-cmb-vlt-quc : quale coefficiente   *
      *                                           di cambio            *
      *                                           - 00 : Cambio uffi-  *
      *                                                  ciale         *
      *                                           - 01 : Cambio alter- *
      *                                                  nativo 1      *
      *                                             ..                 *
      *                                           - 09 : Cambio alter- *
      *                                                  nativo 9      *
      *                                                                *
      *                                                                *
      *              Output : w-coe-cmb-vlt-ope : "CC" = determinazio- *
      *                                                  ne eseguita   *
      *                                           "##" = determinazio- *
      *                                                  ne non ese-   *
      *                                                  guita         *
      *                                                                *
      *                       w-coe-cmb-vlt-drc : data riferimento per *
      *                                           il cambio determina- *
      *                                           ta                   *
      *                                                                *
      *                       w-coe-cmb-vlt-cdc : coefficiente di cam- *
      *                                           bio determinato, op- *
      *                                           pure zero se non de- *
      *                                           terminato            *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [vlt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfvlt"                          .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *    *===========================================================*
      *    * Work per subroutine di Find                               *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Sigla valuta passata                                  *
      *        *-------------------------------------------------------*
           05  w-fnd-inp-sgv              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Data di riferimento passata                           *
      *        *-------------------------------------------------------*
           05  w-fnd-inp-drv              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Quale coefficiente di cambio cercare                  *
      *        *  - 00 : Cambio ufficiale                              *
      *        *  - 01 : Cambio addizionale 1                          *
      *        *  - ..                                                 *
      *        *  - 09 : Cambio addizionale 9                          *
      *        *-------------------------------------------------------*
           05  w-fnd-inp-qcc              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di uscita                                        *
      *        *  - Spaces : Nessuna azione, riprendere accettazione   *
      *        *  - S      : Selezione effettuata                      *
      *        *  - I      : Richiesta di Insr                         *
      *        *-------------------------------------------------------*
           05  w-fnd-out-tex              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data di riferimento selezionata                       *
      *        *-------------------------------------------------------*
           05  w-fnd-out-drv              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Coefficiente di cambio selezionato                    *
      *        *-------------------------------------------------------*
           05  w-fnd-out-ccv              pic  9(06)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Massimo numero records nel buffer                     *
      *        *-------------------------------------------------------*
           05  w-fnd-max-rec              pic  9(04)       value  90  .
      *        *-------------------------------------------------------*
      *        * Buffer records                                        *
      *        *-------------------------------------------------------*
           05  w-fnd-buf-rec.
      *            *---------------------------------------------------*
      *            * Elementi nel buffer records                       *
      *            *---------------------------------------------------*
               10  w-fnd-ele-buf  occurs 90.
      *                *-----------------------------------------------*
      *                * Data rilevazione cambio                       *
      *                *-----------------------------------------------*
                   15  w-fnd-dat-vlt      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Coefficiente di cambio ufficiale              *
      *                *-----------------------------------------------*
                   15  w-fnd-cdc-vlt      pic  9(06)v9(05)            .
      *                *-----------------------------------------------*
      *                * Coefficiente di cambio addizionale richiesto  *
      *                *-----------------------------------------------*
                   15  w-fnd-cdc-add      pic  9(06)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Box, linea in alto a sinistra                         *
      *        *-------------------------------------------------------*
           05  w-fnd-box-ull              pic  9(03)       value  04  .
      *        *-------------------------------------------------------*
      *        * Box, posizione in alto a sinistra                     *
      *        *-------------------------------------------------------*
           05  w-fnd-box-ulp              pic  9(03)       value  09  .
      *        *-------------------------------------------------------*
      *        * Box, linea in basso a destra                          *
      *        *-------------------------------------------------------*
           05  w-fnd-box-lrl              pic  9(03)       value  21  .
      *        *-------------------------------------------------------*
      *        * Box, posizione in basso a destra                      *
      *        *-------------------------------------------------------*
           05  w-fnd-box-lrp              pic  9(03)       value  72  .
      *        *-------------------------------------------------------*
      *        * Numero records visualizzabili per ogni pagina         *
      *        *-------------------------------------------------------*
           05  w-fnd-nrv-ppg              pic  9(03)       value  10  .
      *        *-------------------------------------------------------*
      *        * Posizione a video di accettazione                     *
      *        *-------------------------------------------------------*
           05  w-fnd-pos-acc              pic  9(03)       value  23  .
      *        *-------------------------------------------------------*
      *        * Contatore records presenti nel buffer                 *
      *        *-------------------------------------------------------*
           05  w-fnd-nto-rec              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Contatore pagine da visualizzare                      *
      *        *-------------------------------------------------------*
           05  w-fnd-nto-pdv              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice del record nel buffer attualmente trattato     *
      *        *-------------------------------------------------------*
           05  w-fnd-irc-att              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice della pagina attualmente trattata              *
      *        *-------------------------------------------------------*
           05  w-fnd-ipg-att              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Numero linea video relativa al record nel buffer at-  *
      *        * tualmente trattato                                    *
      *        *-------------------------------------------------------*
           05  w-fnd-lin-att              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per calcoli locali                             *
      *        *-------------------------------------------------------*
           05  w-fnd-wrk-are.
      *            *---------------------------------------------------*
      *            * Determinazione prima linea per records            *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-plr          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Determinazione ultima linea per records           *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-ulr          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Determinazione indice primo record da visualizza- *
      *            * re                                                *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-ipr          pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Determinazione indice ultimo record da visualiz-  *
      *            * zare                                              *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-iur          pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per literal di pagina nr .. di ..              *
      *        *-------------------------------------------------------*
           05  w-fnd-wrk-lit.
      *            *---------------------------------------------------*
      *            * Nr pagina attuale editata                         *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-lat          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Nr pagine totali editate                          *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-lto          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Literal completo allineato a sinistra             *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-las          pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Contatori caratteri literal                       *
      *            *---------------------------------------------------*
               10  w-fnd-wrk-ltl          pic  9(02)                  .
               10  w-fnd-wrk-lc1          pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente di cambio         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-coe-cmb-vlt
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
           if        w-coe-cmb-vlt-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-coe-cmb-vlt-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-coe-cmb-vlt-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-coe-cmb-vlt-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-coe-cmb-vlt-ope    =    "A+" or
                     w-coe-cmb-vlt-ope    =    "F+" or
                     w-coe-cmb-vlt-ope    =    "I+"
                     perform   aco-000    thru aco-999
           else if   w-coe-cmb-vlt-ope    =    "CC"
                     perform   cco-000    thru cco-999                .
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
      *              * Open file [vlt]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
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
      *              * Close file [vlt]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test di cancellabilita' per il modulo                     *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore delle 'open' in corso per il    *
      *              * modulo e' pari a zero si dichiara che e' can-   *
      *              * cellabile, altrimento che non lo e'             *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        =    zero
                     move  spaces         to   w-coe-cmb-vlt-ope      .
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
           move      "pdcc0250"           to   s-pro                  .
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
           move      w-coe-cmb-vlt-sdv    to   w-coe-cmb-vlt-s01      .
           move      w-coe-cmb-vlt-drc    to   w-coe-cmb-vlt-s02      .
           move      w-coe-cmb-vlt-quc    to   w-coe-cmb-vlt-s03      .
           if        w-coe-cmb-vlt-s03    >    09
                     move  zero           to   w-coe-cmb-vlt-s03      .
           move      w-coe-cmb-vlt-snd    to   w-coe-cmb-vlt-s04      .
           move      w-coe-cmb-vlt-cdc    to   w-coe-cmb-vlt-s05      .
           move      w-coe-cmb-vlt-tdc    to   w-coe-cmb-vlt-s11      .
           if        w-coe-cmb-vlt-s11    not  = "*" and
                     w-coe-cmb-vlt-s11    not  = "/"
                     move  "/"            to   w-coe-cmb-vlt-s11      .
           move      v-edm                to   w-coe-cmb-vlt-edm      .
           move      v-ufk                to   w-coe-cmb-vlt-ufk      .
       acc-250.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di visualizzazione data   *
      *              * di riferimento di default                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-coe-cmb-vlt-f01      .
       acc-300.
      *              *-------------------------------------------------*
      *              * Eventuale preparazione del valore di default,   *
      *              * se richiesto                                    *
      *              *-------------------------------------------------*
       acc-305.
      *                  *---------------------------------------------*
      *                  * Test se richiesto                           *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-s04    not  = "S"
                     go to acc-400.
       acc-310.
      *                  *---------------------------------------------*
      *                  * Start su [vlt]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SGLDRE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-coe-cmb-vlt-s01    to   rf-vlt-sgl-vlt         .
           move      99999999             to   rf-vlt-dat-rev         .
           if        w-coe-cmb-vlt-s02    not  = zero
                     subtract  19000000   from rf-vlt-dat-rev
                     subtract  w-coe-cmb-vlt-s02
                                          from rf-vlt-dat-rev         .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : default inalterato        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to acc-400.
       acc-315.
      *                  *---------------------------------------------*
      *                  * Read Next su [vlt]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : default inalterato           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to acc-400.
       acc-320.
      *                  *---------------------------------------------*
      *                  * Test Max su [vlt]                           *
      *                  *---------------------------------------------*
           if        rf-vlt-sgl-vlt       not  = w-coe-cmb-vlt-s01
                     go to acc-400.
       acc-325.
      *                  *---------------------------------------------*
      *                  * Se tipo di coefficiente non compatibile con *
      *                  * quello passato : no default                 *
      *                  *---------------------------------------------*
           if        rf-vlt-tdc-vlt       not  = w-coe-cmb-vlt-s11
                     go to acc-400.
       acc-330.
      *                  *---------------------------------------------*
      *                  * Preparazione default a seconda di quale sia *
      *                  * il coefficiente richiesto                   *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-s03    =    zero
                     move  rf-vlt-cdc-vlt to   w-coe-cmb-vlt-s05
           else      move  rf-vlt-cdc-add
                          (w-coe-cmb-vlt-s03)
                                          to   w-coe-cmb-vlt-s05      .
       acc-335.
      *                  *---------------------------------------------*
      *                  * Visualizzazione della data relativa al va-  *
      *                  * lore di default richiesto, solo se e' stata *
      *                  * passata una linea ed una posizione per la   *
      *                  * visualizzazione                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore di default a zero : no visua- *
      *                      * lizzazione                              *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-s05    =    zero
                     go to acc-400.
      *                      *-----------------------------------------*
      *                      * Se data di riferimento a zero : no vi-  *
      *                      * sualizzazione                           *
      *                      *-----------------------------------------*
           if        rf-vlt-dat-vlt       =    zero
                     go to acc-400.
      *                      *-----------------------------------------*
      *                      * Se non e' stata passata una posizione   *
      *                      * per il default : no visualizzazione     *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-dln    =    zero or
                     w-coe-cmb-vlt-dps    =    zero
                     go to acc-400.
      *                      *-----------------------------------------*
      *                      * Visualizzazione data di riferimento     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      w-coe-cmb-vlt-dln    to   v-lin                  .
           move      w-coe-cmb-vlt-dps    to   v-pos                  .
           move      rf-vlt-dat-vlt       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Flag di visualizzazione data di riferi- *
      *                      * mento di default in On                  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-coe-cmb-vlt-f01      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Accettazione numerica                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-coe-cmb-vlt-edm    to   v-edm                  .
           move      w-coe-cmb-vlt-ufk    to   v-ufk                  .
           move      w-coe-cmb-vlt-lin    to   v-lin                  .
           move      w-coe-cmb-vlt-pos    to   v-pos                  .
           move      w-coe-cmb-vlt-s05    to   v-num                  .
       acc-500.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-coe-cmb-vlt-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di rientro        *
      *              *-------------------------------------------------*
           if        w-coe-cmb-vlt-ope    =    "A+"
                     go to aco-200
           else if   w-coe-cmb-vlt-ope    =    "I+"
                     go to aco-400
           else if   w-coe-cmb-vlt-ope    =    "F+"
                     go to aco-600
           else      go to aco-200.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro 'A+', da impostazione normale        *
      *              *-------------------------------------------------*
       aco-210.
      *                  *---------------------------------------------*
      *                  * Tipo operazione a: non-continuazione        *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-coe-cmb-vlt-ope      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di salvataggio     *
      *                  *---------------------------------------------*
           move      v-num                to   w-coe-cmb-vlt-s05      .
       aco-220.
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt: non si esegue la rivisua-   *
      *                  * lizzazione del valore impostato             *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-240.
       aco-230.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione del valore impostato      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-coe-cmb-vlt-edm    to   v-edm                  .
           move      w-coe-cmb-vlt-lin    to   v-lin                  .
           move      w-coe-cmb-vlt-pos    to   v-pos                  .
           move      w-coe-cmb-vlt-s05    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-240.
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di uscita          *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-s05    to   w-coe-cmb-vlt-cdc      .
       aco-245.
      *                  *---------------------------------------------*
      *                  * Visualizzazione a spaces della data relati- *
      *                  * va al default, solo se il flag indica che   *
      *                  * si era eseguita la visualizzazione          *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-f01    =    spaces
                     go to aco-250.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      w-coe-cmb-vlt-dln    to   v-lin                  .
           move      w-coe-cmb-vlt-dps    to   v-pos                  .
           move      zero                 to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-250.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-400.
      *              *-------------------------------------------------*
      *              * Se rientro 'I+', da digitazione tasto Insr      *
      *              *-------------------------------------------------*
       aco-410.
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : continuazione           *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-coe-cmb-vlt-ope      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       aco-420.
      *                  *---------------------------------------------*
      *                  * Accettazione numerica                       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-coe-cmb-vlt-edm    to   v-edm                  .
           move      w-coe-cmb-vlt-ufk    to   v-ufk                  .
           move      w-coe-cmb-vlt-lin    to   v-lin                  .
           move      w-coe-cmb-vlt-pos    to   v-pos                  .
           move      w-coe-cmb-vlt-s05    to   v-num                  .
       aco-430.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-600.
      *              *-------------------------------------------------*
      *              * Se rientro 'F+', da digitazione tasto Find      *
      *              *-------------------------------------------------*
       aco-610.
      *                  *---------------------------------------------*
      *                  * Esecuzione subroutine di Find               *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-s01    to   w-fnd-inp-sgv          .
           move      w-coe-cmb-vlt-s02    to   w-fnd-inp-drv          .
           move      w-coe-cmb-vlt-s03    to   w-fnd-inp-qcc          .
           perform   sub-rou-fnd-000      thru sub-rou-fnd-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di uscita     *
      *                  * dalla subroutine                            *
      *                  *---------------------------------------------*
           if        w-fnd-out-tex        =    spaces
                     go to aco-620
           else if   w-fnd-out-tex        =    "S"
                     go to aco-630
           else if   w-fnd-out-tex        =    "I"
                     go to aco-640.
       aco-620.
      *                  *---------------------------------------------*
      *                  * Se uscita senza alcuna azione particolare   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-coe-cmb-vlt-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Accettazione numerica                   *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-coe-cmb-vlt-edm    to   v-edm                  .
           move      w-coe-cmb-vlt-ufk    to   v-ufk                  .
           move      w-coe-cmb-vlt-lin    to   v-lin                  .
           move      w-coe-cmb-vlt-pos    to   v-pos                  .
           move      w-coe-cmb-vlt-s05    to   v-num                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-630.
      *                  *---------------------------------------------*
      *                  * Se uscita per selezione effettuata          *
      *                  *---------------------------------------------*
       aco-632.
      *                      *-----------------------------------------*
      *                      * Memorizzazione valore selezionato       *
      *                      *-----------------------------------------*
           move      w-fnd-out-ccv        to   w-coe-cmb-vlt-s05      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione del valore selezio-   *
      *                      * nato                                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-coe-cmb-vlt-edm    to   v-edm                  .
           move      w-coe-cmb-vlt-lin    to   v-lin                  .
           move      w-coe-cmb-vlt-pos    to   v-pos                  .
           move      w-coe-cmb-vlt-s05    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore determinato in area di uscita    *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-s05    to   w-coe-cmb-vlt-cdc      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-coe-cmb-vlt-ope      .
       aco-634.
      *                      *-----------------------------------------*
      *                      * Visualizzazione a spaces della data re- *
      *                      * lativa al default, solo se il flag in-  *
      *                      * dica che si era eseguita la visualizza- *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-f01    =    spaces
                     go to aco-636.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      w-coe-cmb-vlt-dln    to   v-lin                  .
           move      w-coe-cmb-vlt-dps    to   v-pos                  .
           move      zero                 to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-636.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-640.
      *                  *---------------------------------------------*
      *                  * Se uscita per richiesta di Insr             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Function-key a Insr                     *
      *                      *-----------------------------------------*
           move      "INSR"               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Calcolo del coefficiente di cambio per una certa valuta   *
      *    * ad una certa data                                         *
      *    *-----------------------------------------------------------*
       cco-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
           move      w-coe-cmb-vlt-sdv    to   w-coe-cmb-vlt-s01      .
           move      w-coe-cmb-vlt-drc    to   w-coe-cmb-vlt-s02      .
           move      w-coe-cmb-vlt-quc    to   w-coe-cmb-vlt-s03      .
           if        w-coe-cmb-vlt-s03    >    09
                     move  zero           to   w-coe-cmb-vlt-s03      .
           move      w-coe-cmb-vlt-tdc    to   w-coe-cmb-vlt-s11      .
           if        w-coe-cmb-vlt-s11    not  = "*" and
                     w-coe-cmb-vlt-s11    not  = "/"
                     move  "/"            to   w-coe-cmb-vlt-s11      .
       cco-100.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-s01    not  = c-sgl and
                     w-coe-cmb-vlt-s01    not  = spaces
                     go to cco-200.
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio                      *
      *                  *---------------------------------------------*
           move      c-cdc                to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cco-999.
       cco-200.
      *              *-------------------------------------------------*
      *              * Start su [vlt]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SGLDRE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-coe-cmb-vlt-s01    to   rf-vlt-sgl-vlt         .
           move      99999999             to   rf-vlt-dat-rev         .
           if        w-coe-cmb-vlt-s02    not  = zero
                     subtract  19000000   from rf-vlt-dat-rev
                     subtract  w-coe-cmb-vlt-s02
                                          from rf-vlt-dat-rev         .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
      *              *-------------------------------------------------*
      *              * Se start errata : a coefficiente non determina- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cco-800.
       cco-300.
      *              *-------------------------------------------------*
      *              * Read Next su [vlt]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
      *              *-------------------------------------------------*
      *              * Se fine file : a coefficiente non determinato   *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cco-800.
       cco-400.
      *              *-------------------------------------------------*
      *              * Test Max su [vlt]                               *
      *              *-------------------------------------------------*
           if        rf-vlt-sgl-vlt       not  = w-coe-cmb-vlt-s01
                     go to cco-800.
       cco-500.
      *              *-------------------------------------------------*
      *              * Se tipo di coefficiente non compatibile con     *
      *              * quello passato : fine ricerca                   *
      *              *-------------------------------------------------*
           if        rf-vlt-tdc-vlt       not  = w-coe-cmb-vlt-s11
                     go to cco-800.
       cco-600.
      *              *-------------------------------------------------*
      *              * Coefficiente determinato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data rilevazione cambio                     *
      *                  *---------------------------------------------*
           move      rf-vlt-dat-vlt       to   w-coe-cmb-vlt-drc      .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio                      *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-s03    =    zero
                     move  rf-vlt-cdc-vlt to   w-coe-cmb-vlt-cdc
           else      move  rf-vlt-cdc-add
                          (w-coe-cmb-vlt-s03)
                                          to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cco-999.
       cco-800.
      *              *-------------------------------------------------*
      *              * Coefficiente non determinato                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione ad errore                   *
      *                  *---------------------------------------------*
           move      "##"                 to   w-coe-cmb-vlt-ope      .
      *                  *---------------------------------------------*
      *                  * Data rilevazione cambio                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-coe-cmb-vlt-drc      .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cco-999.
       cco-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di Find                                        *
      *    *                                                           *
      *    * Input  : w-fnd-inp-sgv = Sigla valuta interessata         *
      *    *                                                           *
      *    *          w-fnd-inp-drv = Data di riferimento              *
      *    *                                                           *
      *    *          w-fnd-inp-qcc = Quale coefficiente di cambio     *
      *    *                          cercare                          *
      *    *                            - 00 : Cambio ufficiale        *
      *    *                            - 01 : Cambio addizionale 1    *
      *    *                            - ..                           *
      *    *                            - 09 : Cambio addizionale 9    *
      *    *                                                           *
      *    * Output : w-fnd-out-tex = Tipo di uscita dalla subroutine  *
      *    *                            - Spaces : Nessuna azione, ri- *
      *    *                                       prendere l'accetta- *
      *    *                                       zione               *
      *    *                            - S      : Selezione effettua- *
      *    *                                       ta                  *
      *    *                            - I      : Richiesta di Insr   *
      *    *                                                           *
      *    *          w-fnd-out-drv = Data di riferimento selezionata  *
      *    *                                                           *
      *    *          w-fnd-out-ccv = Coefficiente di cambio selezio-  *
      *    *                          nato                             *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sub-rou-fnd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri in input              *
      *              *-------------------------------------------------*
           if        w-fnd-inp-qcc        >    09
                     move  zero           to   w-fnd-inp-qcc          .
       sub-rou-fnd-050.
      *              *-------------------------------------------------*
      *              * Contatore records nel buffer : a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   w-fnd-nto-rec          .
       sub-rou-fnd-100.
      *              *-------------------------------------------------*
      *              * Start su [vlt]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SGLDRE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-fnd-inp-sgv        to   rf-vlt-sgl-vlt         .
           move      99999999             to   rf-vlt-dat-rev         .
           if        w-fnd-inp-drv        not  = zero
                     subtract  19000000   from rf-vlt-dat-rev
                     subtract  w-fnd-inp-drv
                                          from rf-vlt-dat-rev         .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
      *              *-------------------------------------------------*
      *              * Se start errata : a fine lettura                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sub-rou-fnd-400.
       sub-rou-fnd-150.
      *              *-------------------------------------------------*
      *              * Read Next su [vlt]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofvlt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vlt                 .
      *              *-------------------------------------------------*
      *              * Se fine file : a fine lettura                   *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to sub-rou-fnd-400.
       sub-rou-fnd-200.
      *              *-------------------------------------------------*
      *              * Test Max su [vlt]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Per sigla valuta                            *
      *                  *---------------------------------------------*
           if        rf-vlt-sgl-vlt       not  = w-fnd-inp-sgv
                     go to sub-rou-fnd-400.
      *                  *---------------------------------------------*
      *                  * compatibilita' tra il tipo di coefficiente  *
      *                  * letto e quello passato per controllo        *
      *                  *---------------------------------------------*
           if        rf-vlt-tdc-vlt       not  = w-coe-cmb-vlt-s11
                     go to sub-rou-fnd-400.
       sub-rou-fnd-250.
      *              *-------------------------------------------------*
      *              * Incremento contatore records nel buffer         *
      *              *-------------------------------------------------*
           add       1                    to   w-fnd-nto-rec          .
       sub-rou-fnd-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record nel buffer               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data rilevazione cambio                     *
      *                  *---------------------------------------------*
           move      rf-vlt-dat-vlt       to   w-fnd-dat-vlt
                                              (w-fnd-nto-rec)         .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio ufficiale            *
      *                  *---------------------------------------------*
           move      rf-vlt-cdc-vlt       to   w-fnd-cdc-vlt
                                              (w-fnd-nto-rec)         .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio addizionale richie-  *
      *                  * sto                                         *
      *                  *---------------------------------------------*
           if        w-fnd-inp-qcc        =    zero
                     move  zero           to   w-fnd-cdc-add
                                              (w-fnd-nto-rec)
           else      move  rf-vlt-cdc-add
                          (w-fnd-inp-qcc) to   w-fnd-cdc-add
                                              (w-fnd-nto-rec)         .
       sub-rou-fnd-350.
      *              *-------------------------------------------------*
      *              * Se si e' raggiunto il massimo numero di records *
      *              * memorizzabili si va a fine lettura, altrimenti  *
      *              * si ricicla a leggere il record successivo       *
      *              *-------------------------------------------------*
           if        w-fnd-nto-rec        =    w-fnd-max-rec
                     go to sub-rou-fnd-400
           else      go to sub-rou-fnd-150.
       sub-rou-fnd-400.
      *              *-------------------------------------------------*
      *              * Se zero records trovati                         *
      *              *-------------------------------------------------*
       sub-rou-fnd-405.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-fnd-nto-rec        not  = zero
                     go to sub-rou-fnd-450.
       sub-rou-fnd-410.
      *                  *---------------------------------------------*
      *                  * Normalizzazione parametri in uscita         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di uscita : nessuna azione, ri-    *
      *                      * prendere l'accettazione                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-out-tex          .
      *                      *-----------------------------------------*
      *                      * Data di riferimento selezionata a zero  *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-out-drv          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio selezionato a    *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-fnd-out-ccv          .
       sub-rou-fnd-415.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sub-rou-fnd-999.
       sub-rou-fnd-450.
      *              *-------------------------------------------------*
      *              * Se un solo record trovato                       *
      *              *-------------------------------------------------*
       sub-rou-fnd-455.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-fnd-nto-rec        not  = 1
                     go to sub-rou-fnd-500.
       sub-rou-fnd-460.
      *                  *---------------------------------------------*
      *                  * A visualizzazione lista e scelta            *
      *                  *---------------------------------------------*
           go to     sub-rou-fnd-500.
       sub-rou-fnd-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione lista e scelta                  *
      *              *-------------------------------------------------*
       sub-rou-fnd-510.
      *                  *---------------------------------------------*
      *                  * Determinazione numero pagine da visualizza- *
      *                  * re, in funzione del numero di records vi-   *
      *                  * sualizzabili per pagina                     *
      *                  *---------------------------------------------*
           move      w-fnd-nto-rec        to   w-fnd-nto-pdv          .
           subtract  1                    from w-fnd-nto-pdv          .
           divide    w-fnd-nrv-ppg        into w-fnd-nto-pdv          .
           add       1                    to   w-fnd-nto-pdv          .
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice del record nel buf- *
      *                  * fer attualmente trattato                    *
      *                  *---------------------------------------------*
           move      1                    to   w-fnd-irc-att          .
       sub-rou-fnd-520.
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-530.
      *                  *---------------------------------------------*
      *                  * Visualizzazione box vuoto                   *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      w-fnd-box-ull        to   v-lin                  .
           move      w-fnd-box-ulp        to   v-pos                  .
           move      w-fnd-box-lrl        to   v-lto                  .
           move      w-fnd-box-lrp        to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Note operative di titolo                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-fnd-box-lrp        to   v-car                  .
           subtract  w-fnd-box-ulp        from v-car                  .
           subtract  04                   from v-car                  .
           add       01                   to   v-car                  .
           move      w-fnd-box-ull        to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-fnd-box-ulp        to   v-pos                  .
           add       02                   to   v-pos                  .
           move      "      Selezionare il coefficiente di cambio deside
      -              "rato      "         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titolo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-fnd-box-lrp        to   v-car                  .
           subtract  w-fnd-box-ulp        from v-car                  .
           subtract  04                   from v-car                  .
           add       01                   to   v-car                  .
           move      w-fnd-box-ull        to   v-lin                  .
           add       02                   to   v-lin                  .
           move      w-fnd-box-ulp        to   v-pos                  .
           add       02                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura finale                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-fnd-box-lrp        to   v-car                  .
           subtract  w-fnd-box-ulp        from v-car                  .
           subtract  04                   from v-car                  .
           add       01                   to   v-car                  .
           move      w-fnd-box-lrl        to   v-lin                  .
           subtract  02                   from v-lin                  .
           move      w-fnd-box-ulp        to   v-pos                  .
           add       02                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-540.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina contenente il record *
      *                  * attualmente trattato                        *
      *                  *---------------------------------------------*
           perform   sub-rou-fnd-900      thru sub-rou-fnd-959        .
       sub-rou-fnd-550.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-600.
      *                  *---------------------------------------------*
      *                  * Determinazione numero linea a video relati- *
      *                  * va al record attualmente trattato           *
      *                  *---------------------------------------------*
       sub-rou-fnd-601.
           move      w-fnd-irc-att        to   w-fnd-lin-att          .
       sub-rou-fnd-602.
           if        w-fnd-lin-att        >    w-fnd-nrv-ppg
                     subtract  w-fnd-nrv-ppg
                                          from w-fnd-lin-att
                     go to sub-rou-fnd-602.
           add       w-fnd-box-ull        to   w-fnd-lin-att          .
           add       02                   to   w-fnd-lin-att          .
       sub-rou-fnd-605.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-fnd-irc-att        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-fnd-irc-att        <    w-fnd-nto-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-fnd-ipg-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-fnd-ipg-att        <    w-fnd-nto-pdv
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-fnd-lin-att        to   v-lin                  .
           move      w-fnd-pos-acc        to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-610.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tasto di termina-  *
      *                  * zione usato                                 *
      *                  *---------------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to sub-rou-fnd-650
           else if   v-key                =    "UP  "
                     go to sub-rou-fnd-800
           else if   v-key                =    "DOWN"
                     go to sub-rou-fnd-825
           else if   v-key                =    "EXIT"
                     go to sub-rou-fnd-700
           else if   v-key                =    "NXSC"
                     go to sub-rou-fnd-850
           else if   v-key                =    "PRSC"
                     go to sub-rou-fnd-875
           else      go to sub-rou-fnd-605.
       sub-rou-fnd-650.
      *                  *---------------------------------------------*
      *                  * Se Return o Slct o Do                       *
      *                  *---------------------------------------------*
       sub-rou-fnd-655.
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di uscita a selezione effet-   *
      *                          * tuata                               *
      *                          *-------------------------------------*
           move      "S"                  to   w-fnd-out-tex          .
      *                          *-------------------------------------*
      *                          * Data di riferimento selezionata     *
      *                          *-------------------------------------*
           move      w-fnd-dat-vlt
                    (w-fnd-irc-att)       to   w-fnd-out-drv          .
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio selezionato  *
      *                          *-------------------------------------*
           if        w-fnd-inp-qcc        =    zero
                     move  w-fnd-cdc-vlt
                          (w-fnd-irc-att) to   w-fnd-out-ccv
           else      move  w-fnd-cdc-add
                          (w-fnd-irc-att) to   w-fnd-out-ccv          .
       sub-rou-fnd-660.
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     sub-rou-fnd-999.
       sub-rou-fnd-700.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
       sub-rou-fnd-705.
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in uscita     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di uscita : nessuna azione,    *
      *                          * riprendere l'accettazione           *
      *                          *-------------------------------------*
           move      spaces               to   w-fnd-out-tex          .
      *                          *-------------------------------------*
      *                          * Data di riferimento selezionata a   *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-fnd-out-drv          .
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio selezionato  *
      *                          * a zero                              *
      *                          *-------------------------------------*
           move      zero                 to   w-fnd-out-ccv          .
       sub-rou-fnd-710.
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     sub-rou-fnd-999.
       sub-rou-fnd-800.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
       sub-rou-fnd-805.
      *                      *-----------------------------------------*
      *                      * Decremento indice del record nel buf-   *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           subtract  1                    from w-fnd-irc-att          .
       sub-rou-fnd-810.
      *                      *-----------------------------------------*
      *                      * Se si era alla prima linea di accetta-  *
      *                      * zione si visualizza la pagina relativa  *
      *                      * al record attualmente trattato          *
      *                      *-----------------------------------------*
           move      w-fnd-box-ull        to   w-fnd-wrk-plr          .
           add       03                   to   w-fnd-wrk-plr          .
           if        w-fnd-lin-att        >    w-fnd-wrk-plr
                     go to sub-rou-fnd-815.
           perform   sub-rou-fnd-900      thru sub-rou-fnd-959        .
       sub-rou-fnd-815.
      *                      *-----------------------------------------*
      *                      * Si ricicla all'accettazione             *
      *                      *-----------------------------------------*
           go to     sub-rou-fnd-600.
       sub-rou-fnd-825.
      *                  *---------------------------------------------*
      *                  * Se Down                                     *
      *                  *---------------------------------------------*
       sub-rou-fnd-830.
      *                      *-----------------------------------------*
      *                      * Incremento indice del record nel buf-   *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           add       1                    to   w-fnd-irc-att          .
       sub-rou-fnd-835.
      *                      *-----------------------------------------*
      *                      * Se si era all'ultima linea di accetta-  *
      *                      * zione si visualizza la pagina relativa  *
      *                      * al record attualmente trattato          *
      *                      *-----------------------------------------*
           move      w-fnd-box-lrl        to   w-fnd-wrk-ulr          .
           subtract  03                   from w-fnd-wrk-ulr          .
           if        w-fnd-lin-att        <    w-fnd-wrk-ulr
                     go to sub-rou-fnd-840.
           perform   sub-rou-fnd-900      thru sub-rou-fnd-959        .
       sub-rou-fnd-840.
      *                      *-----------------------------------------*
      *                      * Si ricicla all'accettazione             *
      *                      *-----------------------------------------*
           go to     sub-rou-fnd-600.
       sub-rou-fnd-850.
      *                  *---------------------------------------------*
      *                  * Se Nxsc                                     *
      *                  *---------------------------------------------*
       sub-rou-fnd-855.
      *                      *-----------------------------------------*
      *                      * Determinazione indice del record nel    *
      *                      * buffer relativo al primo record della   *
      *                      * pagina successiva                       *
      *                      *-----------------------------------------*
           add       1                    to   w-fnd-ipg-att          .
           move      w-fnd-ipg-att        to   w-fnd-irc-att          .
           multiply  w-fnd-nrv-ppg        by   w-fnd-irc-att          .
           subtract  w-fnd-nrv-ppg        from w-fnd-irc-att          .
           add       01                   to   w-fnd-irc-att          .
       sub-rou-fnd-860.
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina relativa al re-  *
      *                      * cord nel buffer cosi' determinato       *
      *                      *-----------------------------------------*
           perform   sub-rou-fnd-900      thru sub-rou-fnd-959        .
       sub-rou-fnd-865.
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione                 *
      *                      *-----------------------------------------*
           go to     sub-rou-fnd-600.
       sub-rou-fnd-875.
      *                  *---------------------------------------------*
      *                  * Se Prsc                                     *
      *                  *---------------------------------------------*
       sub-rou-fnd-880.
      *                      *-----------------------------------------*
      *                      * Determinazione indice del record nel    *
      *                      * buffer relativo al primo record della   *
      *                      * pagina precedente                       *
      *                      *-----------------------------------------*
           subtract  1                    from w-fnd-ipg-att          .
           move      w-fnd-ipg-att        to   w-fnd-irc-att          .
           multiply  w-fnd-nrv-ppg        by   w-fnd-irc-att          .
           subtract  w-fnd-nrv-ppg        from w-fnd-irc-att          .
           add       01                   to   w-fnd-irc-att          .
       sub-rou-fnd-885.
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina relativa al re-  *
      *                      * cord nel buffer cosi' determinato       *
      *                      *-----------------------------------------*
           perform   sub-rou-fnd-900      thru sub-rou-fnd-959        .
       sub-rou-fnd-890.
      *                      *-----------------------------------------*
      *                      * Riciclo ad accettazione                 *
      *                      *-----------------------------------------*
           go to     sub-rou-fnd-600.
       sub-rou-fnd-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina contenente il record at- *
      *              * tualmente trattato                              *
      *              *-------------------------------------------------*
       sub-rou-fnd-905.
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-910.
      *                  *---------------------------------------------*
      *                  * Determinazione prima linea per records      *
      *                  *---------------------------------------------*
           move      w-fnd-box-ull        to   w-fnd-wrk-plr          .
           add       03                   to   w-fnd-wrk-plr          .
      *                  *---------------------------------------------*
      *                  * Determinazione ultima linea per records     *
      *                  *---------------------------------------------*
           move      w-fnd-box-lrl        to   w-fnd-wrk-ulr          .
           subtract  03                   from w-fnd-wrk-ulr          .
      *                  *---------------------------------------------*
      *                  * Determinazione numero pagina da visualizza- *
      *                  * re                                          *
      *                  *---------------------------------------------*
           move      w-fnd-irc-att        to   w-fnd-ipg-att          .
           subtract  1                    from w-fnd-ipg-att          .
           divide    w-fnd-nrv-ppg        into w-fnd-ipg-att          .
           add       1                    to   w-fnd-ipg-att          .
      *                  *---------------------------------------------*
      *                  * Determinazione indice ultimo record da vi-  *
      *                  * sualizzare                                  *
      *                  *---------------------------------------------*
           move      w-fnd-ipg-att        to   w-fnd-wrk-iur          .
           multiply  w-fnd-nrv-ppg        by   w-fnd-wrk-iur          .
      *                  *---------------------------------------------*
      *                  * Determinazione indice primo record da vi-   *
      *                  * sualizzare                                  *
      *                  *---------------------------------------------*
           move      w-fnd-wrk-iur        to   w-fnd-wrk-ipr          .
           subtract  w-fnd-nrv-ppg        from w-fnd-wrk-ipr          .
           add       1                    to   w-fnd-wrk-ipr          .
       sub-rou-fnd-915.
      *                  *---------------------------------------------*
      *                  * Se indice primo record, che si incrementa,  *
      *                  * superiore ad indice ultimo record, che e'   *
      *                  * costante : fine visualizzazione             *
      *                  *---------------------------------------------*
           if        w-fnd-wrk-ipr        >   w-fnd-wrk-iur
                     go to sub-rou-fnd-950.
      *                  *---------------------------------------------*
      *                  * Abblencamento preliminare riga              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-fnd-box-lrp        to   v-car                  .
           subtract  w-fnd-box-ulp        from v-car                  .
           subtract  04                   from v-car                  .
           add       01                   to   v-car                  .
           move      w-fnd-wrk-plr        to   v-lin                  .
           move      w-fnd-box-ulp        to   v-pos                  .
           add       02                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se indice primo record, che si incrementa,  *
      *                  * superiore ad ultimo record in assoluto buf- *
      *                  * ferizato : ad incrementi e riciclo          *
      *                  *---------------------------------------------*
           if        w-fnd-wrk-ipr        >   w-fnd-nto-rec
                     go to sub-rou-fnd-940.
       sub-rou-fnd-920.
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data rilevazione cambio                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-fnd-dat-vlt
                    (w-fnd-wrk-ipr)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           if        v-edt                =    spaces
                     move  "Base    "     to   v-edt                  .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-fnd-wrk-plr        to   v-lin                  .
           move      12                   to   v-pos                  .
           move      v-edt                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Literal ':'                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-fnd-wrk-plr        to   v-lin                  .
           move      21                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<GBD"               to   v-edm                  .
           move      w-fnd-wrk-plr        to   v-lin                  .
           move      23                   to   v-pos                  .
           if        w-fnd-inp-qcc        =    zero
                     move  w-fnd-cdc-vlt
                          (w-fnd-wrk-ipr) to   v-num
           else      move  w-fnd-cdc-add
                          (w-fnd-wrk-ipr) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-940.
      *                  *---------------------------------------------*
      *                  * Incremento prima linea per records          *
      *                  *---------------------------------------------*
           add       1                    to   w-fnd-wrk-plr          .
      *                  *---------------------------------------------*
      *                  * Incremento indice primo record da visua-    *
      *                  * lizzare                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-fnd-wrk-ipr          .
       sub-rou-fnd-945.
      *                  *---------------------------------------------*
      *                  * Riciclo su record succdessivo               *
      *                  *---------------------------------------------*
           go to     sub-rou-fnd-915.
       sub-rou-fnd-950.
      *                  *---------------------------------------------*
      *                  * Preparazione literal 'Pagina .. di ..'      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-fnd-ipg-att        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-fnd-wrk-lat          .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-fnd-nto-pdv        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-fnd-wrk-lto          .
      *
           move      spaces               to   w-fnd-wrk-las          .
           string    "Pagina "
                                delimited by   size
                     w-fnd-wrk-lat
                                delimited by   spaces
                     " di "
                                delimited by   size
                     w-fnd-wrk-lto
                                delimited by   spaces
                                          into w-fnd-wrk-las          .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza del literal        *
      *                  *---------------------------------------------*
           move      zero                 to   w-fnd-wrk-lc1          .
           inspect   w-fnd-wrk-las    tallying w-fnd-wrk-lc1
                                  for trailing spaces                 .
           move      20                   to   w-fnd-wrk-ltl          .
           subtract  w-fnd-wrk-lc1        from w-fnd-wrk-ltl          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      w-fnd-wrk-ltl        to   v-car                  .
           move      w-fnd-box-lrl        to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-fnd-box-ulp        to   v-pos                  .
           add       w-fnd-box-lrp        to   v-pos                  .
           subtract  w-fnd-wrk-ltl        from v-pos                  .
           divide    2                    into v-pos                  .
           move      w-fnd-wrk-las        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sub-rou-fnd-959.
           exit.
       sub-rou-fnd-999.
           exit.
