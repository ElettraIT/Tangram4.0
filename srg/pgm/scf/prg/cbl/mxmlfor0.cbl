       Identification Division.
       Program-Id.                                 mxmlfor0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    mov                 *
      *                                   Fase:    xmlfor              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/21    *
      *                       Ultima revisione:    NdK del 03/05/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Lettura dati fattura 'xml' fornitore        *
      *                                                                *
      *                    Utilizzato da: pffo3000 e pscf300j          *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *                                                                *
      *        Input  : m-xml-for-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : m-xml-for-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : m-xml-for-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : m-xml-for-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "RX" - Lettura dati documento 'xml'                            *
      *                                                                *
      *                                                                *
      *        Input  : m-xml-for-tip-ope = "RX"                       *
      *                 m-xml-for-nom-xml = nome file ricevuto         *
      *                                                                *
      *                                                                *
      *        Output : m-xml-for-doc-tes = dati documento             *
      *                 m-xml-for-doc-iva                              *
      *                 m-xml-for-doc-pag                              *
      *                                                                *
      *                 m-xml-for-exi-sts = spaces: lettura OK         *
      *                                     #     : errore             *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "RR" - Lettura riga documento 'xml'                            *
      *                                                                *
      *        ___ DA IMPLEMENTARE ___                                 *
      *                                                                *
      *        Input  : m-xml-for-tip-ope = "RR"                       *
      *                 m-xml-for-nom-xml = nome file ricevuto         *
      *                 m-xml-for-num-lin = numero linea               *
      *                                                                *
      *                                                                *
      *        Output : m-xml-for-doc-rig = riga documento             *
      *                                                                *
      *                 m-xml-for-exi-sts = spaces: lettura OK         *
      *                                     #     : errore             *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mopsys" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [xfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfxfr"                          .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione valori da 'xfr'                    *
      *        *-------------------------------------------------------*
           05  w-det-val-xfr.
      *            *---------------------------------------------------*
      *            * Tipo elemento                                     *
      *            *---------------------------------------------------*
               10  w-det-val-xfr-tip      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatore elemento                                *
      *            *---------------------------------------------------*
               10  w-det-val-xfr-ctr      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Valore elemento                                   *
      *            *---------------------------------------------------*
               10  w-det-val-xfr-imp      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione data numerica                      *
      *        *-------------------------------------------------------*
           05  w-det-dat-num.
      *            *---------------------------------------------------*
      *            * Data in input                                     *
      *            *---------------------------------------------------*
               10  w-det-dat-num-alf      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Data in output                                    *
      *            *---------------------------------------------------*
               10  w-det-dat-num-dat      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-det-dat-num-saa      pic  9(04)                  .
               10  w-det-dat-num-mes      pic  9(02)                  .
               10  w-det-dat-num-gio      pic  9(02)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice Iva                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acl"                   .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

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
      *    * Area di comunicazione per lettura documenti 'xml'         *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/prg/cpy/mxmlfor0.mdl"                   .

      ******************************************************************
       Procedure Division                using m-xml-for              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   m-xml-for-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        m-xml-for-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   m-xml-for-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   m-xml-for-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Lettura documento                           *
      *                  *---------------------------------------------*
           else if   m-xml-for-tip-ope    =    "RX"
                     perform rea-000      thru rea-999
      *                  *---------------------------------------------*
      *                  * Lettura riga documento                      *
      *                  *---------------------------------------------*
           else if   m-xml-for-tip-ope    =    "RR"
                     perform rig-000      thru rig-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione area dati in output             *
      *              *-------------------------------------------------*
           initialize                          m-xml-for-doc-tes      .
           initialize                          m-xml-for-doc-rig      .
           initialize                          m-xml-for-doc-iva      .
           initialize                          m-xml-for-doc-pag      .
       opn-100.
      *              *-------------------------------------------------*
      *              * [xfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xfr                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice Iva             *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-opn-000  thru cod-mne-zci-opn-999    .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
       cls-100.
      *              *-------------------------------------------------*
      *              * [xfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xfr                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice Iva            *
      *              *-------------------------------------------------*
           perform   cod-mne-zci-cls-000  thru cod-mne-zci-cls-999    .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   m-xml-for-exi-sts
           else      move  "#"            to   m-xml-for-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Lettura documento 'xml'                                   *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Test su nome file                               *
      *              *-------------------------------------------------*
           if        m-xml-for-nom-xml    =    spaces
                     move  "#"            to   m-xml-for-exi-sts
                     go to rea-900.
       rea-100.
      *              *-------------------------------------------------*
      *              * Start su [xfr]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NOMXML    "         to   f-key                  .
           move      m-xml-for-nom-xml    to   rf-xfr-nom-xml         .
           move      zero                 to   rf-xfr-num-prg         .
           move      "pgm/scf/fls/ioc/obj/iofxfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xfr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : a riciclo                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rea-900.
       rea-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [xfr]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofxfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-xfr                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a riciclo                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rea-900.
       rea-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-xfr-nom-xml       not  = m-xml-for-nom-xml
                     go to rea-900.
       rea-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       rea-500.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo record          *
      *              *-------------------------------------------------*
           if        rf-xfr-tip-rec       =    "T"
                     perform rea-tes-000
                                          thru rea-tes-999
           else if   rf-xfr-tip-rec       =    "R"
                     perform rea-rig-000
                                          thru rea-rig-999
           else      perform rea-pie-000
                                          thru rea-pie-999            .
       rea-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     rea-200.
       rea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-999.
       rea-999.
           exit.

      *    *===========================================================*
      *    * Lettura documento 'xml'                                   *
      *    *                                                           *
      *    * Subroutine per dati di testata                            *
      *    *-----------------------------------------------------------*
       rea-tes-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del prompt elemento      *
      *              *-------------------------------------------------*
           if        rf-xfr-pmt-ele      =     "Data"
                     go to rea-tes-100
           else if   rf-xfr-pmt-ele      =     "Numero"
                     go to rea-tes-200
           else if   rf-xfr-pmt-ele      =     "ImportoTotaleDocumento"
                     go to rea-tes-300
           else if   rf-xfr-pmt-ele      =     "TipoDocumento"
                     go to rea-tes-400
           else      go to rea-tes-900.
       rea-tes-100.
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data alfanumerica in formato numerico       *
      *                  *---------------------------------------------*
           move      rf-xfr-val-ele       to   w-det-dat-num-alf      .
           perform   det-dat-num-000      thru det-dat-num-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      w-det-dat-num-dat    to   m-xml-for-dat-doc      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-tes-900.
       rea-tes-200.
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      rf-xfr-val-ele       to   m-xml-for-num-doc      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-tes-900.
       rea-tes-300.
      *              *-------------------------------------------------*
      *              * Totale documento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      12                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      v-num                to   m-xml-for-imp-doc      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-tes-900.
       rea-tes-400.
      *              *-------------------------------------------------*
      *              * Tipo documento                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      rf-xfr-val-ele       to   m-xml-for-tip-doc      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-tes-900.
       rea-tes-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-tes-999.
       rea-tes-999.
           exit.

      *    *===========================================================*
      *    * Lettura documento 'xml'                                   *
      *    *                                                           *
      *    * Subroutine per dati righe                                 *
      *    *                                                           *
      *    * N.B.: solo per determinare il numero di righe documento   *
      *    *-----------------------------------------------------------*
       rea-rig-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del prompt elemento      *
      *              *-------------------------------------------------*
           if        rf-xfr-pmt-ele       =    "NumeroLinea"
                     go to rea-rig-100
           else      go to rea-rig-900.
       rea-rig-100.
      *              *-------------------------------------------------*
      *              * Numero linea                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione contatore elemento              *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      rf-xfr-ctr-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-val-xfr-ctr      .
      *                  *---------------------------------------------*
      *                  * Eventuale incremento numero righe           *
      *                  *---------------------------------------------*
           if        v-num                >    m-xml-for-num-rig
                     move  v-num          to   m-xml-for-num-rig      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-rig-900.
       rea-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-rig-999.
       rea-rig-999.
           exit.

      *    *===========================================================*
      *    * Lettura documento 'xml'                                   *
      *    *                                                           *
      *    * Subroutine per dati piede                                 *
      *    *-----------------------------------------------------------*
       rea-pie-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        rf-xfr-tip-ele       =    "riepi"
                     perform rea-rie-000  thru rea-rie-999
           else if   rf-xfr-tip-ele       =    "pagam"
                     perform rea-pag-000  thru rea-pag-999
           else      perform rea-rie-000  thru rea-rie-999            .
       rea-pie-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-pie-999.
       rea-pie-999.
           exit.

      *    *===========================================================*
      *    * Lettura documento 'xml'                                   *
      *    *                                                           *
      *    * Subroutine per valori di riepilogo Iva                    *
      *    *-----------------------------------------------------------*
       rea-rie-000.
      *              *-------------------------------------------------*
      *              * Conversione preliminare contatore               *
      *              *-------------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      rf-xfr-ctr-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-val-xfr-ctr      .
      *              *-------------------------------------------------*
      *              * Test se valore compatibile con castelletto Iva  *
      *              *-------------------------------------------------*
           if        w-det-val-xfr-ctr    <    1 or
                     w-det-val-xfr-ctr    >    6
                     go to rea-rie-900.
       rea-rie-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del prompt elemento      *
      *              *-------------------------------------------------*
           if        rf-xfr-pmt-ele      =     "AliquotaIVA"
                     go to rea-rie-100
           else if   rf-xfr-pmt-ele      =     "Natura"
                     go to rea-rie-200
           else if   rf-xfr-pmt-ele      =     "ImponibileImporto"
                     go to rea-rie-300
           else if   rf-xfr-pmt-ele      =     "Imposta"
                     go to rea-rie-400
           else      go to rea-rie-900.
       rea-rie-100.
      *              *-------------------------------------------------*
      *              * Aliquota IVA                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sostituzione '.' con ','                    *
      *                  *---------------------------------------------*
           inspect   rf-xfr-val-ele
                                replacing all  "."
                                          by   ","                    .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      v-num                to   m-xml-for-iva-cod
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-rie-900.
       rea-rie-200.
      *              *-------------------------------------------------*
      *              * Natura                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura tabella codici Iva     *
      *                  *---------------------------------------------*
           move      "MN"                 to   w-cod-mne-zci-ope      .
           move      rf-xfr-val-ele       to   w-cod-mne-zci-alf      .
           perform   cod-mne-zci-cll-000  thru cod-mne-zci-cll-999    .
      *                  *---------------------------------------------*
      *                  * Test se individuati piu' codici Iva         *
      *                  *---------------------------------------------*
           if        w-cod-mne-zci-num    >    1
                     go to rea-rie-900.
      *                  *---------------------------------------------*
      *                  * Test se non individuati codici Iva          *
      *                  *---------------------------------------------*
           if        w-cod-mne-zci-num    =    zero
                     go to rea-rie-900.
      *                  *---------------------------------------------*
      *                  * Codice Iva individuato, se unico            *
      *                  *---------------------------------------------*
           move      w-cod-mne-zci-cod    to   m-xml-for-iva-cod
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-rie-900.
       rea-rie-300.
      *              *-------------------------------------------------*
      *              * Imponibile Importo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sostituzione '.' con ','                    *
      *                  *---------------------------------------------*
           inspect   rf-xfr-val-ele
                                replacing all  "."
                                          by   ","                    .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      12                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           multiply  100                  by   v-num
                                        giving w-det-val-xfr-imp      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      w-det-val-xfr-imp    to   m-xml-for-iva-ibl
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-rie-900.
       rea-rie-400.
      *              *-------------------------------------------------*
      *              * Imposta                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sostituzione '.' con ','                    *
      *                  *---------------------------------------------*
           inspect   rf-xfr-val-ele
                                replacing all  "."
                                          by   ","                    .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      12                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           multiply  100                  by   v-num
                                        giving w-det-val-xfr-imp      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      w-det-val-xfr-imp    to   m-xml-for-iva-imp
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-rie-900.
       rea-rie-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-rie-999.
       rea-rie-999.
           exit.

      *    *===========================================================*
      *    * Lettura documento 'xml'                                   *
      *    *                                                           *
      *    * Subroutine per valori pagamento                           *
      *    *-----------------------------------------------------------*
       rea-pag-000.
      *              *-------------------------------------------------*
      *              * Conversione preliminare contatore               *
      *              *-------------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      rf-xfr-ctr-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-val-xfr-ctr      .
      *              *-------------------------------------------------*
      *              * Test se maggiore del massimo consentito         *
      *              *-------------------------------------------------*
           if        w-det-val-xfr-ctr    >    9
                     move  9              to   w-det-val-xfr-ctr      .
       rea-pag-050.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del prompt elemento      *
      *              *-------------------------------------------------*
           if        rf-xfr-pmt-ele       =    "CondizioniPagamento"
                     go to rea-pag-100
           else if   rf-xfr-pmt-ele       =    "Beneficiario"
                     go to rea-pag-120
           else if   rf-xfr-pmt-ele       =    "ModalitaPagamento"
                     go to rea-pag-140
           else if   rf-xfr-pmt-ele       =    "DataRiferimentoTerminiPa
      -              "gamento"
                     go to rea-pag-160
           else if   rf-xfr-pmt-ele       =    "GiorniTerminiPagamento"
                     go to rea-pag-180
           else if   rf-xfr-pmt-ele       =    "DataScadenzaPagamento"
                     go to rea-pag-200
           else if   rf-xfr-pmt-ele       =    "ImportoPagamento"
                     go to rea-pag-220
           else if   rf-xfr-pmt-ele       =    "CodUfficioPostale"
                     go to rea-pag-240
           else if   rf-xfr-pmt-ele       =    "CognomeQuietanzante"
                     go to rea-pag-260
           else if   rf-xfr-pmt-ele       =    "NomeQuietanzante"
                     go to rea-pag-280
           else if   rf-xfr-pmt-ele       =    "CFQuietanzante"
                     go to rea-pag-300
           else if   rf-xfr-pmt-ele       =    "TitoloQuietanzante"
                     go to rea-pag-320
           else if   rf-xfr-pmt-ele       =    "IstitutoFinanziario"
                     go to rea-pag-340
           else if   rf-xfr-pmt-ele       =    "IBAN"
                     go to rea-pag-360
           else if   rf-xfr-pmt-ele       =    "ABI"
                     go to rea-pag-380
           else if   rf-xfr-pmt-ele       =    "CAB"
                     go to rea-pag-400
           else if   rf-xfr-pmt-ele       =    "BIC"
                     go to rea-pag-420
           else if   rf-xfr-pmt-ele       =    "ScontoPagamentoAnticipat
      -              "o"
                     go to rea-pag-440
           else if   rf-xfr-pmt-ele       =    "DataLimitePagamentoAntic
      -              "ipato"
                     go to rea-pag-460
           else if   rf-xfr-pmt-ele       =    "PenalitaPagamentiRitarda
      -              "ti"
                     go to rea-pag-480
           else if   rf-xfr-pmt-ele       =    "DataDecorrenzaPenale"
                     go to rea-pag-500
           else if   rf-xfr-pmt-ele       =    "CodicePagamento"
                     go to rea-pag-520.
       rea-pag-100.
      *              *-------------------------------------------------*
      *              * Condizioni Pagamento                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-120.
      *              *-------------------------------------------------*
      *              * Beneficiario                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-140.
      *              *-------------------------------------------------*
      *              * Modalita' Pagamento                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      rf-xfr-val-ele       to   m-xml-for-mod-pag
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-160.
      *              *-------------------------------------------------*
      *              * Data Riferimento Termini Pagamento              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-180.
      *              *-------------------------------------------------*
      *              * Giorni Termini Pagamento                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-200.
      *              *-------------------------------------------------*
      *              * Data Scadenza Pagamento                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data alfanumerica in formato numerico       *
      *                  *---------------------------------------------*
           move      rf-xfr-val-ele       to   w-det-dat-num-alf      .
           perform   det-dat-num-000      thru det-dat-num-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      w-det-dat-num-dat    to   m-xml-for-dat-scd
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-220.
      *              *-------------------------------------------------*
      *              * Importo Pagamento                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sostituzione '.' con ','                    *
      *                  *---------------------------------------------*
           inspect   rf-xfr-val-ele
                                replacing all  "."
                                          by   ","                    .
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      12                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           multiply  100                  by   v-num
                                        giving w-det-val-xfr-imp      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      w-det-val-xfr-imp    to   m-xml-for-imp-pag
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-240.
      *              *-------------------------------------------------*
      *              * Codice Ufficio Postale                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-260.
      *              *-------------------------------------------------*
      *              * Cognome Quietanzante                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-280.
      *              *-------------------------------------------------*
      *              * Nome Quietanzante                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-300.
      *              *-------------------------------------------------*
      *              * CF Quietanzante                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-320.
      *              *-------------------------------------------------*
      *              * Titolo Quietanzante                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-340.
      *              *-------------------------------------------------*
      *              * Istituto Finanziario                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-360.
      *              *-------------------------------------------------*
      *              * IBAN                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      rf-xfr-val-ele       to   m-xml-for-cod-ibn
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-380.
      *              *-------------------------------------------------*
      *              * ABI                                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      v-num                to   m-xml-for-cod-abi
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-400.
      *              *-------------------------------------------------*
      *              * CAB                                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      05                   to   v-car                  .
           move      rf-xfr-val-ele       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      v-num                to   m-xml-for-cod-cab
                                              (w-det-val-xfr-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-420.
      *              *-------------------------------------------------*
      *              * BIC                                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-440.
      *              *-------------------------------------------------*
      *              * Sconto Pagamento Anticipato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-460.
      *              *-------------------------------------------------*
      *              * Data Limite Pagamento Anticipato                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-480.
      *              *-------------------------------------------------*
      *              * Penalita' Pagamenti Ritardati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-500.
      *              *-------------------------------------------------*
      *              * Data Decorrenza Penale                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-520.
      *              *-------------------------------------------------*
      *              * Codice Pagamento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attualmente non gestito                     *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rea-pag-900.
       rea-pag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-pag-999.
       rea-pag-999.
           exit.

      *    *===========================================================*
      *    * Lettura riga documento 'xml'                              *
      *    *                                                           *
      *    * ___ DA IMPLEMENTARE CON CHIAVE SU 'xfr'                   *
      *    *-----------------------------------------------------------*
       rig-000.
      *              *-------------------------------------------------*
      *              * Test su nome file                               *
      *              *-------------------------------------------------*
       rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rig-999.
       rig-999.
           exit.

      *    *===========================================================*
      *    * Determinazione di una data in formato numerico            *
      *    *                                                           *
      *    * Valore in input : data in formato AAAA-MM-GG              *
      *    *                                                           *
      *    * Valore in output: data in formato SAAMMGG                 *
      *    *-----------------------------------------------------------*
       det-dat-num-000.
      *              *-------------------------------------------------*
      *              * Estrazione elementi                             *
      *              *-------------------------------------------------*
           move      w-det-dat-num-alf    to   w-all-str-alf          .
           move      "-"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *              *-------------------------------------------------*
      *              * Conversione elementi                            *
      *              *-------------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      02                   to   v-car                  .
           move      w-all-str-cat (1)
                    (03 : 02)             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-dat-num-saa      .
      *
           move      "CV"                 to   v-ope                  .
           move      02                   to   v-car                  .
           move      w-all-str-cat (2)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-dat-num-mes      .
      *
           move      "CV"                 to   v-ope                  .
           move      02                   to   v-car                  .
           move      w-all-str-cat (3)    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-dat-num-gio      .
      *              *-------------------------------------------------*
      *              * Data in formato numerico                        *
      *              *-------------------------------------------------*
           move      w-det-dat-num-saa    to   s-saa                  .
           add       100                  to   s-saa                  .
           move      w-det-dat-num-mes    to   s-mes                  .
           move      w-det-dat-num-gio    to   s-gio                  .
           move      s-dat                to   w-det-dat-num-dat      .
       det-dat-num-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-dat-num-999.
       det-dat-num-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice Iva             *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzci0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


