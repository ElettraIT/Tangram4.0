       Identification Division.
       Program-Id.                                 eleinvh0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    inv                 *
      *                                   Fase:    eleinv              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/11/24    *
      *                       Ultima revisione:    NdK del 25/11/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione Inventario                         *
      *                                                                *
      *                    Preparazione JSON per grafici               *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      *                    Chiamato da 'eleinv1h.cbl'                  *
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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [miu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmiu"                          .
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cpw"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-tip-grf              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-ele              pic  9(05)                  .
           05  w-exe-ctr-not              pic  9(05)                  .
           05  w-exe-ctr-spn              pic  9(05)                  .
           05  w-exe-ctr-vst              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per emissione JSON                             *
      *        *-------------------------------------------------------*
           05  w-exe-jsn-vle              pic  x(05)                  .
           05  w-exe-jsn-vlg              pic  x(05)                  .
           05  w-exe-jsn-pme              pic  x(15)                  .
           05  w-exe-jsn-pmg              pic  x(15)                  .
           05  w-exe-jsn-coe              pic  x(07)                  .
           05  w-exe-jsn-cog              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione tabella utenti                     *
      *        *-------------------------------------------------------*
           05  w-det-tbl-ute.
      *            *---------------------------------------------------*
      *            * Tabella codici                                    *
      *            *---------------------------------------------------*
               10  w-det-tbl-ute-ele  occurs  10
                                 indexed  by   w-det-tbl-ute-inx      .
                   15  w-det-tbl-cod-ute  pic  x(08)                  .
                   15  w-det-tbl-col-ute  pic  x(10)                  .
                   15  w-det-tbl-spn-ute  pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-ute-max      pic  9(03) value 10         .
               10  w-det-tbl-ute-ctr      pic  9(03)                  .
               10  w-det-tbl-ute-ctx      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione tabella responsabili               *
      *        *-------------------------------------------------------*
           05  w-det-tbl-rsm.
      *            *---------------------------------------------------*
      *            * Tabella codici                                    *
      *            *---------------------------------------------------*
               10  w-det-tbl-cod-ele  occurs  30
                                 indexed  by   w-det-tbl-rsm-inx      .
                   15  w-det-tbl-cod-rsp  pic  9(05)                  .
                   15  w-det-tbl-des-rsp  pic  x(40)                  .
                   15  w-det-tbl-col-rsp  pic  x(10)                  .
                   15  w-det-tbl-spn-rsp  pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-rsm-max      pic  9(03) value 30         .
               10  w-det-tbl-rsm-ctr      pic  9(03)                  .
               10  w-det-tbl-rsm-ctx      pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division                                             .
      ******************************************************************

      *================================================================*
      * Main                                                           *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           perform   ext-prm-000          thru ext-prm-999            .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   opn-fls-000          thru opn-fls-999            .
       main-100.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura e preparazione html            *
      *              *-------------------------------------------------*
           perform   exe-cph-000          thru exe-cph-999            .
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   cls-fls-000          thru cls-fls-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *-----------------------------------------------------------*
       ext-prm-000.
      *              *-------------------------------------------------*
      *              * Data di sistema                                 *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * CORRETTIVO PROVVISORIO                          *
      *              *-------------------------------------------------*
           if        w-exe-dat-exe        <    999999
                     add  1000000         to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-tip-grf          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      1                    to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           move      o-pst                to   w-cgi-str-var          .
           perform   cgi-str-ext-000      thru cgi-str-ext-999        .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni parametri estratti             *
      *              *-------------------------------------------------*
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [miu]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [miu]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-ele          .
           move      zero                 to   w-exe-ctr-not          .
           move      zero                 to   w-exe-ctr-spn          .
           move      zero                 to   w-exe-ctr-vst          .
      *              *-------------------------------------------------*
      *              * Preparazione tabella utenti                     *
      *              *-------------------------------------------------*
           perform   det-tbl-ute-000      thru det-tbl-ute-999        .
      *              *-------------------------------------------------*
      *              * Preparazione tabella responsabili               *
      *              *-------------------------------------------------*
           perform   det-tbl-rsm-000      thru det-tbl-rsm-999        .
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [miu]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      01                   to   rf-miu-tip-mag         .
           move      zero                 to   rf-miu-num-mag         .
           move      spaces               to   rf-miu-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-800.
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [miu]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-800.
       exe-cph-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
       exe-cph-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento contatori                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Elementi totali                             *
      *                  *---------------------------------------------*
           add       1                    to   w-exe-ctr-ele          .
      *                  *---------------------------------------------*
      *                  * Con note                                    *
      *                  *---------------------------------------------*
           if        rf-miu-ann-rlv       not  = spaces
                     add  1               to   w-exe-ctr-not          .
      *                  *---------------------------------------------*
      *                  * Vistati dai magazzinieri                    *
      *                  *---------------------------------------------*
           if        rf-miu-flg-rlv       not  = spaces
                     add  1               to   w-exe-ctr-spn          .
      *                  *---------------------------------------------*
      *                  * Spuntati dai supervisori                    *
      *                  *---------------------------------------------*
           if        rf-miu-flg-spv       not  = spaces
                     add  1               to   w-exe-ctr-vst          .
      *                  *---------------------------------------------*
      *                  * Aggiornamento tabella responsabili          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se presente codice responsabile    *
      *                      *-----------------------------------------*
           if        rf-miu-cod-rsm       =    zero
                     go to exe-cph-600.
      *                      *-----------------------------------------*
      *                      * Ricerca in tabella                      *
      *                      *-----------------------------------------*
           set       w-det-tbl-rsm-inx    to   1                      .
           search    w-det-tbl-cod-ele
                     when    w-det-tbl-cod-rsp
                            (w-det-tbl-rsm-inx)
                                          =    rf-miu-cod-rsm
                     go to   exe-cph-520.
           go to     exe-cph-540.
       exe-cph-520.
      *                      *-----------------------------------------*
      *                      * Se trovato                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore rilevazioni    *
      *                          *-------------------------------------*
           if        w-exe-tip-grf        =    "grf_rsp"
                     add  1               to    w-det-tbl-spn-rsp
                                               (w-det-tbl-rsm-inx)
           else      add  rf-miu-qta-rlv  to    w-det-tbl-spn-rsp
                                               (w-det-tbl-rsm-inx)    .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     exe-cph-600.
       exe-cph-540.
      *                      *-----------------------------------------*
      *                      * Se non trovato                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     exe-cph-600.
       exe-cph-600.
      *                  *---------------------------------------------*
      *                  * Aggiornamento tabella utenti                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se presente codice utente          *
      *                      *-----------------------------------------*
           if        rf-miu-ide-ute       =    spaces
                     go to exe-cph-700.
      *                      *-----------------------------------------*
      *                      * Ricerca in tabella                      *
      *                      *-----------------------------------------*
           set       w-det-tbl-ute-inx    to   1                      .
           search    w-det-tbl-ute-ele
                     when    w-det-tbl-cod-ute
                            (w-det-tbl-ute-inx)
                                          =    rf-miu-ide-ute
                     go to   exe-cph-620.
           go to     exe-cph-640.
       exe-cph-620.
      *                      *-----------------------------------------*
      *                      * Se trovato                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento contatore rilevazioni    *
      *                          *-------------------------------------*
           add       1                    to    w-det-tbl-spn-ute
                                               (w-det-tbl-ute-inx)    .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     exe-cph-700.
       exe-cph-640.
      *                      *-----------------------------------------*
      *                      * Se non trovato                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     exe-cph-700.
       exe-cph-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-200.
       exe-cph-800.
      *              *-------------------------------------------------*
      *              * Emissione risultati                             *
      *              *-------------------------------------------------*
           perform   exe-cph-pre-000      thru exe-cph-pre-999        .
      *              *-------------------------------------------------*
      *              * Emissione risultati in formato JSON             *
      *              *-------------------------------------------------*
           perform   exe-cph-jsn-000      thru exe-cph-jsn-999        .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di preparazione dei dati                       *
      *    *-----------------------------------------------------------*
       exe-cph-pre-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di grafico      *
      *              *-------------------------------------------------*
           if        w-exe-tip-grf        =    "grf_not"
                     go to exe-cph-pre-100
           else if   w-exe-tip-grf        =    "grf_spn"
                     go to exe-cph-pre-200
           else if   w-exe-tip-grf        =    "grf_vst"
                     go to exe-cph-pre-300
           else if   w-exe-tip-grf        =    "grf_rsp" or
                     w-exe-tip-grf        =    "grf_rsq"
                     go to exe-cph-pre-600
           else      go to exe-cph-pre-900.
       exe-cph-pre-100.
      *              *-------------------------------------------------*
      *              * Se grafico 'note'                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing contatore totale elementi           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-ele        to   p-num                  .
           subtract  w-exe-ctr-not        from p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-jsn-vle          .
      *                  *---------------------------------------------*
      *                  * Prompt contatore totale elementi            *
      *                  *---------------------------------------------*
           move      "Senza note"         to   w-exe-jsn-pme          .
      *                  *---------------------------------------------*
      *                  * Colore contatore totale elementi            *
      *                  *---------------------------------------------*
           move      "#FF5733"            to   w-exe-jsn-coe          .
      *                  *---------------------------------------------*
      *                  * Editing contatore elementi con note         *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-not        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-jsn-vlg          .
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione contatore elemen- *
      *                  * ti con note                                 *
      *                  *---------------------------------------------*
           if        w-exe-ctr-not        =    0
                     move  "0"            to   w-exe-jsn-vlg          .   
      *                  *---------------------------------------------*
      *                  * Prompt contatore elementi con note          *
      *                  *---------------------------------------------*
           move      "Con note"           to   w-exe-jsn-pmg          .
      *                  *---------------------------------------------*
      *                  * Colore contatore elementi con note          *
      *                  *---------------------------------------------*
           move      "orange"             to   w-exe-jsn-cog          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-900.
       exe-cph-pre-200.
      *              *-------------------------------------------------*
      *              * Se grafico 'spunta'                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing contatore totale elementi           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-ele        to   p-num                  .
           subtract  w-exe-ctr-spn        from p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-jsn-vle          .
      *                  *---------------------------------------------*
      *                  * Prompt contatore totale elementi            *
      *                  *---------------------------------------------*
           move      "Da vistare"         to   w-exe-jsn-pme          .
      *                  *---------------------------------------------*
      *                  * Editing contatore elementi spuntati         *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-spn        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-jsn-vlg          .
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione contatore elemen- *
      *                  * ti spuntati                                 *
      *                  *---------------------------------------------*
           if        w-exe-ctr-spn        =    0
                     move  "0"            to   w-exe-jsn-vlg          .   
      *                  *---------------------------------------------*
      *                  * Prompt contatore elementi spuntati          *
      *                  *---------------------------------------------*
           move      "Vistati"            to   w-exe-jsn-pmg          .
      *                  *---------------------------------------------*
      *                  * Colore contatore elementi spuntati          *
      *                  *---------------------------------------------*
           move      "#3357FF"            to   w-exe-jsn-cog          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-900.
       exe-cph-pre-300.
      *              *-------------------------------------------------*
      *              * Se grafico 'vistato'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing contatore totale elementi           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-ele        to   p-num                  .
           subtract  w-exe-ctr-vst        from p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-jsn-vle          .
      *                  *---------------------------------------------*
      *                  * Prompt contatore totale elementi            *
      *                  *---------------------------------------------*
           move      "Da spuntare"        to   w-exe-jsn-pme          .
      *                  *---------------------------------------------*
      *                  * Editing contatore elementi vistati          *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-vst        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-jsn-vlg          .
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione contatore elemen- *
      *                  * ti spuntati                                 *
      *                  *---------------------------------------------*
           if        w-exe-ctr-vst        =    0
                     move  "0"            to   w-exe-jsn-vlg          .   
      *                  *---------------------------------------------*
      *                  * Prompt contatore elementi spuntati          *
      *                  *---------------------------------------------*
           move      "Spuntati"           to   w-exe-jsn-pmg          .
      *                  *---------------------------------------------*
      *                  * Colore contatore elementi spuntati          *
      *                  *---------------------------------------------*
           move      "#3357FF"            to   w-exe-jsn-cog          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-900.
       exe-cph-pre-600.
      *              *-------------------------------------------------*
      *              * Se grafico 'responsabili'                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-900.
       exe-cph-pre-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-pre-999.
       exe-cph-pre-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per emissione risultati in formato JSON        *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di grafico      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se grafico per utenti                  *
      *                  *---------------------------------------------*
           if        w-exe-tip-grf        not  = "grf_ute"
                     go to exe-cph-jsn-030.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento utenti            *
      *                  *---------------------------------------------*
           perform   exe-cph-jsn-ute-000  thru exe-cph-jsn-ute-999    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-jsn-900.
       exe-cph-jsn-030.
      *                  *---------------------------------------------*
      *                  * Test se grafici per responsabili            *
      *                  *---------------------------------------------*
           if        w-exe-tip-grf        not  = "grf_rsp" and
                     w-exe-tip-grf        not  = "grf_rsq"
                     go to exe-cph-jsn-050.
      *                  *---------------------------------------------*
      *                  * Subroutine di trattamento responsabili      *
      *                  *---------------------------------------------*
           perform   exe-cph-jsn-rsp-000  thru exe-cph-jsn-rsp-999    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-jsn-900.
       exe-cph-jsn-050.
      *              *-------------------------------------------------*
      *              * Header per JSON                                 *
      *              *-------------------------------------------------*
           display   "Content-Type: application/json"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * DEBUG                                           *
      *              *-------------------------------------------------*
           if        w-exe-jsn-vle        =    spaces
                     move  "10"           to   w-exe-jsn-vle          .
           if        w-exe-jsn-coe        =    spaces
                     move  "#800000"      to   w-exe-jsn-coe          .
           if        w-exe-jsn-pme        =    spaces
                     move  "PROVA 1"      to   w-exe-jsn-pme          .
      *
           if        w-exe-jsn-vlg        =    spaces
                     move  "200"          to   w-exe-jsn-vlg          .
           if        w-exe-jsn-cog        =    spaces
                     move  "#FFFF00"      to   w-exe-jsn-cog          .
           if        w-exe-jsn-pmg        =    spaces
                     move  "PROVA 2"      to   w-exe-jsn-pmg          .
       exe-cph-jsn-100.
      *              *-------------------------------------------------*
      *              * Apertura                                        *
      *              *-------------------------------------------------*
           display   "["                                              .
           display   "   {"                                           .
       exe-cph-jsn-200.
      *              *-------------------------------------------------*
      *              * 1. valore                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """value"" : "
                                delimited by   size
                     w-exe-jsn-vle
                                delimited by   spaces
                     ","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * 1. colore                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """color"" : """
                                delimited by   size
                     w-exe-jsn-coe
                                delimited by   spaces
                     ""","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * 1. etichetta                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """label"" : """
                                delimited by   size
                     w-exe-jsn-pme
                                delimited by   size
                     """"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       exe-cph-jsn-400.
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           display   "   },"                                          .
           display   "   {"                                           .
       exe-cph-jsn-500.
      *              *-------------------------------------------------*
      *              * 2. valore                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """value"" : "
                                delimited by   size
                     w-exe-jsn-vlg
                                delimited by   spaces
                     ","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * 2. colore                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """color"" : """
                                delimited by   size
                     w-exe-jsn-cog
                                delimited by   spaces
                     ""","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * 2. etichetta                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """label"" : """
                                delimited by   size
                     w-exe-jsn-pmg
                                delimited by   size
                     """"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       exe-cph-jsn-800.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           display   "   }"                                           .
           display   "]"                                              .
       exe-cph-jsn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-999.
       exe-cph-jsn-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per emissione risultati in formato JSON        *
      *    *                                                           *
      *    * Sub-subroutine di emissione JSON per utenti               *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-ute-000.
      *              *-------------------------------------------------*
      *              * Header per JSON                                 *
      *              *-------------------------------------------------*
           display   "Content-Type: application/json"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Apertura                                        *
      *              *-------------------------------------------------*
           display   "["                                              .
           display   "   {"                                           .
       exe-cph-jsn-ute-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tbl-ute-ctx      .
       exe-cph-jsn-ute-200.
           add       1                    to   w-det-tbl-ute-ctx      .
           if        w-det-tbl-ute-ctx    >    w-det-tbl-ute-ctr
                     go to exe-cph-jsn-ute-800.
           if        w-det-tbl-ute-ctx    >    w-det-tbl-ute-max
                     go to exe-cph-jsn-ute-800.
       exe-cph-jsn-ute-300.
      *              *-------------------------------------------------*
      *              * Valore                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-det-tbl-spn-ute
                    (w-det-tbl-ute-ctx)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           if        w-det-tbl-spn-ute
                    (w-det-tbl-ute-ctx)   =    0   or
                     w-det-tbl-spn-ute
                    (w-det-tbl-ute-ctx)   not  numeric
                     move  "0"            to   p-edt                  .   
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """value"" : "
                                delimited by   size
                     p-edt
                                delimited by   spaces
                     ","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Colore                                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """color"" : """
                                delimited by   size
                     w-det-tbl-col-ute
                    (w-det-tbl-ute-ctx)
                                delimited by   spaces
                     ""","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Etichetta                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """label"" : """
                                delimited by   size
                     w-det-tbl-cod-ute
                    (w-det-tbl-ute-ctx)
                                delimited by   size
                     """"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       exe-cph-jsn-400.
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da emettere                         *
      *                  *---------------------------------------------*
           if        w-det-tbl-ute-ctx    =    w-det-tbl-ute-ctr
                     go to exe-cph-jsn-ute-700.
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "   },"                                          .
           display   "   {"                                           .
       exe-cph-jsn-ute-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-ute-200.
       exe-cph-jsn-ute-800.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           display   "   }"                                           .
           display   "]"                                              .
       exe-cph-jsn-ute-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-ute-999.
       exe-cph-jsn-ute-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per emissione risultati in formato JSON        *
      *    *                                                           *
      *    * Sub-subroutine di emissione JSON per responsabili         *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-rsp-000.
      *              *-------------------------------------------------*
      *              * Header per JSON                                 *
      *              *-------------------------------------------------*
           display   "Content-Type: application/json"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Apertura                                        *
      *              *-------------------------------------------------*
           display   "["                                              .
           display   "   {"                                           .
       exe-cph-jsn-rsp-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tbl-rsm-ctx      .
       exe-cph-jsn-rsp-200.
           add       1                    to   w-det-tbl-rsm-ctx      .
           if        w-det-tbl-rsm-ctx    >    w-det-tbl-rsm-ctr
                     go to exe-cph-jsn-rsp-800.
           if        w-det-tbl-rsm-ctx    >    w-det-tbl-rsm-max
                     go to exe-cph-jsn-rsp-800.
       exe-cph-jsn-rsp-300.
      *              *-------------------------------------------------*
      *              * Valore                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-det-tbl-spn-rsp
                    (w-det-tbl-rsm-ctx)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           if        w-det-tbl-spn-rsp
                    (w-det-tbl-rsm-ctx)   =    0   or
                     w-det-tbl-spn-rsp
                    (w-det-tbl-rsm-ctx)   not  numeric
                     move  "0"            to   p-edt                  .   
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """value"" : "
                                delimited by   size
                     p-edt
                                delimited by   spaces
                     ","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Colore                                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """color"" : """
                                delimited by   size
                     w-det-tbl-col-rsp
                    (w-det-tbl-rsm-ctx)
                                delimited by   spaces
                     ""","
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Etichetta                                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """label"" : """
                                delimited by   size
                     w-det-tbl-des-rsp
                    (w-det-tbl-rsm-ctx)
                                delimited by   size
                     """"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       exe-cph-jsn-400.
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da emettere                         *
      *                  *---------------------------------------------*
           if        w-det-tbl-rsm-ctx    =    w-det-tbl-rsm-ctr
                     go to exe-cph-jsn-rsp-700.
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "   },"                                          .
           display   "   {"                                           .
       exe-cph-jsn-rsp-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-rsp-200.
       exe-cph-jsn-rsp-800.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           display   "   }"                                           .
           display   "]"                                              .
       exe-cph-jsn-rsp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-rsp-999.
       exe-cph-jsn-rsp-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome campo           *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "tip_grf"
                     move  w-all-str-cat (2)
                                          to   w-exe-tip-grf          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tabella utenti                             *
      *    *                                                           *
      *    * Giorgio - ___ MIGLIORABILE ___                            *
      *    *-----------------------------------------------------------*
       det-tbl-ute-000.
      *              *-------------------------------------------------*
      *              * Forzatura numero elementi                       *
      *              *-------------------------------------------------*
           move      07                   to   w-det-tbl-ute-ctr      .
      *              *-------------------------------------------------*
      *              * Codici utente                                   *
      *              *-------------------------------------------------*
           move      "giulia  "           to   w-det-tbl-cod-ute (01) .
           move      "giorgio "           to   w-det-tbl-cod-ute (02) .
           move      "luca    "           to   w-det-tbl-cod-ute (03) .
           move      "andrea  "           to   w-det-tbl-cod-ute (04) .
           move      "cristina"           to   w-det-tbl-cod-ute (05) .
           move      "michela "           to   w-det-tbl-cod-ute (06) .
           move      "mario   "           to   w-det-tbl-cod-ute (07) .
      *              *-------------------------------------------------*
      *              * Colori utente                                   *
      *              *-------------------------------------------------*
           move      "pink    "           to   w-det-tbl-col-ute (01) .
           move      "orange  "           to   w-det-tbl-col-ute (02) .
           move      "green   "           to   w-det-tbl-col-ute (03) .
           move      "red     "           to   w-det-tbl-col-ute (04) .
           move      "white   "           to   w-det-tbl-col-ute (05) .
           move      "brown   "           to   w-det-tbl-col-ute (06) .
           move      "grey    "           to   w-det-tbl-col-ute (07) .
       det-tbl-ute-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tbl-ute-999.
       det-tbl-ute-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tabella responsabili                       *
      *    *-----------------------------------------------------------*
       det-tbl-rsm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tbl-rsm-ctr      .
       det-tbl-rsm-050.
      *              *-------------------------------------------------*
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
       det-tbl-rsm-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [zrm]                         *
      *              *-------------------------------------------------*
           move      "ST"                 to   f-ope                  .
           move      "CODRSP    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-zrm-cod-rsp         .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: a close                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-rsm-900.
       det-tbl-rsm-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [zrm]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-rsm-900.
       det-tbl-rsm-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [zrm]                      *
      *              *-------------------------------------------------*
       det-tbl-rsm-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-tbl-rsm-500.
      *              *-------------------------------------------------*
      *              * Incremento tabella                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento tabella                          *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-rsm-ctr      .
           move      rf-zrm-cod-rsp       to   w-det-tbl-cod-rsp
                                              (w-det-tbl-rsm-ctr)     .
           move      rf-zrm-des-rsp       to   w-det-tbl-des-rsp
                                              (w-det-tbl-rsm-ctr)     .
      *                  *---------------------------------------------*
      *                  * Assegnazione colore da anagrafica           *
      *                  *---------------------------------------------*
           if        rf-zrm-alx-exp       =    spaces
                     go to det-tbl-rsm-550.
           move      rf-zrm-alx-exp       to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)     .
           go to     det-tbl-rsm-600.
       det-tbl-rsm-550.
      *                  *---------------------------------------------*
      *                  * Assegnazione colore se non trovata in ana-  *
      *                  * grafica (vedi 'pmag0600')                   *
      *                  * ___ DA ELIMINARE ___                        *
      *                  *---------------------------------------------*
           if        rf-zrm-cod-rsp       =    001
                     move  "orange"       to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    002
                     move  "brown"        to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    003
                     move  "red"          to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    004
                     move  "green"        to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    005
                     move  "purple"       to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    006
                     move  "yellow"       to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    007
                     move  "blue"         to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    008
                     move  "lime"         to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    009
                     move  "lightblue"    to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    010
                     move  "white"        to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    011
                     move  "magenta"      to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    012
                     move  "aquamarine"   to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    013
                     move  "maroon"       to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    014
                     move  "#FBB117"      to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else if   rf-zrm-cod-rsp       =    015
                     move  "#EDE6D6"      to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)
           else      move  "grey"         to   w-det-tbl-col-rsp
                                              (w-det-tbl-rsm-ctr)     .
       det-tbl-rsm-600.
      *                  *---------------------------------------------*
      *                  * Normalizzazione spunte                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tbl-spn-rsp
                                              (w-det-tbl-rsm-ctr)     .
       det-tbl-rsm-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-tbl-rsm-200.
       det-tbl-rsm-900.
      *              *-------------------------------------------------*
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tbl-rsm-999.
       det-tbl-rsm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines di trattamento variabile POST                 *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
