       Identification Division.
       Program-Id.                                 eleodsZ0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    eleods              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 05/01/24    *
      *                       Ultima revisione:    NdK del 29/05/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione spedizioni                         *
      *                                                                *
      *                    Stampa etichetta Zebra carpenteria pronta   *
      *                                                                *
      *                    Il programma richiama lo script Perl        *
      *                    '/abd/etc/t_ele_zbr_stp' con il path        *
      *                    completo                                    *
      *                                                                *
      *              !!!   La stampante viene letta dai parametri      *
      *                    'xpg440' con la fase 'eleods'               *
      *                                                                *
      *                    ELETTRA                                     *
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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mppssx"  *
      *    *-----------------------------------------------------------*
       01  j.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  j-ope                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *-------------------------------------------------------*
           05  j-tre                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Chiave record                                         *
      *        *-------------------------------------------------------*
           05  j-kre                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  j-dat.
               10  j-chr occurs 2048      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        *-------------------------------------------------------*
           05  j-rsc                      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return message                                        *
      *        *-------------------------------------------------------*
           05  j-msg                      pic  x(80)                  .

      *    *===========================================================*
      *    * Work per records di [pss] 'pms'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wpsspms0.cpw"                   .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [hsx]                                                 *
      *        *-------------------------------------------------------*
           copy      "ele/ods/fls/rec/rfhsx"                          .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *-----------------------------------------------------------*
       01  w-cgi-str.
      *        *-------------------------------------------------------*
      *        * Variabile POST da trattare                            *
      *        *-------------------------------------------------------*
           05  w-cgi-str-var.
               10  filler    occurs 1800  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Delimitatore                                          *
      *        *-------------------------------------------------------*
           05  w-cgi-str-del              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore delle stringhe da estrarre                     *
      *        *-------------------------------------------------------*
           05  w-cgi-str-fld  occurs 20   pic  x(90)                  .
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-cgi-str-ctr              pic  9(02)                  .
           05  w-cgi-str-num              pic  9(02)                  .
           05  w-cgi-str-max              pic  9(02) value 20         .
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  w-cgi-tip-ope              pic  x(02)                  .

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
           05  w-exe-prt-ods              pic  x(11)                  .
           05  w-exe-prt-cnv              pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Descrizione per voci descrittive in fattura           *
      *        *-------------------------------------------------------*
           05  w-det-des-zvf.
               10  w-det-des-zvf-num      pic  9(01)                  .
               10  w-det-des-zvf-cod      pic  x(03)                  .
               10  w-det-des-zvf-lng      pic  x(03)                  .
               10  w-det-des-zvf-des      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det pathname file sequenziale in output      *
      *        *-------------------------------------------------------*
           05  w-det-pth-fso.
               10  w-det-pth-fso-pth      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Per Fine riga                                         *
      *        *-------------------------------------------------------*
           05  w-det-fin-rig.
      *            *---------------------------------------------------*
      *            * Separatore 'CR'                                   *
      *            *---------------------------------------------------*
               10  w-det-fin-rig-chr      pic  x(01)   value ";"      .

      *    *===========================================================*
      *    * Work-area per bufferizzazione dati da stampare nelle      *
      *    * etichette segnacolli                                      *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
       01  w-out.
      *        *-------------------------------------------------------*
      *        * Dati etichette                                        *
      *        *-------------------------------------------------------*
           05  w-out-eti.
      *            *---------------------------------------------------*
      *            * Tipo record                                       *
      *            *                                                   *
      *            * - 'cod_stp' : Codice stampante                    *
      *            * - 'tip_eti' : Tipo etichette                      *
      *            * - 'num_eti' : Numero di etichette                 *
      *            *                                                   *
      *            * - 'num_doc' : Numero documento da stampare        *
      *            * - 'rag_cli' : Ragione sociale cliente             *
      *            *                                                   *
      *            * - 'tra_cur' : Trasporto a cura                    *
      *            * - 'cod_vet' : Codice vettore                      *
      *            * - 'rag_vet' : Ragione sociale vettore             *
      *            *                                                   *
      *            * - 'cod_spd' : Codice spedizione                   *
      *            * - 'des_spd' : Descrizione spedizione              *
      *            * - 'cod_por' : Codice porto                        *
      *            * - 'des_por' : Descrizione porto                   *
      *            * - 'spe_spd' : Spese di spedizione                 *
      *            *                                                   *
      *            * - 'ann_sp1' : Annotazioni per la spedizione       *
      *            * - 'ann_sp2' : "           "   "  "                *
      *            * - 'ann_sp3' : "           "   "  "                *
      *            * - 'ann_sp4' : "           "   "  "                *
      *            *---------------------------------------------------*
               10  w-out-eti-tip-rec      pic  x(07)                  .
               10  w-out-eti-sep-001      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore record                                     *
      *            *---------------------------------------------------*
               10  w-out-eti-val-rec      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Carattere di fine riga                            *
      *            *---------------------------------------------------*
               10  w-out-eti-fin-rig      pic  x(01)                  .

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
           move      spaces               to   w-exe-prt-ods          .
           move      zero                 to   w-exe-prt-cnv          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      01                   to   w-cgi-str-num          .
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
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione in numerico protocollo          *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-ods        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-prt-cnv          .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
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
      *              * [ost]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *              *-------------------------------------------------*
      *              * [zvf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *              *-------------------------------------------------*
      *              * [hsx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "ele/ods/fls/ioc/obj/iofhsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsx                 .
      *              *-------------------------------------------------*
      *              * [mppssf]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [ost]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *              *-------------------------------------------------*
      *              * [zvf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *              *-------------------------------------------------*
      *              * [hsx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "ele/ods/fls/ioc/obj/iofhsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsx                 .
      *              *-------------------------------------------------*
      *              * [mppssf]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *              *-------------------------------------------------*
      *              * Test di cancellabilita' per modulo     "mppssf" *
      *              *-------------------------------------------------*
           move      "X?"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
           if        j-rsc                not  = spaces
                     go to cls-fls-999.
      *              *-------------------------------------------------*
      *              * Cancellazione modulo                   "mppssf" *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mppssf"                         .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [ost]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * Lettura [ost]                                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-exe-prt-cnv        to   rf-ost-num-prt         .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to exe-cph-400.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
       exe-cph-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione [hsx]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "ele/ods/fls/ioc/obj/iofhsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsx                 .
      *              *-------------------------------------------------*
      *              * Lettura record testata da stampare              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-exe-prt-cnv        to   rf-hsx-num-prt         .
           move      "ele/ods/fls/ioc/obj/iofhsx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hsx                 .
      *              *-------------------------------------------------*
      *              * Subroutine di stampa                            *
      *              *-------------------------------------------------*
           perform   exe-cph-eti-000      thru exe-cph-eti-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-900.
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'estrazione di max 20 coppie campi/valori da *
      *    * variabile POST letta                                      *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-cgi-str-var     = Variabile POST letta         *
      *    *                                                           *
      *    *          w-cgi-str-del     = Delimitatore (&)             *
      *    *                                                           *
      *    * Output : w-cgi-str-fld (i) = coppie estratte              *
      *    *                                                           *
      *    *          w-cgi-str-num     = Numero coppie estratte       *
      *    *-----------------------------------------------------------*
       cgi-str-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "&"                  to   w-cgi-str-del          .
           move      zero                 to   w-cgi-str-num          .
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-200.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-300.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
           go to     cgi-str-ext-200.
       cgi-str-ext-300.
      *              *-------------------------------------------------*
      *              * Test preliminare se il campo e' vuoto           *
      *              *-------------------------------------------------*
           if        w-cgi-str-var        =    spaces
                     go to cgi-str-ext-900.
       cgi-str-ext-400.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-cgi-str-var
                                delimited by   w-cgi-str-del
                                          into w-cgi-str-fld (01)
                                               w-cgi-str-fld (02)
                                               w-cgi-str-fld (03)
                                               w-cgi-str-fld (04)
                                               w-cgi-str-fld (05)
                                               w-cgi-str-fld (06)
                                               w-cgi-str-fld (07)
                                               w-cgi-str-fld (08)
                                               w-cgi-str-fld (09)
                                               w-cgi-str-fld (10)
                                               w-cgi-str-fld (11)
                                               w-cgi-str-fld (12)
                                               w-cgi-str-fld (13)
                                               w-cgi-str-fld (14)
                                               w-cgi-str-fld (15)
                                               w-cgi-str-fld (16)
                                               w-cgi-str-fld (17)
                                               w-cgi-str-fld (18)
                                               w-cgi-str-fld (19)
                                               w-cgi-str-fld (20)     .
       cgi-str-ext-500.
      *              *-------------------------------------------------*
      *              * Ciclo di verifica numero stringhe estratte      *
      *              *-------------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-600.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-900.
           if        w-cgi-str-fld
                    (w-cgi-str-ctr)       =    spaces
                     go to cgi-str-ext-900.
           add       1                    to   w-cgi-str-num          .
       cgi-str-ext-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-600.
       cgi-str-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-999.
       cgi-str-ext-999.
           exit.

      *    *===========================================================*
      *    * Operazioni su parametri in input                          *
      *    *-----------------------------------------------------------*
       ope-prm-inp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cgi-tip-ope        =    "NO"
                     go to ope-prm-inp-100
           else if   w-cgi-tip-ope        =    "EX"
                     go to ope-prm-inp-500.
       ope-prm-inp-100.
      *              *=================================================*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di normalizzazione                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-120.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-120.
       ope-prm-inp-500.
      *              *=================================================*
      *              * Estrazione coppie                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di estrazione                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-520.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
      *                  *---------------------------------------------*
      *                  * Estrazione                                  *
      *                  *---------------------------------------------*
           move      w-cgi-str-fld
                    (w-cgi-str-ctr)       to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           perform   ext-prm-ass-000      thru ext-prm-ass-999        .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-520.
       ope-prm-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ope-prm-inp-999.
       ope-prm-inp-999.
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
           if        w-all-str-cat (1)    =    "prt_ods"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-ods          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di stampa etichette segnacolli                 *
      *    *-----------------------------------------------------------*
        exe-cph-eti-000.
      *              *-------------------------------------------------*
      *              * Apertura file sequenziale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione pathname file sequenziale in *
      *                  * output                                      *
      *                  *---------------------------------------------*
           perform   exe-cph-pth-000      thru exe-cph-pth-999        .
      *                  *---------------------------------------------*
      *                  * Apertura del file in output                 *
      *                  *---------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      "seq "               to   g-nam                  .
           move      w-det-pth-fso-pth    to   g-pat                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
        exe-cph-eti-100.
      *              *-------------------------------------------------*
      *              * Emissione codice stampante                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "cod_stp"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RD"                 to   j-ope                  .
           move      "PMS"                to   j-tre                  .
           move      "eleods"             to   j-kre                  .
           call      "swd/mod/prg/obj/mppssf"
                                        using  r
                                               j                      .
      *                  *---------------------------------------------*
      *                  * Record in area di lavoro                    *
      *                  *---------------------------------------------*
           move      j-dat                to   w-pms                  .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      w-pms-cod-stp        to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-150.
      *              *-------------------------------------------------*
      *              * Emissione tipo etichette                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "tip_eti"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      "3"                  to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-200.
      *              *-------------------------------------------------*
      *              * Emissione numero di etichette                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "num_eti"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Attualmente forzato a 1                     *
      *                  *---------------------------------------------*
           move      "1"                  to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-250.
      *              *-------------------------------------------------*
      *              * Emissione numero documento                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "num_doc"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-ost-num-prt
                    (06 :06)              to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      p-edt                to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-225.
      *              *-------------------------------------------------*
      *              * Emissione Ragione sociale cliente               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "rag_cli"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [cli]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [cli]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      rf-cli-rag-soc       to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-300.
      *              *-------------------------------------------------*
      *              * Emissione trasporto a cura                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "tra_cur"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           if        rf-hsx-tra-cur       =    zero
                     move  "(Non specif.)   "
                                          to   w-out-eti-val-rec
           else if   rf-hsx-tra-cur       =    10
                     move  "MITTENTE        "
                                          to   w-out-eti-val-rec
           else if   rf-hsx-tra-cur       =    20
                     move  "DESTINATARIO    "
                                          to   w-out-eti-val-rec
           else if   rf-hsx-tra-cur       =    30
                     move  "VETTORE         "
                                          to   w-out-eti-val-rec
           else      move  "(Non specif.)   "
                                          to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-350.
      *              *-------------------------------------------------*
      *              * Emissione Codice vettore                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "cod_vet"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-hsx-cod-vet       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      p-edt                to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-400.
      *              *-------------------------------------------------*
      *              * Emissione Ragione sociale vettore               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "rag_vet"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [vet]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [vet]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET    "         to   f-key                  .
           move      rf-hsx-cod-vet       to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      rf-vet-rag-soc       to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-450.
      *              *-------------------------------------------------*
      *              * Emissione Codice spedizione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "cod_spd"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice spedizione                           *
      *                  *---------------------------------------------*
           move      rf-hsx-voc-des (1)   to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-500.
      *              *-------------------------------------------------*
      *              * Emissione Descrizione spedizione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "des_spd"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Lettura voce descrittiva                    *
      *                  *---------------------------------------------*
           move      01                   to   w-det-des-zvf-num      .
           move      rf-hsx-voc-des (1)   to   w-det-des-zvf-cod      .
           move      rf-ost-cod-lng       to   w-det-des-zvf-lng      .
           perform   det-des-zvf-000      thru det-des-zvf-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione spedizione                      *
      *                  *---------------------------------------------*
           move      w-det-des-zvf-des    to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-550.
      *              *-------------------------------------------------*
      *              * Emissione Codice porto                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "cod_por"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice porto                                *
      *                  *---------------------------------------------*
           move      rf-hsx-voc-des (2)   to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-600.
      *              *-------------------------------------------------*
      *              * Emissione Descrizione porto                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "des_por"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Lettura voce descrittiva                    *
      *                  *---------------------------------------------*
           move      02                   to   w-det-des-zvf-num      .
           move      rf-hsx-voc-des (2)   to   w-det-des-zvf-cod      .
           move      rf-ost-cod-lng       to   w-det-des-zvf-lng      .
           perform   det-des-zvf-000      thru det-des-zvf-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione porto                           *
      *                  *---------------------------------------------*
           move      w-det-des-zvf-des    to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-650.
      *              *-------------------------------------------------*
      *              * Emissione Spese di spedizione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "spe_spd"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-hsx-spe-spd       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore record                               *
      *                  *---------------------------------------------*
           move      p-edt                to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-700.
      *              *-------------------------------------------------*
      *              * Emissione Annotazioni spedizione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "ann_sp1"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice porto                                *
      *                  *---------------------------------------------*
           move      rf-hsx-ann-rig (1)   to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-720.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "ann_sp2"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice porto                                *
      *                  *---------------------------------------------*
           move      rf-hsx-ann-rig (2)   to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-730.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "ann_sp3"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice porto                                *
      *                  *---------------------------------------------*
           move      rf-hsx-ann-rig (3)   to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-740.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-eti              .
      *                  *---------------------------------------------*
      *                  * Tipo record                                 *
      *                  *---------------------------------------------*
           move      "ann_sp4"            to   w-out-eti-tip-rec      .
      *                  *---------------------------------------------*
      *                  * Codice porto                                *
      *                  *---------------------------------------------*
           move      rf-hsx-ann-rig (4)   to   w-out-eti-val-rec      .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           perform   exe-cph-put-000      thru  exe-cph-put-999       .
        exe-cph-eti-800.
      *              *-------------------------------------------------*
      *              * Stampa tramite apposito script                  *
      *              *                                                 *
      *              * IMPORTANTE: il path deve essere completo!       *
      *              *-------------------------------------------------*
           move      spaces               to   o-shs                  .
      *
           string    "/abd/etc/t_ele_zbr_stp "  
                                delimited by   size
                     rf-ost-num-prt
                                delimited by   spaces
                                          into o-shs                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
        exe-cph-eti-850.
      *              *-------------------------------------------------*
      *              * Chiusura files temporaneo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo utilizzato             *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
        exe-cph-eti-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to      exe-cph-eti-999.
        exe-cph-eti-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura singola riga                      *
      *    *-----------------------------------------------------------*
        exe-cph-put-000.
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           move      ";"                  to   w-out-eti-sep-001      .
      *              *-------------------------------------------------*
      *              * Carattere di fine riga                          *
      *              *-------------------------------------------------*
           move      w-det-fin-rig-chr    to   w-out-eti-fin-rig      .
      *              *-------------------------------------------------*
      *              * Scrittura sequenziale                           *
      *              *-------------------------------------------------*
           move      w-out-eti            to   g-rec                  .
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
        exe-cph-put-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to      exe-cph-put-999.
        exe-cph-put-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Determinazione pathname completo per il file di appoggio  *
      *    * in output                                                 *
      *    *-----------------------------------------------------------*
       exe-cph-pth-000.
      *              *-------------------------------------------------*
      *              * Preparazione nome file completo                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-pth-fso-pth      .
      *              *-------------------------------------------------*
      *              * Pathname di base da segreteria                  *
      *              *-------------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "spl "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                     .
      *              *-------------------------------------------------*
      *              * String                                          *
      *              *-------------------------------------------------*
           string    s-pat      delimited by   spaces
                     "/"        delimited by   size
                     rf-ost-num-prt
                                delimited by   size
                     ".csv"     delimited by   size
                                          into w-det-pth-fso-pth      .
       exe-cph-pth-999.
           exit.

      *    *===========================================================*
      *    * Descrizione in lingua per la stampa di : voci descrittive *
      *    * in fattura per i clienti                                  *
      *    *-----------------------------------------------------------*
       det-des-zvf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-des-zvf-des      .
       det-des-zvf-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [zvf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua del cliente                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-det-des-zvf-num    to   rf-zvf-num-def         .
           move      w-det-des-zvf-cod    to   rf-zvf-cod-def         .
           move      w-det-des-zvf-lng    to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zvf-200.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zvf-des-stp       to   w-det-des-zvf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua per l'Italia                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-det-des-zvf-num    to   rf-zvf-num-def         .
           move      w-det-des-zvf-cod    to   rf-zvf-cod-def         .
           move      "I  "                to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zvf-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zvf-des-stp       to   w-det-des-zvf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-400.
      *              *-------------------------------------------------*
      *              * Uscita con valore a spaces                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
