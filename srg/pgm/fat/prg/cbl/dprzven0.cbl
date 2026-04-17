       Identification Division.
       Program-Id.                                 dprzven0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/08/91    *
      *                       Ultima revisione:    NdK del 26/07/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione dei prezzi di vendita             *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-ven-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-ven-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-ven-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-prz-ven-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "PV" - Determinazione prezzo di vendita                        *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-ven-tip-ope = "PV"                       *
      *                                                                *
      *                 d-prz-ven-cod-cli = Codice cliente             *
      *                                                                *
      *                 d-prz-ven-sgl-vpp = Sigla valuta per il prezzo *
      *                                                                *
      *                 d-prz-ven-dec-vpp = Decimali valuta per prezzo *
      *                                                                *
      *                 d-prz-ven-num-pro = Codice numerico magazzino  *
      *                                                                *
      *                 d-prz-ven-cod-lst = Codice listino             *
      *                                                                *
      *                 d-prz-ven-prz-lsb = Prezzo di listino base     *
      *                                                                *
      *                 d-prz-ven-sgl-vlb = Sigla valuta listino base  *
      *                                                                *
      *                 d-prz-ven-dec-vlb = Decimali valuta listino    *
      *                                     base                       *
      *                                                                *
      *                 d-prz-ven-tfu-prz = Tipo determinazione prezzo *
      *                                     - Spaces : Standard        *
      *                                     - #      : Particolare     *
      *                                                                *
      *                 d-prz-ven-dat-rfp = Data riferimento prezzo    *
      *                                                                *
      *                 d-prz-ven-qta-rif = Quantita' di riferimento   *
      *                                    (opzionale)                 *
      *                                                                *
      *        Output : d-prz-ven-prz-ven = Prezzo di vendita          *
      *                                                                *
      *                 d-prz-ven-prz-bas = Prezzo di listino base,    *
      *                                     alla data di riferimento   *
      *                                                                *
      *                 d-prz-ven-dat-rfp = Data riferimento prezzo    *
      *                                     determinato                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "EC" - Determinazione prezzo per eco-contributi                *
      *                                                                *
      *                                                                *
      *        Input  : d-prz-ven-tip-ope = "EC"                       *
      *                                                                *
      *                 d-prz-ven-num-pro = Codice numerico magazzino  *
      *                                                                *      *                                                                *
      *                                                                *
      *        Output : d-prz-ven-prz-ven = Prezzo di vendita          *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Tipo determinazione prezzo di vendita                 *
      *        *-------------------------------------------------------*
           05  w-prs-tfu-prz              pic  x(42)                  .

      *    *===========================================================*
      *    * Work-area per determinazione prezzo                       *
      *    *-----------------------------------------------------------*
       01  w-dpv.
      *        *-------------------------------------------------------*
      *        * Work per determinazione funzionamento prezzo di ven-  *
      *        * dita                                                  *
      *        *-------------------------------------------------------*
           05  w-dpv-tfu-prz.
      *            *---------------------------------------------------*
      *            * Numero elementi in tabella                        *
      *            *---------------------------------------------------*
               10  w-dpv-tfu-prz-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero massimo elementi in tabella                *
      *            *---------------------------------------------------*
               10  w-dpv-tfu-prz-max      pic  9(03) value 14         .
      *            *---------------------------------------------------*
      *            * Tabella tipi di calcolo                           *
      *            *---------------------------------------------------*
               10  w-dpv-tfu-prz-tbl occurs 14.
      *                *-----------------------------------------------*
      *                * Codice di calcolo                             *
      *                *-----------------------------------------------*
                   15  w-dpv-tfu-prz-cdc  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Segnale di continuazione con calcolo succes-  *
      *                * sivo                                          *
      *                *-----------------------------------------------*
                   15  w-dpv-tfu-prz-sdc  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work-area di comodo                               *
      *            *---------------------------------------------------*
               10  w-dpv-tfu-prz-wst.
                   15  w-dpv-tfu-prz-wel occurs 14.
                       20  w-dpv-tfu-prz-aaa
                                          pic  9(02)                  .
                       20  w-dpv-tfu-prz-bbb
                                          pic  x(01)                  .
               10  w-dpv-tfu-prz-wcc      pic  9(02)                  .
               10  w-dpv-tfu-prz-wsc      pic  x(01)                  .
               10  w-dpv-tfu-prz-c01      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 1                                 *
      *        *-------------------------------------------------------*
           05  w-dpv-ctr-001              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per flag di determinazione effettuata          *
      *        *-------------------------------------------------------*
           05  w-dpv-flg-dte              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per flag di significativita' determinazione    *
      *        * effettuata                                            *
      *        *-------------------------------------------------------*
           05  w-dpv-flg-sde              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per flag di significativita' determinazione    *
      *        * effettuata in scansione scaglioni quantita'           *
      *        *-------------------------------------------------------*
           05  w-dpv-flg-ssq              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per codice di calcolo in esame                 *
      *        *-------------------------------------------------------*
           05  w-dpv-wrk-cdc              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per segnale di continuazione con il calcolo    *
      *        * successivo                                            *
      *        *-------------------------------------------------------*
           05  w-dpv-wrk-sdc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di comodo per presenza direttive listino         *
      *        *-------------------------------------------------------*
           05  w-dpv-flg-drg              pic  x(01)                  .
           05  w-dpv-flg-som              pic  x(01)                  .
           05  w-dpv-flg-par              pic  x(01)                  .
           05  w-dpv-per-lst occurs 05    pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Comodo per eco-contributo                             *
      *        *-------------------------------------------------------*
           05  w-dpv-prz-wrk              pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per data di riferimento                        *
      *        *-------------------------------------------------------*
           05  w-dpv-dat-rif              pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Cal                               *
      *    *-----------------------------------------------------------*
       01  w-cal.
      *        *-------------------------------------------------------*
      *        * Work per Cal prezzo netto                             *
      *        *-------------------------------------------------------*
           05  w-cal-prz-net.
               10  w-cal-prz-net-som      pic  x(01)                  .
               10  w-cal-prz-net-prz      pic  9(09)                  .
               10  w-cal-prz-net-psc occurs 05
                                          pic  9(02)v9(01)            .
               10  w-cal-prz-net-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Cal importo scontato o maggiorato            *
      *        *-------------------------------------------------------*
           05  w-cal-imp-som.
               10  w-cal-imp-som-som      pic  x(01)                  .
               10  w-cal-imp-som-iml      pic s9(11)                  .
               10  w-cal-imp-som-psc      pic  9(02)v9(01)            .
               10  w-cal-imp-som-w01      pic  9(03)v9(01)            .
               10  w-cal-imp-som-w02      pic s9(14)v9(01)            .
               10  w-cal-imp-som-imn      pic s9(11)                  .
               10  w-cal-imp-som-ams      pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione prezzo di vendi- *
      *    * ta                                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dprzven0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-prz-ven              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-prz-ven-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-prz-ven-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-prz-ven-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-prz-ven-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di vendita            *
      *                  *---------------------------------------------*
           else if   d-prz-ven-tip-ope    =    "PV"
                     perform dpv-000      thru dpv-999
      *                  *---------------------------------------------*
      *                  * Determinazione eco-contributo               *
      *                  *---------------------------------------------*
           else if   d-prz-ven-tip-ope    =    "EC"
                     perform ecr-000      thru ecr-999                .
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
       opn-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo determinazione prezzo di vendita       *
      *                  *---------------------------------------------*
           perform   prs-tfu-prz-000      thru prs-tfu-prz-999        .
       opn-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zls]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *                  *---------------------------------------------*
      *                  * [lst]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * [lsd]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
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
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [zls]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *                  *---------------------------------------------*
      *                  * [lst]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * [lsd]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
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
                     move  spaces         to   d-prz-ven-exi-sts
           else      move  "#"            to   d-prz-ven-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di listino di vendita               *
      *    *-----------------------------------------------------------*
       dpv-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione data di riferimento prezzo      *
      *              *-------------------------------------------------*
           move      d-prz-ven-dat-rfp    to   w-dpv-dat-rif          .
      *              *-------------------------------------------------*
      *              * Preparazione prezzo di listino base alla data   *
      *              *-------------------------------------------------*
           perform   dpv-bas-000          thru dpv-bas-999            .
      *              *-------------------------------------------------*
      *              * Eventuale ritaratura dei valori in base alla    *
      *              * presenza di particolari direttive per il listi- *
      *              * no in corso di trattamento, se non base         *
      *              *-------------------------------------------------*
           perform   rit-val-000          thru rit-val-999            .
       dpv-010.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-prz-ven-prz-ven      .
           move      zero                 to   d-prz-ven-tip-prd      .
       dpv-015.
      *              *-------------------------------------------------*
      *              * Determinazione tipo funzionamento prezzo di     *
      *              * vendita                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo determinazio- *
      *                  * ne richiesto                                *
      *                  *---------------------------------------------*
           if        d-prz-ven-tfu-prz    =    spaces
                     go to dpv-020
           else      go to dpv-040.
       dpv-020.
      *                  *---------------------------------------------*
      *                  * Tipo determinazione richiesta : Standard    *
      *                  *---------------------------------------------*
           move      w-prs-tfu-prz        to   w-dpv-tfu-prz-wst      .
       dpv-022.
           perform   ctl-tfu-prz-000      thru ctl-tfu-prz-999        .
           if        w-dpv-tfu-prz-ele    =    zero
                     move  "02.03.01 "    to   w-dpv-tfu-prz-wst
                     go to dpv-022.
           go to     dpv-100.
       dpv-040.
      *                  *---------------------------------------------*
      *                  * Tipo determinazione richiesta : Particolare *
      *                  *---------------------------------------------*
           move      d-prz-ven-tfu-prz    to   w-dpv-tfu-prz-wst      .
           perform   ctl-tfu-prz-000      thru ctl-tfu-prz-999        .
           if        w-dpv-tfu-prz-ele    =    zero
                     go to dpv-020.
           go to     dpv-100.
       dpv-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-dpv-ctr-001          .
       dpv-120.
           add       1                    to   w-dpv-ctr-001          .
           if        w-dpv-ctr-001        >    w-dpv-tfu-prz-ele
                     go to dpv-900.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di determinazione effet-  *
      *              * tuata a : No                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpv-flg-dte          .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di significativita' de-   *
      *              * terminazione effettuata a : No                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpv-flg-sde          .
      *              *-------------------------------------------------*
      *              * Codice di calcolo in work di comodo             *
      *              *-------------------------------------------------*
           move      w-dpv-tfu-prz-cdc
                    (w-dpv-ctr-001)       to   w-dpv-wrk-cdc          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del codice di calcolo    *
      *              *-------------------------------------------------*
           if        w-dpv-wrk-cdc        =    00
                     go to dpv-200
           else if   w-dpv-wrk-cdc        =    01
                     go to dpv-300
           else if   w-dpv-wrk-cdc        =    02
                     go to dpv-400
           else if   w-dpv-wrk-cdc        =    03
                     go to dpv-500.
       dpv-200.
      *              *-------------------------------------------------*
      *              * Codice di calcolo : Nessun calcolo              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A controllo finale                          *
      *                  *---------------------------------------------*
           go to     dpv-800.
       dpv-300.
      *              *-------------------------------------------------*
      *              * Codice di calcolo : Da prezzo di listino        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino storico    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      01                   to   rf-lsd-tip-rec         .
           move      d-prz-ven-cod-lst    to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lsd-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lsd-num-pro         .
           move      w-dpv-dat-rif        to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-350.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-350.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 01                or
                     rf-lsd-cod-lst       not  = d-prz-ven-cod-lst or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = d-prz-ven-sgl-vpp or
                     rf-lsd-num-pro       not  = d-prz-ven-num-pro
                     go to dpv-350.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                      *-----------------------------------------*
      *                      * Tipo prezzo di vendita determinato :    *
      *                      * Prezzo di listino                       *
      *                      *-----------------------------------------*
           move      01                   to   d-prz-ven-tip-prd      .
      *                      *-----------------------------------------*
      *                      * Se prezzo a zero : A controllo finale   *
      *                      *-----------------------------------------*
           if        rf-lsd-prz-lst       =    zero
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prezzo                  *
      *                      *-----------------------------------------*
           move      rf-lsd-prz-lst       to   d-prz-ven-prz-ven      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data validita' finale   *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-fin       not  = zero
                     move  rf-lsd-dva-fin to   d-prz-ven-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpv-800.
       dpv-350.
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino attuale    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che si tratti del  *
      *                      * listino Base o uno alternativo          *
      *                      *-----------------------------------------*
           if        d-prz-ven-cod-lst    =    spaces
                     go to dpv-360
           else      go to dpv-380.
       dpv-360.
      *                      *-----------------------------------------*
      *                      * Se listino base                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Controllo sigla e decimali valuta   *
      *                          *-------------------------------------*
           if        d-prz-ven-sgl-vlb    not  = d-prz-ven-sgl-vpp or
                     d-prz-ven-dec-vlb    not  = d-prz-ven-dec-vpp
                     go to dpv-800.
      *                          *-------------------------------------*
      *                          * Flag di determinazione effettuata : *
      *                          * Si                                  *
      *                          *-------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                          *-------------------------------------*
      *                          * Tipo prezzo di vendita determinato: *
      *                          * Prezzo di listino                   *
      *                          *-------------------------------------*
           move      01                   to   d-prz-ven-tip-prd      .
      *                          *-------------------------------------*
      *                          * Se prezzo a zero : A controllo fi-  *
      *                          * nale                                *
      *                          *-------------------------------------*
           if        d-prz-ven-prz-lsb    =    zero
                     go to dpv-800.
      *                           *------------------------------------*
      *                           * Flag di significativita' determi-  *
      *                           * nazione effettuata a : Si          *
      *                           *------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                          *-------------------------------------*
      *                          * Bufferizzazione prezzo              *
      *                          *-------------------------------------*
           move      d-prz-ven-prz-lsb    to   d-prz-ven-prz-ven      .
      *                          *-------------------------------------*
      *                          * A controllo finale                  *
      *                          *-------------------------------------*
           go to     dpv-800.
       dpv-380.
      *                      *-----------------------------------------*
      *                      * Se listino alternativo                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura record [lst]                *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      01                   to   rf-lst-tip-rec         .
           move      d-prz-ven-cod-lst    to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lst-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                          *-------------------------------------*
      *                          * Se lettura errata : A controllo fi- *
      *                          * nale                                *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-800.
      *                          *-------------------------------------*
      *                          * Controllo su decimali valuta        *
      *                          *-------------------------------------*
           if        rf-lst-dec-vlt       not  = d-prz-ven-dec-vpp
                     go to dpv-800.
      *                          *-------------------------------------*
      *                          * Flag di determinazione effettuata : *
      *                          * Si                                  *
      *                          *-------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                          *-------------------------------------*
      *                          * Tipo prezzo di vendita determinato: *
      *                          * Prezzo di listino                   *
      *                          *-------------------------------------*
           move      01                   to   d-prz-ven-tip-prd      .
      *                          *-------------------------------------*
      *                          * Se prezzo a zero : A controllo fi-  *
      *                          * nale                                *
      *                          *-------------------------------------*
           if        rf-lst-prz-lst       =    zero
                     go to dpv-800.
      *                           *------------------------------------*
      *                           * Flag di significativita' determi-  *
      *                           * nazione effettuata a : Si          *
      *                           *------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                          *-------------------------------------*
      *                          * Bufferizzazione prezzo              *
      *                          *-------------------------------------*
           move      rf-lst-prz-lst       to   d-prz-ven-prz-ven      .
      *                          *-------------------------------------*
      *                          * Bufferizzazione data valid. finale  *
      *                          *-------------------------------------*
           if        rf-lst-dva-fin       not  = zero
                     move  rf-lst-dva-fin to   d-prz-ven-dat-rfp      .
      *                          *-------------------------------------*
      *                          * A controllo finale                  *
      *                          *-------------------------------------*
           go to     dpv-800.
       dpv-400.
      *              *-------------------------------------------------*
      *              * Codice di calcolo : Prezzo netto concordato per *
      *              * il cliente                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        d-prz-ven-cod-cli    =    zero
                     go to dpv-800.
       dpv-410.
      *                  *---------------------------------------------*
      *                  * Scansione preliminare scaglioni quantita'   *
      *                  *---------------------------------------------*
           perform   dpv-ssq-000          thru dpv-ssq-999            .
      *                  *---------------------------------------------*
      *                  * Test su esito scansione                     *
      *                  *---------------------------------------------*
           if        w-dpv-flg-ssq        =    spaces
                     go to dpv-420.
      *                  *---------------------------------------------*
      *                  * Operazioni per esito scansione positivo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                      *-----------------------------------------*
      *                      * Tipo prezzo di vendita determinato :    *
      *                      * Prezzo netto concordato                 *
      *                      *-----------------------------------------*
           move      02                   to   d-prz-ven-tip-prd      .
      *                      *-----------------------------------------*
      *                      * Se prezzo a zero : A controllo finale   *
      *                      *-----------------------------------------*
           if        rf-lst-prz-lst       =    zero
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prezzo                  *
      *                      *-----------------------------------------*
           move      rf-lst-prz-lst       to   d-prz-ven-prz-ven      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data validita' finale   *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       not  = zero
                     move  rf-lst-dva-fin to   d-prz-ven-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpv-800.
       dpv-420.
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo netto concordato sto- *
      *                  * rico                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      02                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      d-prz-ven-cod-cli    to   rf-lsd-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lsd-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lsd-num-pro         .
           move      w-dpv-dat-rif        to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-450.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-450.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 02                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = d-prz-ven-cod-cli or
                     rf-lsd-sgl-vlt       not  = d-prz-ven-sgl-vpp or
                     rf-lsd-num-pro       not  = d-prz-ven-num-pro
                     go to dpv-450.
      *                      *-----------------------------------------*
      *                      * Controllo su decimali valuta            *
      *                      *-----------------------------------------*
           if        rf-lsd-dec-vlt       not  = d-prz-ven-dec-vpp
                     go to dpv-450.
      *                      *-----------------------------------------*
      *                      * Test se trattamento prezzo netto        *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-prz       not  = "S"
                     go to dpv-450.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                      *-----------------------------------------*
      *                      * Tipo prezzo di vendita determinato :    *
      *                      * Prezzo netto concordato                 *
      *                      *-----------------------------------------*
           move      02                   to   d-prz-ven-tip-prd      .
      *                      *-----------------------------------------*
      *                      * Se prezzo a zero : A controllo finale   *
      *                      *-----------------------------------------*
           if        rf-lsd-prz-lst       =    zero
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prezzo                  *
      *                      *-----------------------------------------*
           move      rf-lsd-prz-lst       to   d-prz-ven-prz-ven      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data validita' finale   *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-fin       not  = zero
                     move  rf-lsd-dva-fin to   d-prz-ven-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpv-800.
       dpv-450.
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo netto concordato at-  *
      *                  * tuale                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura record [lst]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      02                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      d-prz-ven-cod-cli    to   rf-lst-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lst-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : A controllo finale  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Controllo su decimali valuta            *
      *                      *-----------------------------------------*
           if        rf-lst-dec-vlt       not  = d-prz-ven-dec-vpp
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Test se trattamento prezzo netto        *
      *                      *-----------------------------------------*
           if        rf-lst-snx-prz       not  = "S"
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio validita'        *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       =    zero
                     go to dpv-470.
           if        rf-lst-dva-ini       >    w-dpv-dat-rif
                     go to dpv-800.
       dpv-470.
      *                      *-----------------------------------------*
      *                      * Test su data di fine validita'          *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       =    zero
                     go to dpv-480.
           if        rf-lst-dva-fin       <    w-dpv-dat-rif
                     go to dpv-800.
       dpv-480.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                      *-----------------------------------------*
      *                      * Tipo prezzo di vendita determinato :    *
      *                      * Prezzo netto concordato                 *
      *                      *-----------------------------------------*
           move      02                   to   d-prz-ven-tip-prd      .
      *                      *-----------------------------------------*
      *                      * Se prezzo a zero : A controllo finale   *
      *                      *-----------------------------------------*
           if        rf-lst-prz-lst       =    zero
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prezzo                  *
      *                      *-----------------------------------------*
           move      rf-lst-prz-lst       to   d-prz-ven-prz-ven      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data validita' finale   *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       not  = zero
                     move  rf-lst-dva-fin to   d-prz-ven-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpv-800.
       dpv-500.
      *              *-------------------------------------------------*
      *              * Codice di calcolo : Prezzo di listino per cam-  *
      *              * pagna di vendita                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino per campa- *
      *                  * gna di vendita attuale                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura record [lst]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      03                   to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lst-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-550.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lst-dva-ini       >    w-dpv-dat-rif
                     go to dpv-550.
      *                      *-----------------------------------------*
      *                      * Test su data di fine campagna           *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       <    w-dpv-dat-rif
                     go to dpv-550.
      *                      *-----------------------------------------*
      *                      * Controllo su decimali valuta            *
      *                      *-----------------------------------------*
           if        rf-lst-dec-vlt       not  = d-prz-ven-dec-vpp
                     go to dpv-550.
      *                      *-----------------------------------------*
      *                      * Test se trattamento prezzo in campagna  *
      *                      *-----------------------------------------*
           if        rf-lst-snx-prz       not  = "S"
                     go to dpv-550.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                      *-----------------------------------------*
      *                      * Tipo prezzo di vendita determinato :    *
      *                      * Prezzo per campagna promozionale        *
      *                      *-----------------------------------------*
           move      03                   to   d-prz-ven-tip-prd      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prezzo                  *
      *                      *-----------------------------------------*
           move      rf-lst-prz-lst       to   d-prz-ven-prz-ven      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data validita' finale   *
      *                      *-----------------------------------------*
           if        rf-lst-dva-fin       not  = zero
                     move  rf-lst-dva-fin to   d-prz-ven-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpv-800.
       dpv-550.
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo di listino per campa- *
      *                  * gna di vendita storico                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      03                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lsd-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lsd-num-pro         .
           move      w-dpv-dat-rif        to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At End'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 03                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = d-prz-ven-sgl-vpp or
                     rf-lsd-num-pro       not  = d-prz-ven-num-pro
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Test su data di inizio campagna         *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-ini       >    w-dpv-dat-rif
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Test se trattamento prezzo in campagna  *
      *                      *-----------------------------------------*
           if        rf-lsd-snx-prz       not  = "S"
                     go to dpv-800.
      *                      *-----------------------------------------*
      *                      * Flag di determinazione effettuata : Si  *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-dte          .
      *                      *-----------------------------------------*
      *                      * Flag di significativita' determinazione *
      *                      * effettuata a : Si                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-dpv-flg-sde          .
      *                      *-----------------------------------------*
      *                      * Tipo prezzo di vendita determinato :    *
      *                      * Prezzo per campagna promozionale        *
      *                      *-----------------------------------------*
           move      03                   to   d-prz-ven-tip-prd      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione prezzo                  *
      *                      *-----------------------------------------*
           move      rf-lsd-prz-lst       to   d-prz-ven-prz-ven      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data validita' finale   *
      *                      *-----------------------------------------*
           if        rf-lsd-dva-fin       not  = zero
                     move  rf-lsd-dva-fin to   d-prz-ven-dat-rfp      .
      *                      *-----------------------------------------*
      *                      * A controllo finale                      *
      *                      *-----------------------------------------*
           go to     dpv-800.
       dpv-800.
      *              *-------------------------------------------------*
      *              * Controllo finale                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione con il calcolo     *
      *                  * successivo in work di comodo                *
      *                  *---------------------------------------------*
           move      w-dpv-tfu-prz-sdc
                    (w-dpv-ctr-001)       to   w-dpv-wrk-sdc          .
      *                  *---------------------------------------------*
      *                  * Se le direttive indicano un listino di tipo *
      *                  * 'parziale', se il prezzo non e' stato de-   *
      *                  * terminato, si effettua un altro passaggio   *
      *                  * per la lettura del listino 'base'           *
      *                  *---------------------------------------------*
           if        w-dpv-wrk-cdc        =    01       and
                     w-dpv-flg-par        not  = spaces and
                     d-prz-ven-cod-lst    not  = spaces and
                     d-prz-ven-prz-ven    =    zero
                     move spaces          to   d-prz-ven-cod-lst
                     go to dpv-300.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore del se-   *
      *                  * gnale di continuazione                      *
      *                  *---------------------------------------------*
           if        w-dpv-wrk-sdc        =    " "
                     go to dpv-810
           else if   w-dpv-wrk-sdc        =    "."
                     go to dpv-830
           else if   w-dpv-wrk-sdc        =    "#"
                     go to dpv-840
           else if   w-dpv-wrk-sdc        =    "z"
                     go to dpv-850.
       dpv-810.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Interrompi subi- *
      *                  * to il calcolo                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpv-900.
       dpv-830.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo sia nel caso in cui non  *
      *                  * sia stato trovato il record contenente il   *
      *                  * prezzo, sia nel caso che il record sia sta- *
      *                  * to trovato ma con prezzo a zero             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non effettuata : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-dpv-flg-dte        =    spaces
                     go to dpv-120.
      *                      *-----------------------------------------*
      *                      * Se determinazione non significativa :   *
      *                      * riciclo                                 *
      *                      *-----------------------------------------*
           if        w-dpv-flg-sde        =    spaces
                     go to dpv-120.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpv-900.
       dpv-840.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se se non e'   *
      *                  * stata trovato il record contenente il prez- *
      *                  * zo                                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non effettuata : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-dpv-flg-dte        =    spaces
                     go to dpv-120.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpv-900.
       dpv-850.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione : Continua con il  *
      *                  * calcolo successivo solamente se il record   *
      *                  * del prezzo e' stato trovato, ma con prezzo  *
      *                  * a zero                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se determinazione non significativa :   *
      *                      * riciclo                                 *
      *                      *-----------------------------------------*
           if        w-dpv-flg-sde        =    spaces
                     go to dpv-120.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     dpv-900.
       dpv-900.
      *              *-------------------------------------------------*
      *              * Applicazione eventuali sconti o maggiorazioni   *
      *              * in funzione delle direttive generali listino    *
      *              *-------------------------------------------------*
           perform   som-prz-000          thru som-prz-999            .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpv-999.
       dpv-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di listino di vendita               *
      *    *                                                           *
      *    * Subroutine di scansione scaglioni quantita'               *
      *    *-----------------------------------------------------------*
       dpv-ssq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di scansione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpv-flg-ssq          .
       dpv-ssq-100.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura per max 9 elementi             *
      *              *-------------------------------------------------*
           move      zero                 to   w-dpv-ctr-001          .
       dpv-ssq-200.
           add       1                    to   w-dpv-ctr-001          .
           if        w-dpv-ctr-001        >    9
                     go to dpv-ssq-900.
      *              *-------------------------------------------------*
      *              * Lettura record [lst] scaglione                  *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "LSTPRO    "         to   f-key                  .
           move      60                   to   rf-lst-tip-rec         .
           add       w-dpv-ctr-001        to   rf-lst-tip-rec         .
           move      spaces               to   rf-lst-cod-lst         .
           move      d-prz-ven-cod-cli    to   rf-lst-cod-cli         .
           move      d-prz-ven-sgl-vlb    to   rf-lst-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lst-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad uscita            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-ssq-900.
       dpv-ssq-400.
      *              *-------------------------------------------------*
      *              * Sel su [lst]                                    *
      *              *-------------------------------------------------*
           if        d-prz-ven-qta-rif    not  > rf-lst-qta-rif
                     go to dpv-ssq-800.
       dpv-ssq-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     dpv-ssq-200.
       dpv-ssq-800.
      *              *-------------------------------------------------*
      *              * Se prezzo determinato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di determinazione effettuata : Si      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-dpv-flg-ssq          .
       dpv-ssq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpv-ssq-999.
       dpv-ssq-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di listino di vendita               *
      *    *                                                           *
      *    * Determinazione prezzo di listino base alla data           *
      *    *-----------------------------------------------------------*
       dpv-bas-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-prz-ven-prz-bas      .
       dpv-bas-100.
      *              *-------------------------------------------------*
      *              * Controllo sigla e decimali valuta               *
      *              *-------------------------------------------------*
           if        d-prz-ven-sgl-vlb    not  = d-prz-ven-sgl-vpp or
                     d-prz-ven-dec-vlb    not  = d-prz-ven-dec-vpp
                     go to dpv-bas-900.
       dpv-bas-200.
      *              *-------------------------------------------------*
      *              * Listino base passato dal chiamante              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se a zero                              *
      *                  *---------------------------------------------*
           if        d-prz-ven-prz-lsb    =    zero
                     go to dpv-bas-900.
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      d-prz-ven-prz-lsb    to   d-prz-ven-prz-bas      .
       dpv-bas-300.
      *              *-------------------------------------------------*
      *              * Eventuale lettura storico                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione prezzo listino base storico  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [lsd]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LSTPRODVF "         to   f-key                  .
           move      01                   to   rf-lsd-tip-rec         .
           move      spaces               to   rf-lsd-cod-lst         .
           move      zero                 to   rf-lsd-cod-cli         .
           move      d-prz-ven-sgl-vpp    to   rf-lsd-sgl-vlt         .
           move      d-prz-ven-num-pro    to   rf-lsd-num-pro         .
           move      w-dpv-dat-rif        to   rf-lsd-dva-fin         .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-bas-900.
       dpv-bas-320.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [lsd]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
      *                          *-------------------------------------*
      *                          * Test se 'At End'                    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to dpv-bas-900.
       dpv-bas-330.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-lsd-tip-rec       not  = 01                or
                     rf-lsd-cod-lst       not  = spaces            or
                     rf-lsd-cod-cli       not  = zero              or
                     rf-lsd-sgl-vlt       not  = d-prz-ven-sgl-vpp or
                     rf-lsd-num-pro       not  = d-prz-ven-num-pro
                     go to dpv-bas-900.
       dpv-bas-400.
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           if        rf-lsd-prz-lst       not  = zero
                     move  rf-lsd-prz-lst to   d-prz-ven-prz-bas      .
       dpv-bas-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dpv-bas-999.
       dpv-bas-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di listino di vendita               *
      *    *                                                           *
      *    * Eventuale ritaratura dei valori in base alla presenza di  *
      *    * particolari direttive per il listino in corso di tratta-  *
      *    * mento, se non base                                        *
      *    *-----------------------------------------------------------*
       rit-val-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpv-flg-drg          .
           move      spaces               to   w-dpv-flg-som          .
           move      spaces               to   w-dpv-flg-par          .
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        d-prz-ven-cod-lst    =    spaces
                     go to rit-val-900.
       rit-val-100.
      *              *-------------------------------------------------*
      *              * Lettura direttive generali listino              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *                  *---------------------------------------------*
      *                  * Lettura per codice                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODLST    "         to   f-key                  .
           move      d-prz-ven-cod-lst    to   rf-zls-cod-lst         .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rit-val-900.
      *                  *---------------------------------------------*
      *                  * Test su direttive generali                  *
      *                  *---------------------------------------------*
           if        rf-zls-drg-lst       not  = "M" and
                     rf-zls-drg-lst       not  = "S" and
                     rf-zls-drg-lst       not  = "P"
                     go to rit-val-900.
       rit-val-200.
      *              *-------------------------------------------------*
      *              * Se listino 'parziale'                           *
      *              *-------------------------------------------------*
           if        rf-zls-drg-lst       =    "P"
                     move  "#"            to   w-dpv-flg-par
                     go to rit-val-900.
      *              *-------------------------------------------------*
      *              * Attivazione flag direttiva                      *
      *              *-------------------------------------------------*
           move      "#"                  to   w-dpv-flg-drg          .
      *              *-------------------------------------------------*
      *              * Ritaratura del codice listino per determinare   *
      *              * il prezzo                                       *
      *              *-------------------------------------------------*
           move      rf-zls-rif-lst       to   d-prz-ven-cod-lst      .
      *              *-------------------------------------------------*
      *              * Indicatore sconto o maggiorazione               *
      *              *-------------------------------------------------*
           if        rf-zls-drg-lst       =    "M"
                     move  "+"            to   w-dpv-flg-som
           else      move  "-"            to   w-dpv-flg-som          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione %                               *
      *              *-------------------------------------------------*
           move      rf-zls-per-lst (1)   to   w-dpv-per-lst (1)      .
           move      rf-zls-per-lst (2)   to   w-dpv-per-lst (2)      .
           move      rf-zls-per-lst (3)   to   w-dpv-per-lst (3)      .
           move      rf-zls-per-lst (4)   to   w-dpv-per-lst (4)      .
           move      rf-zls-per-lst (5)   to   w-dpv-per-lst (5)      .
       rit-val-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rit-val-999.
       rit-val-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo di listino di vendita               *
      *    *                                                           *
      *    * Eventuale ritaratura del prezzo determinato in base alla  *
      *    * presenza di particolari direttive per il listino          *
      *    *-----------------------------------------------------------*
       som-prz-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        w-dpv-flg-drg        =    spaces
                     go to som-prz-900.
           if        d-prz-ven-prz-ven    =    zero
                     go to som-prz-900.
       som-prz-100.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      d-prz-ven-prz-ven    to   w-cal-prz-net-prz      .
           move      w-dpv-flg-som        to   w-cal-prz-net-som      .
           move      w-dpv-per-lst (1)    to   w-cal-prz-net-psc (1)  .
           move      w-dpv-per-lst (2)    to   w-cal-prz-net-psc (2)  .
           move      w-dpv-per-lst (3)    to   w-cal-prz-net-psc (3)  .
           move      w-dpv-per-lst (4)    to   w-cal-prz-net-psc (4)  .
           move      w-dpv-per-lst (5)    to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   d-prz-ven-prz-ven      .
       som-prz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     som-prz-999.
       som-prz-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo determinazione prezzo di *
      *    *                             vendita                       *
      *    *-----------------------------------------------------------*
       prs-tfu-prz-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tfu-prz]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione lettura                         *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     move  spaces         to   w-prs-tfu-prz
           else      move  s-alf          to   w-prs-tfu-prz          .
       prs-tfu-prz-999.
           exit.

      *    *===========================================================*
      *    * Controllo tipo determinazione prezzo di vendita           *
      *    *-----------------------------------------------------------*
       ctl-tfu-prz-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero elementi in tabella     *
      *              *-------------------------------------------------*
           move      zero                 to   w-dpv-tfu-prz-ele      .
       ctl-tfu-prz-100.
      *              *-------------------------------------------------*
      *              * Test se valore a spazi                          *
      *              *-------------------------------------------------*
           if        w-dpv-tfu-prz-wst    =    spaces
                     go to ctl-tfu-prz-999.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione stringa letta                *
      *              *-------------------------------------------------*
           move      zero                 to   w-dpv-tfu-prz-c01      .
       ctl-tfu-prz-120.
           add       1                    to   w-dpv-tfu-prz-c01      .
           if        w-dpv-tfu-prz-c01    >    w-dpv-tfu-prz-max
                     go to ctl-tfu-prz-999.
      *                  *---------------------------------------------*
      *                  * Controllo formale se valore ammissibile     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su codice di calcolo                    *
      *                      *-----------------------------------------*
           if        w-dpv-tfu-prz-aaa
                    (w-dpv-tfu-prz-c01)   not  numeric
                     go to ctl-tfu-prz-120.
      *                  *---------------------------------------------*
      *                  * Codice di calcolo in work di comodo         *
      *                  *---------------------------------------------*
           move      w-dpv-tfu-prz-aaa
                    (w-dpv-tfu-prz-c01)   to   w-dpv-tfu-prz-wcc      .
      *                  *---------------------------------------------*
      *                  * Controllo su valori ammessi                 *
      *                  *---------------------------------------------*
           if        w-dpv-tfu-prz-wcc    not  = 00 and
                     w-dpv-tfu-prz-wcc    not  = 01 and
                     w-dpv-tfu-prz-wcc    not  = 02 and
                     w-dpv-tfu-prz-wcc    not  = 03
                     go to ctl-tfu-prz-120.
      *                  *---------------------------------------------*
      *                  * Segnale di continuazione calcolo in work di *
      *                  * comodo                                      *
      *                  *---------------------------------------------*
           move      w-dpv-tfu-prz-bbb
                    (w-dpv-tfu-prz-c01)   to   w-dpv-tfu-prz-wsc      .
      *                  *---------------------------------------------*
      *                  * Controllo su valori ammessi                 *
      *                  *---------------------------------------------*
           if        w-dpv-tfu-prz-wsc    not  = " " and
                     w-dpv-tfu-prz-wsc    not  = "." and
                     w-dpv-tfu-prz-wsc    not  = "#" and
                     w-dpv-tfu-prz-wsc    not  = "z"
                     go to ctl-tfu-prz-120.
      *                  *---------------------------------------------*
      *                  * Memorizzazione elemento in tabella          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contatore elementi bufferizzati *
      *                      *-----------------------------------------*
           if        w-dpv-tfu-prz-ele    not  < w-dpv-tfu-prz-max
                     go to ctl-tfu-prz-999.
      *                      *-----------------------------------------*
      *                      * Incremento contatore elementi bufferiz- *
      *                      * zati                                    *
      *                      *-----------------------------------------*
           add       1                    to   w-dpv-tfu-prz-ele      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione elemento                 *
      *                      *-----------------------------------------*
           move      w-dpv-tfu-prz-wcc    to   w-dpv-tfu-prz-cdc
                                              (w-dpv-tfu-prz-ele)     .
           move      w-dpv-tfu-prz-wsc    to   w-dpv-tfu-prz-sdc
                                              (w-dpv-tfu-prz-ele)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione stringa letta      *
      *                      *-----------------------------------------*
           go to     ctl-tfu-prz-120.
       ctl-tfu-prz-999.
           exit.

      *    *===========================================================*
      *    * Determinazione prezzo netto                               *
      *    *                                                           *
      *    * Input  : w-cal-prz-net-som     = sconto o maggiorazione   *
      *    *          w-cal-prz-net-prz     = prezzo lordo             *
      *    *          w-cal-imp-som-psc (n) = sconti                   *
      *    *                                                           *
      *    * Output : w-cal-prz-net-prz     = prezzo netto             *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-prz-net-000.
      *              *-------------------------------------------------*
      *              * Se valore in input a zero : uscita              *
      *              *-------------------------------------------------*
           if        w-cal-prz-net-prz    =    zero
                     go to cal-prz-net-999.
       cal-prz-net-050.
      *              *-------------------------------------------------*
      *              * Se percentuali di sconto tutte a zero, prezzo   *
      *              * netto pari al valore in input : uscita          *
      *              *-------------------------------------------------*
           if        w-cal-prz-net-psc (1)
                                          =    zero and
                     w-cal-prz-net-psc (2)
                                          =    zero and
                     w-cal-prz-net-psc (3)
                                          =    zero and
                     w-cal-prz-net-psc (4)
                                          =    zero and
                     w-cal-prz-net-psc (5)
                                          =    zero
                     go to cal-prz-net-999.
       cal-prz-net-100.
      *              *-------------------------------------------------*
      *              * Ciclo per sconti o maggiorazioni                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-cal-prz-net-ctr      .
       cal-prz-net-200.
           add       1                    to   w-cal-prz-net-ctr      .
           if        w-cal-prz-net-ctr    >    5
                     go to cal-prz-net-800.
      *                  *---------------------------------------------*
      *                  * Richiamo routine di calcolo                 *
      *                  *---------------------------------------------*
           move      w-cal-prz-net-som    to   w-cal-imp-som-som      .
           move      w-cal-prz-net-prz    to   w-cal-imp-som-iml      .
           move      w-cal-prz-net-psc
                    (w-cal-prz-net-ctr)   to   w-cal-imp-som-psc      .
           perform   cal-imp-som-000      thru cal-imp-som-999        .
           move      w-cal-imp-som-imn    to   w-cal-prz-net-prz      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cal-prz-net-200.
       cal-prz-net-800.
       cal-prz-net-999.
           exit.

      *    *===========================================================*
      *    * Routine di calcolo importo scontato o maggiorato          *
      *    *                                                           *
      *    * Input  : w-cal-imp-som-som = sconto o maggiorazione       *
      *    *          w-cal-imp-som-iml = importo lordo da scontare    *
      *    *          w-cal-imp-som-psc = percentuale di sconto        *
      *    *                                                           *
      *    * Output : w-cal-imp-som-imn = importo netto scontato       *
      *    *          w-cal-imp-som-ams = ammontare dello sconto       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       cal-imp-som-000.
      *              *-------------------------------------------------*
      *              * Se percentuale di sconto a zero : uscita        *
      *              *-------------------------------------------------*
           if        w-cal-imp-som-psc    =    zero
                     move  w-cal-imp-som-iml
                                          to   w-cal-imp-som-imn
                     move  zero           to   w-cal-imp-som-ams
                     go to cal-imp-som-999.
       cal-imp-som-100.
      *              *-------------------------------------------------*
      *              * Sconto o maggiorazione                          *
      *              *-------------------------------------------------*
           if        w-cal-imp-som-som    =    "-"
                     subtract  100,0      from w-cal-imp-som-psc
                                        giving w-cal-imp-som-w01
           else      add       100,0      to   w-cal-imp-som-psc
                                        giving w-cal-imp-som-w01      .
      *
           multiply  w-cal-imp-som-iml    by   w-cal-imp-som-w01
                                        giving w-cal-imp-som-w02      .
           divide    100                  into w-cal-imp-som-w02
                                        giving w-cal-imp-som-imn
                                               rounded                .
           subtract  w-cal-imp-som-imn    from w-cal-imp-som-iml
                                        giving w-cal-imp-som-ams      .
       cal-imp-som-999.
           exit.

      *    *===========================================================*
      *    * Determinazione eco-contributo RAEE                        *
      *    *-----------------------------------------------------------*
       ecr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori in output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-prz-ven-prz-ven      .
       ecr-100.
      *              *-------------------------------------------------*
      *              * Start su file [lst]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PROLST    "         to   f-key                  .
           move      11                   to   rf-lst-tip-rec         .
           move      d-prz-ven-num-pro    to   rf-lst-num-pro         .
           move      spaces               to   rf-lst-sgl-vlt         .
           move      spaces               to   rf-lst-cod-lst         .
           move      zero                 to   rf-lst-cod-cli         .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ecr-900.
       ecr-200.
      *              *-------------------------------------------------*
      *              * Next su [lst]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ecr-900.
       ecr-300.
      *              *-------------------------------------------------*
      *              * Max su [lst]                                    *
      *              *-------------------------------------------------*
           if        rf-lst-tip-rec       not  = 11                or
                     rf-lst-num-pro       not  = d-prz-ven-num-pro
                     go to ecr-900.
       ecr-400.
      *              *-------------------------------------------------*
      *              * Sel su [lst]                                    *
      *              *-------------------------------------------------*
       ecr-600.
      *              *-------------------------------------------------*
      *              * Determinazione prezzo                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Moltiplicazione per quantita' riferimento   *
      *                  *---------------------------------------------*
           multiply  rf-lst-qta-rif       by   rf-lst-prz-lst
                                        giving w-dpv-prz-wrk          .
      *                  *---------------------------------------------*
      *                  * Incremento                                  *
      *                  *---------------------------------------------*
           add       w-dpv-prz-wrk        to   d-prz-ven-prz-ven      .
       ecr-800.
      *              *-------------------------------------------------*
      *              * Riciclo a record [lst] successivo               *
      *              *-------------------------------------------------*
           go to     ecr-200.
       ecr-900.
      *              *-------------------------------------------------*
      *              * Abbattimento di 2 decimali prezzo               *
      *              *-------------------------------------------------*
           divide    100                  into  d-prz-ven-prz-ven     .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ecr-999.
       ecr-999.
           exit.

