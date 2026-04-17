       Identification Division.
       Program-Id.                                 sirbfow0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    sirbfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/06/23    *
      *                       Ultima revisione:    NdK del 23/09/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni di scrittura [bfo]               *
      *                                                                *
      *                    SIRI                                        *
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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *        * [hbc]                                                 *
      *        *-------------------------------------------------------*
           copy      "sir/agb/fls/rec/rfhbc"                          .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-tip-ope              pic  x(02)                  .
           05  w-exe-prt-bfo              pic  x(11)                  .
           05  w-exe-prr-bfo              pic  x(05)                  .
           05  w-exe-prt-ods              pic  x(11)                  .
           05  w-exe-qta-let              pic  x(12)                  .
           05  w-exe-qtv-bfo              pic  x(12)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-klb-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  x(07)                  .
           05  w-exe-bcd-el1              pic  x(80)                  .
           05  w-exe-bcd-el2              pic  x(80)                  .
           05  w-exe-bcd-el3              pic  x(80)                  .
           05  w-exe-bcd-el4              pic  x(80)                  .
           05  w-exe-bcd-el5              pic  x(80)                  .
           05  w-exe-bcd-el6              pic  x(80)                  .
           05  w-exe-sgl-bnc              pic  x(10)                  .
           05  w-exe-ann-let              pic  x(256)                 .
           05  w-exe-fil-nam              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-fld  occurs 20   pic  x(90)                  .
           05  w-exe-prm-ctr              pic  9(02)                  .
           05  w-exe-prm-max              pic  9(02) value 20         .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-exe-qta-cnv              pic  9(10)v9(03)            .
           05  w-exe-qtv-cnv              pic  9(10)v9(03)            .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-bft.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-nds      pic  9(11)                  .
               10  w-slc-num-bft-nds-r redefines
                   w-slc-num-bft-nds.
                   15  w-slc-num-bft-nsa  pic  9(03)                  .
                   15  w-slc-num-bft-ndp  pic  9(02)                  .
                   15  w-slc-num-bft-npg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Valori in input                                   *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-prt      pic  9(11)                  .
               10  w-slc-num-bft-pro      pic  9(07)                  .
               10  w-slc-num-bft-spl      pic  9(05)                  .
               10  w-slc-num-ods-prt      pic  9(11)                  .

      *    *===========================================================*
      *    * Work area per compattamenti                               *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Per compattamento sub-righe                           *
      *        *-------------------------------------------------------*
           05  w-cmp-sub-rig.
      *            *---------------------------------------------------*
      *            * Indice di scansione                               *
      *            *---------------------------------------------------*
               10  w-cmp-sub-rig-inx      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio intero record                         *
      *            *---------------------------------------------------*
               10  w-cmp-sub-rig-rec.
                   15  filler occurs 1024 pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione righe prodotto                     *
      *        *-------------------------------------------------------*
           05  w-det-rig-pro.
      *            *---------------------------------------------------*
      *            * Contatori righe                                   *
      *            *---------------------------------------------------*
               10  w-det-rig-pro-ctr      pic  9(03)                  .
               10  w-det-rig-pro-c01      pic  9(03)                  .
               10  w-det-rig-pro-max      pic  9(03) value 999        .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zub]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zub.
               10  w-let-arc-zub-flg      pic  x(01)                  .
               10  w-let-arc-zub-dpz      pic  9(02)                  .
               10  w-let-arc-zub-cod      pic  x(07)                  .
               10  w-let-arc-zub-des      pic  x(30)                  .
               10  w-let-arc-zub-inx      pic  9(07)                  .

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
           move      spaces               to   w-exe-tip-ope          .
           move      spaces               to   w-exe-prt-bfo          .
           move      spaces               to   w-exe-prr-bfo          .
           move      spaces               to   w-exe-num-pro          .
           move      spaces               to   w-exe-qta-let          .
           move      spaces               to   w-exe-qtv-bfo          .
           move      spaces               to   w-exe-sgl-bnc          .
           move      spaces               to   w-exe-ann-let          .
           move      spaces               to   w-exe-bcd-el1          .
           move      spaces               to   w-exe-bcd-el2          .
           move      spaces               to   w-exe-bcd-el3          .
           move      spaces               to   w-exe-bcd-el4          .
           move      spaces               to   w-exe-bcd-el5          .
           move      spaces               to   w-exe-bcd-el6          .
           move      spaces               to   w-exe-fil-nam          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      14                   to   w-cgi-str-num          .
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
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-prt      .
      *                  *---------------------------------------------*
      *                  *              !!! IMPORTANTE !!!             *
      *                  *                                             *
      *                  * N.B.: la prima cifra, quella del secolo, e' *
      *                  *       pari a quella reale del protocollo se *
      *                  *       si tratta di ordini di spedizione,    *
      *                  *       mentre inizia per '9' se si tratta di *
      *                  *       carichi 'bfo'                         *
      *                  *                                             *
      *                  * '12401000001' = Protocollo 'ods'            *
      *                  *                                             *
      *                  * '92401000001' = Protocollo 'bfo'            *
      *                  *---------------------------------------------*
           add       80000000000          to   w-slc-num-bft-prt      .   
      *                  *---------------------------------------------*
      *                  * Protocollo ordine di spedizione             *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-ods        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-ods-prt      .
      *                  *---------------------------------------------*
      *                  * Codice numerico prodotto                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      07                   to   p-car                  .
           move      w-exe-num-pro        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-pro      .
      *                  *---------------------------------------------*
      *                  * Progressivo sub-riga                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      03                   to   p-car                  .
           move      w-exe-prr-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-spl      .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qta-let        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qta-cnv          .
      *                  *---------------------------------------------*
      *                  * Quantita' verificata                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qtv-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qtv-cnv          .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
      *                  *---------------------------------------------*
      *                  * Sigla bancale                               *
      *                  *---------------------------------------------*
           move      w-exe-sgl-bnc        to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-sgl-bnc          .
       ext-prm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-999.
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [hbc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [hbc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-exe-tip-ope        =    "WR"
                     perform exe-cph-wrc-000
                                          thru exe-cph-wrc-999
           else if   w-exe-tip-ope        =    "WI"
                     perform exe-cph-wic-000
                                          thru exe-cph-wic-999
           else if   w-exe-tip-ope        =    "DR"
                     perform exe-cph-drc-000
                                          thru exe-cph-drc-999
           else if   w-exe-tip-ope        =    "DA"
                     perform exe-cph-dac-000
                                          thru exe-cph-dac-999
           else if   w-exe-tip-ope        =    "AB"
                     perform exe-cph-abb-000
                                          thru exe-cph-abb-999
           else      perform exe-cph-err-000
                                          thru exe-cph-err-999        .
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
      *    * Subroutine di scrittura riga                              *
      *    *-----------------------------------------------------------*
       exe-cph-wrc-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo e progressivo riga       *
      *                  *---------------------------------------------*
           if        w-slc-num-bft-prt    not  = zero and
                     w-slc-num-bft-pro    not  = zero and
                     w-slc-num-bft-spl    not  = zero
                     go to exe-cph-wrc-050.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wrc-900.
       exe-cph-wrc-050.
      *              *-------------------------------------------------*
      *              * Assemblaggio dati barcode                       *
      *              *-------------------------------------------------*
       exe-cph-wrc-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [hbc]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "PRTPRO    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-slc-num-bft-spl    to   rf-hbc-num-spl         .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wrc-600.
       exe-cph-wrc-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-hbc-ide-dat         .
           move      "web"                to   rf-hbc-ide-ute         .
           move      "sirbfo"             to   rf-hbc-ide-fas         .
           move      w-exe-qta-cnv        to   rf-hbc-qta-spl         .
      *
           move      w-exe-bcd-el1        to   rf-hbc-sez-bcd (1)     .
           move      w-exe-bcd-el2        to   rf-hbc-sez-bcd (2)     .
           move      w-exe-bcd-el3        to   rf-hbc-sez-bcd (3)     .
           move      w-exe-bcd-el4        to   rf-hbc-sez-bcd (4)     .
           move      w-exe-bcd-el5        to   rf-hbc-sez-bcd (5)     .
           move      w-exe-bcd-el6        to   rf-hbc-sez-bcd (6)     .
      *
           move      w-exe-sgl-bnc        to   rf-hbc-sgl-bnc         .
           move      w-exe-ann-let        to   rf-hbc-ann-spl         .
           move      spaces               to   rf-hbc-flg-del         .
           move      spaces               to   rf-hbc-alx-exp         .
       exe-cph-wrc-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [hbc]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-hbc-000      thru upd-rec-hbc-999        .
       exe-cph-wrc-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wrc-900.
       exe-cph-wrc-600.
      *              *-------------------------------------------------*
      *              * Se record [hbc] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [hbc] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-hbc-000      thru nor-rec-hbc-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wrc-100.
       exe-cph-wrc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wrc-999.
       exe-cph-wrc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura collegamento a immagine           *
      *    *-----------------------------------------------------------*
       exe-cph-wic-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo e progressivo riga       *
      *                  *---------------------------------------------*
           if        w-slc-num-bft-prt    not  = zero and
                     w-slc-num-bft-spl    not  = zero and
                     w-exe-fil-nam        not  = spaces
                     go to exe-cph-wic-050.
      *                  *---------------------------------------------*
      *                  * Preparazione messaggio                      *
      *                  *---------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wic-900.
       exe-cph-wic-050.
      *              *-------------------------------------------------*
      *              * Assemblaggio ___                                *
      *              *-------------------------------------------------*
       exe-cph-wic-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [hbc]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       exe-cph-wic-150.
      *              *-------------------------------------------------*
      *              * Ottenimento record [hbc]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "PRTPRO    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      zero                 to   rf-hbc-num-pro         .
           move      w-slc-num-bft-spl    to   rf-hbc-num-spl         .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wic-600.
       exe-cph-wic-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-hbc-ide-dat         .
           move      "web"                to   rf-hbc-ide-ute         .
           move      "sirbfo"             to   rf-hbc-ide-fas         .
           move      w-exe-fil-nam        to   rf-hbc-pth-img         .
           move      spaces               to   rf-hbc-alx-exp         .
       exe-cph-wic-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [hbc]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-hbc-000      thru upd-rec-hbc-999        .
       exe-cph-wic-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wic-900.
       exe-cph-wic-600.
      *              *-------------------------------------------------*
      *              * Se record [hbc] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [hbc] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-hbc-000      thru nor-rec-hbc-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wic-100.
       exe-cph-wic-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wic-999.
       exe-cph-wic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di cancellazione riga                          *
      *    *-----------------------------------------------------------*
       exe-cph-drc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *              *-------------------------------------------------*
      *              * Composizione chiave [hbc]                       *
      *              *-------------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-slc-num-bft-spl    to   rf-hbc-num-spl         .
      *              *-------------------------------------------------*
      *              * Delete record [hbc]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       exe-cph-drc-200.
      *              *-------------------------------------------------*
      *              * Subroutine di ricompattamento righe scansione   *
      *              *-------------------------------------------------*
           perform   exe-cph-drc-cmp-000  thru exe-cph-drc-cmp-999    .
       exe-cph-drc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-drc-999.
       exe-cph-drc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di cancellazione di tutte le righe  prodotto   *
      *    *-----------------------------------------------------------*
       exe-cph-dac-000.
      *              *-------------------------------------------------*
      *              * Controlli preliminari                           *
      *              *-------------------------------------------------*
           if        w-slc-num-bft-prt    =    zero
                     go to exe-cph-dac-900.
           if        w-slc-num-bft-pro    =    zero
                     go to exe-cph-dac-900.
       exe-cph-dac-100.
      *              *-------------------------------------------------*
      *              * Determinazione righe prodotto                   *
      *              *-------------------------------------------------*
           perform   det-rig-pro-000      thru det-rig-pro-999        .
           if        w-det-rig-pro-ctr    =    zero
                     go to exe-cph-dac-900.
       exe-cph-dac-150.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione righe determinate            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rig-pro-c01      .
       exe-cph-dac-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-rig-pro-c01      .
           if        w-det-rig-pro-c01    >    w-det-rig-pro-ctr
                     go to exe-cph-dac-900.
           if        w-det-rig-pro-c01    >    w-det-rig-pro-max
                     go to exe-cph-dac-900.
       exe-cph-dac-400.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *              *-------------------------------------------------*
      *              * Composizione chiave [hbc]                       *
      *              *-------------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-det-rig-pro-c01    to   rf-hbc-num-spl         .
      *              *-------------------------------------------------*
      *              * Delete record [hbc]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       exe-cph-dac-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-dac-200.
       exe-cph-dac-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-dac-999.
       exe-cph-dac-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di abbinamento riga carico a nuova riga lista  *
      *    *-----------------------------------------------------------*
       exe-cph-abb-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su parametri in input                  *
      *                  *---------------------------------------------*
           if        w-slc-num-bft-prt    not  = zero and
                     w-slc-num-ods-prt    not  = zero and
                     w-slc-num-bft-pro    not  = zero and
                     w-slc-num-bft-spl    not  = zero
                     go to exe-cph-abb-100.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-abb-900.
       exe-cph-abb-100.
      *              *-------------------------------------------------*
      *              * Lettura record relativo a BFO                   *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRTPRO    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-slc-num-bft-spl    to   rf-hbc-num-spl         .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-abb-900.
       exe-cph-abb-300.
      *              *-------------------------------------------------*
      *              * Identificativi                                  *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-hbc-ide-dat         .
           move      "web"                to   rf-hbc-ide-ute         .
           move      "sirbfo"             to   rf-hbc-ide-fas         .
      *              *-------------------------------------------------*
      *              * Nuovo protocollo                                *
      *              *-------------------------------------------------*
           move      w-slc-num-ods-prt    to   rf-hbc-num-prt         .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       exe-cph-abb-500.
      *              *-------------------------------------------------*
      *              * Ottenimento record [hbc] relativo al BFO        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "PRTPRO    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-slc-num-bft-spl    to   rf-hbc-num-spl         .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-abb-900.
       exe-cph-wrc-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-hbc-ide-dat         .
           move      "web"                to   rf-hbc-ide-ute         .
           move      "sirbfo"             to   rf-hbc-ide-fas         .
           move      "S"                  to   rf-hbc-flg-del         .
           move      w-slc-num-ods-prt    to   rf-hbc-prt-ods         .
       exe-cph-wrc-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [hbc]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-hbc-000      thru upd-rec-hbc-999        .
       exe-cph-abb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-abb-999.
       exe-cph-abb-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Determinazione righe  prodotto                            *
      *    *-----------------------------------------------------------*
       det-rig-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe prodotto        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rig-pro-ctr      .
       det-rig-pro-100.
      *              *-------------------------------------------------*
      *              * Start su [hbc]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "PRTPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      zero                 to   rf-hbc-num-spl         .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a riciclo              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-rig-pro-900.
       det-rig-pro-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [hbc]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a riciclo                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-rig-pro-900.
       det-rig-pro-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a riciclo                 *
      *              *-------------------------------------------------*
           if        rf-hbc-num-prt       not  = w-slc-num-bft-prt
                     go to  det-rig-pro-900.
           if        rf-hbc-num-pro       not  = w-slc-num-bft-pro
                     go to  det-rig-pro-900.
       det-rig-pro-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [hbc]                              *
      *              *-------------------------------------------------*
       det-rig-pro-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-det-rig-pro-ctr      .
       det-rig-pro-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-rig-pro-200.
       det-rig-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-rig-pro-999.
       det-rig-pro-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di cancellazione riga                          *
      *    *                                                           *
      *    * Sub-subroutine di compattamento sub-righe successive      *
      *    *-----------------------------------------------------------*
       exe-cph-drc-cmp-000.
      *              *-------------------------------------------------*
      *              * Preparazione indice di scansione                *
      *              *-------------------------------------------------*
           move      w-slc-num-bft-spl    to   w-cmp-sub-rig-inx      .
       exe-cph-drc-cmp-200.
           add       1                    to   w-cmp-sub-rig-inx      .
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRTPRO    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-cmp-sub-rig-inx    to   rf-hbc-num-spl         .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-drc-cmp-900.
       exe-cph-drc-cmp-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record trovato                  *
      *              *-------------------------------------------------*
           move      rf-hbc               to   w-cmp-sub-rig-rec      .
       exe-cph-drc-cmp-400.
      *              *-------------------------------------------------*
      *              * Delete record [hbc]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       exe-cph-drc-cmp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *              *-------------------------------------------------*
      *              * Recupero record salvato                         *
      *              *-------------------------------------------------*
           move      w-cmp-sub-rig-rec    to   rf-hbc                 .
      *              *-------------------------------------------------*
      *              * Campi chiave                                    *
      *              *-------------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-cmp-sub-rig-inx    to   rf-hbc-num-spl         .
           subtract  1                    from rf-hbc-num-spl         .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       exe-cph-drc-cmp-600.
      *              *-------------------------------------------------*
      *              * Riciclo a record successivo                     *
      *              *-------------------------------------------------*
           go to     exe-cph-drc-cmp-200.
       exe-cph-drc-cmp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-drc-cmp-999.
       exe-cph-drc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per messaggio di aggiornamento                 *
      *    *-----------------------------------------------------------*
       exe-cph-agg-000.
      *              *-------------------------------------------------*
      *              * ATTUALMENTE INIBITO                             *
      *              *-------------------------------------------------*
           go to     exe-cph-agg-900.
       exe-cph-agg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-agg-999.
       exe-cph-agg-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di uscita per errore                           *
      *    *-----------------------------------------------------------*
       exe-cph-err-000.
      *              *-------------------------------------------------*
      *              * Preparazione messaggio                          *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           
           move      "<h1> Tipo operazione :"
                                          to   w-all-str-cat (1)      .
           move      w-exe-tip-ope        to   w-all-str-cat (2)      .
           move      "non riconosciuto </h1>"
                                          to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-prm-msg          .
      *              *-------------------------------------------------*
      *              * Messaggio in uscita                             *
      *              *-------------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   w-exe-prm-msg                                    .
           display   "</div>"                                         .
       exe-cph-err-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-err-999.
       exe-cph-err-999.
           exit.

      *    *===========================================================*
      *    * Routine di scrittura record [hbc] normalizzato            *
      *    *-----------------------------------------------------------*
       nor-rec-hbc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *              *-------------------------------------------------*
      *              * Campi chiave                                    *
      *              *-------------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-hbc-num-prt         .
           move      w-slc-num-bft-pro    to   rf-hbc-num-pro         .
           move      w-slc-num-bft-spl    to   rf-hbc-num-spl         .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       nor-rec-hbc-999.
           exit.

      *    *===========================================================*
      *    * Routine di update record [hbc]                            *
      *    *-----------------------------------------------------------*
       upd-rec-hbc-000.
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [hbc]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhbc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hbc                 .
       upd-rec-hbc-999.
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
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "tip_ope"
                     move  w-all-str-cat (2)
                                          to   w-exe-tip-ope
      *              *-------------------------------------------------*
      *              * Protocollo BFO                                  *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "prt_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-bfo
      *              *-------------------------------------------------*
      *              * Protocollo ODS                                  *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "prt_ods"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-ods
      *              *-------------------------------------------------*
      *              * Progressivo sub-riga                            *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "prr_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-prr-bfo
      *              *-------------------------------------------------*
      *              * Codice numerico prodotto                        *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "num_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-num-pro
      *              *-------------------------------------------------*
      *              * Quantita' in riga                               *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "qta_let"
                     move  w-all-str-cat (2)
                                          to   w-exe-qta-let
      *              *-------------------------------------------------*
      *              * Numero bancale                                  *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "num_bnc"
                     move  w-all-str-cat (2)
                                          to   w-exe-sgl-bnc
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "ann_let"
                     move  w-all-str-cat (2)
                                          to   w-exe-ann-let
      *              *-------------------------------------------------*
      *              * Dati barcode                                    *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "bcd_el1"
                     move  w-all-str-cat (2)
                                          to   w-exe-bcd-el1
           else if   w-all-str-cat (1)    =    "bcd_el2"
                     move  w-all-str-cat (2)
                                          to   w-exe-bcd-el2
           else if   w-all-str-cat (1)    =    "bcd_el3"
                     move  w-all-str-cat (2)
                                          to   w-exe-bcd-el3
           else if   w-all-str-cat (1)    =    "bcd_el4"
                     move  w-all-str-cat (2)
                                          to   w-exe-bcd-el4
           else if   w-all-str-cat (1)    =    "bcd_el5"
                     move  w-all-str-cat (2)
                                          to   w-exe-bcd-el5
           else if   w-all-str-cat (1)    =    "bcd_el6"
                     move  w-all-str-cat (2)
                                          to   w-exe-bcd-el6
      *              *-------------------------------------------------*
      *              * Filename                                        *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "fil_nam"
                     move  w-all-str-cat (2)
                                          to   w-exe-fil-nam          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
