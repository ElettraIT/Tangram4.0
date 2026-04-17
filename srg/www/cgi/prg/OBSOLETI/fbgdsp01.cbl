       Identification Division.
       Program-Id.                                 fbgdsp01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    fbgpro              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/07/03    *
      *                       Ultima revisione:    NdK del 27/09/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Lista prodotti per Browser Internet - ZATTI *
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      
      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di visualizzazione testata                       *
      *        *-------------------------------------------------------*
           05  w-exe-flg-tst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-alf-pro              pic  x(14)                  .
           05  w-exe-alf-prv              pic  x(14)                  .
           05  w-exe-nxt-pro              pic  x(14)                  .
           05  w-exe-pth-img              pic  x(18)                  .
           05  w-exe-hid-cod              pic  x(22)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-x01              pic  x(80)                  .
           05  w-exe-prm-x02              pic  x(80)                  .
           05  w-exe-prm-x03              pic  x(80)                  .
           05  w-exe-prm-xxx              pic  x(80)                  .
           05  w-exe-prm-001.
               10  w-exe-prm-p01          pic  x(06)                  .
               10  w-exe-prm-v01          pic  x(14)                  .
           05  w-exe-prm-002.
               10  w-exe-prm-p02          pic  x(11)                  .
               10  w-exe-prm-v02          pic  x(40)                  .
           05  w-exe-prm-003.
               10  w-exe-prm-p03          pic  x(01)                  .
               10  w-exe-prm-v03          pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione dati                       *
      *        *-------------------------------------------------------*
           05  w-exe-gia-pro              pic  x(12)                  .
           05  w-exe-qta-orc              pic  x(12)                  .
           05  w-exe-qta-orf              pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione dati tabella               *
      *        *-------------------------------------------------------*
           05  w-exe-pmt-001              pic  x(40)                  .
           05  w-exe-val-001              pic  x(40)                  .
           05  w-exe-flg-001              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det : quantita' in giacenza fisica           *
      *        *-------------------------------------------------------*
           05  w-det-qta-gcf.
      *            *---------------------------------------------------*
      *            * Codice dipendenza                         [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-gcf-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto                           [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-gcf-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data disponibilita'                       [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-gcf-dat      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Quantita' in giacenza                    [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-gcf-qta      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Work per Det : quantita' in spedizione e in ordine da *
      *        * clienti                                               *
      *        *-------------------------------------------------------*
           05  w-det-qta-orc.
      *            *---------------------------------------------------*
      *            * Codice dipendenza                         [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto                           [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data disponibilita'                       [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-dat      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione tipo riga         [work] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-wtr.
                   15  w-det-qta-orc-wtp  pic  x(01)                  .
                   15  w-det-qta-orc-wtf  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Quantita' in ordine da clienti           [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-qoc      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' primo ordine cliente           [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-qpo      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Data consegna primo ordine cliente       [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-dpo      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Quantita' in spedizione                  [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orc-qis      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Work per Det : quantita' in ordine a fornitori        *
      *        *-------------------------------------------------------*
           05  w-det-qta-orf.
      *            *---------------------------------------------------*
      *            * Codice dipendenza                         [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto                           [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data disponibilita'                       [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-dat      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione tipo riga         [work] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-wtr.
                   15  w-det-qta-orf-wtp  pic  x(01)                  .
                   15  w-det-qta-orf-wtf  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Quantita' in ordine a fornitori          [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-qof      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' primo ordine fornitore         [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-qpo      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Data consegna primo ordine fornitore     [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-dpo      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-qta-orf-dri      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Work per Det : quantita' in giacenza magazzini 1 e 2  *
      *        *-------------------------------------------------------*
           05  w-det-qta-1e2.
      *            *---------------------------------------------------*
      *            * Quantita' in giacenza                             *
      *            *---------------------------------------------------*
               10  w-det-qta-1e2-qta      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
      *        *-------------------------------------------------------*
      *        * Work per uppercase                                    *
      *        *-------------------------------------------------------*
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per padding campi alfanumerici con 'z'           *
      *        *-------------------------------------------------------*
           05  w-pad-zzz.
               10  w-pad-zzz-alf.
                   15  w-pad-zzz-alf-chr
                                   occurs 20
                                          pic  x(01)                  .
               10  w-pad-zzz-ctr          pic  9(02)                  .


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
      *              * Ciclo di lettura e preparazione xml             *
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
      *              * Data di sistema                                 *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * CORRETTIVO PROVVISORIO                          *
      *              *                                                 *
      *              * ___                                             *
      *              *-------------------------------------------------*
______*    add       1000000              to   w-exe-dat-exe          .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-prm-001          .
           move      spaces               to   w-exe-prm-002          .
           move      spaces               to   w-exe-prm-003          .
           move      spaces               to   w-exe-prm-x01          .
           move      spaces               to   w-exe-prm-x02          .
           move      spaces               to   w-exe-prm-x03          .
           move      spaces               to   w-exe-prm-xxx          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione primi 2 parametri                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
           unstring  o-pat
                                delimited by "&"
                                          into w-exe-prm-x01
                                    count in   w-exe-ctr-001          .
           add       2                    to   w-exe-ctr-001          .
           unstring  o-pat                into w-exe-prm-x02
                                  with pointer w-exe-ctr-001          .
      *              *-------------------------------------------------*
      *              * Estrazione 1. parametro - componenti            *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
           unstring  w-exe-prm-x01
                                delimited by "="
                                          into w-exe-prm-p01
                                    count in   w-exe-ctr-001          .
           add       2                    to   w-exe-ctr-001          .
           unstring  w-exe-prm-x01        into w-exe-prm-v01
                                  with pointer w-exe-ctr-001          .
      *              *-------------------------------------------------*
      *              * Estrazione 2. parametro - componenti            *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
           unstring  w-exe-prm-x02
                                delimited by "="
                                          into w-exe-prm-p02
                                    count in   w-exe-ctr-001          .
           add       2                    to   w-exe-ctr-001          .
           unstring  w-exe-prm-x02        into w-exe-prm-v02
                                  with pointer w-exe-ctr-001          .
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      w-exe-prm-v01        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   w-exe-alf-pro          .
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      w-exe-prm-v02        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   w-exe-alf-prv          .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * [ofr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magazz.  *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-sld-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da      *
      *              * evadere riga ordine cliente                     *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da      *
      *              * evadere riga ordine fornitore                   *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-qev-rof-tip-ope      .
           move      "pgm/orf/prg/obj/dqevrof0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-rof
                                               rf-ofr                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * [ofr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione saldo di magazz. *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-sld-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-sld-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
           if        d-sld-mag-exi-sts    not  = spaces
                     go to cls-fls-700.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       cls-fls-700.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da     *
      *              * evadere riga ordine cliente                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
           if        d-qev-roc-exi-sts    not  = spaces
                     go to cls-fls-750.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       cls-fls-750.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da     *
      *              * evadere riga ordine fornitore                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-qev-rof-tip-ope      .
           move      "pgm/orf/prg/obj/dqevrof0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-rof
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-qev-rof-tip-ope      .
           move      "pgm/orf/prg/obj/dqevrof0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-rof
                                               rf-ofr                 .
           if        d-qev-rof-exi-sts    not  = spaces
                     go to cls-fls-800.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/orf/prg/obj/dqevrof0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       cls-fls-800.
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione xml                       *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Eventuale determinazione codice successivo      *
      *              *-------------------------------------------------*
           if        w-exe-alf-pro        not  = spaces
                     go to exe-cph-050.
           perform   nxt-pro-000          thru nxt-pro-999            .
           move      w-exe-nxt-pro        to   w-exe-alf-pro          .
       exe-cph-050.
      *              *-------------------------------------------------*
      *              * Flag di emissione testata documento             *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-tst          .
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Start su file [dcp]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-exe-alf-pro        to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-600.
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Next su [dcp]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Next errata : ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-600.
       exe-cph-300.
      *              *-------------------------------------------------*
      *              * Max su [dcp]                                    *
      *              *-------------------------------------------------*
           if        rf-dcp-alf-pro       not  = w-exe-alf-pro
                     go to exe-cph-600.
       exe-cph-400.
      *              *-------------------------------------------------*
      *              * Sel su [dcp]                                    *
      *              *-------------------------------------------------*
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Raccolta dati supplementari                     *
      *              *-------------------------------------------------*
           perform   exe-rds-000          thru exe-rds-999            .
      *              *-------------------------------------------------*
      *              * Emissione corpo documento                       *
      *              *-------------------------------------------------*
           perform   emi-cor-000          thru emi-cor-999            .
      *              *-------------------------------------------------*
      *              * Ad emissione piede                              *
      *              *-------------------------------------------------*
           go to     exe-cph-800.
       exe-cph-600.
      *              *-------------------------------------------------*
      *              * Start su file [dcp] con bar-code                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "KLBPRO    "         to   f-key                  .
           move      w-exe-alf-pro        to   rf-dcp-klb-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-800.
       exe-cph-620.
      *              *-------------------------------------------------*
      *              * Next su [dcp]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Next errata : ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-800.
       exe-cph-630.
      *              *-------------------------------------------------*
      *              * Max su [dcp]                                    *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       not  = w-exe-alf-pro
                     go to exe-cph-800.
       exe-cph-640.
      *              *-------------------------------------------------*
      *              * Sel su [dcp]                                    *
      *              *-------------------------------------------------*
       exe-cph-650.
      *              *-------------------------------------------------*
      *              * Ritaratura codice richiesto                     *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-exe-alf-pro          .
      *              *-------------------------------------------------*
      *              * Raccolta dati supplementari                     *
      *              *-------------------------------------------------*
           perform   exe-rds-000          thru exe-rds-999            .
      *              *-------------------------------------------------*
      *              * Emissione corpo documento                       *
      *              *-------------------------------------------------*
           perform   emi-cor-000          thru emi-cor-999            .
      *              *-------------------------------------------------*
      *              * Ad emissione piede                              *
      *              *-------------------------------------------------*
           go to     exe-cph-800.
       exe-cph-800.
      *              *-------------------------------------------------*
      *              * Emissione piede documento                       *
      *              *-------------------------------------------------*
           perform   emi-pie-000          thru emi-pie-999            .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Determinazione eventuale codice prodotto successivo       *
      *    *-----------------------------------------------------------*
       nxt-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-nxt-pro          .
       nxt-pro-100.
      *              *-------------------------------------------------*
      *              * Start su file [dcp]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-exe-alf-prv        to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to nxt-pro-900.
       nxt-pro-200.
      *              *-------------------------------------------------*
      *              * Next su [dcp]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Next errata : ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to nxt-pro-900.
       nxt-pro-250.
      *              *-------------------------------------------------*
      *              * Next su [dcp]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Next errata : ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to nxt-pro-900.
       nxt-pro-300.
      *              *-------------------------------------------------*
      *              * Max su [dcp]                                    *
      *              *-------------------------------------------------*
       nxt-pro-400.
      *              *-------------------------------------------------*
      *              * Sel su [dcp]                                    *
      *              *-------------------------------------------------*
       nxt-pro-500.
      *              *-------------------------------------------------*
      *              * In campo di destinazione                        *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-exe-nxt-pro          .
       nxt-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     nxt-pro-999.
       nxt-pro-999.
           exit.

      *    *===========================================================*
      *    * Raccolta dati supplementari                               *
      *    *-----------------------------------------------------------*
       exe-rds-000.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' in giacenza fisica :   *
      *              * magazzino 1 (sede)                              *
      *              *-------------------------------------------------*
           move      01                   to   w-det-qta-gcf-dpz      .
           move      rf-dcp-num-pro       to   w-det-qta-gcf-cod      .
           move      w-exe-dat-exe        to   w-det-qta-gcf-dat      .
           perform   det-qta-gcf-000      thru det-qta-gcf-999        .
           move      w-det-qta-gcf-qta    to   w-det-qta-1e2-qta      .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' in giacenza fisica :   *
      *              * magazzino 2                                     *
      *              *-------------------------------------------------*
           move      02                   to   w-det-qta-gcf-dpz      .
           move      rf-dcp-num-pro       to   w-det-qta-gcf-cod      .
           move      w-exe-dat-exe        to   w-det-qta-gcf-dat      .
           perform   det-qta-gcf-000      thru det-qta-gcf-999        .
           add       w-det-qta-gcf-qta    to   w-det-qta-1e2-qta      .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' in spedizione e in or- *
      *              * dine da clienti                                 *
      *              *-------------------------------------------------*
           move      01                   to   w-det-qta-orc-dpz      .
           move      rf-dcp-num-pro       to   w-det-qta-orc-cod      .
           move      w-exe-dat-exe        to   w-det-qta-orc-dat      .
           perform   det-qta-orc-000      thru det-qta-orc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' ordinata a fornitori   *
      *              *-------------------------------------------------*
           move      01                   to   w-det-qta-orf-dpz      .
           move      rf-dcp-num-pro       to   w-det-qta-orf-cod      .
           move      w-exe-dat-exe        to   w-det-qta-orf-dat      .
           perform   det-qta-orf-000      thru det-qta-orf-999        .
       exe-rds-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-rds-999.
       exe-rds-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-tst-000.
      *              *-------------------------------------------------*
      *              * Flag di testata documento                       *
      *              *-------------------------------------------------*
           if        w-exe-flg-tst        not  = spaces
                     go to emi-tst-999.
      *
           move      "#"                  to   w-exe-flg-tst          .
       emi-tst-100.
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           display   "<?xml version=""1.0""?>"
                                          with no advancing           .
           display   ""                                               .
           display   "<root>"                                         .
       emi-tst-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *-----------------------------------------------------------*
       emi-cor-000.
      *              *-------------------------------------------------*
      *              * Emissione testata                               *
      *              *-------------------------------------------------*
           perform   emi-tst-000          thru emi-tst-999            .
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Premessa corpo                                  *
      *              *-------------------------------------------------*
       emi-cor-300.
      *              *-------------------------------------------------*
      *              * Trattamento giacenza                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing giacenza                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      w-det-qta-1e2-qta    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-gia-pro          .
      *                  *---------------------------------------------*
      *                  * Composizione xml                            *
      *                  *---------------------------------------------*
           move      "gia"                to   w-exe-pmt-001          .
           move      w-exe-gia-pro        to   w-exe-val-001          .
           perform   emi-rig-000          thru emi-rig-999            .
       emi-cor-400.
      *              *-------------------------------------------------*
      *              * Trattamento in ordine a clienti                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing quantita' in ordine                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      w-det-qta-orc-qoc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-qta-orc          .
      *                  *---------------------------------------------*
      *                  * Composizione xml                            *
      *                  *---------------------------------------------*
           move      "imp"                to   w-exe-pmt-001          .
           move      w-exe-qta-orc        to   w-exe-val-001          .
           perform   emi-rig-000          thru emi-rig-999            .
       emi-cor-600.
      *              *-------------------------------------------------*
      *              * Trattamento in ordine a fornitori               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing quantita' in ordine                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      w-det-qta-orf-qof    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-qta-orf          .
      *                  *---------------------------------------------*
      *                  * Composizione xml                            *
      *                  *---------------------------------------------*
           move      "ord"                to   w-exe-pmt-001          .
           move      w-exe-qta-orf        to   w-exe-val-001          .
           perform   emi-rig-000          thru emi-rig-999            .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Fine corpo                                      *
      *              *-------------------------------------------------*
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione singola riga tabella                            *
      *    *-----------------------------------------------------------*
       emi-rig-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari                        *
      *              *-------------------------------------------------*
       emi-rig-100.
      *              *-------------------------------------------------*
      *              * Composizione xml                                *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
           move      w-exe-pmt-001        to   w-all-str-cat (2)      .
           move      ">"                  to   w-all-str-cat (3)      .
           move      w-exe-val-001        to   w-all-str-cat (4)      .
           move      "</"                 to   w-all-str-cat (5)      .
           move      w-exe-pmt-001        to   w-all-str-cat (6)      .
           move      ">"                  to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
       emi-rig-300.
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       emi-rig-800.
      *              *-------------------------------------------------*
      *              * Fine elemento                                   *
      *              *-------------------------------------------------*
       emi-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-rig-999.
       emi-rig-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Chiusura xml                                    *
      *              *-------------------------------------------------*
           display   "</root>"                                        .
       emi-pie-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
           exit.

      *    *===========================================================*
      *    * Routine di Det : Giacenza fisica                          *
      *    *-----------------------------------------------------------*
       det-qta-gcf-000.
      *              *-------------------------------------------------*
      *              * Preparazione link-area                          *
      *              *-------------------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0000                 to   d-sld-mag-tip-sld      .
           move      w-det-qta-gcf-dat    to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      w-det-qta-gcf-dpz    to   d-sld-mag-cod-dpz      .
           move      01                   to   d-sld-mag-tip-mag      .
           move      w-det-qta-gcf-cod    to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "T"                  to   d-sld-mag-uot-dsl      .
           move      spaces               to   d-sld-mag-cod-dsl      .
      *              *-------------------------------------------------*
      *              * Richiamo del sottoprogramma                     *
      *              *-------------------------------------------------*
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
      *              *-------------------------------------------------*
      *              * Memorizzazione risultato                        *
      *              *-------------------------------------------------*
           move      d-sld-mag-sld-mag    to   w-det-qta-gcf-qta      .
       det-qta-gcf-999.
           exit.

      *    *===========================================================*
      *    * Routine di Det : quantita' in spedizione e in ordine da   *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
       det-qta-orc-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione valori in output               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-qta-orc-qoc      .
           move      zero                 to   w-det-qta-orc-qpo      .
           move      zero                 to   w-det-qta-orc-dpo      .
           move      zero                 to   w-det-qta-orc-qis      .
      *              *-------------------------------------------------*
      *              * Start su file [ocr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      w-det-qta-orc-dpz    to   rf-ocr-cod-dpz         .
           move      spaces               to   rf-ocr-flg-rch         .
           move      01                   to   rf-ocr-tip-mag         .
           move      w-det-qta-orc-cod    to   rf-ocr-num-pro         .
           move      zero                 to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qta-orc-999.
       det-qta-orc-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [ocr]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qta-orc-999.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ocr-cod-dpz       not  = w-det-qta-orc-dpz or
                     rf-ocr-flg-rch       not  = spaces            or
                     rf-ocr-tip-mag       not  = 01                or
                     rf-ocr-num-pro       not  = w-det-qta-orc-cod
                     go to det-qta-orc-999.
      *              *-------------------------------------------------*
      *              * Selezione sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo ordine                         *
      *                  *                                             *
      *                  * Non si considerano gli ordini pro-forma     *
      *                  *---------------------------------------------*
           if        rf-ocr-tip-ord       =    "P"
                     go to det-qta-orc-100.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-ocr-dat-doc       >    w-det-qta-orc-dat
                     go to det-qta-orc-100.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo riga                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo riga in comodo ridefinito          *
      *                      *-----------------------------------------*
           move      rf-ocr-tip-rig       to   w-det-qta-orc-wtr      .
      *                      *-----------------------------------------*
      *                      * Se prodotto similare  : riciclo         *
      *                      *-----------------------------------------*
           if        w-det-qta-orc-wtf    not  = spaces
                     go to det-qta-orc-100.
      *                  *---------------------------------------------*
      *                  * Determinazione quantita' evasa riga ordine  *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivi                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' in ordine da clienti          *
      *                      *-----------------------------------------*
           add       d-qev-roc-qta-dev    to   w-det-qta-orc-qoc      .
      *                      *-----------------------------------------*
      *                      * Quantita' in corso di spedizione        *
      *                      *-----------------------------------------*
           add       d-qev-roc-qta-ics    to   w-det-qta-orc-qis      .
      *                      *-----------------------------------------*
      *                      * Se quantita' da evadere a zero : a ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        d-qev-roc-qta-dev    =    zero
                     go to det-qta-orc-180.
      *                      *-----------------------------------------*
      *                      * Dati primo ordine : se non ancora de-   *
      *                      * terminati, quelli della riga ordine at- *
      *                      * tuale, altrimenti, se la data consegna  *
      *                      * e' inferiore a quella finora memorizza- *
      *                      * ta, si memorizzano i valori dell'ordine *
      *                      * attuale                                 *
      *                      *-----------------------------------------*
           if        w-det-qta-orc-qpo    =    zero and
                     w-det-qta-orc-dpo    =    zero
                     move  d-qev-roc-qta-dev
                                          to   w-det-qta-orc-qpo
                     move  rf-ocr-dcn-prv to   w-det-qta-orc-dpo
           else if   rf-ocr-dcn-prv       <    w-det-qta-orc-dpo
                     move  d-qev-roc-qta-dev
                                          to   w-det-qta-orc-qpo
                     move  rf-ocr-dcn-prv to   w-det-qta-orc-dpo      .
       det-qta-orc-180.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale record [ocr]     *
      *              *-------------------------------------------------*
           go to     det-qta-orc-100.
       det-qta-orc-999.
           exit.

      *    *===========================================================*
      *    * Routine di Det : quantita' in ordine a fornitori          *
      *    *-----------------------------------------------------------*
       det-qta-orf-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione valori in output               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-qta-orf-qof      .
           move      zero                 to   w-det-qta-orf-qpo      .
           move      zero                 to   w-det-qta-orf-dpo      .
      *              *-------------------------------------------------*
      *              * Start su file [ofr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      w-det-qta-orf-dpz    to   rf-ofr-cod-dpz         .
           move      spaces               to   rf-ofr-flg-rch         .
           move      01                   to   rf-ofr-tip-mag         .
           move      w-det-qta-orf-cod    to   rf-ofr-num-mag         .
           move      zero                 to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qta-orf-999.
       det-qta-orf-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [ofr]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qta-orf-999.
       det-qta-orf-120.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ofr-cod-dpz       not  = w-det-qta-orf-dpz or
                     rf-ofr-flg-rch       not  = spaces            or
                     rf-ofr-tip-mag       not  = 01                or
                     rf-ofr-num-mag       not  = w-det-qta-orf-cod
                     go to det-qta-orf-999.
       det-qta-orf-140.
      *              *-------------------------------------------------*
      *              * Selezione sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-ofr-dat-doc       >    w-det-qta-orf-dat
                     go to det-qta-orf-100.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo riga                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo riga in comodo ridefinito          *
      *                      *-----------------------------------------*
           move      rf-ofr-tip-rig       to   w-det-qta-orf-wtr      .
      *                      *-----------------------------------------*
      *                      * Test su tipo funzionamento              *
      *                      *-----------------------------------------*
           if        w-det-qta-orf-wtf    not  = spaces
                     go to det-qta-orf-100.
      *                  *---------------------------------------------*
      *                  * Determinazione status della riga ordine     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione, relativamente alla riga *
      *                      * ordine letta, di :                      *
      *                      *                                         *
      *                      *  - Quantita' in ordine                  *
      *                      *  - Quantita' ricevuta                   *
      *                      *  - Quantita' in corso di ricevimento    *
      *                      *                                         *
      *                      *  - Quantita' ancora da ricevere         *
      *                      *                                         *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           move      "pgm/orf/prg/obj/dqevrof0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-rof
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivi                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' ordinata a fornitori          *
      *                      *-----------------------------------------*
           if        d-qev-rof-snx-tum    =    "S"
                     move  d-qev-rof-fda-dri
                                          to   w-det-qta-orf-dri
           else      move  d-qev-rof-qta-dri
                                          to   w-det-qta-orf-dri      .
           add       w-det-qta-orf-dri    to   w-det-qta-orf-qof      .
      *                      *-----------------------------------------*
      *                      * Se quantita' da ricevere a zero : a ri- *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-det-qta-orf-dri    =    zero
                     go to det-qta-orf-180.
      *                      *-----------------------------------------*
      *                      * Dati primo ordine : se non ancora de-   *
      *                      * terminati, quelli della riga ordine at- *
      *                      * tuale, altrimenti, se la data consegna  *
      *                      * e' inferiore a quella finora memorizza- *
      *                      * ta, si memorizzano i valori dell'ordine *
      *                      * attuale                                 *
      *                      *-----------------------------------------*
           if        w-det-qta-orf-qpo    =    zero and
                     w-det-qta-orf-dpo    =    zero
                     move  w-det-qta-orf-dri
                                          to   w-det-qta-orf-qpo
                     move  rf-ofr-dcn-prv to   w-det-qta-orf-dpo
           else if   rf-ofr-dcn-prv       <    w-det-qta-orf-dpo
                     move  w-det-qta-orf-dri
                                          to   w-det-qta-orf-qpo
                     move  rf-ofr-dcn-prv to   w-det-qta-orf-dpo      .
       det-qta-orf-180.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale record [ofr]     *
      *              *-------------------------------------------------*
           go to     det-qta-orf-100.
       det-qta-orf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
