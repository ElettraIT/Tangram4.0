       Identification Division.
       Program-Id.                                 zttpro01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    zttpro              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 11/02/14    *
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
      *        * [mau]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmau"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      
      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

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
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Flag di visualizzazione testata                       *
      *        *-------------------------------------------------------*
           05  w-exe-flg-tst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di prodotto individuato                          *
      *        *-------------------------------------------------------*
           05  w-exe-flg-ext              pic  x(01)                  .
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
           05  w-exe-pth-img              pic  x(22)                  .
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
           05  w-exe-klb-img              pic  x(15)                  .
           05  w-exe-klb-pro              pic  x(50)                  .
           05  w-exe-prz-lst              pic  x(12)                  .
           05  w-exe-gia-pro              pic  x(20)                  .
           05  w-exe-cop-sfn              pic  x(14)                  .
           05  w-exe-qta-cfz              pic  x(12)                  .
           05  w-exe-lot-ven              pic  x(12)                  .
           05  w-exe-prm-ubi              pic  x(28)                  .
           05  w-exe-qta-orf              pic  x(12)                  .
           05  w-exe-dat-cns              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per emissione html                             *
      *        *-------------------------------------------------------*
           05  w-exe-wdt-001              pic  x(08) value "WIDTH=80" .
           05  w-exe-wdt-002              pic  x(09) value "WIDTH=120".
           05  w-exe-wdt-003              pic  x(09) value "WIDTH=120".
           05  w-exe-wdt-004              pic  x(09) value "WIDTH=250".
           05  w-exe-wdt-005              pic  x(09) value "WIDTH=250".

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
      *        * Work per Det parametri di ubicazione prodotto         *
      *        *-------------------------------------------------------*
           05  w-det-prm-ubi.
      *            *---------------------------------------------------*
      *            * Literal                                           *
      *            *---------------------------------------------------*
               10  w-det-prm-ubi-lit      pic  x(28)                  .
      *            *---------------------------------------------------*
      *            * Valori in input                                   *
      *            *---------------------------------------------------*
               10  w-det-prm-ubi-dpz      pic  9(02)                  .
               10  w-det-prm-ubi-tip      pic  9(02)                  .
               10  w-det-prm-ubi-num      pic  9(07)                  .
               10  w-det-prm-ubi-var      pic  x(14)                  .
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
      *            *---------------------------------------------------*
      *            * Giacenze e comodi giacenza                        *
      *            *---------------------------------------------------*
               10  w-det-qta-gia-d01      pic s9(08)v9(03)            .
               10  w-det-qta-gia-d02      pic s9(08)v9(03)            .
               10  w-det-qta-gia-dxx      pic s9(08)v9(03)            .
               10  w-det-qta-gia-rsp      pic  x(01)                  .

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
      *              *-------------------------------------------------*
           if        w-exe-dat-exe        <    999999
                     add  1000000         to   w-exe-dat-exe          .
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
           move      w-exe-prm-v01        to   w-all-str-alf          .
           move      14                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-alf-prv          .
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
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
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
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
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
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Flag di prodotto trovato                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-ext          .
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
      *              * Flag di prodotto trovato                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-exe-flg-ext          .
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
           go to     exe-cph-880.
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
      *              * Flag di prodotto trovato                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-exe-flg-ext          .
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
           go to     exe-cph-880.
       exe-cph-800.
      *              *-------------------------------------------------*
      *              * Start su archivio [aaf]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "COPSFN    "         to   f-key                  .
           move      w-exe-alf-pro        to   rf-aaf-cop-sfn         .
           move      01                   to   rf-aaf-tip-mag         .
           move      zero                 to   rf-aaf-cod-dcf         .
           move      zero                 to   rf-aaf-num-pro         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : oltre                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-890.
       exe-cph-820.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [aaf]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-890.
       exe-cph-830.
      *              *-------------------------------------------------*
      *              * Test max su archivio [aaf]                      *
      *              *-------------------------------------------------*
           if        rf-aaf-cop-sfn       >    w-exe-alf-pro
                     go to exe-cph-890.
           if        rf-aaf-tip-mag       not  = 01
                     go to exe-cph-890.
       exe-cph-840.
      *              *-------------------------------------------------*
      *              * Sel su [aaf]                                    *
      *              *-------------------------------------------------*
       exe-cph-850.
      *              *-------------------------------------------------*
      *              * Letture addizionali                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura del record [dcp]                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-aaf-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcp-des-pro         .
       exe-cph-860.
      *              *-------------------------------------------------*
      *              * Flag di prodotto trovato                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-exe-flg-ext          .
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
           go to     exe-cph-880.
       exe-cph-880.
      *              *-------------------------------------------------*
      *              * Emissione piede documento                       *
      *              *-------------------------------------------------*
           perform   emi-pie-000          thru emi-pie-999            .
       exe-cph-890.
      *              *-------------------------------------------------*
      *              * Test se trovato almeno un codice                *
      *              *-------------------------------------------------*
           if        w-exe-flg-ext        not  = spaces
                     go to exe-cph-900.
      *              *-------------------------------------------------*
      *              * Preparazione per elemento vuoto                 *
      *              *-------------------------------------------------*
           perform   exe-vuo-000          thru exe-vuo-999            .
      *              *-------------------------------------------------*
      *              * Emissione corpo documento                       *
      *              *-------------------------------------------------*
           perform   emi-cor-000          thru emi-cor-999            .
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
      *    * Preparazione elemento vuoto                               *
      *    *-----------------------------------------------------------*
       exe-vuo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [dcp]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Codice e descrizione prodotto                   *
      *              *-------------------------------------------------*
           move      w-exe-alf-pro        to   rf-dcp-alf-pro         .
           move      "(Prodotto non esistente !)"
                                          to   rf-dcp-des-pro         .
      *              *-------------------------------------------------*
      *              * Codice prodotto per il fornitore                *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-cop-sfn          .
      *              *-------------------------------------------------*
      *              * Determinazione giacenza fisica                  *
      *              *-------------------------------------------------*
           move      zero                 to   d-sld-mag-sld-mag      .
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-prm-ubi          .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' ordinata               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-qta-orf-qof      .
           move      zero                 to   w-det-qta-orf-dpo      .
      *              *-------------------------------------------------*
      *              * Determinazione giacenza fisica                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-qta-gia-d01      .
           move      zero                 to   w-det-qta-gia-d02      .
       exe-vuo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-vuo-999.
       exe-vuo-999.
           exit.

      *    *===========================================================*
      *    * Raccolta dati supplementari                               *
      *    *-----------------------------------------------------------*
       exe-rds-000.
      *              *-------------------------------------------------*
      *              * Codice prodotto per il fornitore                *
      *              *-------------------------------------------------*
           perform   cop-sfn-000          thru cop-sfn-999            .
      *              *-------------------------------------------------*
      *              * Determinazione giacenza fisica - Sede           *
      *              *-------------------------------------------------*
           move      01                   to   w-det-qta-gcf-dpz      .
           move      rf-dcp-num-pro       to   w-det-qta-gcf-cod      .
           move      w-exe-dat-exe        to   w-det-qta-gcf-dat      .
           perform   det-qta-gcf-000      thru det-qta-gcf-999        .
           move      d-sld-mag-sld-mag    to   w-det-qta-gia-d01      .
      *              *-------------------------------------------------*
      *              * Determinazione giacenza fisica - Verona         *
      *              *-------------------------------------------------*
           move      02                   to   w-det-qta-gcf-dpz      .
           move      rf-dcp-num-pro       to   w-det-qta-gcf-cod      .
           move      w-exe-dat-exe        to   w-det-qta-gcf-dat      .
           perform   det-qta-gcf-000      thru det-qta-gcf-999        .
           move      d-sld-mag-sld-mag    to   w-det-qta-gia-d02      .
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione                       *
      *              *-------------------------------------------------*
           move      01                   to   w-det-prm-ubi-dpz      .
           move      01                   to   w-det-prm-ubi-tip      .
           move      rf-dcp-num-pro       to   w-det-prm-ubi-num      .
           move      spaces               to   w-det-prm-ubi-var      .
           perform   det-prm-ubi-000      thru det-prm-ubi-999        .
           move      w-det-prm-ubi-lit    to   w-exe-prm-ubi          .
      *              *-------------------------------------------------*
      *              * Determinazione quantita' ordinata               *
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
      *    * Determinazione codice prodotto per il fornitore           *
      *    *-----------------------------------------------------------*
       cop-sfn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-cop-sfn          .
      *              *-------------------------------------------------*
      *              * Lettura fornitore preferenziale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      01                   to   rf-aaq-tip-mag         .
           move      rf-dcp-num-pro       to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se record [aaq] non trovato : oltre         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cop-sfn-900.
       cop-sfn-050.
      *              *-------------------------------------------------*
      *              * Test su elemento trovato                        *
      *              *-------------------------------------------------*
           if        rf-aaq-dcf-pfz       =    zero
                     go to cop-sfn-900.
       cop-sfn-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [aaf]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRFNFM    "         to   f-key                  .
           move      01                   to   rf-aaf-tip-mag         .
           move      rf-dcp-num-pro       to   rf-aaf-num-pro         .
           move      rf-aaq-dcf-pfz       to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : oltre                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cop-sfn-900.
       cop-sfn-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [aaf]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cop-sfn-900.
       cop-sfn-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [aaf]                      *
      *              *-------------------------------------------------*
           if        rf-aaf-tip-mag       not  = 01             or
                     rf-aaf-num-pro       not  = rf-dcp-num-pro or
                     rf-aaf-cod-dcf       not  = rf-aaq-dcf-pfz
                     go to cop-sfn-900.
       cop-sfn-400.
      *              *-------------------------------------------------*
      *              * In campo di destinazione                        *
      *              *-------------------------------------------------*
           move      rf-aaf-cop-sfn       to   w-exe-cop-sfn          .
       cop-sfn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cop-sfn-999.
       cop-sfn-999.
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
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   "<HEAD>"                                         .
           display   "<TITLE>Prodotti TANGRAM</TITLE>"                .
           display   "</HEAD>"                                        .
      *              *-------------------------------------------------*
      *              * Css                                             *
      *              *-------------------------------------------------*
           display   "<LINK REL=stylesheet type=text/css href=../css/dkb
      -              ".css>"                                          .
           display   "<STYLE TYPE=text/css>"                          .
           display   "@import url(../css/dkb.css);"                   .
           display   "</STYLE>"                                       .
      *              *-------------------------------------------------*
      *              * Colori sfondo                                   *
      *              *-------------------------------------------------*
           display   "<BODY BGCOLOR=#ffffff TEXT=#220000 VLINK=#ffffff o
      -              "nLoad=""document.ins.cod.focus();"">"           .
      *              *-------------------------------------------------*
      *              * Intestazione                                    *
      *              *-------------------------------------------------*
           display   "<HR>"                                           .
           display   "<CENTER>"                                       .
           display   "<A HREF=../index.html><img src='../icons/zatti.gif
      -              "'></A>"                                         .
           display   "</CENTER>"                                      .
______*    display   "<BODY TEXT=#000000>"                            .
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<FORM NAME=ins METHOD=POST ACTION=/cgi-bin/zttpro0
      -              "1>"                                             .
      *              *-------------------------------------------------*
      *              * Area di input                                   *
      *              *-------------------------------------------------*
           display   "<HR>"                                           .
           display   "<TABLE  WIDTH=100% BORDER=0 CELLSPACING=0 CELLPADD
      -              "ING=0>"                                         .
           display   "<TR ALIGN=LEFT>"                                .
           display   "<TD><INPUT TYPE=TEXT NAME=cod VALUE=''> Codice pro
      -              "dotto </TD>"                                    .
      *              *-------------------------------------------------*
      *              * Hidden                                          *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD><INPUT TYPE=HIDDEN NAME=hid VALUE='"
                                delimited by   size
                     w-exe-alf-pro
                                delimited by   spaces
                     "'></TD>"
                                delimited by   size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
           display   "</TABLE>"                                       .
      *              *-------------------------------------------------*
      *              * Area di conferma                                *
      *              *-------------------------------------------------*
           display   "<TABLE BORDER=0 CELLSPACING=0 CELLPADDING=1>"   .
           display   "<TR ALIGN=LEFT>"                                .
           display   "<TD><INPUT TYPE=SUBMIT VALUE=Conferma ></td>"   .
           display   "</TR>"                                          .
           display   "</TABLE>"                                       .
      *              *-------------------------------------------------*
      *              * Inizio corpo                                    *
      *              *-------------------------------------------------*
           display   "<HR>"                                           .
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
       emi-cor-030.
      *              *-------------------------------------------------*
      *              * Preparazione pathname immagine                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-pth-img          .       
           string    "../icons/" 
                                delimited by size
                     rf-dcp-alf-pro
                    (01 : 02)   delimited by spaces
                     "/"        delimited by size
                     rf-dcp-alf-pro
                                delimited by spaces
                                          into w-exe-pth-img          .
      *              *-------------------------------------------------*
      *              * Preparazione linee in base a giacenza Verona    *
      *              *-------------------------------------------------*
           if        w-det-qta-gia-d02    =    zero
                     move  "6"            to   w-det-qta-gia-rsp
           else      move  "7"            to   w-det-qta-gia-rsp      .
       emi-cor-050.
      *              *-------------------------------------------------*
      *              * Premessa corpo                                  *
      *              *-------------------------------------------------*
           display   "<table border=1 cellspacing=0 cellpadding=1 class=
      -              "bordotab>"                                       .
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Trattamento codice e descrizione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "Codice"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-002
                                delimited by size
                     "> "
                                delimited by size
                     rf-dcp-alf-pro
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     "Descrizione"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     rf-dcp-des-pro
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' ROWSPAN="
                                delimited by size
                     w-det-qta-gia-rsp
                                delimited by size
                     " "
                                delimited by size
                     w-exe-wdt-005
                                delimited by size
                     "> "
                                delimited by size
                     "<IMG BORDER=0 SRC='"
                                delimited by size
                     w-exe-pth-img
                                delimited by spaces
                     ".jpg' ALT=CODICE WIDTH=300>"
                                delimited by size
                     "</TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-200.
      *              *-------------------------------------------------*
      *              * Trattamento prezzo e unita' di misura           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing prezzo                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-dcp-prz-lst       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-prz-lst          .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
           if        w-exe-prz-lst        =    spaces
                     move  "-"            to   w-exe-prz-lst          .
      *
           if        rf-dcp-umi-ven       =    spaces
                     move  "-"            to   rf-dcp-umi-ven         .
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "Prezzo"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by spaces
                     "> "
                                delimited by size
                     w-exe-prz-lst
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     "Unita' di misura"
                                delimited by size
                     " </TD>"
                                delimited by size
                     " <TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     rf-dcp-umi-ven
                                delimited by size
                     " </TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-300.
      *              *-------------------------------------------------*
      *              * Trattamento giacenza e codice acquisto          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing giacenza Sede                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      w-det-qta-gia-d01    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-gia-pro          .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
           if        w-exe-gia-pro        =    spaces
                     move  "-"            to   w-exe-gia-pro          .
      *
           if        w-exe-cop-sfn        =    spaces
                     move  "-"            to   w-exe-cop-sfn          .
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "Giacenza"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-002
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-gia-pro
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     " Codice di acquisto"
                                delimited by size
                     " </TD>"
                                delimited by size
                     " <TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-cop-sfn
                                delimited by spaces
                     " </TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-350.
      *              *-------------------------------------------------*
      *              * Trattamento eventuale giacenza Verona           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da visualizzare                     *
      *                  *---------------------------------------------*
           if        w-det-qta-gia-d02    =    zero
                     go to emi-cor-400.
      *                  *---------------------------------------------*
      *                  * Editing giacenza Verona                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BGD"               to   v-edm                  .
           move      w-det-qta-gia-d02    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-gia-pro          .
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "Giacenza VR"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-002
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-gia-pro
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     " --- "
                                delimited by size
                     " </TD>"
                                delimited by size
                     " <TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     " - "
                                delimited by size
                     " </TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-400.
      *              *-------------------------------------------------*
      *              * Trattamento ubicazione e quantita' per cartone  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing quantita' per cartone               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-dcp-qta-cfz       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-qta-cfz          .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
           if        w-exe-prm-ubi        =    spaces
                     move  "-"            to   w-exe-prm-ubi          .
      *
           if        w-exe-qta-cfz        =    spaces
                     move  "-"            to   w-exe-qta-cfz          .
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "Ubicazione"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-002
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-prm-ubi
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     " Quantita' cartone"
                                delimited by size
                     " </TD>"
                                delimited by size
                     " <TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-qta-cfz
                                delimited by spaces
                     " </TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-500.
      *              *-------------------------------------------------*
      *              * Trattamento barcode e inner-box                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing inner-box                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      rf-dcp-lot-ven       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-lot-ven          .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se barcode a spazi                 *
      *                      *-----------------------------------------*
           if        rf-dcp-klb-pro       =    spaces
                     move  "-"            to   w-exe-klb-pro
                     move  spaces         to   w-exe-klb-img
                     go to emi-cor-520.
      *                      *-----------------------------------------*
      *                      * Visualizzazione bar-code attualmente    *
      *                      * inibita                                 *
      *                      * ___                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-exe-klb-img          .
           move      rf-dcp-klb-pro       to   w-exe-klb-pro          .
           go to     emi-cor-520.
      *                      *-----------------------------------------*
      *                      * Concatenamento con link                 *
      *                      *-----------------------------------------*
           move      "<IMG BORDER=0 "     to   w-exe-klb-img          .
      *
           move      60                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "SRC='../cgi-bin/zttbcd01?COD="
                                          to   w-all-str-cat (1)      .
           move      rf-dcp-klb-pro       to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           move      w-all-str-alf        to   w-exe-klb-pro          .
       emi-cor-520.
           if        w-exe-lot-ven        =    spaces
                     move  "-"            to   w-exe-lot-ven          .
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "Bar-code"
                                delimited by size
                     " </td>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-002
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-klb-img
                                delimited by size
                     w-exe-klb-pro
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     " Inner-box"
                                delimited by size
                     " </td>"
                                delimited by size
                     " <TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-lot-ven
                                delimited by spaces
                     " </TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-600.
      *              *-------------------------------------------------*
      *              * Trattamento in ordine e consegna prevista       *
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
      *                  * Editing consegna prevista                   *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-det-qta-orf-dpo    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-dat-cns          .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
           if        w-exe-qta-orf        =    spaces
                     move  "-"            to   w-exe-qta-orf          .
      *
           if        w-exe-dat-cns        =    spaces
                     move  "-"            to   w-exe-dat-cns          .
      *                  *---------------------------------------------*
      *                  * Composizione html                           *
      *                  *---------------------------------------------*
           display   "<TR ALIGN=CENTER>"                              .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-001
                                delimited by size
                     "> "
                                delimited by size
                     "In ordine"
                                delimited by size
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-002
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-qta-orf
                                delimited by spaces
                     " </TD>"
                                delimited by size
                     "<TD CLASS='classetd' "
                                delimited by size
                     w-exe-wdt-003
                                delimited by size
                     "> "
                                delimited by size
                     " Consegna prevista"
                                delimited by size
                     " </TD>"
                                delimited by size
                     " <TD CLASS='classetd2' "
                                delimited by size
                     w-exe-wdt-004
                                delimited by size
                     "> "
                                delimited by size
                     w-exe-dat-cns
                                delimited by size
                     " </TD>"
                                delimited by size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *
           display   "</TR>"                                          .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Fine corpo                                      *
      *              *-------------------------------------------------*
           display   "</TABLE>"                                       .
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Chiusura html                                   *
      *              *-------------------------------------------------*
           display   "<HR>"                                           .
______*    display   "<ADDRESS>"                                      .
______*    display   "<FONT SIZE=2>dKb - Informatica &copy"           .
______*    display   "</ADDRESS>"                                     .
           display   "</BODY>"                                        .
           display   "</HTML>"                                        .
       emi-pie-999.
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
      *              * Salvataggio valore determinato                  *
      *              *-------------------------------------------------*
           move      d-sld-mag-sld-mag    to   w-det-qta-gia-dxx      .
       det-qta-gcf-100.
      *              *-------------------------------------------------*
      *              * Preparazione link-area                          *
      *              *                                                 *
      *              * SCHEMA 2                                        *
      *              *-------------------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0300                 to   d-sld-mag-tip-sld      .
           move      w-det-qta-gcf-dat    to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      w-det-qta-gcf-dpz    to   d-sld-mag-cod-dpz      .
           move      01                   to   d-sld-mag-tip-mag      .
           move      w-det-qta-gcf-cod    to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "T"                  to   d-sld-mag-uot-dsl      .
           move      spaces               to   d-sld-mag-cod-dsl      .
           move      "U"                  to   d-sld-mag-uot-ctm      .
           move      "VE "                to   d-sld-mag-cod-ctm      .
           move      spaces               to   d-sld-mag-tip-arc      .
           move      "T"                  to   d-sld-mag-uot-arc      .
           move      zero                 to   d-sld-mag-cod-arc      .
           move      "T"                  to   d-sld-mag-uot-dpa      .
           move      spaces               to   d-sld-mag-dpz-arc      .
      *              *-------------------------------------------------*
      *              * Richiamo del sottoprogramma                     *
      *              *-------------------------------------------------*
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
      *              *-------------------------------------------------*
      *              * Valore determinato                              *
      *              *-------------------------------------------------*
           add       d-sld-mag-sld-mag    to   w-det-qta-gia-dxx      .
           move      w-det-qta-gia-dxx    to   d-sld-mag-sld-mag      .
       det-qta-gcf-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione parametri di ubicazione         *
      *    *-----------------------------------------------------------*
       det-prm-ubi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-prm-ubi-lit      .
       det-prm-ubi-100.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-det-prm-ubi-num    =    zero
                     go to det-prm-ubi-900.
       det-prm-ubi-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [mau]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Lettura record [mau]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      w-det-prm-ubi-dpz    to   rf-mau-cod-dpz         .
           move      w-det-prm-ubi-tip    to   rf-mau-tip-mag         .
           move      w-det-prm-ubi-num    to   rf-mau-num-mag         .
           move      w-det-prm-ubi-var    to   rf-mau-var-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *                  *---------------------------------------------*
      *                  * Se record [mau] non trovato : ad uscita     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-prm-ubi-900.
       det-prm-ubi-300.
      *              *-------------------------------------------------*
      *              * Composizione stringa                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri di ubicazione                     *
      *                  *---------------------------------------------*
           move      28                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      rf-mau-prm-ubi (1)   to   w-all-str-cat (1)      .
           move      rf-mau-prm-ubi (2)   to   w-all-str-cat (2)      .
           move      rf-mau-prm-ubi (3)   to   w-all-str-cat (3)      .
           move      rf-mau-prm-ubi (4)   to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
       det-prm-ubi-800.
      *              *-------------------------------------------------*
      *              * Valore preparato in literal di uscita           *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-det-prm-ubi-lit      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-prm-ubi-999.
       det-prm-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-prm-ubi-999.
       det-prm-ubi-999.
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
