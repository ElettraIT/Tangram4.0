       Identification Division.
       Program-Id.                                 acmndcc0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 17/12/94    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice cliente          *
      *                    commerciale                                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      *       Metodi di ricerca :                                      *
      *                                                                *
      *       '-'  seguito da return = Ricerca per ragione sociale     *
      *                                commerciale cliente             *
      *                                                                *
      *                       oppure   Ricerca per Partita Iva         *
      *                                cliente                         *
      *                                                                *
      *                       oppure   Ricerca per Codice Fiscale      *
      *                                cliente                         *
      *                                                                *
      *                       oppure   Ricerca per Numero telefono     *
      *                                cliente (numero preceduto da    *
      *                                         '.')                   *
      *                                                                *
      *                       oppure   Ricerca per Indirizzo e-mail    *
      *                                cliente                         *
      *                                                                *
      *       '--' seguito da return = Ricerca per ragione sociale     *
      *                                contabile cliente               *
      *                                                                *
      *       '-'  seguito da return = Ricerca per ragione sociale     *
      *            e da 'find'         commerciale cliente dicotomica  *
      *                               (componente ragione sociale)     *
      *                                                                *
      *       '-'  seguito da valore = Ricerca per iniziale mnemonico  *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-dcc-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-dcc-cod : codice cliente       *
      *                                                                *
      *                       w-cod-mne-dcc-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-dcc-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-dcc-rln : linea ragione soc.   *
      *                                                                *
      *                       w-cod-mne-dcc-rps : posizione rag. soc.  *
      *                                                                *
      *                       w-cod-mne-dcc-vln : linea indirizzo      *
      *                                                                *
      *                       w-cod-mne-dcc-vps : posizione indirizzo  *
      *                                                                *
      *                       w-cod-mne-dcc-lln : linea localita'      *
      *                                                                *
      *                       w-cod-mne-dcc-lps : posizione localita'  *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-dcc-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-dcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-dcc-cod : codice cliente       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-dcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-dcc-cod : codice cliente       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-dcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-dcc-cod : codice cliente       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "NT"  Richiesta eventuali note associate al Cliente            *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "NT"                 *
      *                       w-cod-mne-dcc-prg : nome del programma   *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "NN"  Richiesta normalizzazione note associate al Cliente      *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "NN"                 *
      *                       w-cod-mne-dcc-prg : forzato a spazi      *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [rlt]                                        *
      *    *-----------------------------------------------------------*
           select  optional  rlt   assign  to disk     f-rlt-pat
                             organization  is relative
                             access   mode is dynamic
                             relative key  is          w-rlt-krn
                             file status   is          f-rlt-sts      .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [rlt]                                    *
      *    *-----------------------------------------------------------*
       fd  rlt           label record standard                        .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  rlt-rec.
      *        *-------------------------------------------------------*
      *        * Area per anagrafica commerciale cliente [dcc]         *
      *        *-------------------------------------------------------*
           05  rlt-rec-rec-dcc.
               10  filler occurs 2048     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per anagrafica contabile cliente [cli]           *
      *        *-------------------------------------------------------*
           05  rlt-rec-rec-cli.
               10  filler occurs 2048     pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per file relative di appoggio [rlt]             *
      *    *-----------------------------------------------------------*
       01  f-rlt.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-rlt-nam                  pic  x(04) value "rlt "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-rlt-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-rlt-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * File area addizionale per file relative di appoggio [rlt] *
      *    *-----------------------------------------------------------*
       01  w-rlt.
      *        *-------------------------------------------------------*
      *        * Key record number                                     *
      *        *-------------------------------------------------------*
           05  w-rlt-krn                  pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Numero records memorizzati                            *
      *        *-------------------------------------------------------*
           05  w-rlt-num                  pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Numero record attualmente in trattamento              *
      *        *-------------------------------------------------------*
           05  w-rlt-att                  pic  9(08)                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcx"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

      *    *===========================================================*
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux-mne-dcc.
           05  w-aux-mne-dcc-c01          pic  9(03)                  .
           05  w-aux-mne-dcc-c02          pic  9(03)                  .
           05  w-aux-mne-dcc-c03          pic  9(03)                  .
           05  w-aux-mne-dcc-c04          pic  9(03)                  .
           05  w-aux-mne-dcc-c05          pic  9(03)                  .
           05  w-aux-mne-dcc-c09          pic  9(03)                  .
           05  w-aux-mne-dcc-nli          pic  9(03)                  .
           05  w-aux-mne-dcc-m10          pic  x(10)                  .
           05  w-aux-mne-dcc-mmi          pic  x(10)                  .
           05  w-aux-mne-dcc-mma          pic  x(10)                  .
           05  w-aux-mne-dcc-alf          pic  x(10)                  .
           05  w-aux-mne-dcc-alf-r  redefines
               w-aux-mne-dcc-alf.
               10  w-aux-mne-dcc-al1      pic  x(01)                  .
               10  w-aux-mne-dcc-al2      pic  x(01)                  .
               10  w-aux-mne-dcc-al8      pic  x(08)                  .
           05  w-aux-mne-dcc-alf-rr redefines
               w-aux-mne-dcc-alf.
               10  w-aux-mne-dcc-ar1      pic  x(01)                  .
               10  w-aux-mne-dcc-ar9      pic  x(09)                  .
           05  w-aux-mne-dcc-tpf          pic  x(01)                  .
           05  w-aux-mne-dcc-crb          pic  9(03)                  .
           05  w-aux-mne-dcc-cpb          pic  9(03)                  .
           05  w-aux-mne-dcc-cpa          pic  9(03)                  .
           05  w-aux-mne-dcc-max          pic  9(03) value 120        .
           05  w-aux-mne-dcc-buf
                               occurs 120.
               10  w-aux-mne-dcc-tbu      pic  9(02)                  .
               10  w-aux-mne-dcc-cbu      pic  9(07)                  .
               10  w-aux-mne-dcc-dbu      pic  x(04)                  .
               10  w-aux-mne-dcc-mbu      pic  x(10)                  .
               10  w-aux-mne-dcc-rbu.
                   20  w-aux-mne-dcc-fbu  pic  x(01)                  .
                   20  filler             pic  x(39)                  .
               10  w-aux-mne-dcc-2bu      pic  x(40)                  .
               10  w-aux-mne-dcc-vbu      pic  x(40)                  .
               10  w-aux-mne-dcc-lbu      pic  x(40)                  .
               10  w-aux-mne-dcc-pbu      pic  x(11)                  .
               10  w-aux-mne-dcc-sbu      pic  9(02)                  .
               10  w-aux-mne-dcc-xbu      pic  9(07)                  .
           05  w-aux-mne-dcc-edp.
               10  w-aux-mne-dcc-ep1      pic  x(03)                  .
               10  w-aux-mne-dcc-ep2      pic  x(03)                  .
           05  w-aux-mne-dcc-ltp.
               10  filler                 pic  x(17)                  .
           05  w-aux-mne-dcc-rup          pic  x(40)                  .
           05  w-aux-mne-dcc-r01 redefines
               w-aux-mne-dcc-rup.
               10  w-aux-mne-dcc-p00      pic  9(11)                  .
               10  w-aux-mne-dcc-p99      pic  x(29)                  .
           05  w-aux-mne-dcc-r02 redefines
               w-aux-mne-dcc-rup.
               10  w-aux-mne-dcc-f00.
                   15  w-aux-mne-dcc-f01  pic  x(03)                  .
                   15  w-aux-mne-dcc-f02  pic  x(03)                  .
                   15  w-aux-mne-dcc-f03  pic  9(02)                  .
                   15  w-aux-mne-dcc-f04  pic  x(01)                  .
                   15  w-aux-mne-dcc-f05  pic  9(02)                  .
                   15  w-aux-mne-dcc-f06  pic  x(01)                  .
                   15  w-aux-mne-dcc-f07  pic  9(03)                  .
                   15  w-aux-mne-dcc-f08  pic  x(01)                  .
               10  w-aux-mne-dcc-f99      pic  x(24)                  .
           05  w-aux-mne-dcc-r03 redefines
               w-aux-mne-dcc-rup.
               10  w-aux-mne-dcc-e00.
                   15  w-aux-mne-dcc-e01  pic  x(02)                  .
                   15  w-aux-mne-dcc-e02  pic  x(01)                  .
                   15  w-aux-mne-dcc-e03  pic  9(08)                  .
                   15  w-aux-mne-dcc-e04  pic  x(05)                  .
               10  w-aux-mne-dcc-e99      pic  x(24)                  .
           05  w-aux-mne-dcc-r04 redefines
               w-aux-mne-dcc-rup.
               10  w-aux-mne-dcc-t00      pic  x(01)                  .
               10  w-aux-mne-dcc-t01      pic  9(02)                  .
               10  w-aux-mne-dcc-t99      pic  x(37)                  .
           05  w-aux-mne-dcc-rmx.
               10  w-aux-mne-dcc-rch
                               occurs 40  pic  x(01)                  .
           05  w-aux-mne-dcc-fes          pic  x(01)                  .
           05  w-aux-mne-dcc-sxs          pic  9(02)                  .
           05  w-aux-mne-dcc-sxc          pic  9(07)                  .
           05  w-aux-mne-dcc-sxd          pic  x(04)                  .
           05  w-aux-mne-dcc-sts          pic  x(40)                  .
           05  w-aux-mne-dcc-fxr          pic  x(01)                  .
           05  w-aux-mne-dcc-rta          pic  9(02)                  .
           05  w-aux-mne-dcc-rca          pic  9(07)                  .
           05  w-aux-mne-dcc-rda          pic  x(04)                  .

      *    *===========================================================*
      *    * Work per subroutine di ricerca totale                     *
      *    *-----------------------------------------------------------*
       01  w-rcr-tot-dcc.
      *        *-------------------------------------------------------*
      *        * Valore della personalizzazione sul tipo di ricerca    *
      *        * totale da eseguire                                    *
      *        *                                                       *
      *        *  - A : Solo su anagrafica commerciale                 *
      *        *  - T : Anche su anagrafica contabile                  *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-pf1          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore da ricercare, normalizzato                     *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-vnr          pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records esaminati                           *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-cre          pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Flag di fine ricerca                                  *
      *        *                                                       *
      *        *  - Spaces : No                                        *
      *        *  - S      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-ffr          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di selezione effettuata                          *
      *        *                                                       *
      *        *  - Spaces : No                                        *
      *        *  - S      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-fsl          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records selezionati                         *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-crs          pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Indice di match                                       *
      *        *                                                       *
      *        *  - 00 : No match                                      *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-mch          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per test su resto pari a 10 o 100                *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcc-wr1          pic  9(01)                  .
           05  w-rcr-tot-dcc-wr2          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Tipo di trattamento status clienti in ricerca         *
      *        *                                                       *
      *        *  - 'N' : Nessun trattamento, si ignora lo status      *
      *        *  - 'S' : Si evidenzia lo status                       *
      *        *  - 'E' : Esclusione clienti con status non 'normale'  *
      *        *-------------------------------------------------------*
           05  w-prs-ttr-sts              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutine relativa alle note                    *
      *    *-----------------------------------------------------------*
       01  w-not-cli.
      *        *-------------------------------------------------------*
      *        * Numero di linee necessarie                            *
      *        *-------------------------------------------------------*
           05  w-not-cli-num-lin          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero di linee da togliere eventualmente             *
      *        *-------------------------------------------------------*
           05  w-not-cli-num-lis          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente salvato                                *
      *        *-------------------------------------------------------*
           05  w-not-cli-cod-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Buffer annotazioni                                    *
      *        *-------------------------------------------------------*
           05  w-not-cli-ctr-ele          pic  9(03)                  .
           05  w-not-cli-inx-ele          pic  9(03)                  .
           05  w-not-cli-max-ele          pic  9(03)       value 30   .
           05  w-not-cli-not-buf occurs 30.
               10  w-not-cli-cod-ann      pic  9(03)                  .
               10  w-not-cli-cod-ang      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi                                                *
      *        *-------------------------------------------------------*
           05  w-not-cli-ctr-001          pic  9(02)                  .
           05  w-not-cli-ctr-002          pic  9(02)                  .
           05  w-not-cli-ctr-003          pic  9(02)                  .

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
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-dcc
                                               v
                                               s                      .
      ******************************************************************

      *    *===========================================================*
      *    * Declaratives                                              *
      *    *-----------------------------------------------------------*
       Declaratives.
       Decl Section.
           Use after standard error procedure on rlt                  .
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-auc-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using f-rlt                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      f-rlt-sts            to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Test su tipo di errore intervenuto. Se l'errore *
      *              * non rientra tra i seguenti si termina l'esecu-  *
      *              * zione del programma con segnalazione di errore  *
      *              * fatale.                                         *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-lst and
                     e-sts                not  = e-end-fil and
                     e-sts                not  = e-dup-key and
                     e-sts                not  = e-not-fnd and
                     e-sts                not  = e-opn-err and
                     e-sts                not  = e-use-err and
                     e-sts                not  = e-fil-inc
                     move  "FE"           to   s-ope
                     move  f-rlt-nam      to   s-nam
                     move  f-rlt-pat      to   s-pat
                     move  f-rlt-sts      to   s-sts
                     call  "swd/mod/prg/obj/msegrt"
                                         using s                      .
       End Declaratives.

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cod-mne-dcc-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-dcc-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-dcc-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-dcc-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-dcc-ope    =    "A+" or
                     w-cod-mne-dcc-ope    =    "I+" or
                     w-cod-mne-dcc-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-mne-dcc-ope    =    "NT" or
                     w-cod-mne-dcc-ope    =    "NN"
                     perform   not-000    thru not-999                .
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
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa e' la prima Open per il modulo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di trattamento status clienti          *
      *                  *---------------------------------------------*
           perform   prs-ttr-sts-000      thru prs-ttr-sts-999        .
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione relativa al tipo  *
      *                  * di ricerca totale                           *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[trt-pf1]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-rcr-tot-dcc-pf1
           else      move  "A"            to   w-rcr-tot-dcc-pf1      .
           if        w-rcr-tot-dcc-pf1    not  = "T"
                     move  "A"            to   w-rcr-tot-dcc-pf1      .
       opn-300.
      *                  *---------------------------------------------*
      *                  * Open files                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazioni preliminari             *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-mne-dcc-prg      .
           move      spaces               to   w-cod-mne-dcc-snm      .
           move      zero                 to   w-not-cli-cod-cli      .
       opn-310.
      *                      *-----------------------------------------*
      *                      * Open file [dcc]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Open file [dcx]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                      *-----------------------------------------*
      *                      * Open file [cli]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Open modulo di determinazione contatti  *
      *                      *-----------------------------------------*
           perform   con-arc-tip-opn-000  thru con-arc-tip-opn-999    .
      *                      *-----------------------------------------*
      *                      * Open file relative di appoggio [rlt]    *
      *                      *-----------------------------------------*
           perform   rlt-opn-000          thru rlt-opn-999            .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo di trattamento status    *
      *    *                             clienti in ricerca            *
      *    *-----------------------------------------------------------*
       prs-ttr-sts-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[ttr-sts]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-ttr-sts
           else      move  spaces         to   w-prs-ttr-sts          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-ttr-sts        not  = "S" and
                     w-prs-ttr-sts        not  = "E"
                     move  "N"            to   w-prs-ttr-sts          .
       prs-ttr-sts-999.
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
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
       cls-300.
      *                  *---------------------------------------------*
      *                  * Close files                                 *
      *                  *---------------------------------------------*
       cls-310.
      *                      *-----------------------------------------*
      *                      * Close file [dcc]                        *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Close file [dcx]                        *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                      *-----------------------------------------*
      *                      * Close file [cli]                        *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Close modulo di determinazione contatti *
      *                      *-----------------------------------------*
           perform   con-arc-tip-cls-000  thru con-arc-tip-cls-999    .
      *                      *-----------------------------------------*
      *                      * Close file relative di appoggio [rlt]   *
      *                      *-----------------------------------------*
           perform   rlt-cls-000          thru rlt-cls-999            .
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
                     move  spaces         to   w-cod-mne-dcc-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-150.
           move      "P?"                 to   s-ope                  .
           move      "pcge4000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-150.
           if        v-pfk (04)           =    "INSR"
                     move  spaces         to   v-pfk (04)             .
       acc-150.
      *              *-------------------------------------------------*
      *              * Abilitazione eventuale del tasto Pf2            *
      *              *-------------------------------------------------*
           if        w-cod-mne-dcc-cod    =    zero
                     go to acc-200.
           move      "[2] "               to   v-pfk (15)             .
      *
           move      "P?"                 to   s-ope                  .
           move      "pgep6000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-200.
           if        v-pfk (15)           =    "[2] "
                     move  spaces         to   v-pfk (15)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare segnale di cliente  *
      *              * individuato tramite mnemonico                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cod-mne-dcc-snm      .
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
           move      w-cod-mne-dcc-cod    to   w-cod-mne-dcc-num      .
           move      v-edm                to   w-cod-mne-dcc-edm      .
           move      v-ufk                to   w-cod-mne-dcc-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-dcc-edm    to   v-edm                  .
           move      w-cod-mne-dcc-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-dcc-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-dcc-ufk    to   v-ufk                  .
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           move      w-cod-mne-dcc-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-dcc-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare segnale di cliente  *
      *              * individuato tramite mnemonico                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cod-mne-dcc-snm      .
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-dcc-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find o Insr                  *
      *              *-------------------------------------------------*
           if        w-cod-mne-dcc-ope    =    "F+" or 
                     w-cod-mne-dcc-ope    =    "I+"
                     go to aco-100.
       aco-025.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-dcc-ufk    to   v-ufk                  .
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           move      w-cod-mne-dcc-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-dcc-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-dcc-alf      .
      *              *-------------------------------------------------*
      *              * Test se Exit o Delt                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-175.
       aco-050.
      *              *-------------------------------------------------*
      *              * Rivisualizzazione campo editato                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           move      w-cod-mne-dcc-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-dcc-num    to   w-cod-mne-dcc-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-dcc-rln      .
           move      zero                 to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find o da Insr                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata                 *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Se selezione effettuata                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio valore numerico             *
      *                      *-----------------------------------------*
           move      s-num                to   w-cod-mne-dcc-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-dcc-edm    to   v-edm                  .
           move      w-cod-mne-dcc-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-dcc-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore numerico editato *
      *                      * e preparazione valore in uscita         *
      *                      *-----------------------------------------*
           go to     aco-050.
       aco-125.
      *              *-------------------------------------------------*
      *              * Preparazione uscita per Find precablato         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : esecuzione Find         *
      *                  *---------------------------------------------*
           move      "F+"                 to   w-cod-mne-dcc-ope      .
      *                  *---------------------------------------------*
      *                  * Simulazione tasto Find                      *
      *                  *---------------------------------------------*
           move      "FIND"               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-175.
      *              *-------------------------------------------------*
      *              * Test se blanks embedded                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-dcc-alf    =    spaces
                     move  zero           to   w-cod-mne-dcc-num
                     go to aco-075.
           if        w-cod-mne-dcc-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-dcc-c01      .
       aco-176.
           add       1                    to   w-aux-mne-dcc-c01      .
           if        w-aux-mne-dcc-c01    >    10
                     go to aco-180.
           if        w-cod-mne-dcc-cha
                    (w-aux-mne-dcc-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-dcc-c01      .
           if        w-aux-mne-dcc-c01    >    10
                     go to aco-180.
           if        w-cod-mne-dcc-cha
                    (w-aux-mne-dcc-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Test preliminari sulla stringa inserita         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza stringa inserita   *
      *                  *---------------------------------------------*
           move      w-cod-mne-dcc-alf    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
      *                  *---------------------------------------------*
      *                  * Se oltre 7 caratteri, si condidera un mne-  *
      *                  * monico                                      *
      *                  *---------------------------------------------*
           if        w-all-str-lun        >    7
                     go to aco-210.
      *                  *---------------------------------------------*
      *                  * Conversione valore alfanumerico in numerico *
      *                  *---------------------------------------------*
       aco-185.
           move      zero                 to   w-cod-mne-dcc-num      .
           move      zero                 to   w-aux-mne-dcc-c01      .
       aco-190.
           add       1                    to   w-aux-mne-dcc-c01      .
           if        w-aux-mne-dcc-c01    >    10
                     go to aco-200.
           if        w-cod-mne-dcc-cha
                    (w-aux-mne-dcc-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-dcc-cha
                    (w-aux-mne-dcc-c01)   <    "0" or
                     w-cod-mne-dcc-cha
                    (w-aux-mne-dcc-c01)   >    "9"
                     go to aco-210.
           multiply  10                   by   w-cod-mne-dcc-num      .
           move      w-cod-mne-dcc-cha
                    (w-aux-mne-dcc-c01)   to   w-cod-mne-dcc-chn (10) .
           go to     aco-190.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-mne-dcc-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-dcc-edm    to   v-edm                  .
           move      w-cod-mne-dcc-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-dcc-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-dcc-alf
                     go to aco-050.
       aco-210.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In base al contenuto del campo si desume    *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Campo alfanumerico accettato in comodo  *
      *                      * di trattamento                          *
      *                      *-----------------------------------------*
           move      w-cod-mne-dcc-alf    to   w-aux-mne-dcc-alf      .
      *                      *-----------------------------------------*
      *                      * Se il primo carattere e' '-' e tutto il *
      *                      * resto a spazi : ricerca per ragione so- *
      *                      * ciale commerciale                       *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-al1    =    "-"    and
                     w-aux-mne-dcc-al2    =    spaces and
                     w-aux-mne-dcc-al8    =    spaces and
                     w-cod-mne-dcc-rln    not  = zero
                     go to aco-500.
      *                      *-----------------------------------------*
      *                      * Se il primo ed il secondo carattere     *
      *                      * sono '-' ed il resto e' a spazi : ri-   *
      *                      * cerca per ragione sociale contabile     *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-al1    =    "-"    and
                     w-aux-mne-dcc-al2    =    "-"    and
                     w-aux-mne-dcc-al8    =    spaces and
                     w-cod-mne-dcc-rln    not  = zero
                     go to aco-800.
      *                      *-----------------------------------------*
      *                      * Se il primo carattere e' '-' ed il res- *
      *                      * to e' diverso da spazi : ricerca per    *
      *                      * mnemonico multipla                      *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-al1    =    "-"  and
                     w-aux-mne-dcc-ar9    not  = spaces
                     go to aco-224.
       aco-220.
      *                      *-----------------------------------------*
      *                      * Preparazione min-max per ricerca per    *
      *                      * mnemonico singola                       *
      *                      *-----------------------------------------*
           move      w-cod-mne-dcc-alf    to   w-aux-mne-dcc-mmi      .
           move      w-cod-mne-dcc-alf    to   w-aux-mne-dcc-mma      .
      *                          *-------------------------------------*
      *                          * A ricerca per mnemonico             *
      *                          *-------------------------------------*
           go to     aco-230.
       aco-224.
      *                      *-----------------------------------------*
      *                      * Preparazione min-max per ricerca per    *
      *                      * mnemonico multipla                      *
      *                      *-----------------------------------------*
           move      w-aux-mne-dcc-ar9    to   w-aux-mne-dcc-mmi      .
           move      w-aux-mne-dcc-mmi    to   w-aux-mne-dcc-mma      .
           move      w-aux-mne-dcc-mma    to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   w-aux-mne-dcc-mma      .
      *                          *-------------------------------------*
      *                          * A ricerca per mnemonico             *
      *                          *-------------------------------------*
           go to     aco-230.
       aco-230.
      *                  *=============================================*
      *                  * Ricerca per mnemonico                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-dcc-tpf      .
           move      zero                 to   w-aux-mne-dcc-crb      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare segnale di  *
      *                      * cliente individuato tramite mnemonico   *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-mne-dcc-snm      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-mne-dcc-mmi    to   rf-cli-cod-mne         .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-240.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per mnemonico       *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 10 *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-m10      .
      *                      *-----------------------------------------*
      *                      * Segnale di lettura tramite mnemonico    *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-mne-dcc-snm      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-m10    >    w-aux-mne-dcc-mma
                     go to aco-250.
       aco-245.
      *                      *-----------------------------------------*
      *                      * Selezione sul record                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura record [dcc]                *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-cli-cod-cli       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                          *-------------------------------------*
      *                          * Flag esito operazione               *
      *                          *-------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-aux-mne-dcc-fes
           else      move  "N"            to   w-aux-mne-dcc-fes      .
      *                          *-------------------------------------*
      *                          * Test su status cliente              *
      *                          *-------------------------------------*
           if        w-prs-ttr-sts        =    "E" and
                     rf-dcc-sta-tus       not  = 01
                     go to aco-240.
      *                          *-------------------------------------*
      *                          * Normalizzazioni se record non esi-  *
      *                          * stente                              *
      *                          *-------------------------------------*
           if        w-aux-mne-dcc-fes    not  = spaces
                     move  rf-cli-rag-soc
                                          to   rf-dcc-rag-soc
                     move  rf-cli-via-cli
                                          to   rf-dcc-via-dcc
                     move  rf-cli-loc-cli
                                          to   rf-dcc-loc-dcc         .
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di max records letti con   *
      *                      * lo stesso mnemonico                     *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-cli       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      spaces               to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-240.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-dcc-crb    =    1
                     go to aco-300
           else      go to aco-325.
       aco-275.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il mnemonico impostato *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del segnale di record   *
      *                      * non trovato                             *
      *                      *-----------------------------------------*
           if        w-cod-mne-dcc-rln    =    zero
                     go to aco-276.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-276.
           if        w-cod-mne-dcc-vln    =    zero
                     go to aco-277.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-vln    to   v-lin                  .
           move      w-cod-mne-dcc-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-277.
           if        w-cod-mne-dcc-lln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-lln    to   v-lin                  .
           move      w-cod-mne-dcc-lps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-278.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-025.
       aco-300.
      *                  *---------------------------------------------*
      *                  * Se un record con il mnemonico impostato     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se in ricerca totale, ma non effettuata *
      *                      * la selezione : al box di scelta         *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-tpf    =    "X"    and
                     w-rcr-tot-dcc-fsl    =    spaces
                     go to aco-325.
      *                      *-----------------------------------------*
      *                      * Codice numerico in valore numerico      *
      *                      *-----------------------------------------*
           move      w-aux-mne-dcc-cbu (1)
                                          to   w-cod-mne-dcc-num      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione ed uscita             *
      *                      *-----------------------------------------*
           go to     aco-200.
       aco-325.
      *                  *---------------------------------------------*
      *                  * Se non piu' di centoventi records con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-aux-mne-dcc-crb    to   w-aux-mne-dcc-cpb      .
           subtract  1                    from w-aux-mne-dcc-cpb      .
           divide    6                    into w-aux-mne-dcc-cpb      .
           add       1                    to   w-aux-mne-dcc-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-dcc-c01      .
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
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "        Selezionare il cliente commerciale desider
      -              "ato       "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Codice cliente   :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Ragione sociale  :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Indirizzo        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Localita'        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Partita iva      :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Mnemonico :"        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   aco-vrt-000          thru aco-vrt-999            .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-350.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-aux-mne-dcc-c01    to   w-aux-mne-dcc-nli      .
       aco-355.
           if        w-aux-mne-dcc-nli    >    6
                     subtract  6          from w-aux-mne-dcc-nli
                     go to aco-355.
           add       06                   to   w-aux-mne-dcc-nli      .
       aco-360.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice e dipendenza                 *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-mne-dcc-cbu
                    (w-aux-mne-dcc-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-aux-mne-dcc-dbu
                    (w-aux-mne-dcc-c01)   =    spaces
                     move   v-edt         to   v-alf
           else      move   spaces        to   v-alf
                     string v-edt
                                delimited by   spaces
                            "-" delimited by   size
                            w-aux-mne-dcc-dbu
                           (w-aux-mne-dcc-c01)
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Trattamento status                  *
      *                          *-------------------------------------*
           move      w-aux-mne-dcc-dbu
                    (w-aux-mne-dcc-c01)   to   w-aux-mne-dcc-sxd      .
           move      w-aux-mne-dcc-sbu
                    (w-aux-mne-dcc-c01)   to   w-aux-mne-dcc-sxs      .
           move      w-aux-mne-dcc-xbu
                    (w-aux-mne-dcc-c01)   to   w-aux-mne-dcc-sxc      .
           perform   aco-sts-000          thru aco-sts-999            .
      *                          *-------------------------------------*
      *                          * Status                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      w-aux-mne-dcc-sts    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-362.
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-dcc-2bu
                    (w-aux-mne-dcc-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-dcc-vbu
                    (w-aux-mne-dcc-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-dcc-lbu
                    (w-aux-mne-dcc-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Partita Iva                         *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-dcc-pbu
                    (w-aux-mne-dcc-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Mnemonico                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-aux-mne-dcc-mbu
                    (w-aux-mne-dcc-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-dcc-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-dcc-c01    <    w-aux-mne-dcc-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-dcc-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-dcc-cpa    <    w-aux-mne-dcc-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-dcc-nli    to   v-lin                  .
           move      27                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-380.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-382
           else if   v-key                =    "UP  "
                     go to aco-384
           else if   v-key                =    "DOWN"
                     go to aco-386
           else if   v-key                =    "EXIT"
                     go to aco-398
           else if   v-key                =    "NXSC"
                     go to aco-392
           else if   v-key                =    "PRSC"
                     go to aco-394
           else      go to aco-375.
       aco-382.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      w-aux-mne-dcc-cbu
                    (w-aux-mne-dcc-c01)   to   w-cod-mne-dcc-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-dcc-c01      .
           if        w-aux-mne-dcc-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-dcc-c01    <    w-aux-mne-dcc-crb
                     add   1              to   w-aux-mne-dcc-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-dcc-nli    =    12
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-vrt-000          thru aco-vrt-999            .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-dcc-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-dcc-cpa      .
       aco-396.
           move      w-aux-mne-dcc-cpa    to   w-aux-mne-dcc-c01      .
           multiply  6                    by   w-aux-mne-dcc-c01      .
           subtract  5                    from w-aux-mne-dcc-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-dcc-tpf    not  = "M"
                     go to aco-510
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di centoventi record con lo stesso  *
      *                  * mnemonico impostato                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se programma richiamabile          *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-025.
      *                      *-----------------------------------------*
      *                      * Preparazione variabile per tipo inter-  *
      *                      * rogazione                               *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "M"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Preparazione variabile per codice mne-  *
      *                      * monico                                  *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-mne"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      w-aux-mne-dcc-mmi    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * A preparazione uscita per Find precabl. *
      *                      *-----------------------------------------*
           go to     aco-125.
       aco-500.
      *              *=================================================*
      *              * Se ricerca per ragione sociale commerciale      *
      *              *-------------------------------------------------*
       aco-502.
      *                  *---------------------------------------------*
      *                  * Spaces in ragione sociale di comodo         *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-dcc-rup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-cod-mne-dcc-vln    =    zero
                     go to aco-504.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-vln    to   v-lin                  .
           move      w-cod-mne-dcc-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-504.
           if        w-cod-mne-dcc-lln    =    zero
                     go to aco-510.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-lln    to   v-lin                  .
           move      w-cod-mne-dcc-lps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-510.
      *                  *---------------------------------------------*
      *                  * Accettazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      w-aux-mne-dcc-rup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-dcc-rup      .
       aco-512.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-514.
      *                      *-----------------------------------------*
      *                      * Visualizzazione ragione sociale a spa-  *
      *                      * ces                                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Rientro ad accettazione codice          *
      *                      *-----------------------------------------*
           go to     aco-025.
       aco-514.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-516.
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-075.
       aco-516.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
       aco-517.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to aco-700.
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : rientro ad   *
      *                      * accettazione ragione sociale in upper-  *
      *                      * case                                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-rup    =    spaces
                     go to aco-510.
       aco-520.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore impostato in *
      *                      * formato privo di spaces e di caratteri  *
      *                      * diversi da A..Z - 0..9, e salvataggio   *
      *                      * del risultato                           *
      *                      *-----------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-rcr-tot-dcc-vnr      .
      *                      *-----------------------------------------*
      *                      * Se il valore normalizzato e' a spaces : *
      *                      * rientro ad accettazione ragione sociale *
      *                      * in uppercase                            *
      *                      *-----------------------------------------*
           if        w-rcr-tot-dcc-vnr    =    spaces
                     go to aco-510.
       aco-530.
      *                      *=========================================*
      *                      * Esecuzione ricerca totale               *
      *                      *-----------------------------------------*
           perform   aco-fnd-000          thru aco-fnd-999            .
      *                          *-------------------------------------*
      *                          * A controllo su numero records tro-  *
      *                          * vati                                *
      *                          *-------------------------------------*
           go to     aco-250.
       aco-700.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : rientro ad   *
      *                      * accettazione codice                     *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-rup    =    spaces or
                     w-aux-mne-dcc-rup    =    "*"
                     go to aco-025.
       aco-720.
      *                  *---------------------------------------------*
      *                  * Determinazione se impostazione pari a :     *
      *                  *                                             *
      *                  * - Ragione sociale                           *
      *                  *                                             *
      *                  * - Partita iva                               *
      *                  * - Codice fiscale                            *
      *                  * - Codice fiscale cliente estero             *
      *                  *                                             *
      *                  * - Numero telefonico                         *
      *                  * - Indirizzo e-mail                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se partita iva                     *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-p00    not  numeric
                     go to aco-740.
           if        w-aux-mne-dcc-p99    =    spaces
                     move  "P"            to   w-aux-mne-dcc-tpf
                     go to aco-780.
       aco-740.
      *                      *-----------------------------------------*
      *                      * Test se codice fiscale                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-dcc-c01      .
           inspect   w-aux-mne-dcc-f00
                                      tallying w-aux-mne-dcc-c01
                                          for  all   " "              .
           if        w-aux-mne-dcc-c01    >    zero
                     go to aco-750.
           if        w-aux-mne-dcc-f01    not  alphabetic or
                     w-aux-mne-dcc-f02    not  alphabetic or
                     w-aux-mne-dcc-f03    not  numeric    or
                     w-aux-mne-dcc-f04    not  alphabetic or
                     w-aux-mne-dcc-f05    not  numeric    or
                     w-aux-mne-dcc-f06    not  alphabetic or
                     w-aux-mne-dcc-f07    not  numeric    or
                     w-aux-mne-dcc-f08    not  alphabetic
                     go to aco-750.
           if        w-aux-mne-dcc-f99    =    spaces
                     move  "F"            to   w-aux-mne-dcc-tpf
                     go to aco-780.
       aco-750.
      *                      *-----------------------------------------*
      *                      * Test se codice fiscale estero           *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-e01    not  alphabetic or
                     w-aux-mne-dcc-e03    not  numeric
                     go to aco-760.
           if        w-aux-mne-dcc-e99    =    spaces
                     move  "F"            to   w-aux-mne-dcc-tpf
                     go to aco-780.
       aco-760.
      *                      *-----------------------------------------*
      *                      * Test se telefono                        *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-t00    not  = "."   or
                     w-aux-mne-dcc-t01    not  numeric
                     go to aco-765.
           move      "T"                  to   w-aux-mne-dcc-tpf      .
           go to     aco-780.
       aco-765.
      *                      *-----------------------------------------*
      *                      * Test se indirizzo e-mail                *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-dcc-c09      .
           inspect   w-aux-mne-dcc-rup 
                                      tallying w-aux-mne-dcc-c09
                                          for  all "@"                .
           if        w-aux-mne-dcc-c09    =    1
                     move  "E"            to   w-aux-mne-dcc-tpf
                     go to aco-780.
       aco-770.
      *                  *---------------------------------------------*
      *                  * Se ragione sociale                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il primo carattere digitato e' '*',  *
      *                      * significa che la ricerca deve essere    *
      *                      * estesa anche alla ragione sociale delle *
      *                      * dipendenze                              *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-rup
                    (01 : 01)             =    "*"
                     move  "D"            to   w-aux-mne-dcc-tpf
                     move  w-aux-mne-dcc-rup
                          (02 : 19)       to   w-aux-mne-dcc-rup
           else      move  "R"            to   w-aux-mne-dcc-tpf      .
       aco-780.
      *              *=================================================*
      *              * Se ricerca per elementi particolari             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine in funzione del tipo ricerca     *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-tpf    =    "P"
                     perform  aco-piv-000 thru aco-piv-999
           else if   w-aux-mne-dcc-tpf    =    "F"
                     perform  aco-cfi-000 thru aco-cfi-999
           else if   w-aux-mne-dcc-tpf    =    "T"
                     perform  aco-tel-000 thru aco-tel-999
           else if   w-aux-mne-dcc-tpf    =    "E"
                     perform  aco-eml-000 thru aco-eml-999
           else      perform  aco-rag-000 thru aco-rag-999            .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-fxr    =    spaces
                     go to aco-125
           else if   w-aux-mne-dcc-fxr    =    "A"
                     go to aco-510
           else if   w-aux-mne-dcc-fxr    =    "C"
                     go to aco-250
           else      go to aco-125.
       aco-800.
      *              *=================================================*
      *              * Se ricerca per ragione sociale contabile        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   aco-rsc-000          thru aco-rsc-999            .
      *                  *---------------------------------------------*
      *                  * Test sul flag di uscita                     *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-fxr    =    spaces
                     go to aco-125
           else if   w-aux-mne-dcc-fxr    =    "X"
                     go to aco-075
           else if   w-aux-mne-dcc-fxr    =    "U"
                     go to aco-025
           else if   w-aux-mne-dcc-fxr    =    "C"
                     go to aco-250
           else      go to aco-125.
       aco-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione ricerca totale                                 *
      *    *-----------------------------------------------------------*
       aco-fnd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-dcc-fxr      .
       aco-fnd-100.
      *              *-------------------------------------------------*
      *              * Esecuzione ricerca totale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri iniziali                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca                     *
      *                          *-------------------------------------*
           move      "X"                  to   w-aux-mne-dcc-tpf      .
       aco-fnd-535.
      *                          *-------------------------------------*
      *                          * Salvataggio immagine video          *
      *                          *-------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box superiore                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      06                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Literal nel box                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "                        Ricerca clienti in esecuzi
      -              "one                         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box inferiore                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Literal nel box                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "Numero clienti esaminati.......:           Clienti
      -              " selezionati.....:          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Literal nel box                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "            Per interrompere la ricerca premere il
      -              " tasto di uscita            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box centrale, parte sinistra        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      40                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box centrale, parte destra          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnd-540.
      *                          *-------------------------------------*
      *                          * Numero records memorizzati in file  *
      *                          * relative di appoggio [rlt] : zero   *
      *                          *-------------------------------------*
           move      zero                 to   w-rlt-num              .
      *                          *-------------------------------------*
      *                          * Numero record attualmente in trat-  *
      *                          * tamento del file relative di appog- *
      *                          * gio [rlt] : zero                    *
      *                          *-------------------------------------*
           move      zero                 to   w-rlt-att              .
      *                          *-------------------------------------*
      *                          * Flag di fine ricerca : No           *
      *                          *-------------------------------------*
           move      spaces               to   w-rcr-tot-dcc-ffr      .
      *                          *-------------------------------------*
      *                          * Flag di selezione effettuata : No   *
      *                          *-------------------------------------*
           move      spaces               to   w-rcr-tot-dcc-fsl      .
      *                          *-------------------------------------*
      *                          * Contatore records esaminati : a ze- *
      *                          * ro                                  *
      *                          *-------------------------------------*
           move      zero                 to   w-rcr-tot-dcc-cre      .
      *                          *-------------------------------------*
      *                          * Contatore records selezionati : a   *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-rcr-tot-dcc-crs      .
      *                          *-------------------------------------*
      *                          * Contatore records nel buffer : a    *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
       aco-fnd-550.
      *                          *-------------------------------------*
      *                          * Start su [dcc] per ragione sociale  *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      spaces               to   rf-dcc-rag-key         .
           move      zero                 to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                          *-------------------------------------*
      *                          * Se Start errata : a fine trattamen- *
      *                          * to                                  *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnd-660.
       aco-fnd-555.
      *                          *-------------------------------------*
      *                          * Test se l'utente ha premuto uno dei *
      *                          * seguenti tasti :                    *
      *                          * - Exit : sempre ammesso             *
      *                          * - Slct o                            *
      *                          *   Rtrn : solo se in questo momento  *
      *                          *          c'e' un record in tratta-  *
      *                          *          mento del file relative    *
      *                          *          di appoggio [rlt]          *
      *                          * - Up   o                            *
      *                          *   Prsc : solo se in questo momento  *
      *                          *          c'e' un record in tratta-  *
      *                          *          mento del file relative    *
      *                          *          di appoggio [rlt], che non *
      *                          *          e' il primo                *
      *                          * - Down o                            *
      *                          *   Nxsc : solo se in questo momento  *
      *                          *          c'e' un record in tratta-  *
      *                          *          mento del file relative    *
      *                          *          di appoggio [rlt], che non *
      *                          *          e' l'ultimo                *
      *                          *-------------------------------------*
           move      "AA"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      08                   to   v-pos                  .
           if        w-rlt-att            >    1
                     move  "UP  "         to   v-pfk (01)
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-rlt-att            <    w-rlt-num
                     move  "DOWN"         to   v-pfk (02)
                     move  "NXSC"         to   v-pfk (08)             .
           if        w-rlt-att            not  = zero
                     move  "SLCT"         to   v-pfk (10)
                     move  "RTRN"         to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        v-key                not  = spaces
                     go to aco-fnd-670.
       aco-fnd-560.
      *                          *-------------------------------------*
      *                          * Se flag di fine ricerca in On : in- *
      *                          * vece di eseguire la lettura si ri-  *
      *                          * cicla per vedere quale tasto fun-   *
      *                          * zione digita l'utente               *
      *                          *-------------------------------------*
           if        w-rcr-tot-dcc-ffr    =    "S"
                     go to aco-fnd-555.
      *                          *-------------------------------------*
      *                          * Read Next su [dcc] per ragione so-  *
      *                          * ciale                               *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                          *-------------------------------------*
      *                          * Se At End : a fine trattamento      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnd-660.
       aco-fnd-565.
      *                          *-------------------------------------*
      *                          * Se non ricerca totale : si esclude  *
      *                          * il trattamento dell'anagrafica con- *
      *                          * tabile                              *
      *                          *-------------------------------------*
           if        w-rcr-tot-dcc-pf1    not  = "T"
                     move  spaces         to   rf-cli
                     go to aco-fnd-570.
      *                          *-------------------------------------*
      *                          * Lettura record [cli] corrispondente *
      *                          * e, se record non esistente : record *
      *                          * a spaces                            *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-dcc-cod-cli       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                          *-------------------------------------*
      *                          * Se record non esistente : lo si po- *
      *                          * ne tutto a spaces                   *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-cli                 .
       aco-fnd-570.
      *                          *-------------------------------------*
      *                          * Incremento e visualizzazione del    *
      *                          * numero records esaminati            *
      *                          *-------------------------------------*
       aco-fnd-571.
           add       1                    to   w-rcr-tot-dcc-cre      .
       aco-fnd-572.
           if        w-rcr-tot-dcc-cre    not  < 100
                     move  w-rcr-tot-dcc-cre
                                          to   w-rcr-tot-dcc-wr2
                     if    w-rcr-tot-dcc-wr2
                                          =    zero
                           go to aco-fnd-573
                     else  go to aco-fnd-575.
           if        w-rcr-tot-dcc-cre    not  < 10
                     move  w-rcr-tot-dcc-cre
                                          to   w-rcr-tot-dcc-wr1
                     if    w-rcr-tot-dcc-wr1
                                          =    zero
                           go to aco-fnd-573
                     else  go to aco-fnd-575.
       aco-fnd-573.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rcr-tot-dcc-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnd-575.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * ragione sociale in anagrafica com-  *
      *                          * merciale                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-rag-soc       =    spaces
                     go to aco-fnd-577.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-rag-soc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 01                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  01             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-577.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * ragione sociale 1. per invio docu-  *
      *                          * menti in anagrafica commerciale     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-rs1-doc       =    spaces
                     go to aco-fnd-579.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-rs1-doc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 11                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  11             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-579.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * ragione sociale 2. per invio docu-  *
      *                          * menti in anagrafica commerciale     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-rs2-doc       =    spaces
                     go to aco-fnd-580.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-rs2-doc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 12                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  12             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-580.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per       *
      *                          * l'indirizzo in anagrafica commer-   *
      *                          * ciale                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-via-dcc       =    spaces
                     go to aco-fnd-581.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-via-dcc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 12                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  13             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-581.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per       *
      *                          * la localita' in anagrafica commer-  *
      *                          * ciale                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-loc-dcc       =    spaces
                     go to aco-fnd-583.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-loc-dcc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 12                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  13             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-583.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * nota generica 1. in anagrafica com- *
      *                          * merciale                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se si tratta di una dipendenza  *
      *                              * non si esegue il test, in quan- *
      *                              * to il valore e' condiviso con   *
      *                              * l'anagrafica principale         *
      *                              *---------------------------------*
           if        rf-dcc-dpz-cli       not  = spaces
                     go to aco-fnd-584.
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-not-g01       =    spaces
                     go to aco-fnd-584.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-not-g01       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 21                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  21             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-584.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * nota generica 2. in anagrafica com- *
      *                          * merciale                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se si tratta di una dipendenza  *
      *                              * non si esegue il test, in quan- *
      *                              * to il valore ' condiviso con    *
      *                              * l'anagrafica principale         *
      *                              *---------------------------------*
           if        rf-dcc-dpz-cli       not  = spaces
                     go to aco-fnd-585.
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-not-g02       =    spaces
                     go to aco-fnd-585.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-not-g02       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 21                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  22             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-585.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * nota generica 3. in anagrafica com- *
      *                          * merciale                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcc-not-g03       =    spaces
                     go to aco-fnd-587.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcc-not-g03       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 21                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  23             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-587.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per l'in- *
      *                          * terlocutore principale in anagrafi- *
      *                          * ca commerciale                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura dati correlati          *
      *                              *---------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      02                   to   d-con-arc-tip-arc      .
           move      rf-dcc-cod-cli       to   d-con-arc-cod-arc      .
           move      rf-dcc-dpz-cli       to   d-con-arc-dpz-arc      .
           move      spaces               to   d-con-arc-tip-sel      .
           perform   con-arc-tip-cll-000  thru con-arc-tip-cll-999    .
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        d-con-arc-int-con (01)
                                          =    spaces
                     go to aco-fnd-589.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      d-con-arc-int-con (01)
                                          to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 21                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  31             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-589.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per l'in- *
      *                          * terlocutore alternativo in anagra-  *
      *                          * fica commerciale                    *
      *                          *                                     *
      *                          * N.B.: Non piu' gestito              *
      *                          *-------------------------------------*
       aco-fnd-600.
      *                          *-------------------------------------*
      *                          * Se la scheda contabile non e' stata *
      *                          * trovata : si omettono tutti i tests *
      *                          * sull'anagrafica contabile           *
      *                          *-------------------------------------*
           if        rf-cli               =    spaces
                     go to aco-fnd-620.
       aco-fnd-602.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * ragione sociale in anagrafica con-  *
      *                          * tabile                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-cli-rag-soc       =    spaces
                     go to aco-fnd-604.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-cli-rag-soc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 21                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  51             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-604.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per l'in- *
      *                          * terlocutore in anagrafica contabile *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura dati correlati          *
      *                              *---------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      01                   to   d-con-arc-tip-arc      .
           move      rf-cli-cod-cli       to   d-con-arc-cod-arc      .
           move      spaces               to   d-con-arc-dpz-arc      .
           move      spaces               to   d-con-arc-tip-sel      .
           perform   con-arc-tip-cll-000  thru con-arc-tip-cll-999    .
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        d-con-arc-int-con (01)
                                          =    spaces
                     go to aco-fnd-620.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      d-con-arc-int-con (01)
                                          to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcc-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 21                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  81             to   w-rcr-tot-dcc-mch
                     go to aco-fnd-625.
       aco-fnd-620.
      *                          *-------------------------------------*
      *                          * Se non c'e' stato alcun match : si  *
      *                          * ricicla su anagrafica cliente com-  *
      *                          * merciale successiva                 *
      *                          *-------------------------------------*
           go to     aco-fnd-555.
       aco-fnd-625.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record              *
      *                          *-------------------------------------*
       aco-fnd-630.
      *                              *---------------------------------*
      *                              * Visualizzazione, se non gia' e- *
      *                              * seguita, del numero records se- *
      *                              * lezionati                       *
      *                              *---------------------------------*
       aco-fnd-631.
           if        w-rcr-tot-dcc-cre    not  < 100
                     move  w-rcr-tot-dcc-cre
                                          to   w-rcr-tot-dcc-wr2
                     if    w-rcr-tot-dcc-wr2
                                          =    zero
                           go to aco-fnd-633
                     else  go to aco-fnd-632.
           if        w-rcr-tot-dcc-cre    not  < 10
                     move  w-rcr-tot-dcc-cre
                                          to   w-rcr-tot-dcc-wr1
                     if    w-rcr-tot-dcc-wr1
                                          =    zero
                           go to aco-fnd-633
                     else  go to aco-fnd-632.
           go to     aco-fnd-633.
       aco-fnd-632.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rcr-tot-dcc-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnd-633.
      *                          *-------------------------------------*
      *                          * Incremento e visualizzazione del    *
      *                          * numero records selezionati          *
      *                          *-------------------------------------*
           add       1                    to   w-rcr-tot-dcc-crs      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-rcr-tot-dcc-crs    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnd-635.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer, a meno di non aver raggiunto   *
      *                          * il massimo                          *
      *                          *-------------------------------------*
           if        w-aux-mne-dcc-crb    not  < w-aux-mne-dcc-max
                     go to aco-fnd-640.
           add       1                    to   w-aux-mne-dcc-crb      .
       aco-fnd-640.
      *                          *-------------------------------------*
      *                          * Bufferizzazione vera e propria, con *
      *                          * indice di tipo match, a meno di non *
      *                          * aver raggiunto il massimo           *
      *                          *-------------------------------------*
           if        w-aux-mne-dcc-crb    not  < w-aux-mne-dcc-max
                     go to aco-fnd-645.
           move      w-rcr-tot-dcc-mch    to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-cod-cli       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-dpz-cli       to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
       aco-fnd-645.
      *                          *-------------------------------------*
      *                          * Incremento numero di records memo-  *
      *                          * rizzati nel file relative di appog- *
      *                          * gio [rlt]                           *
      *                          *-------------------------------------*
           add       1                    to   w-rlt-num              .
      *                          *-------------------------------------*
      *                          * Scrittura file relative di appoggio *
      *                          * [rlt]                               *
      *                          *-------------------------------------*
           move      w-rlt-num            to   w-rlt-krn              .
           move      spaces               to   rlt-rec                .
           move      rf-dcc               to   rlt-rec-rec-dcc        .
           move      rf-cli               to   rlt-rec-rec-cli        .
           perform   rlt-put-000          thru rlt-put-999            .
      *                          *-------------------------------------*
      *                          * Se e' il primo record scritto, lo   *
      *                          * si visualizza                       *
      *                          *-------------------------------------*
           if        w-rlt-num            >    1
                     go to aco-fnd-650.
           move      1                    to   w-rlt-att              .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           perform   aco-vrc-000          thru aco-vrc-999            .
       aco-fnd-650.
      *                          *-------------------------------------*
      *                          * Riciclo a lettura anagrafica com-   *
      *                          * merciale successiva                 *
      *                          *-------------------------------------*
           go to     aco-fnd-555.
       aco-fnd-660.
      *                          *-------------------------------------*
      *                          * Fine lettura                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione del numero re-  *
      *                              * cords esaminati                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rcr-tot-dcc-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Flag di fine ricerca : Si       *
      *                              *---------------------------------*
           move      "S"                  to   w-rcr-tot-dcc-ffr      .
      *                              *---------------------------------*
      *                              * Literal nel box inferiore per   *
      *                              * fine ricerca                    *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "                                 FINE RICERCA     
      -              "                            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * funzione per dar modo all'ope-  *
      *                              * ratore di effettuare una scel-  *
      *                              * ta ben precisa                  *
      *                              *---------------------------------*
           go to     aco-fnd-555.
       aco-fnd-665.
      *                          *-------------------------------------*
      *                          * Fine ricerca totale                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazione function key    *
      *                              *---------------------------------*
           move      spaces               to   v-key                  .
      *                              *---------------------------------*
      *                              * Ripristino immagine video       *
      *                              *---------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * A controllo su numero records   *
      *                              * trovati                         *
      *                              *---------------------------------*
           move      "C"                  to   w-aux-mne-dcc-fxr      .
           go to     aco-fnd-900.
       aco-fnd-670.
      *                          *-------------------------------------*
      *                          * Trattamento tasti funzione asin-    *
      *                          * croni                               *
      *                          *-------------------------------------*
       aco-fnd-671.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tasto  *
      *                              * funzione premuto                *
      *                              *---------------------------------*
           if        v-key                =    "UP  " or
                     v-key                =    "PRSC"
                     go to aco-fnd-672
           else if   v-key                =    "DOWN" or
                     v-key                =    "NXSC"
                     go to aco-fnd-673
           else if   v-key                =    "SLCT" or
                     v-key                =    "RTRN"
                     go to aco-fnd-674
           else if   v-key                =    "EXIT"
                     go to aco-fnd-675
           else      go to aco-fnd-560.
       aco-fnd-672.
      *                              *---------------------------------*
      *                              * Se Up o Prsc                    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero record    *
      *                                  * del file relative di ap-    *
      *                                  * poggio [rlt] attualmente    *
      *                                  * in trattamento              *
      *                                  *-----------------------------*
           subtract  1                    from   w-rlt-att            .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-dcc      to   rf-dcc                 .
           move      rlt-rec-rec-cli      to   rf-cli                 .
      *                                  *-----------------------------*
      *                                  * Visualizzazione record del  *
      *                                  * file relative di appoggio   *
      *                                  * [rlt] attualmente in trat-  *
      *                                  * tamento                     *
      *                                  *-----------------------------*
           perform   aco-vrc-000          thru aco-vrc-999            .
      *                                  *-----------------------------*
      *                                  * Continuazione lettura       *
      *                                  *-----------------------------*
           go to     aco-fnd-560.
       aco-fnd-673.
      *                              *---------------------------------*
      *                              * Se Down o Nxsc                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero record    *
      *                                  * del file relative di ap-    *
      *                                  * poggio [rlt] attualmente    *
      *                                  * in trattamento              *
      *                                  *-----------------------------*
           add       1                    to   w-rlt-att              .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-dcc      to   rf-dcc                 .
           move      rlt-rec-rec-cli      to   rf-cli                 .
      *                                  *-----------------------------*
      *                                  * Visualizzazione record del  *
      *                                  * file relative di appoggio   *
      *                                  * [rlt] attualmente in trat-  *
      *                                  * tamento                     *
      *                                  *-----------------------------*
           perform   aco-vrc-000          thru aco-vrc-999            .
      *                                  *-----------------------------*
      *                                  * Continuazione lettura       *
      *                                  *-----------------------------*
           go to     aco-fnd-560.
       aco-fnd-674.
      *                              *---------------------------------*
      *                              * Se Slct o Return                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Flag di selezione effettua- *
      *                                  * ta : Si                     *
      *                                  *-----------------------------*
           move      "S"                  to   w-rcr-tot-dcc-fsl      .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-dcc      to   rf-dcc                 .
           move      rlt-rec-rec-cli      to   rf-cli                 .
      *                                  *-----------------------------*
      *                                  * Si esegue una forzatura co- *
      *                                  * me se si fosse rintracciato *
      *                                  * un solo record, pari al re- *
      *                                  * cord selezionato            *
      *                                  *-----------------------------*
           move      1                    to   w-aux-mne-dcc-crb      .
           move      rf-dcc-cod-cli       to   w-aux-mne-dcc-cbu (1)  .
      *                                  *-----------------------------*
      *                                  * A fine ricerca totale       *
      *                                  *-----------------------------*
           go to     aco-fnd-665.
       aco-fnd-675.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * A fine ricerca totale       *
      *                                  *-----------------------------*
           go to     aco-fnd-665.
       aco-fnd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-fnd-999.
       aco-fnd-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione per ragione sociale commerciale                *
      *    *-----------------------------------------------------------*
       aco-rag-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       aco-rag-050.
      *                  *---------------------------------------------*
      *                  * Preparazione limite massimo                 *
      *                  *---------------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-aux-mne-dcc-rmx      .
           move      40                   to   w-aux-mne-dcc-c01      .
       aco-rag-060.
           if        w-aux-mne-dcc-c01    >    zero
                     if    w-aux-mne-dcc-rch
                          (w-aux-mne-dcc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-dcc-rch
                                              (w-aux-mne-dcc-c01)
                           subtract 1     from w-aux-mne-dcc-c01
                           go to    aco-rag-060.
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
       aco-rag-100.
      *              *-------------------------------------------------*
      *              * Start su [dcc]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      w-aux-mne-dcc-rup    to   rf-dcc-rag-key         .
           move      zero                 to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rag-900.
       aco-rag-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [dcc]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rag-900.
       aco-rag-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max [dcc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su ragione sociale                     *
      *                  *---------------------------------------------*
           if        rf-dcc-rag-key       >    w-aux-mne-dcc-rmx
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rag-900.
       aco-rag-400.
      *              *-------------------------------------------------*
      *              * Selezione sul record [dcc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-tpf    =    "D"
                     go to aco-rag-420.
           if        rf-dcc-dpz-cli       not  = spaces
                     go to aco-rag-200.
       aco-rag-420.
      *                  *---------------------------------------------*
      *                  * Test su status cliente                      *
      *                  *---------------------------------------------*
           if        w-prs-ttr-sts        =    "E" and
                     rf-dcc-sta-tus       not  = 01
                     go to aco-rag-200.
       aco-rag-440.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [cli]                *
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
           move      "CODCLI   "          to   f-key                  .
           move      rf-dcc-cod-cli       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       aco-rag-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Test se piu' di max records letti con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-rag-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-cli       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-dpz-cli       to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-rag-200.
       aco-rag-800.
      *              *-------------------------------------------------*
      *              * Se piu' di centoventi record trovati            *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "A"            to   w-aux-mne-dcc-fxr
                     go to aco-rag-900.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-dcc-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       aco-rag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-rag-999.
       aco-rag-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione per Partita Iva                                *
      *    *-----------------------------------------------------------*
       aco-piv-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       aco-piv-050.
      *                  *---------------------------------------------*
      *                  * Preparazione limite massimo                 *
      *                  *---------------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-aux-mne-dcc-rmx      .
           move      40                   to   w-aux-mne-dcc-c01      .
       aco-piv-060.
           if        w-aux-mne-dcc-c01    >    zero
                     if    w-aux-mne-dcc-rch
                          (w-aux-mne-dcc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-dcc-rch
                                              (w-aux-mne-dcc-c01)
                           subtract 1     from w-aux-mne-dcc-c01
                           go to    aco-piv-060.
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
       aco-piv-100.
      *              *-------------------------------------------------*
      *              * Start su [cli]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRTIVA    "         to   f-key                  .
           move      w-aux-mne-dcc-p00    to   rf-cli-prt-iva         .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-piv-900.
       aco-piv-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [cli]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-piv-900.
       aco-piv-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max [cli]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su partita iva                         *
      *                  *---------------------------------------------*
           if        rf-cli-prt-iva       =    w-aux-mne-dcc-p00
                     go to aco-piv-400
           else      move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-piv-900.
       aco-piv-400.
      *              *-------------------------------------------------*
      *              * Selezione sul record [cli]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI   "          to   f-key                  .
           move      rf-cli-cod-cli       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Flag esito operazione                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-aux-mne-dcc-fes
           else      move  "N"            to   w-aux-mne-dcc-fes      .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni se record non esistente *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-fes    not  = spaces
                     move  rf-cli-rag-soc
                                          to   rf-dcc-rag-soc
                     move  rf-cli-via-cli
                                          to   rf-dcc-via-dcc
                     move  rf-cli-loc-cli
                                          to   rf-dcc-loc-dcc         .
      *                  *---------------------------------------------*
      *                  * Test su status cliente                      *
      *                  *---------------------------------------------*
           if        w-prs-ttr-sts        =    "E" and
                     rf-dcc-sta-tus       not  = 01
                     go to aco-piv-200.
       aco-piv-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Test se piu' di max records letti con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-piv-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-cli       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-dpz-cli       to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunti il centoventesimo record   *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-crb    =    w-aux-mne-dcc-max
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-piv-900
           else      go to aco-piv-200.
       aco-piv-800.
      *              *-------------------------------------------------*
      *              * Se piu' di centoventi record trovati            *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "A"            to   w-aux-mne-dcc-fxr
                     go to aco-piv-900.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-dcc-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       aco-piv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-piv-999.
       aco-piv-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione per Codice Fiscale                             *
      *    *-----------------------------------------------------------*
       aco-cfi-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       aco-cfi-050.
      *                  *---------------------------------------------*
      *                  * Preparazione limite massimo                 *
      *                  *---------------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-aux-mne-dcc-rmx      .
           move      40                   to   w-aux-mne-dcc-c01      .
       aco-cfi-060.
           if        w-aux-mne-dcc-c01    >    zero
                     if    w-aux-mne-dcc-rch
                          (w-aux-mne-dcc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-dcc-rch
                                              (w-aux-mne-dcc-c01)
                           subtract 1     from w-aux-mne-dcc-c01
                           go to    aco-cfi-060.
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
       aco-cfi-100.
      *              *-------------------------------------------------*
      *              * Start su [cli]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFIS    "         to   f-key                  .
           move      w-aux-mne-dcc-f00    to   rf-cli-cod-fis         .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-cfi-900.
       aco-cfi-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [cli]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-cfi-900.
       aco-cfi-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max [cli]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice fiscale                      *
      *                  *---------------------------------------------*
           if        rf-cli-cod-fis       =    w-aux-mne-dcc-f00
                     go to aco-cfi-400
           else      move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-cfi-900.
       aco-cfi-400.
      *              *-------------------------------------------------*
      *              * Selezione sul record [cli]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI   "          to   f-key                  .
           move      rf-cli-cod-cli       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Flag esito operazione                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-aux-mne-dcc-fes
           else      move  "N"            to   w-aux-mne-dcc-fes      .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni se record non esistente *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-fes    not  = spaces
                     move  rf-cli-rag-soc
                                          to   rf-dcc-rag-soc
                     move  rf-cli-via-cli
                                          to   rf-dcc-via-dcc
                     move  rf-cli-loc-cli
                                          to   rf-dcc-loc-dcc         .
      *                  *---------------------------------------------*
      *                  * Test su status cliente                      *
      *                  *---------------------------------------------*
           if        w-prs-ttr-sts        =    "E" and
                     rf-dcc-sta-tus       not  = 01
                     go to aco-cfi-200.
       aco-cfi-500.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Test se piu' di max records letti con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-cfi-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-cli       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-dpz-cli       to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunti il centoventesimo record   *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-crb    =    w-aux-mne-dcc-max
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-cfi-900
           else      go to aco-cfi-200.
       aco-cfi-800.
      *              *-------------------------------------------------*
      *              * Se piu' di centoventi record trovati            *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "A"            to   w-aux-mne-dcc-fxr
                     go to aco-cfi-900.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-dcc-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       aco-cfi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-cfi-999.
       aco-cfi-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione per Utenza telefonica                          *
      *    *-----------------------------------------------------------*
       aco-tel-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       aco-tel-050.
      *                  *---------------------------------------------*
      *                  * Preparazione valore                         *
      *                  *---------------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-aux-mne-dcc-rmx      .
           move      w-aux-mne-dcc-rmx
                    (02 : 39)             to   w-aux-mne-dcc-rup      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodi per rottura          *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-rta      .
           move      zero                 to   w-aux-mne-dcc-rca      .
           move      spaces               to   w-aux-mne-dcc-rda      .
       aco-tel-100.
      *              *-------------------------------------------------*
      *              * Start su [adc]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMARC    "         to   f-key                  .
           move      "TEL"                to   rf-adc-tip-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      w-aux-mne-dcc-rup    to   rf-adc-num-con         .
           move      zero                 to   rf-adc-tip-arc         .
           move      zero                 to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-tel-900.
       aco-tel-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [adc]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-tel-900.
       aco-tel-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max [adc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-adc-tip-con       not  = "TEL"             or
                     rf-adc-num-con       not  = w-aux-mne-dcc-rup
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-tel-900.
       aco-tel-400.
      *              *-------------------------------------------------*
      *              * Selezione sul record [adc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = 02
                     go to aco-tel-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI   "          to   f-key                  .
           move      rf-adc-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Flag esito operazione                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-aux-mne-dcc-fes
           else      move  "N"            to   w-aux-mne-dcc-fes      .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni se record non esistente *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-fes    not  = spaces
                     move  rf-adc-des-key
                                          to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Test su status cliente                      *
      *                  *---------------------------------------------*
           if        w-prs-ttr-sts        =    "E" and
                     rf-dcc-sta-tus       not  = 01
                     go to aco-tel-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [cli]                *
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
           move      "CODCLI   "          to   f-key                  .
           move      rf-adc-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       aco-tel-500.
      *              *-------------------------------------------------*
      *              * Test su elementi di rottura                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se zero                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-rta    =    zero and
                     w-aux-mne-dcc-rca    =    zero and
                     w-aux-mne-dcc-rda    =    spaces
                     go to aco-tel-520.
      *                  *---------------------------------------------*
      *                  * Confronto                                   *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-rta    =    rf-adc-tip-arc and
                     w-aux-mne-dcc-rca    =    rf-adc-cod-arc and
                     w-aux-mne-dcc-rda    =    rf-adc-dpz-arc
                     go to aco-tel-200.
       aco-tel-520.
      *              *-------------------------------------------------*
      *              * Comodi per rottura                              *
      *              *-------------------------------------------------*
           move      rf-adc-tip-arc       to   w-aux-mne-dcc-rta      .
           move      rf-adc-cod-arc       to   w-aux-mne-dcc-rca      .
           move      rf-adc-dpz-arc       to   w-aux-mne-dcc-rda      .
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Test se piu' di max records letti con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-tel-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-adc-cod-arc       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-adc-dpz-arc       to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunti il centoventesimo record   *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-crb    =    w-aux-mne-dcc-max
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-tel-900
           else      go to aco-tel-200.
       aco-tel-800.
      *              *-------------------------------------------------*
      *              * Se piu' di centoventi record trovati            *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "A"            to   w-aux-mne-dcc-fxr
                     go to aco-tel-900.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-dcc-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       aco-tel-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-tel-999.
       aco-tel-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione per Indirizzo e-mail                           *
      *    *-----------------------------------------------------------*
       aco-eml-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trasformazione in lowercase                 *
      *                  *---------------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-low-000      thru all-str-low-999        .
           move      w-all-str-alf        to   w-aux-mne-dcc-rup      .
      *                  *---------------------------------------------*
      *                  * Azzeramento contatore records nel buffer    *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodi per rottura          *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-rta      .
           move      zero                 to   w-aux-mne-dcc-rca      .
           move      spaces               to   w-aux-mne-dcc-rda      .
       aco-eml-100.
      *              *-------------------------------------------------*
      *              * Start su [adc]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMARC    "         to   f-key                  .
           move      "EML"                to   rf-adc-tip-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      w-aux-mne-dcc-rup    to   rf-adc-num-con         .
           move      zero                 to   rf-adc-tip-arc         .
           move      zero                 to   rf-adc-cod-arc         .
           move      spaces               to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-eml-900.
       aco-eml-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [adc]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-eml-900.
       aco-eml-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max [cli]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-adc-tip-con       not  = "EML"             or
                     rf-adc-num-con       not  = w-aux-mne-dcc-rup
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-eml-900.
       aco-eml-400.
      *              *-------------------------------------------------*
      *              * Selezione sul record [adc]                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = 02
                     go to aco-eml-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI   "          to   f-key                  .
           move      rf-adc-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Flag esito operazione                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-aux-mne-dcc-fes
           else      move  "N"            to   w-aux-mne-dcc-fes      .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni se record non esistente *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-fes    not  = spaces
                     move  rf-adc-des-key
                                          to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Test su status cliente                      *
      *                  *---------------------------------------------*
           if        w-prs-ttr-sts        =    "E" and
                     rf-dcc-sta-tus       not  = 01
                     go to aco-eml-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [cli]                *
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
           move      "CODCLI   "          to   f-key                  .
           move      rf-adc-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       aco-eml-500.
      *              *-------------------------------------------------*
      *              * Test su elementi di rottura                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se zero                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-rta    =    zero and
                     w-aux-mne-dcc-rca    =    zero and
                     w-aux-mne-dcc-rda    =    spaces
                     go to aco-eml-520.
      *                  *---------------------------------------------*
      *                  * Confronto                                   *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-rta    =    rf-adc-tip-arc and
                     w-aux-mne-dcc-rca    =    rf-adc-cod-arc and
                     w-aux-mne-dcc-rda    =    rf-adc-dpz-arc
                     go to aco-eml-200.
       aco-eml-520.
      *              *-------------------------------------------------*
      *              * Comodi per rottura                              *
      *              *-------------------------------------------------*
           move      rf-adc-tip-arc       to   w-aux-mne-dcc-rta      .
           move      rf-adc-cod-arc       to   w-aux-mne-dcc-rca      .
           move      rf-adc-dpz-arc       to   w-aux-mne-dcc-rda      .
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                  *---------------------------------------------*
      *                  * Test se piu' di max records letti con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-eml-800.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-adc-cod-arc       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-adc-dpz-arc       to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunti il centoventesimo record   *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-crb    =    w-aux-mne-dcc-max
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-eml-900
           else      go to aco-eml-200.
       aco-eml-800.
      *              *-------------------------------------------------*
      *              * Se piu' di centoventi record trovati            *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "A"            to   w-aux-mne-dcc-fxr
                     go to aco-eml-900.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-dcc-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       aco-eml-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-eml-999.
       aco-eml-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Se ricerca per ragione sociale contabile                  *
      *    *-----------------------------------------------------------*
       aco-rsc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-dcc-fxr      .
      *                  *---------------------------------------------*
      *                  * Tipo di ricerca                             *
      *                  *---------------------------------------------*
           move      "C"                  to   w-aux-mne-dcc-tpf      .
      *                  *---------------------------------------------*
      *                  * Spaces in ragione sociale di comodo         *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-dcc-rup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-cod-mne-dcc-vln    =    zero
                     go to aco-rsc-810.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-vln    to   v-lin                  .
           move      w-cod-mne-dcc-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-rsc-810.
           if        w-cod-mne-dcc-lln    =    zero
                     go to aco-rsc-815.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-lln    to   v-lin                  .
           move      w-cod-mne-dcc-lps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-rsc-815.
      *                  *---------------------------------------------*
      *                  * Accettazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      w-aux-mne-dcc-rup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-dcc-rup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-rsc-820.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-dcc-rln    to   v-lin                  .
           move      w-cod-mne-dcc-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Uscita con flag a 'Up'                      *
      *                  *---------------------------------------------*
           move      "U"                  to   w-aux-mne-dcc-fxr      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     aco-rsc-900.
       aco-rsc-820.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     move  "X"            to   w-aux-mne-dcc-fxr
                     go to aco-rsc-900.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : come Up      *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-rup    =    spaces
                     move  "U"            to   w-aux-mne-dcc-fxr
                     go to aco-rsc-900.
       aco-rsc-825.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica contabile per :          *
      *                  * - Ragione sociale                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione limite massimo             *
      *                      *-----------------------------------------*
           move      w-aux-mne-dcc-rup    to   w-aux-mne-dcc-rmx      .
           move      40                   to   w-aux-mne-dcc-c01      .
       aco-rsc-830.
           if        w-aux-mne-dcc-c01    >    zero
                     if    w-aux-mne-dcc-rch
                          (w-aux-mne-dcc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-dcc-rch
                                              (w-aux-mne-dcc-c01)
                           subtract 1     from w-aux-mne-dcc-c01
                           go to    aco-rsc-830.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-dcc-crb      .
       aco-rsc-835.
      *                      *-----------------------------------------*
      *                      * Start su archivio                       *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      w-aux-mne-dcc-rup    to   rf-cli-rag-key         .
           move      zero                 to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                          *-------------------------------------*
      *                          * Se start errata                     *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rsc-900.
       aco-rsc-840.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale archivio            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura sequenziale file [cli]      *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rsc-900.
       aco-rsc-845.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-cli-rag-key       >    w-aux-mne-dcc-rmx
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rsc-900.
       aco-rsc-850.
      *                      *-----------------------------------------*
      *                      * Lettura record [dcc]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI   "          to   f-key                  .
           move      rf-cli-cod-cli       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Flag esito operazione                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-aux-mne-dcc-fes
           else      move  "N"            to   w-aux-mne-dcc-fes      .
      *                      *-----------------------------------------*
      *                      * Normalizzazioni se record non esistente *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-fes    not  = spaces
                     move  rf-cli-rag-soc
                                          to   rf-dcc-rag-soc
                     move  rf-cli-via-cli
                                          to   rf-dcc-via-dcc
                     move  rf-cli-loc-cli
                                          to   rf-dcc-loc-dcc         .
       aco-rsc-855.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-dcc-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di max records letti con   *
      *                      * lo stesso valore                        *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-crb    >    w-aux-mne-dcc-max
                     go to aco-rsc-860.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-dcc-tbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-cli       to   w-aux-mne-dcc-cbu
                                              (w-aux-mne-dcc-crb)     .
           move      spaces               to   w-aux-mne-dcc-dbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-cod-mne       to   w-aux-mne-dcc-mbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-rag-soc       to   w-aux-mne-dcc-rbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-rag-soc       to   w-aux-mne-dcc-2bu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-via-dcc       to   w-aux-mne-dcc-vbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-loc-dcc       to   w-aux-mne-dcc-lbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-cli-prt-iva       to   w-aux-mne-dcc-pbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sbu
                                              (w-aux-mne-dcc-crb)     .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-xbu
                                              (w-aux-mne-dcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunto il centoventesimo record   *
      *                      * in interrogazione per partita iva o per *
      *                      * codice fiscale : come per fine file,    *
      *                      * altrimenti : riciclo in lettura         *
      *                      *-----------------------------------------*
           if        w-aux-mne-dcc-tpf    not  = "P" and
                     w-aux-mne-dcc-tpf    not  = "F"
                     go to aco-rsc-840.
           if        w-aux-mne-dcc-crb    =    w-aux-mne-dcc-max
                     move  "C"            to   w-aux-mne-dcc-fxr
                     go to aco-rsc-900
           else      go to aco-rsc-840.
       aco-rsc-860.
      *                      *-----------------------------------------*
      *                      * Se piu' di centoventi record con la     *
      *                      * stessa ragione sociale impostata        *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge4010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-rsc-815.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-dcc-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       aco-rsc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-rsc-999.
       aco-rsc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di visualizzazione pagina video contenente il  *
      *    * record attualmente trattato                               *
      *    *-----------------------------------------------------------*
       aco-vrt-000.
      *              *-------------------------------------------------*
      *              * Determinazione contatori                        *
      *              *-------------------------------------------------*
           move      w-aux-mne-dcc-c01    to   w-aux-mne-dcc-c02      .
           add       5                    to   w-aux-mne-dcc-c02      .
           divide    6                    into w-aux-mne-dcc-c02      .
           move      w-aux-mne-dcc-c02    to   w-aux-mne-dcc-cpa      .
           subtract  1                    from w-aux-mne-dcc-c02      .
           multiply  6                    by   w-aux-mne-dcc-c02      .
           add       1                    to   w-aux-mne-dcc-c02      .
           add       5
                     w-aux-mne-dcc-c02  giving w-aux-mne-dcc-c03      .
           move      w-aux-mne-dcc-c03    to   w-aux-mne-dcc-c04      .
           if        w-aux-mne-dcc-c03    >    w-aux-mne-dcc-crb
                     move  w-aux-mne-dcc-crb
                                          to   w-aux-mne-dcc-c03      .
           move      07                   to   w-aux-mne-dcc-c05      .
       aco-vrt-100.
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-dcc-c05    to   v-lin                  .
           move      14                   to   v-pos                  .
           move      w-aux-mne-dcc-cbu
                    (w-aux-mne-dcc-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrt-200.
      *              *-------------------------------------------------*
      *              * Codice dipendenza cliente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se presente                            *
      *                  *---------------------------------------------*
           if        w-aux-mne-dcc-dbu
                    (w-aux-mne-dcc-c02)   =    spaces
                     go to aco-vrt-300.
      *                  *---------------------------------------------*
      *                  * Separatore                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-aux-mne-dcc-c05    to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-aux-mne-dcc-c05    to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-aux-mne-dcc-dbu
                    (w-aux-mne-dcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrt-300.
      *              *-------------------------------------------------*
      *              * Ragione sociale cliente                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-dcc-c05    to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-aux-mne-dcc-rbu
                    (w-aux-mne-dcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Pulizia area rimanente                          *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-dcc-c02      .
           add       1                    to   w-aux-mne-dcc-c05      .
           if        w-aux-mne-dcc-c02    not  > w-aux-mne-dcc-c03
                     go to aco-vrt-100.
       aco-vrt-400.
           if        w-aux-mne-dcc-c02    >    w-aux-mne-dcc-c04
                     go to aco-vrt-500.
           if        w-aux-mne-dcc-crb    not  > 6
                     go to aco-vrt-500.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-dcc-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-dcc-c02      .
           add       1                    to   w-aux-mne-dcc-c05      .
           go to     aco-vrt-400.
       aco-vrt-500.
      *              *-------------------------------------------------*
      *              * Editing numero pagina                           *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-mne-dcc-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-mne-dcc-ep1      .
      *              *-------------------------------------------------*
      *              * Editing di pagine                               *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-mne-dcc-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-mne-dcc-ep2      .
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-mne-dcc-ltp      .
           string    "Pagina "
                                delimited by   size
                     w-aux-mne-dcc-ep1
                                delimited by   spaces
                     " di "
                                delimited by   size
                     w-aux-mne-dcc-ep2
                                delimited by   spaces
                                          into w-aux-mne-dcc-ltp      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-aux-mne-dcc-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrt-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di visualizzazione dati ricerca completa       *
      *    *-----------------------------------------------------------*
       aco-vrc-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione record del file relative di     *
      *              * appoggio [rlt] attualmente in trattamento       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero record, literal                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Nr:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Numero record, valore                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      05                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      w-rlt-att            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrc-200.
      *                  *---------------------------------------------*
      *                  * Anagrafica [dcc]                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura dati correlati                  *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      02                   to   d-con-arc-tip-arc      .
           move      rf-dcc-cod-cli       to   d-con-arc-cod-arc      .
           move      rf-dcc-dpz-cli       to   d-con-arc-dpz-arc      .
           move      spaces               to   d-con-arc-tip-sel      .
           perform   con-arc-tip-cll-000  thru con-arc-tip-cll-999    .
      *                      *-----------------------------------------*
      *                      * Parte sinistra del box                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      rf-dcc-via-dcc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      rf-dcc-loc-dcc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale per invio documenti *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-dcc-rs1-doc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-dcc-rs2-doc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Note generiche                      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      02                   to   v-pos                  .
           if        rf-dcc-dpz-cli       =    spaces
                     move  rf-dcc-not-g01 to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      02                   to   v-pos                  .
           if        rf-dcc-dpz-cli       =    spaces
                     move  rf-dcc-not-g02 to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      rf-dcc-not-g03       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Nome interlocutore                  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      d-con-arc-int-con (01)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Trattamento status                  *
      *                          *-------------------------------------*
           move      rf-dcc-dpz-cli       to   w-aux-mne-dcc-sxd      .
           move      rf-dcc-sta-tus       to   w-aux-mne-dcc-sxs      .
           move      rf-dcc-sta-tuc       to   w-aux-mne-dcc-sxc      .
           perform   aco-sts-000          thru aco-sts-999            .
      *                          *-------------------------------------*
      *                          * Allineamento a destra               *
      *                          *-------------------------------------*
           move      38                   to   w-all-str-lun          .
           move      w-aux-mne-dcc-sts    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                          *-------------------------------------*
      *                          * Status                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrc-400.
      *                  *---------------------------------------------*
      *                  * Anagrafica [cli]                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura dati correlati                  *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      01                   to   d-con-arc-tip-arc      .
           move      rf-cli-cod-cli       to   d-con-arc-cod-arc      .
           move      spaces               to   d-con-arc-dpz-arc      .
           move      spaces               to   d-con-arc-tip-sel      .
           perform   con-arc-tip-cll-000  thru con-arc-tip-cll-999    .
      *                      *-----------------------------------------*
      *                      * Parte destra del box                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-cli-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-cli-via-cli       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-cli-loc-cli       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Nome interlocutore                  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      d-con-arc-int-con (01)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Partita iva                         *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rf-cli-prt-iva       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Mnemonico                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      rf-cli-cod-mne       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrc-800.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-vrc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-vrc-999.
       aco-vrc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di trattamento status cliente                  *
      *    *-----------------------------------------------------------*
       aco-sts-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-mne-dcc-sts      .
       aco-sts-100.
      *              *-------------------------------------------------*
      *              * Test preliminare se dipendenza                  *
      *              *-------------------------------------------------*
           if        w-aux-mne-dcc-sxd    not  = spaces
                     go to aco-sts-900.
      *              *-------------------------------------------------*
      *              * Test preliminare se status normale              *
      *              *-------------------------------------------------*
           if        w-aux-mne-dcc-sxs    =    01
                     go to aco-sts-900.
       aco-sts-200.
      *              *-------------------------------------------------*
      *              * Editing eventuale codice sostitutivo            *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-mne-dcc-sxc    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-sts-300.
      *              *-------------------------------------------------*
      *              * Preparazione stringa di status                  *
      *              *                                                 *
      *              *  - 01 : Normale                                 *
      *              *  - 11 : Esauriti i rapporti commerciali         *
      *              *  - 21 : Sostituito da ns. nuovo cliente         *
      *              *  - 51 : Cessata attivita'                       *
      *              *  - 52 : Cessata attivita', ma sostituito da ns. *
      *              *         nuovo cliente                           *
      *              *  - 61 : In contenzioso                          *
      *              *  - 62 : In contenzioso, ma sostituito da ns.    *
      *              *         nuovo cliente                           *
      *              *  - 71 : Fallito                                 *
      *              *  - 72 : Fallito, ma sostituito da ns. nuovo     *
      *              *         cliente                                 *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           if        w-aux-mne-dcc-sxs    =    11
                     move  "! Esauriti rapporti   "
                                          to   w-all-str-cat (1)
                     move  spaces         to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    21
                     move  "! Sostituito da       "
                                          to   w-all-str-cat (1)
                     move  v-edt          to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    51
                     move  "! Cessata attivita'   "
                                          to   w-all-str-cat (1)
                     move  spaces         to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    52
                     move  "! Sostituito da       "
                                          to   w-all-str-cat (1)
                     move  v-edt          to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    61
                     move  "! In contenzioso      "
                                          to   w-all-str-cat (1)
                     move  spaces         to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    62
                     move  "! Sostituito da       "
                                          to   w-all-str-cat (1)
                     move  v-edt          to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    71
                     move  "! Fallito             "
                                          to   w-all-str-cat (1)
                     move  spaces         to   w-all-str-cat (2)
           else if   w-aux-mne-dcc-sxs    =    72
                     move  "! Sostituito da       "
                                          to   w-all-str-cat (1)
                     move  v-edt          to   w-all-str-cat (2)
           else      move  "?"            to   w-all-str-cat (1)
                     move  spaces         to   w-all-str-cat (2)      .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
       aco-sts-500.
      *              *-------------------------------------------------*
      *              * Stringa composta in campo di uscita             *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-aux-mne-dcc-sts      .
       aco-sts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-sts-999.
       aco-sts-999.
           exit.

      *    *===========================================================*
      *    * Routines per la gestione del file relative di appoggio    *
      *    * [rlt]                                                     *
      *    *-----------------------------------------------------------*

      *    *-----------------------------------------------------------*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rlt-opn-000.
      *              *-------------------------------------------------*
      *              * Richiesta pathname al modulo segreteria         *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-rlt-pat              .
       rlt-opn-100.
      *              *-------------------------------------------------*
      *              * Operazione di open                              *
      *              *-------------------------------------------------*
           open      i-o   rlt                                        .
       rlt-opn-200.
      *              *-------------------------------------------------*
      *              * Se record locked si esegue una pausa di un      *
      *              * secondo e poi si ritorna a rileggere            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-rlt-sts            not  = e-use-err
                     go to rlt-opn-999. 
      *                  *---------------------------------------------*
      *                  * Attesa di 1 secondo                         *
      *                  *---------------------------------------------*
           move      "WT"                 to   s-ope                  .
           move      01                   to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * A open                                      *
      *                  *---------------------------------------------*
           go to     rlt-opn-100.
       rlt-opn-999.
           exit.

      *    *-----------------------------------------------------------*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       rlt-cls-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     rlt                                              .
      *              *-------------------------------------------------*
      *              * Delete file                                     *
      *              *-------------------------------------------------*
           delete    file    rlt                                      .
       rlt-cls-999.
           exit.

      *    *-----------------------------------------------------------*
      *    * Put                                                       *
      *    *-----------------------------------------------------------*
       rlt-put-000.
      *              *-------------------------------------------------*
      *              * 1. tentativo : rewrite                          *
      *              *-------------------------------------------------*
           rewrite   rlt-rec invalid key
                             go to   rlt-put-200.
           go to     rlt-put-999.
       rlt-put-200.
      *              *-------------------------------------------------*
      *              * 2. tentativo : write                            *
      *              *-------------------------------------------------*
           write     rlt-rec invalid key
                             go to   rlt-put-000.
           go to     rlt-put-999.
       rlt-put-999.
           exit.

      *    *-----------------------------------------------------------*
      *    * Get                                                       *
      *    *-----------------------------------------------------------*
       rlt-get-000.
      *              *-------------------------------------------------*
      *              * Read                                            *
      *              *-------------------------------------------------*
           read      rlt   with no lock
                           invalid key
                           move    spaces to   rlt-rec                .
       rlt-get-999.
           exit.

      *    *===========================================================*
      *    * Annotazioni cliente                                       *
      *    *-----------------------------------------------------------*
       not-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se richiesta normalizzazione           *
      *                  *---------------------------------------------*
           if        w-cod-mne-dcc-ope    =    "NN"
                     move  zero           to   w-not-cli-cod-cli
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Test se presente codice programma           *
      *                  *---------------------------------------------*
           if        w-cod-mne-dcc-prg    =    spaces
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione codice programma  *
      *                  *---------------------------------------------*
           if        w-cod-mne-dcc-prg
                    (01 : 07)             =    "pxpg001"
                     move  "pxpg0012"     to   w-cod-mne-dcc-prg      .
      *                  *---------------------------------------------*
      *                  * Test se stesso cliente                      *
      *                  *                                             *
      *                  * Da rivedere                                 *
      *                  *---------------------------------------------*
______*    if        w-cod-mne-dcc-cod    =    w-not-cli-cod-cli
______*              go to not-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione cliente in corso di tratta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-cod-mne-dcc-cod    to   w-not-cli-cod-cli      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione annotazioni per il cliente  *
      *                  *---------------------------------------------*
           perform   not-buf-000          thru not-buf-999            .
           if        w-not-cli-ctr-ele    =    zero
                     go to not-900.
       not-100.
      *              *-------------------------------------------------*
      *              * Inizio scansione tabella bufferizzata           *
      *              *-------------------------------------------------*
           move      zero                 to   w-not-cli-inx-ele      .
       not-200.
      *              *-------------------------------------------------*
      *              * Incremento e test su contatore                  *
      *              *-------------------------------------------------*
           add       1                    to   w-not-cli-inx-ele      .
           if        w-not-cli-inx-ele    >    w-not-cli-ctr-ele
                     go to not-900.
           if        w-not-cli-inx-ele    >    w-not-cli-max-ele
                     go to not-900.
       not-400.
      *              *-------------------------------------------------*
      *              * Reperimento della annotazione in base al suo    *
      *              * codice diretto o generico                       *
      *              *-------------------------------------------------*
           perform   not-ann-000          thru not-ann-999            .
       not-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione note                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero linee necessarie      *
      *                  *---------------------------------------------*
           move      11                   to   w-not-cli-ctr-001      .
           move      zero                 to   w-not-cli-num-lin      .
       not-520.
           subtract  1                    from w-not-cli-ctr-001      .
           if        w-not-cli-ctr-001    <    1
                     go to not-540.
           if        rf-dcx-ann-rig
                    (w-not-cli-ctr-001)   =    spaces
                     go to not-520.
           move      w-not-cli-ctr-001    to   w-not-cli-num-lin      .
       not-540.
      *                  *---------------------------------------------*
      *                  * Test sul numero di linee determinate        *
      *                  *---------------------------------------------*
           if        w-not-cli-num-lin    =    zero
                     go to not-900.
       not-600.
      *                  *---------------------------------------------*
      *                  * Costruzione box                             *
      *                  *---------------------------------------------*
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
      *                      * Determinazione eventuale fattore di     *
      *                      * sottrazione se raggiunto fine video     *
      *                      *-----------------------------------------*
           move      zero                 to   w-not-cli-num-lis      .
      *
           move      w-cod-mne-dcc-lin    to   w-not-cli-ctr-003      .
           add       4                    to   w-not-cli-ctr-003      .
           add       w-not-cli-num-lin    to   w-not-cli-ctr-003      .
      *
           if        w-not-cli-ctr-003    >    24
                     subtract  24         from w-not-cli-ctr-003
                                        giving w-not-cli-num-lis      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
      *
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           subtract  3                    from v-pos                  .
      *
           move      w-cod-mne-dcc-lin    to   v-lto                  .
           add       4                    to   v-lto                  .
           add       w-not-cli-num-lin    to   v-lto                  .
           subtract  w-not-cli-num-lis    from v-lto                  .
      *
           move      w-cod-mne-dcc-pos    to   v-pto                  .
           add       40                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Separazione prompt nel box              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           add       2                    to   v-lin                  .
           add       w-not-cli-num-lin    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           subtract  1                    from v-pos                  .
      *
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
      *
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-not-cli-num-lin    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           add       5                    to   v-pos                  .
      *
           move      "Digitare 'S' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-610.
      *                  *---------------------------------------------*
      *                  * Visualizzazione righe annotazione           *
      *                  *---------------------------------------------*
           move      zero                 to   w-not-cli-ctr-001      .
       not-620.
           add       1                    to   w-not-cli-ctr-001      .
           if        w-not-cli-ctr-001    >    w-not-cli-num-lin
                     go to not-630.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           add       w-not-cli-ctr-001    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           subtract  1                    from v-pos                  .
           move      rf-dcx-ann-rig
                    (w-not-cli-ctr-001)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     not-620.
       not-630.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-640.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione per presa visione                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
      *
           move      w-cod-mne-dcc-lin    to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-not-cli-num-lin    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-mne-dcc-pos    to   v-pos                  .
           add       38                   to   v-pos                  .
      *
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Test su carattere accettato                     *
      *              *-------------------------------------------------*
           if        v-alf                not  = "S"
                     go to not-640.
       not-700.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-800.
      *              *-------------------------------------------------*
      *              * Riciclo a nota successiva                       *
      *              *-------------------------------------------------*
           go to     not-200.
       not-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-999.
       not-999.
           exit.

      *    *===========================================================*
      *    * Annotazioni cliente                                       *
      *    *                                                           *
      *    * Bufferizzazione                                           *
      *    *-----------------------------------------------------------*
       not-buf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-not-cli-ctr-ele      .
       not-buf-100.
      *              *-------------------------------------------------*
      *              * Start su [dcx] per ricerca eventuali note       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CLIPRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-dcc-cod    to   rf-dcx-cod-cli         .
           move      spaces               to   rf-dcx-dpz-cli         .
           move      w-cod-mne-dcc-prg    to   rf-dcx-cod-prg         .
           move      zero                 to   rf-dcx-cod-ann         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to not-buf-900.
       not-buf-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [dcx]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to not-buf-900.
       not-buf-300.
      *              *-------------------------------------------------*
      *              * Test max su [dcx]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-cli       not  = w-cod-mne-dcc-cod
                     go to not-buf-900.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza cliente           *
      *                  *---------------------------------------------*
           if        rf-dcx-dpz-cli       not  = spaces
                     go to not-buf-900.
      *                  *---------------------------------------------*
      *                  * Test su codice programma                    *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-prg       not  = w-cod-mne-dcc-prg
                     go to not-buf-900.
       not-buf-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [dcx]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Presumendo che questa bufferizzazione ri-   *
      *                  * guardi i programmi che trattano il video,   *
      *                  * si escludono le note destinate alla sola    *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-dcx-snx-stp       =    "P"
                     go to not-buf-200.
      *                  *---------------------------------------------*
      *                  * Determinazione data attuale                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su eventuale data iniziale             *
      *                  *---------------------------------------------*
           if        rf-dcx-dat-ini       =    zero
                     go to not-buf-420.
           if        rf-dcx-dat-ini       not  < s-dat
                     go to not-buf-200.
       not-buf-420.
      *                  *---------------------------------------------*
      *                  * Test su eventuale data finale               *
      *                  *---------------------------------------------*
           if        rf-dcx-dat-fin       =    zero
                     go to not-buf-600.
           if        rf-dcx-dat-fin       <    s-dat
                     go to not-buf-200.
       not-buf-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           add       1                    to   w-not-cli-ctr-ele      .
           if        w-not-cli-ctr-ele    >    w-not-cli-max-ele
                     go to not-buf-900.
           move      rf-dcx-cod-ann       to   w-not-cli-cod-ann
                                              (w-not-cli-ctr-ele)     .
           if        rf-dcx-cod-ang       not  numeric
                     move  zero           to   rf-dcx-cod-ang         .
           move      rf-dcx-cod-ang       to   w-not-cli-cod-ang
                                              (w-not-cli-ctr-ele)     .
       not-buf-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     not-buf-200.
       not-buf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-buf-999.
       not-buf-999.
           exit.

      *    *===========================================================*
      *    * Annotazioni cliente                                       *
      *    *                                                           *
      *    * Reperimento annotazione                                   *
      *    *-----------------------------------------------------------*
       not-ann-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della presenza di un     *
      *              * codice annotazione generica                     *
      *              *-------------------------------------------------*
           if        w-not-cli-cod-ang
                    (w-not-cli-inx-ele)   =    zero
                     go to not-ann-500.
       not-ann-100.
      *              *-------------------------------------------------*
      *              * Annotazione generica                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcx]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Lettura [dcx]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      zero                 to   rf-dcx-cod-cli         .
           move      spaces               to   rf-dcx-dpz-cli         .
           move      w-not-cli-cod-ang
                    (w-not-cli-inx-ele)   to   rf-dcx-cod-ann         .
           move      spaces               to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     not-ann-900.
       not-ann-500.
      *              *-------------------------------------------------*
      *              * Annotazione specifica                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcx]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Lettura [dcx]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      w-cod-mne-dcc-cod    to   rf-dcx-cod-cli         .
           move      spaces               to   rf-dcx-dpz-cli         .
           move      w-not-cli-cod-ann
                    (w-not-cli-inx-ele)   to   rf-dcx-cod-ann         .
           move      w-cod-mne-dcc-prg    to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     not-ann-900.
       not-ann-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-ann-999.
       not-ann-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione contatti                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

