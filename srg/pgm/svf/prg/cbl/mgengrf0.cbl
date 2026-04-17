       Identification Division.
       Program-Id.                                 mgengrf0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 02/01/11    *
      *                       Ultima revisione:    NdK del 03/05/18    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la generazione grafici a colonne                    *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * N.B. : Il modulo si serve di 't_chart' che invia i dati ad una *
      *        apposita pagina Web ed una mail all'utente con il link  *
      *        del grafico, ma usando un server senza autenticazione   *
      *        autenticazione (!)                                      *
      *                                                                *
      *        ___ MIGLIORARE QUESTIONE MAIL ___                       *
      *        ___ USANDO 't_sender' ???     ___                       *
      *                                                                *
      *        ___ VEDI ALTRE NOTE su 'tmp'  ___                       *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "NO" - Normalizzazione dati grafico                            *
      *                                                                *
      *                                                                *
      *        Input  : m-gen-grf-tip-ope = "NO"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "EG" - Emissione grafico                                       *
      *                                                                *
      *                                                                *
      *        Input  : m-gen-grf-tip-ope = "EG"                       *
      *                                                                *
      *                 m-gen-grf-tip-grf = tipo di grafico            *
      *                                     - 'C' : Column (colonne)   *
      *                                     - 'P' : Pie    (torta)     *
      *                                                                *
      *                 m-gen-grf-tip-arc = tipo archivio              *
      *                                                                *
      *                                     - 'C' : Clienti            *
      *                                     - 'F' : Fornitori          *
      *                                     - 'A' : Agenti             *
      *                                     - 'T' : Tipi movimento     *
      *                                                                *
      *                                    (spaces : totali generali)  *
      *                                                                *
      *                 m-gen-grf-cod-arc = codice numerico archivio   *
      *                                    (zero : totali generali)    *
      *                                                                *
      *                 m-gen-grf-mne-arc = codice alfanumerico        *
      *                                    (solo se archivio 'T')      *
      *                                    (spaces : totali generali)  *
      *                                                                *
      *                 m-gen-grf-num-prd = numero periodi (max 3)     *
      *                                                                *
      *                 m-gen-grf-tit-grf = titolo del grafico         *
      *                                                                *
      *                 m-gen-grf-fil-nam = nome del grafico           *
      *                                                                *
      *                 m-gen-grf-cst-prd = castelletto dati           *
      *                                    (3 periodi da 12 mesi)      *
      *                                                                *
      *                                                                *
      *        Output : m-gen-grf-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-npe                  pic  x(08)                  .
           05  f-xxx-ppb                  pic  x(40)                  .
           05  f-xxx-ptc                  pic  x(40)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-ute                  pic  x(08)                  .
           05  f-xxx-azi                  pic  x(04)                  .
           05  f-xxx-fas                  pic  x(08)                  .
           05  f-xxx-emu                  pic  x(40)                  .
           05  f-xxx-srv                  pic  x(40)                  .
           05  f-xxx-ads                  pic  x(20)                  .
           05  f-xxx-uid                  pic  x(40)                  .
           05  f-xxx-pwd                  pic  x(20)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per emissione grafico                            *
      *        *-------------------------------------------------------*
           05  w-det-sqz-grf.
               10  w-det-sqz-grf-min      pic  9(02)                  .
               10  w-det-sqz-grf-max      pic  9(02)                  .
               10  w-det-sqz-grf-ctr      pic  9(02)                  .
               10  w-det-sqz-grf-ctp      pic  9(02)                  .
               10  w-det-sqz-grf-ct0      pic  9(02)                  .
               10  w-det-sqz-grf-ct1      pic  9(02)                  .
               10  w-det-sqz-grf-val      pic s9(13)v9(02)            .
               10  w-det-sqz-grf-vl1      pic s9(13)v9(01)            .

      *    *===========================================================*
      *    * Work area per la definizione dei record per output        *
      *    *-----------------------------------------------------------*
       01  w-out.
      *        *-------------------------------------------------------*
      *        * Stringa di output                                     *
      *        *-------------------------------------------------------*
           05  w-out-str-out.
               10  w-out-str-chr occurs 5120
                                          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per scrittura records in file sequenziale di appog-  *
      *    * gio                                                       *
      *    *-----------------------------------------------------------*
       01  w-scr-fso.
      *        *-------------------------------------------------------*
      *        * Carattere di fine riga                                *
      *        *-------------------------------------------------------*
           05  w-scr-fso-cfr              pic  x(01)   value H"0D"    .
      *        *-------------------------------------------------------*
      *        * Comodi per assemblaggio                               *
      *        *-------------------------------------------------------*
           05  w-scr-ctr-001              pic  9(05)                  .
           05  w-scr-ctr-002              pic  9(05)                  .
           05  w-scr-ctr-003              pic  9(05)                  .
           05  w-scr-ctr-004              pic  9(05)                  .
           05  w-scr-ctr-005              pic  9(05)                  .
           05  w-scr-lun-ele              pic  9(03)                  .
           05  w-scr-str-tip              pic  x(02)                  .
           05  w-scr-str-sav              pic  x(02)                  .
           05  w-scr-str-sgn              pic  x(01)                  .
           05  w-scr-str-ele.
               10  filler     occurs 200  pic  x(01)                  .
           05  w-scr-str-elx.
               10  filler     occurs 200  pic  x(01)                  .
           05  w-scr-dat-dat              pic  9(07)                  .
           05  w-scr-dat-ann              pic  9(04)                  .
           05  w-scr-dat-mes              pic  9(02)                  .
           05  w-scr-dat-gio              pic  9(02)                  .

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
      *    * Area di comunicazione per generazione grafici             *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mgengrf0.mdl"                   .

      ******************************************************************
       Procedure Division                using m-gen-grf              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   m-gen-grf-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           if        m-gen-grf-tip-ope    =    "NO"
                     perform nor-dat-grf-000
                                          thru nor-dat-grf-999
      *                  *---------------------------------------------*
      *                  * Emissione grafico                           *
      *                  *---------------------------------------------*
           else if   m-gen-grf-tip-ope    =    "EG"
                     perform exe-sqz-grf-000
                                          thru exe-sqz-grf-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Normalizzazione preliminare dati per grafico              *
      *    *-----------------------------------------------------------*
       nor-dat-grf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo per periodi                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-sqz-grf-ct0      .
       nor-dat-grf-200.
           add       1                    to   w-det-sqz-grf-ct0      .
           if        w-det-sqz-grf-ct0    >    3
                     go to nor-dat-grf-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione date                        *
      *                  *---------------------------------------------*
           move      zero                 to   m-gen-grf-prd-min
                                              (w-det-sqz-grf-ct0)     .
           move      zero                 to   m-gen-grf-prd-max
                                              (w-det-sqz-grf-ct0)     .
      *                  *---------------------------------------------*
      *                  * Ciclo per mesi                              *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-sqz-grf-ct1      .
       nor-dat-grf-300.
           add       1                    to   w-det-sqz-grf-ct1      .
           if        w-det-sqz-grf-ct1    >    12
                     go to nor-dat-grf-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      spaces               to   m-gen-grf-mes-des
                                              (w-det-sqz-grf-ct0,
                                               w-det-sqz-grf-ct1)     .
           move      zero                 to   m-gen-grf-mes-val
                                              (w-det-sqz-grf-ct0,
                                               w-det-sqz-grf-ct1)     .
      *                  *---------------------------------------------*
      *                  * Riciclo per mesi                            *
      *                  *---------------------------------------------*
           go to     nor-dat-grf-300.
       nor-dat-grf-800.
      *                  *---------------------------------------------*
      *                  * Riciclo per periodi                         *
      *                  *---------------------------------------------*
           go to     nor-dat-grf-200.
       nor-dat-grf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     nor-dat-grf-999.
       nor-dat-grf-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico                    *
      *    *-----------------------------------------------------------*
       exe-sqz-grf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di status                  *
      *              *-------------------------------------------------*
           move      spaces               to   m-gen-grf-exi-sts      .
       exe-sqz-grf-100.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari di apertura              *
      *              *-------------------------------------------------*
           perform   exe-sqz-grf-opn-000  thru exe-sqz-grf-opn-999    .
       exe-sqz-grf-150.
      *              *-------------------------------------------------*
      *              * Normalizzazione record di output                *
      *              *-------------------------------------------------*
           move      spaces               to   g-rec                  .
       exe-sqz-grf-200.
      *              *-------------------------------------------------*
      *              * Emissione dati iniziali                         *
      *              *-------------------------------------------------*
           perform   exe-sqz-grf-ini-000  thru exe-sqz-grf-ini-999    .
       exe-sqz-grf-400.
      *              *-------------------------------------------------*
      *              * Emissione dati in funzione del tipo di grafico  *
      *              *-------------------------------------------------*
           if        m-gen-grf-tip-grf    =    "C"
                     perform  exe-sqz-grf-col-000
                                          thru exe-sqz-grf-col-999
           else if   m-gen-grf-tip-grf    =    "P"
                     perform  exe-sqz-grf-pie-000
                                          thru exe-sqz-grf-pie-999
           else      perform  exe-sqz-grf-col-000
                                          thru exe-sqz-grf-col-999    .
       exe-sqz-grf-800.
      *              *-------------------------------------------------*
      *              * Operazioni finali                               *
      *              *-------------------------------------------------*
           perform   exe-sqz-grf-cls-000  thru exe-sqz-grf-cls-999    .
       exe-sqz-grf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-999.
       exe-sqz-grf-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico                    *
      *    *                                                           *
      *    * Operazioni preliminari di apertura                        *
      *    *-----------------------------------------------------------*
       exe-sqz-grf-opn-000.
      *              *-------------------------------------------------*
      *              * Lettura User ID per invio posta sa segreteria   *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "uid-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   f-xxx-uid              .
      *              *-------------------------------------------------*
      *              * Lettura Password per invio posta sa segreteria  *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "pwd-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   f-xxx-pwd              .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al     *
      *              * mail server                                     *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "msa-isp"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   f-xxx-srv              .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa al-    *
      *              * la agente di spedizione 't_sender'              *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "ads-ssm"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   f-xxx-ads              .
      *              *-------------------------------------------------*
      *              * Richiesta informazioni generali a segreteria    *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-ute                to   f-xxx-ute              .
           move      s-azi                to   f-xxx-azi              .
           move      s-fas                to   f-xxx-fas              .
           move      s-adx                to   f-xxx-emu              .
      *              *-------------------------------------------------*
      *              * Pathname di base da segreteria                  *
      *              *-------------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "asc "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                     .
           move      s-pat                to   f-xxx-ppb              .
      *              *-------------------------------------------------*
      *              * Pathname dei comandi da segreteria              *
      *              *-------------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "etc "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                     .
           move      s-pat                to   f-xxx-ptc              .
       exe-sqz-grf-opn-100.
      *              *-------------------------------------------------*
      *              * Test se presente codice archivio                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        m-gen-grf-tip-arc    not  = "T"
                     go to exe-sqz-grf-opn-150.
      *                  *---------------------------------------------*
      *                  * Forzatura fase per nome file                *
      *                  *---------------------------------------------*
           move      m-gen-grf-mne-arc    to   f-xxx-fas              .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-opn-200.
       exe-sqz-grf-opn-150.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        m-gen-grf-cod-arc    =    zero
                     go to exe-sqz-grf-opn-200.
      *                  *---------------------------------------------*
      *                  * Editing codice archivio                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      m-gen-grf-cod-arc    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio nome fase                      *
      *                  *---------------------------------------------*
           move      08                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      m-gen-grf-tip-arc    to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           perform   all-str-asx-000      thru all-str-asx-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura fase per nome file                *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   f-xxx-fas              .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-opn-200.
       exe-sqz-grf-opn-200.
      *              *-------------------------------------------------*
      *              * Preparazione pathname                           *
      *              *                                                 *
      *              * ___ VALUTARE DI METTERE IN '/abd/tmp' ___       *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-pat              .
           string    f-xxx-ppb  delimited by   spaces
                     "/"        delimited by   size
                     f-xxx-ute  delimited by   spaces
                     "/tmp/"    delimited by   size
                     f-xxx-azi  delimited by   spaces
                     "_"        delimited by   size
                     f-xxx-fas  delimited by   spaces
                     ".csv"     delimited by   size
                                          into f-xxx-pat              .
      *              *-------------------------------------------------*
      *              * Apertura del file in output                     *
      *              *-------------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      "seq "               to   g-nam                  .
           move      f-xxx-pat            to   g-pat                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       exe-sqz-grf-opn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-opn-999.
       exe-sqz-grf-opn-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico                    *
      *    *                                                           *
      *    * Emissioni iniziali                                        *
      *    *-----------------------------------------------------------*
       exe-sqz-grf-ini-000.
      *              *-------------------------------------------------*
      *              * Prima riga di intestazione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione ragione sociale azienda        *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Preparazione titolo del grafico             *
      *                  *---------------------------------------------*
           move      50                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      m-gen-grf-tit-grf    to   w-all-str-cat (1)      .
           move      "-"                  to   w-all-str-cat (2)      .
           move      s-asx                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Inizio                                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "iA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Titolo del grafico                          *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-all-str-alf        to   w-scr-str-ele          .
           move      50                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Sottotitolo del grafico                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il grafico e' di tipo 'pie', vengono *
      *                      * esposte le due date di riferimento      *
      *                      *-----------------------------------------*
           if        m-gen-grf-tip-grf    not  = "P"
                     go to exe-sqz-grf-ini-100.
      *                      *-----------------------------------------*
      *                      * Editing data min                        *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      "S"                  to   p-edm                  .
           move      m-gen-grf-prd-min (1)
                                          to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *                      *-----------------------------------------*
      *                      * Editing data max                        *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      "S"                  to   p-edm                  .
           move      m-gen-grf-prd-max (1)
                                          to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (4)      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      50                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
      *
           move      "Periodo dal"        to   w-all-str-cat (1)      .
           move      "al"                 to   w-all-str-cat (3)      .
           move      "- Tangram (c)"      to   w-all-str-cat (5)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Valore assemblato                       *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-all-str-alf        to   w-scr-str-ele          .
           move      50                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     exe-sqz-grf-ini-175.
       exe-sqz-grf-ini-100.
      *                      *-----------------------------------------*
      *                      * Se  grafico a 'colonne'                 *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      "Tangram (c)"        to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-ini-150.
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Titolo Asse 'Y'                             *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      "Valori in euro"     to   w-scr-str-ele          .
           move      26                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-ini-175.
      *                  *---------------------------------------------*
      *                  * Inibizione del nome file esportazione       *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-ini-190.
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Nome file esportazione                      *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      m-gen-grf-fil-nam    to   w-scr-str-ele          .
           move      06                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-ini-190.
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Composizione carattere di fine record       *
      *                  *---------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-sqz-grf-ini-200.
      *              *-------------------------------------------------*
      *              * Seconda riga di intestazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul tipo di grafico                    *
      *                  *---------------------------------------------*
           if        m-gen-grf-tip-grf    not  = "C"
                     go to exe-sqz-grf-ini-900.
      *                  *---------------------------------------------*
      *                  * Denominazione colonne                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizio                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           move      "iA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * Elemento iniziale                       *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      "Categories"         to   w-scr-str-ele          .
           move      10                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-ini-210.
      *                      *-----------------------------------------*
      *                      * Ciclo                                   *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-sqz-grf-ctr      .
       exe-sqz-grf-ini-220.
           add       1                    to   w-det-sqz-grf-ctr      .
      *                      *-----------------------------------------*
      *                      * Determinazione minimo                   *
      *                      *-----------------------------------------*
           move      m-gen-grf-prd-min (01)
                                          to   s-dat                  .
           move      s-mes                to   w-det-sqz-grf-min      .
           if        w-det-sqz-grf-ctr    <    w-det-sqz-grf-min
                     go to exe-sqz-grf-ini-220.
      *                      *-----------------------------------------*
      *                      * Determinazione massimo                  *
      *                      *-----------------------------------------*
           move      m-gen-grf-prd-max (01)
                                          to   s-dat                  .
           move      s-mes                to   w-det-sqz-grf-max      .
           if        w-det-sqz-grf-ctr    >    w-det-sqz-grf-max or
                     w-det-sqz-grf-ctr    >    12
                     go to exe-sqz-grf-ini-800.
      *                      *-----------------------------------------*
      *                      * Literal del mese, da segreteria         *
      *                      *-----------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-det-sqz-grf-ctr    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      s-alf                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           if        w-det-sqz-grf-ctr    =    w-det-sqz-grf-max
                     move  "Af"           to   w-scr-str-tip
           else      move  "AA"           to   w-scr-str-tip          .
      *
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     exe-sqz-grf-ini-220.
       exe-sqz-grf-ini-800.
      *              *-------------------------------------------------*
      *              * Operazioni finali                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione carattere di fine record       *
      *                  *---------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-sqz-grf-ini-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-ini-999.
       exe-sqz-grf-ini-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico a 'colonne'        *
      *    *                                                           *
      *    * Ciclo di emissione dati                                   *
      *    *-----------------------------------------------------------*
       exe-sqz-grf-col-000.
      *              *-------------------------------------------------*
      *              * Ciclo per periodi                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sqz-grf-ctp      .
       exe-sqz-grf-col-200.
           add       1                    to   w-det-sqz-grf-ctp      .
           if        w-det-sqz-grf-ctp    >    m-gen-grf-num-prd
                     go to exe-sqz-grf-col-900.
       exe-sqz-grf-col-300.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari periodo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo per legenda                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizio                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           move      "iA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * Preparazione literal per anno           *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
      *
           move      m-gen-grf-prd-min
                    (w-det-sqz-grf-ctp)   to   s-dat                  .
           move      s-saa                to   p-num                  .
           add       1900                 to   p-num                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Elemento iniziale                       *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      p-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-col-410.
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-sqz-grf-ctr      .
       exe-sqz-grf-col-420.
           add       1                    to   w-det-sqz-grf-ctr      .
      *                  *---------------------------------------------*
      *                  * Determinazione minimo                       *
      *                  *---------------------------------------------*
           move      m-gen-grf-prd-min
                    (w-det-sqz-grf-ctp)   to   s-dat                  .
           move      s-mes                to   w-det-sqz-grf-min      .
           if        w-det-sqz-grf-ctr    <    w-det-sqz-grf-min
                     go to exe-sqz-grf-col-420.
      *                  *---------------------------------------------*
      *                  * Determinazione massimo                      *
      *                  *---------------------------------------------*
           move      m-gen-grf-prd-max
                    (w-det-sqz-grf-ctp)   to   s-dat                  .
           move      s-mes                to   w-det-sqz-grf-max      .
           if        w-det-sqz-grf-ctr    >    w-det-sqz-grf-max or
                     w-det-sqz-grf-ctr    >    12
                     go to exe-sqz-grf-col-800.
      *                  *---------------------------------------------*
      *                  * Preparazione dato mensile                   *
      *                  *---------------------------------------------*
           move      m-gen-grf-mes-val
                    (w-det-sqz-grf-ctp,
                     w-det-sqz-grf-ctr)   to   w-det-sqz-grf-val      .      
      *
           divide    100                  into w-det-sqz-grf-val
                                        giving w-det-sqz-grf-vl1      .
      *                  *---------------------------------------------*
      *                  * Editing dato mensile                        *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-det-sqz-grf-vl1    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      p-edt                to   w-scr-str-ele          .
           move      13                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           if        w-det-sqz-grf-ctr    =    w-det-sqz-grf-max
                     move  "Nf"           to   w-scr-str-tip
           else      move  "NN"           to   w-scr-str-tip          .
      *
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-col-420.
       exe-sqz-grf-col-800.
      *              *-------------------------------------------------*
      *              * Operazioni finali                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ultimo record                       *
      *                  *---------------------------------------------*
           if        w-det-sqz-grf-ctp    =    m-gen-grf-num-prd
                     go to exe-sqz-grf-col-820.
      *                  *---------------------------------------------*
      *                  * Composizione carattere di fine record       *
      *                  *---------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-col-820.
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-sqz-grf-col-850.
      *              *-------------------------------------------------*
      *              * Riciclo su periodo                              *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-col-200.
       exe-sqz-grf-col-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-col-999.
       exe-sqz-grf-col-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico di tipo 'pie'      *
      *    *                                                           *
      *    * Ciclo di emissione dati                                   *
      *    *-----------------------------------------------------------*
       exe-sqz-grf-pie-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-sqz-grf-pie-400.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione dati                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo                                       *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-sqz-grf-ctr      .
           move      1                    to   w-det-sqz-grf-ct0      .
           move      zero                 to   w-det-sqz-grf-ct1      .
       exe-sqz-grf-pie-420.
           add       1                    to   w-det-sqz-grf-ctr      .
           add       1                    to   w-det-sqz-grf-ct1      .
           if        w-det-sqz-grf-ctr    >    m-gen-grf-num-prd
                     go to exe-sqz-grf-pie-900.
           if        w-det-sqz-grf-ctr    >    36
                     go to exe-sqz-grf-pie-900.
           if        w-det-sqz-grf-ct1    >    12
                     add   1              to   w-det-sqz-grf-ct0
                     move  1              to   w-det-sqz-grf-ct1      .
       exe-sqz-grf-pie-440.
      *                  *---------------------------------------------*
      *                  * Descrittivo                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizio                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-out-str-out          .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           move      "iA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * Elemento iniziale                       *
      *                      *-----------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      m-gen-grf-mes-des
                    (w-det-sqz-grf-ct0,
                     w-det-sqz-grf-ct1)   to   w-scr-str-ele          .
           move      40                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                      *-----------------------------------------*
      *                      * S-e-p-a-r-a-t-o-r-i                     *
      *                      *-----------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-pie-460.
      *                  *---------------------------------------------*
      *                  * Preparazione dato                           *
      *                  *---------------------------------------------*
           move      m-gen-grf-mes-val
                    (w-det-sqz-grf-ct0,
                     w-det-sqz-grf-ct1)   to   w-det-sqz-grf-val      .      
      *
           divide    100                  into w-det-sqz-grf-val
                                        giving w-det-sqz-grf-vl1      .
      *                  *---------------------------------------------*
      *                  * Editing dato                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-det-sqz-grf-vl1    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      p-edt                to   w-scr-str-ele          .
           move      13                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Nf"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-pie-800.
      *              *-------------------------------------------------*
      *              * Operazioni finali                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se ultimo record                       *
      *                  *---------------------------------------------*
           if        w-det-sqz-grf-ctr    =    m-gen-grf-num-prd
                     go to exe-sqz-grf-pie-820.
      *                  *---------------------------------------------*
      *                  * Composizione carattere di fine record       *
      *                  *---------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-sqz-grf-pie-820.
      *                  *---------------------------------------------*
      *                  * Scrittura record in output                  *
      *                  *---------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       exe-sqz-grf-pie-850.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-pie-420.
       exe-sqz-grf-pie-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-pie-999.
       exe-sqz-grf-pie-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico                    *
      *    *                                                           *
      *    * Operazioni finali di chiusura                             *
      *    *-----------------------------------------------------------*
       exe-sqz-grf-cls-000.
      *              *-------------------------------------------------*
      *              * Chiusura del file in output                     *
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
       exe-sqz-grf-cls-200.
      *              *-------------------------------------------------*
      *              * Comando di generazione grafico esterna          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        m-gen-grf-tip-arc    not  = "T"
                     go to exe-sqz-grf-cls-250.
      *                  *---------------------------------------------*
      *                  * Forzatura fase per nome file                *
      *                  *---------------------------------------------*
           move      m-gen-grf-mne-arc    to   f-xxx-fas              .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-cls-300.
       exe-sqz-grf-cls-250.
      *                  *---------------------------------------------*
      *                  * Test se presente codice archivio            *
      *                  *---------------------------------------------*
           if        m-gen-grf-cod-arc    =    zero
                     go to exe-sqz-grf-cls-300.
      *                  *---------------------------------------------*
      *                  * Editing codice archivio                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      m-gen-grf-cod-arc    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio nome fase                      *
      *                  *---------------------------------------------*
           move      08                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      m-gen-grf-tip-arc    to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           perform   all-str-asx-000      thru all-str-asx-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura fase per nome file                *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   f-xxx-fas              .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     exe-sqz-grf-cls-300.
       exe-sqz-grf-cls-300.
      *                  *---------------------------------------------*
      *                  * Comando di generazione grafici              *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      f-xxx-ptc            to   w-all-str-cat (1)      .
           move      "/t_chart"           to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   f-xxx-ptc              .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      220                  to   w-all-str-lun          .
           move      09                   to   w-all-str-num          .
           move      f-xxx-ptc            to   w-all-str-cat (1)      .
           move      f-xxx-azi            to   w-all-str-cat (2)      .
           move      f-xxx-ute            to   w-all-str-cat (3)      .
           move      f-xxx-fas            to   w-all-str-cat (4)      .
           move      m-gen-grf-tip-grf    to   w-all-str-cat (5)      .
           move      f-xxx-srv            to   w-all-str-cat (6)      .
           move      f-xxx-emu            to   w-all-str-cat (7)      .
           move      f-xxx-uid            to   w-all-str-cat (8)      .
           move      f-xxx-pwd            to   w-all-str-cat (9)      .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      w-all-str-alf        to   o-shs                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo 'mopsys'                *
      *                  *---------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       exe-sqz-grf-cls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-sqz-grf-cls-999.
       exe-sqz-grf-cls-999.
           exit.

      *    *===========================================================*
      *    * Emissione sequenziale dati per grafico                    *
      *    *                                                           *
      *    * Composizione singolo campo                                *
      *    *                                                           *
      *    * FORMATO : 999;XXXXXXXX;YYYYY;999                          *
      *    *-----------------------------------------------------------*
       cmp-sng-fld-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo elemento        *
      *              *-------------------------------------------------*
           if        w-scr-str-tip        =    "A " or
                     w-scr-str-tip        =    "E "
                     go to cmp-sng-fld-200
           else if   w-scr-str-tip        =    "N " or
                     w-scr-str-tip        =    "V " or
                     w-scr-str-tip        =    "Y "
                     go to cmp-sng-fld-300
           else if   w-scr-str-tip        =    "D "
                     go to cmp-sng-fld-400
           else if   w-scr-str-tip        =    "fr"
                     go to cmp-sng-fld-200
           else      go to cmp-sng-fld-100.
       cmp-sng-fld-100.
      *              *-------------------------------------------------*
      *              * Se separatori                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Check preliminare di congruenza             *
      *                  *---------------------------------------------*
           if        w-scr-str-tip (1:1)  =    "i"    or
                     w-scr-str-tip (2:1)  =    "f"
                     go to cmp-sng-fld-150.
           if        w-scr-str-sav        =    spaces
                     go to cmp-sng-fld-150.
           if        w-scr-str-sav (2:1)  =    spaces or
                     w-scr-str-sav (2:1)  =    "f"    or
                     w-scr-str-sav (1:1)  =    "i"
                     go to cmp-sng-fld-150.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-scr-str-sav        =    "NN" and
                     w-scr-str-tip        =    "NN"
                     go to cmp-sng-fld-150
           else if   w-scr-str-sav        =    "AN" and
                     w-scr-str-tip        =    "NN"
                     go to cmp-sng-fld-150
           else if   w-scr-str-sav        =    "AN" and
                     w-scr-str-tip        =    "NA"
                     go to cmp-sng-fld-150
           else if   w-scr-str-sav        =    "AA" and
                     w-scr-str-tip        =    "AA"
                     go to cmp-sng-fld-150
           else if   w-scr-str-sav        =    "AA" and
                     w-scr-str-tip        =    "AN"
                     go to cmp-sng-fld-150.
       cmp-sng-fld-150.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-scr-str-tip        =    "iA"
                     move  spaces         to   w-scr-str-ele
                     move  01             to   w-scr-lun-ele
           else if   w-scr-str-tip        =    "AA"
                     move  ";"            to   w-scr-str-ele
                     move  03             to   w-scr-lun-ele
           else if   w-scr-str-tip        =    "AN"
                     move  ";"            to   w-scr-str-ele
                     move  02             to   w-scr-lun-ele
           else if   w-scr-str-tip        =    "NA"
                     move  ";"            to   w-scr-str-ele
                     move  02             to   w-scr-lun-ele
           else if   w-scr-str-tip        =    "NN"
                     move  ";"            to   w-scr-str-ele
                     move  01             to   w-scr-lun-ele
           else if   w-scr-str-tip        =    "Af"
                     move  spaces         to   w-scr-str-ele
                     move  02             to   w-scr-lun-ele
           else if   w-scr-str-tip        =    "Nf"
                     go to cmp-sng-fld-999
           else      go to cmp-sng-fld-999.
      *                  *---------------------------------------------*
      *                  * Ad assemblaggio                             *
      *                  *---------------------------------------------*
           go to     cmp-sng-fld-800.
       cmp-sng-fld-200.
      *              *-------------------------------------------------*
      *              * Alfanumerico o espanso                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eliminazione singoli apici eventuali        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * INIBITO                                 *
      *                      *-----------------------------------------*
           go to     cmp-sng-fld-280.
      *                      *-----------------------------------------*
      *                      * Test se presenti (')                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for all   "'"                    .
      *
           if        w-scr-ctr-001        =    zero
                     go to cmp-sng-fld-230
           else if   w-scr-ctr-001        =    1
                     go to cmp-sng-fld-220
           else      go to cmp-sng-fld-224.
       cmp-sng-fld-220.
      *                      *-----------------------------------------*
      *                      * Pulizia di un solo elemento             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Pulizia                             *
      *                          *-------------------------------------*
           move      w-scr-lun-ele        to   w-all-str-lun          .
           move      w-scr-str-ele        to   w-all-str-alf          .
           move      "'"                  to   w-all-str-cat (1)      .
           move      "\'"                 to   w-all-str-cat (2)      .
           perform   all-str-sos-000      thru all-str-sos-999        .
           move      w-all-str-alf        to   w-scr-str-ele          .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     cmp-sng-fld-230.
       cmp-sng-fld-224.
      *                      *-----------------------------------------*
      *                      * Pulizia di + elementi                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scomposizione                       *
      *                          *-------------------------------------*
           move      w-scr-str-ele        to   w-all-str-alf          .
           move      "'"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                          *-------------------------------------*
      *                          * Ri-composizione                     *
      *                          *-------------------------------------*
           move      w-scr-lun-ele        to   w-all-str-lun          .
           move      w-scr-str-ele        to   w-all-str-alf          .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-scr-str-ele          .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     cmp-sng-fld-230.
       cmp-sng-fld-230.
      *                  *---------------------------------------------*
      *                  * Eliminazione punto e virgole eventuali      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se presenti ';'                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for all   ";"                    .
      *
           if        w-scr-ctr-001        =    zero
                     go to cmp-sng-fld-280
           else if   w-scr-ctr-001        =    1
                     go to cmp-sng-fld-240
           else      go to cmp-sng-fld-260.
       cmp-sng-fld-240.
      *                      *-----------------------------------------*
      *                      * Pulizia di un ';'                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Pulizia                             *
      *                          *-------------------------------------*
           move      w-scr-lun-ele        to   w-all-str-lun          .
           move      w-scr-str-ele        to   w-all-str-alf          .
           move      ";"                  to   w-all-str-cat (1)      .
           move      ","                  to   w-all-str-cat (2)      .
           perform   all-str-sos-000      thru all-str-sos-999        .
           move      w-all-str-alf        to   w-scr-str-ele          .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     cmp-sng-fld-280.
       cmp-sng-fld-260.
      *                      *-----------------------------------------*
      *                      * Pulizia di + ';'                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Scomposizione                       *
      *                          *-------------------------------------*
           move      w-scr-str-ele        to   w-all-str-alf          .
           move      ";"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                          *-------------------------------------*
      *                          * Ri-composizione                     *
      *                          *-------------------------------------*
           move      w-scr-lun-ele        to   w-all-str-lun          .
           move      w-scr-str-ele        to   w-all-str-alf          .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-scr-str-ele          .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     cmp-sng-fld-280.
       cmp-sng-fld-280.
      *                  *---------------------------------------------*
      *                  * A scansione                                 *
      *                  *---------------------------------------------*
           go to     cmp-sng-fld-800.
       cmp-sng-fld-300.
      *              *-------------------------------------------------*
      *              * Numerico o valuta                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se tutti zeri                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for all   "0"                    .
           if        w-scr-ctr-001        =    w-scr-lun-ele
                     move  spaces         to   w-scr-str-ele          .
      *                  *---------------------------------------------*
      *                  * Test se tutti zeri (decimali)               *
      *                  *                                             *
      *                  * DA PERFEZIONARE ___                         *
      *                  *---------------------------------------------*
           go to     cmp-sng-fld-360.
      *
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for all   ","                    .
           if        w-scr-ctr-001        not  = 1
                     go to cmp-sng-fld-360.
       cmp-sng-fld-350.
           move      zero                 to   w-scr-ctr-001          .
           move      w-scr-lun-ele        to   w-scr-ctr-005          .
______*    subtract  1                    from w-scr-ctr-005          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for all   "0"                    .
      *
           if        w-scr-ctr-001        =    w-scr-ctr-005
                     move  spaces         to   w-scr-str-ele          .
       cmp-sng-fld-360.
      *                  *---------------------------------------------*
      *                  * Trattamento eventuale segno                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-scr-str-sgn          .
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for all   "-"                    .
           if        w-scr-ctr-001        not  = 1
                     go to cmp-sng-fld-365.
           inspect   w-scr-str-ele
                                replacing all  "-"
                                          by   "0"                    .
           move      "-"                  to   w-scr-str-sgn          .
       cmp-sng-fld-365.
      *                  *---------------------------------------------*
      *                  * Rimozione zeri in testa                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-scr-str-elx          .
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-scr-str-ele    tallying w-scr-ctr-001
                                     for  leading zero
           if        w-scr-ctr-001        not  > zero
                     go to cmp-sng-fld-370.
      *
           add       1                    to   w-scr-ctr-001          .
           unstring  w-scr-str-ele        into w-scr-str-elx
                                  with pointer w-scr-ctr-001          .
           move      w-scr-str-elx        to   w-scr-str-ele          .
       cmp-sng-fld-370.
      *                  *---------------------------------------------*
      *                  * Assemblaggio con segno eventuale            *
      *                  *---------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-scr-str-sgn        to   w-all-str-cat (1)      .
      *
           if        w-scr-str-ele        =    spaces
                     move  "0"            to   w-all-str-cat (2)
           else      move  w-scr-str-ele  to   w-all-str-cat (2)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-scr-str-ele          .
       cmp-sng-fld-380.
      *                  *---------------------------------------------*
      *                  * Conversione virgole in punti                *
      *                  *---------------------------------------------*
           inspect   w-scr-str-ele
                                replacing all  ","
                                          by   "."                    .
      *                  *---------------------------------------------*
      *                  * A scansione                                 *
      *                  *---------------------------------------------*
           go to     cmp-sng-fld-800.
       cmp-sng-fld-400.
      *              *-------------------------------------------------*
      *              * Data                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preliminari                                 *
      *                  *---------------------------------------------*
           move      12                   to   w-scr-lun-ele          .
      *
           if        w-scr-dat-dat        =    zero
                     move  "'0000-00-00'" 
                                          to   w-scr-str-ele
                     go to cmp-sng-fld-800.
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      w-scr-dat-dat        to   s-dat                  .
           move      s-saa                to   w-scr-dat-ann          .
           if        w-scr-dat-dat        >    zero
                     add  1900            to   w-scr-dat-ann          .
           move      s-mes                to   w-scr-dat-mes          .
           move      s-gio                to   w-scr-dat-gio          .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      spaces               to   w-scr-str-ele          .
           string    "'"        delimited by   size
                     w-scr-dat-ann
                                delimited by   spaces
                     "-"        delimited by   size
                     w-scr-dat-mes
                                delimited by   spaces
                     "-"        delimited by   size
                     w-scr-dat-gio
                                delimited by   spaces
                     "'"        delimited by   size
                                          into w-scr-str-ele          .
      *                  *---------------------------------------------*
      *                  * Test per anomalia '2000-00-00'              *
      *                  *---------------------------------------------*
           if        w-scr-str-ele        =    "'2000-00-00'"
                     move  "'0000-00-00'" 
                                          to   w-scr-str-ele          .
      *                  *---------------------------------------------*
      *                  * A scansione                                 *
      *                  *---------------------------------------------*
           go to     cmp-sng-fld-800.
       cmp-sng-fld-800.
      *              *-------------------------------------------------*
      *              * Scansione per assemblaggio                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-scr-ctr-001          .
           inspect   w-out-str-out    tallying w-scr-ctr-001
                                  for trailing spaces                 .
           move      5120                 to   w-scr-ctr-002          .
           subtract  w-scr-ctr-001        from w-scr-ctr-002          .
           add       1                    to   w-scr-ctr-002          .
           move      w-scr-str-ele        to   w-out-str-out
                                              (w-scr-ctr-002 :
                                               w-scr-lun-ele)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      w-scr-str-tip        to   w-scr-str-sav          .
       cmp-sng-fld-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-sng-fld-999.
       cmp-sng-fld-999.
           exit.

      *    *===========================================================*
      *    * Put next generica per output in sequenziale               *
      *    *-----------------------------------------------------------*
       put-nxt-out-000.
      *              *-------------------------------------------------*
      *              * Scrittura su sequenziale                        *
      *              *-------------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       put-nxt-out-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
