       Identification Division.
       Program-Id.                                 acoddcp0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 02/08/91    *
      *                       Ultima revisione:    NdK del 13/04/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice prodotto 'dcp'   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      *       Metodi di ricerca :                                      *
      *                                                                *
      *       '-'  seguito da return = Ricerca per descrizione         *
      *                                                                *
      *       '-'  seguito da return e FIND : ricerca estesa           *
      *                                                                *
      *       '-'  seguito da return e EXPD : video di aiuto           *
      *                                                                *
      *       '-'  seguito da return e SLCT : ricerca estesa sulla     *
      *                                       descrizione ad uso       *
      *                                       interno                  *
      *                                                                *
      *       '-ALF...'     e return = Ricerca per parte iniziale del  *
      *                                codice prodotto                 *
      *                                                                *
      *       '=SIN...'     e return = Ricerca per valore esatto del   *
      *                                sinonimo prodotto               *
      *                                                                *
      *       '+SIN...'     e return = Ricerca per parte iniziale del  *
      *                                sinonimo prodotto               *
      *                                                                *
      *       'F-' seguito da return = Ricerca del codice prodotto per *
      *                                il Fornitore (valore esatto)    *
      *                                                                *
      *       'F+' seguito da return = Ricerca del codice prodotto per *
      *                                il Fornitore (valore iniziale)  *
      *                                                                *
      *       'P-' seguito da return = Ricerca del codice prodotto per *
      *                                il Produttore (valore esatto)   *
      *                                                                *
      *       'P+' seguito da return = Ricerca del codice prodotto per *
      *                                il Produttore (valore iniziale) *
      *                                                                *
      *       'K-' seguito da return = Ricerca del codice prodotto per *
      *                                il Barcode (valore esatto)      *
      *                                                                *
      *       'K+' seguito da return = Ricerca del codice prodotto per *
      *                                il Barcode (valore iniziale)    *
      *                                                                *
      *       'C-' seguito da return = Ricerca del codice prodotto per *
      *                                il Cliente (valore esatto)      *
      *                                                                *
      *       'C+' seguito da return = Ricerca del codice prodotto per *
      *                                il Cliente (valore iniziale)    *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcp-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-dcp-tac : Tipo accettazione    *
      *                                           - N : Numerico       *
      *                                           - A : Alfanumerico   *
      *                                           - S : Sinonimo       *
      *                                           - L : Libera, alfa-  *
      *                                                 numerica non   *
      *                                                 controllata    *
      *                                                                *
      *                       w-cod-cod-dcp-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-dcp-alf : Codice alfanumerico  *
      *                                             Inutile se tipo    *
      *                                             accettazione 'N'   *
      *                                                                *
      *                       w-cod-cod-dcp-lin : linea a video        *
      *                                                                *
      *                       w-cod-cod-dcp-pos : posizione a video    *
      *                                                                *
      *                       w-cod-cod-dcp-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dcp-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcp-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcp-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-dcp-alf : Codice alfanumerico  *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'A' o   *
      *                                             tipo acc.  'S' o   *
      *                                             tipo acc.  'L'     *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcp-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-dcp-alf : Codice alfanumerico  *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'A' o   *
      *                                             tipo acc.  'S' o   *
      *                                             tipo acc.  'L'     *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-cod-dcp-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcp-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcp-num : Codice numerico      *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'N' o   *
      *                                             tipo acc.  'A'     *
      *                                                                *
      *                       w-cod-cod-dcp-alf : Codice alfanumerico  *
      *                                             Solo se tipo ac-   *
      *                                             cettazione 'A' o   *
      *                                             tipo acc.  'S' o   *
      *                                             tipo acc.  'L'     *
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
      *        * Tipo di match                                         *
      *        *-------------------------------------------------------*
           05  rlt-rec-tip-mch            pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area per anagrafica commerciale prodotto [dcp]        *
      *        *-------------------------------------------------------*
           05  rlt-rec-rec-dcp.
               10  filler occurs 2048     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per record estensioni descrizioni [pdx].01       *
      *        *-------------------------------------------------------*
           05  rlt-rec-rec-x01.
               10  rlt-rec-ele-x01
                               occurs 10  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Area per record estensioni descrizioni [pdx].04       *
      *        *-------------------------------------------------------*
           05  rlt-rec-rec-x04.
               10  rlt-rec-ele-x04
                               occurs 10  pic  x(40)                  .

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
      *        *-------------------------------------------------------*
      *        * Indici di comodo 01..10                               *
      *        *-------------------------------------------------------*
           05  w-rlt-i01                  pic  9(03)                  .
           05  w-rlt-i02                  pic  9(03)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .
      *        *-------------------------------------------------------*
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [pdt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfpdt"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

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
      *    * Work per editings                                         *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Per editing del valore numerico                       *
      *        *-------------------------------------------------------*
           05  w-edt-num.
      *            *---------------------------------------------------*
      *            * Valore numerico da editare                        *
      *            *---------------------------------------------------*
               10  w-edt-num-cod          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Valore numerico editato                           *
      *            *---------------------------------------------------*
               10  w-edt-num-edt          pic  x(14)                  .

      *    *===========================================================*
      *    * Work per salvataggi                                       *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio valore alfanumerico proveniente dall'im-  *
      *        * postazione effettivamente eseguita nel chiamante      *
      *        *-------------------------------------------------------*
           05  w-sav-dac-xxx              pic  x(14)                  .

      *    *===========================================================*
      *    * Work per manipolazioni su ridefinizioni                   *
      *    *-----------------------------------------------------------*
       01  w-man.
      *        *-------------------------------------------------------*
      *        * Work per manipolazione area contenente impostazione   *
      *        *-------------------------------------------------------*
           05  w-man-x14.
      *            *---------------------------------------------------*
      *            * Valore da scomporre in parti                      *
      *            *---------------------------------------------------*
               10  w-man-x14-vlr.
                   15  w-man-x14-cha
                                occurs 14 pic  x(01)                  .
               10  w-man-x14-vlx redefines
                   w-man-x14-vlr.
                   15  w-man-x14-chn
                                occurs 14 pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Flag in uscita                                    *
      *            * - # : Valore non accettabile                      *
      *            * - N : Codice prodotto numerico                    *
      *            * - A : Codice prodotto alfanumerico                *
      *            * - S : Sinonimo                                    *
      *            * - C : Codici che iniziano per ..                  *
      *            * - ? : Richiesta di codice numerico corrispondente *
      *            * - - : Valore per richiesta ricerca per descriz.   *
      *            * - P : Codice alfanumerico per la casa produttrice *
      *            * - F : Codice alfanumerico per il fornitore prefe- *
      *            *       renziale                                    *
      *            *---------------------------------------------------*
               10  w-man-x14-flg          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'N' : Valore numerico                   *
      *            *---------------------------------------------------*
               10  w-man-x14-num          pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'A' : Valore alfanumerico               *
      *            *---------------------------------------------------*
               10  w-man-x14-alf          pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'S' : Valore sinonimo                   *
      *            *---------------------------------------------------*
               10  w-man-x14-sin          pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'S' : Carattere di Wildcard             *
      *            *---------------------------------------------------*
               10  w-man-x14-wlc          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Se flag a 'C' : Valore iniziale codice            *
      *            *---------------------------------------------------*
               10  w-man-x14-vic          pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Tabella caratteri riconosciuti come segnalazione  *
      *            * di sinonimo                                       *
      *            *---------------------------------------------------*
               10  w-man-x14-tss          pic  x(04) value "=+.,"     .
      *            *---------------------------------------------------*
      *            * Tabella caratteri riconosciuti come segnalazione  *
      *            * di richiesta codice numerico                      *
      *            *---------------------------------------------------*
               10  w-man-x14-trc          pic  x(01) value "?"        .
      *            *---------------------------------------------------*
      *            * Work locale campi, contatori, indici              *
      *            *---------------------------------------------------*
               10  w-man-x14-w00.
                   15  w-man-x14-n14      pic  9(14)                  .
                   15  w-man-x14-i01      pic  9(02)                  .
                   15  w-man-x14-i02      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Flag di accettazione tramite bar-code             *
      *            *---------------------------------------------------*
               10  w-man-x14-bcd          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flags di utilizzo selezione precedente sinonimo   *
      *            *---------------------------------------------------*
               10  w-man-x14-fss          pic  x(01)                  .
               10  w-man-x14-fsp          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per le impostazioni per la ricerca per mezzo di un   *
      *    * box locale                                                *
      *    *-----------------------------------------------------------*
       01  w-rbl.
      *        *-------------------------------------------------------*
      *        * Tipo di ricerca                                       *
      *        * - S : Su sinonimo                                     *
      *        * - D : Su descrizione prodotto                         *
      *        * - I : Per iniziale codice                             *
      *        * - T : Ricerca totale                                  *
      *        * - P : Per codice per il produttore                    *
      *        * - F : Per codice per il fornitore preferenziale       *
      *        * - C : Per codice per il cliente                       *
      *        *-------------------------------------------------------*
           05  w-rbl-tip-ric              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di uscita da ricerca                             *
      *        * - spaces : per selezione effettuata                   *
      *        * - N      : per selezione non effettuata               *
      *        * - E      : per exit                                   *
      *        *-------------------------------------------------------*
           05  w-rbl-tip-ext              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice per il produttore minimo                       *
      *        *-------------------------------------------------------*
           05  w-rbl-cdp-min              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice per il produttore massimo                      *
      *        *-------------------------------------------------------*
           05  w-rbl-cdp-max.
               10  w-rbl-cdp-mxx
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice per il fornitore preferenziale minimo          *
      *        *-------------------------------------------------------*
           05  w-rbl-cfn-min              pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Codice per il fornitore preferenziale massimo         *
      *        *-------------------------------------------------------*
           05  w-rbl-cfn-max.
               10  w-rbl-cfn-mxx
                               occurs 14  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice per il cliente minimo                          *
      *        *-------------------------------------------------------*
           05  w-rbl-cpc-min              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice per il cliente massimo                         *
      *        *-------------------------------------------------------*
           05  w-rbl-cpc-max.
               10  w-rbl-cpc-mxx
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice libero minimo                                  *
      *        *-------------------------------------------------------*
           05  w-rbl-klb-min              pic  x(13)                  .
      *        *-------------------------------------------------------*
      *        * Codice libero massimo                                 *
      *        *-------------------------------------------------------*
           05  w-rbl-klb-max.
               10  w-rbl-klb-mxx
                               occurs 13  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore sinonimo minimo                                *
      *        *-------------------------------------------------------*
           05  w-rbl-sin-min              pic  x(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore sinonimo massimo                               *
      *        *-------------------------------------------------------*
           05  w-rbl-sin-max.
               10  w-rbl-sin-mxx
                               occurs 13  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore descrizione per l'accettazione                 *
      *        *-------------------------------------------------------*
           05  w-rbl-des-acc              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valore descrizione minimo                             *
      *        *-------------------------------------------------------*
           05  w-rbl-des-min              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Valore descrizione massimo                            *
      *        *-------------------------------------------------------*
           05  w-rbl-des-max.
               10  w-rbl-des-mxx
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work locale contatori ed indici                       *
      *        *-------------------------------------------------------*
           05  w-rbl-wrk-loc.
               10  w-rbl-ctr-001          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Flag di accettazione di 5 campi alfanumerici          *
      *        *-------------------------------------------------------*
           05  w-rbl-des-fl5              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per l'accettazione di 5 campi alfanumerici     *
      *        *-------------------------------------------------------*
           05  w-rbl-des-al1              pic  x(40)                  .
           05  w-rbl-des-al2              pic  x(40)                  .
           05  w-rbl-des-al3              pic  x(40)                  .
           05  w-rbl-des-al4              pic  x(40)                  .
           05  w-rbl-des-al5              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Flag di match di 5 campi alfanumerici                 *
      *        *-------------------------------------------------------*
           05  w-rbl-des-fm5              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per il buffer relativo al box locale                 *
      *    *-----------------------------------------------------------*
       01  w-buf.
      *        *-------------------------------------------------------*
      *        * Contatore records caricati nel buffer                 *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-rec              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Max nr records caricabili nel buffer                  *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-max              pic  9(04) value 200        .
      *        *-------------------------------------------------------*
      *        * Indice elemento da selezionare dal buffer             *
      *        *-------------------------------------------------------*
           05  w-buf-ctr-ibx              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Tabella records caricati                              *
      *        *-------------------------------------------------------*
           05  w-buf-tbl-buf.
      *            *---------------------------------------------------*
      *            * Elemento della tabella records caricati           *
      *            *---------------------------------------------------*
               10  w-buf-ele-buf occurs 200.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-buf-key-ord      pic  x(68)                  .
      *                *-----------------------------------------------*
      *                * Ridefinizione di tipo '01'                    *
      *                *-----------------------------------------------*
                   15  w-buf-key-ord-01  redefines
                       w-buf-key-ord.
      *                    *-------------------------------------------*
      *                    * Codice sinonimo prodotto                  *
      *                    *-------------------------------------------*
                       20  w-buf-key-ord-syn-01
                                          pic  x(14)                  .
      *                    *-------------------------------------------*
      *                    * Desrizione prodotto                       *
      *                    *-------------------------------------------*
                       20  w-buf-key-ord-des-01
                                          pic  x(40)                  .
      *                    *-------------------------------------------*
      *                    * Codice alfanumerico prodotto              *
      *                    *-------------------------------------------*
                       20  w-buf-key-ord-alf-01
                                          pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Tipo di match                                 *
      *                *-----------------------------------------------*
                   15  w-buf-tip-mch      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico del prodotto                  *
      *                *-----------------------------------------------*
                   15  w-buf-cod-num      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice alfanumerico del prodotto              *
      *                *-----------------------------------------------*
                   15  w-buf-cod-alf      pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Codice sinonimo del prodotto                  *
      *                *-----------------------------------------------*
                   15  w-buf-syn-pro      pic  x(13)                  .
      *                *-----------------------------------------------*
      *                * Descrizione del prodotto                      *
      *                *-----------------------------------------------*
                   15  w-buf-des-pro      pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Codice del prodotto per il produttore o per   *
      *                * il fornitore o codice libero                  *
      *                *-----------------------------------------------*
                   15  w-buf-cdp-pfc      pic  x(20)                  .
      *                *-----------------------------------------------*
      *                * Codice del produttore o fornitore o classe    *
      *                *-----------------------------------------------*
                   15  w-buf-cod-pfc      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale del produttore o fornitore o  *
      *                * descrizione classe                            *
      *                *-----------------------------------------------*
                   15  w-buf-rod-pfc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine visualizzabili del buffer               *
      *        *-------------------------------------------------------*
           05  w-buf-tot-pag              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina attualmente trattata                    *
      *        *-------------------------------------------------------*
           05  w-buf-pag-att              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice primo elemento pagina trattata                 *
      *        *-------------------------------------------------------*
           05  w-buf-pag-ipe              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice ultimo elemento pagina trattata                *
      *        *-------------------------------------------------------*
           05  w-buf-pag-iue              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Indice per elemento del buffer attualmente trattato   *
      *        *-------------------------------------------------------*
           05  w-buf-inx-buf              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Linea a video per elemento del buffer trattato        *
      *        *-------------------------------------------------------*
           05  w-buf-num-lnv              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento          *
      *        *-------------------------------------------------------*
           05  w-buf-rig-svk              pic  x(68)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici di comodo locali                  *
      *        *-------------------------------------------------------*
           05  w-buf-i01-lnv              pic  9(04)                  .
           05  w-buf-ctr-001              pic  9(05)                  .
           05  w-buf-ctr-002              pic  9(05)                  .
           05  w-buf-ctr-003              pic  9(05)                  .
           05  w-buf-ctr-004              pic  9(05)                  .
           05  w-buf-ctr-005              pic  9(04)                  .
           05  w-buf-lit-pag              pic  x(15)                  .
           05  w-buf-lit-att              pic  x(02)                  .
           05  w-buf-lit-tot              pic  x(02)                  .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-ble.
           05  w-ble-max                  pic  9(02)                  .
           05  w-ble-flg                  pic  x(01)                  .
           05  w-ble-str.
               10  w-ble-chr    occurs 40 pic  x(01)                  .
           05  w-ble-ctr                  pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/no gestione bar-code per prodotti                  *
      *        *-------------------------------------------------------*
           05  w-prs-snx-gbc.
      *            *---------------------------------------------------*
      *            * Si/no gestione                                    *
      *            *---------------------------------------------------*
               10  w-prs-snx-gbc-sng      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no inserimento automatico                      *
      *            *---------------------------------------------------*
               10  w-prs-snx-gbc-sni      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no gestione barcode confezione                 *
      *            *---------------------------------------------------*
               10  w-prs-snx-gbc-snc      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore della personalizzazione sul tipo di ricerca    *
      *        * totale da eseguire                                    *
      *        *  - A : Solo su anagrafica commerciale                 *
      *        *  - T : Anche su descrizione per il listino            *
      *        *-------------------------------------------------------*
           05  w-prs-trt-pf1              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore della personalizzazione sul tipo di visualiz-  *
      *        * zazione disponibilita'                                *
      *        *  - M : da gestione Magazzino                          *
      *        *  - O : da gestione Ordini clienti                     *
      *        *-------------------------------------------------------*
           05  w-prs-trt-pf2              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no ordinamento per descrizione                     *
      *        *-------------------------------------------------------*
           05  w-prs-snx-opd              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutine di ricerca totale                     *
      *    *-----------------------------------------------------------*
       01  w-rcr-tot-dcp.
      *        *-------------------------------------------------------*
      *        * Valore da ricercare, normalizzato                     *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-vnr          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records esaminati                           *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-cre          pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Flag di fine ricerca                                  *
      *        *  - Spaces : No                                        *
      *        *  - S      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-ffr          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di selezione effettuata                          *
      *        *  - Spaces : No                                        *
      *        *  - S      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-fsl          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records selezionati                         *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-crs          pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Indice di match                                       *
      *        *  - 00 : No match                                      *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-mch          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per test su resto pari a 10 o 100                *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-dcp-wr1          pic  9(01)                  .
           05  w-rcr-tot-dcp-wr2          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per normalizzazione di un campo alfabetico in un va- *
      *    * lore privo di spaces e di caratteri non compresi tra i    *
      *    * limiti A..Z - 0..9                                        *
      *    *                                                           *
      *    * Work per match tra due valori cosi' normalizzati          *
      *    *-----------------------------------------------------------*
       01  w-atz-1t9.
      *        *-------------------------------------------------------*
      *        * Valore da normalizzare, o primo valore per il match   *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-vdn.
               10  w-atz-1t9-vdn-chr
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore normalizzato, o secondo valore per il match    *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-vno.
               10  w-atz-1t9-vno-chr
                               occurs 40  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di match in uscita                               *
      *        *  - Spaces : Ok, match                                 *
      *        *  - N      : Ko, no match                              *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-flg-mch          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici locali                            *
      *        *-------------------------------------------------------*
           05  w-atz-1t9-inx-vdn          pic  9(02)                  .
           05  w-atz-1t9-inx-vno          pic  9(02)                  .
           05  w-atz-1t9-ctr-mch          pic  9(02)                  .

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
      *    * Link-area per accettazione codice prodotto                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-dcp
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
           if        w-cod-cod-dcp-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-dcp-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-dcp-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-dcp-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-dcp-ope    =    "A+" or
                     w-cod-cod-dcp-ope    =    "I+" or
                     w-cod-cod-dcp-ope    =    "F+"
                     perform   aco-000    thru aco-999                .
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
       opn-200.
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione relativa al tipo  *
      *                  * di ricerca totale                           *
      *                  *---------------------------------------------*
           perform   prs-trt-pf1-000      thru prs-trt-pf1-999        .
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione relativa a si/no  *
      *                  * gestione bar-code prodotti                  *
      *                  *---------------------------------------------*
           perform   prs-snx-gbc-000      thru prs-snx-gbc-999        .
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione relativa a si/no  *
      *                  * ordinamento per descrizione                 *
      *                  *---------------------------------------------*
           perform   prs-snx-opd-000      thru prs-snx-opd-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di primo elemento da   *
      *                  * estrarre                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-cod-dcp-fpe      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione sinonimi min e max          *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-sin-min          .
           move      spaces               to   w-rbl-sin-max          .
       opn-300.
      *                  *---------------------------------------------*
      *                  * Open files                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * [dcp]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * [pdx]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                      *-----------------------------------------*
      *                      * [pdk]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                      *-----------------------------------------*
      *                      * [zp1]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                      *-----------------------------------------*
      *                      * [aaq]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                      *-----------------------------------------*
      *                      * [aaf]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                      *-----------------------------------------*
      *                      * [dcf]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                      *-----------------------------------------*
      *                      * [pdt]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *                      *-----------------------------------------*
      *                      * [dcc]                                   *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Open file relative di appoggio [rlt]    *
      *                      *-----------------------------------------*
           perform   rlt-opn-000          thru rlt-opn-999            .
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
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
       cls-300.
      *                  *---------------------------------------------*
      *                  * Close files                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * [dcp]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * [pdx]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                      *-----------------------------------------*
      *                      * [pdk]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                      *-----------------------------------------*
      *                      * [zp1]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *                      *-----------------------------------------*
      *                      * [aaq]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                      *-----------------------------------------*
      *                      * [aaf]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                      *-----------------------------------------*
      *                      * [dcf]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                      *-----------------------------------------*
      *                      * [pdt]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *                      *-----------------------------------------*
      *                      * [dcc]                                   *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Close file relative di appoggio [rlt]   *
      *                      *-----------------------------------------*
           perform   rlt-cls-000          thru rlt-cls-999            .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo ricerca totale           *
      *    *-----------------------------------------------------------*
       prs-trt-pf1-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[trt-pf1]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-trt-pf1
           else      move  "A"            to   w-prs-trt-pf1          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-trt-pf1        not  = "T"
                     move  "A"            to   w-prs-trt-pf1          .
       prs-trt-pf1-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo visualizzazione disponi- *
      *    *                             bilita'                       *
      *    *-----------------------------------------------------------*
       prs-trt-pf2-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[trt-pf2]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-trt-pf2
           else      move  "M"            to   w-prs-trt-pf2          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-trt-pf2        not  = "O"
                     move  "M"            to   w-prs-trt-pf2          .
       prs-trt-pf2-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione bar-code       *
      *    *                             prodotti                      *
      *    *-----------------------------------------------------------*
       prs-snx-gbc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[snx-gbc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-gbc
           else      move  spaces         to   w-prs-snx-gbc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-gbc-sng    not   = "S"
                     move  "N"            to   w-prs-snx-gbc-sng      .
           if        w-prs-snx-gbc-sni    not   = "S"
                     move  "N"            to   w-prs-snx-gbc-sni      .
           if        w-prs-snx-gbc-snc    not   = "S"
                     move  "N"            to   w-prs-snx-gbc-snc      .
       prs-snx-gbc-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No ordinamento per descri- *
      *    *                             zione                         *
      *    *-----------------------------------------------------------*
       prs-snx-opd-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcp[snx-opd]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-opd
           else      move  spaces         to   w-prs-snx-opd          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-opd        not   = "S"
                     move  "N"            to   w-prs-snx-opd          .
       prs-snx-opd-999.
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
                     move  spaces         to   w-cod-cod-dcp-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Trattamento overlay per Pf2                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione relativa al tipo  *
      *                  * visualizzazione disponibilita'              *
      *                  *---------------------------------------------*
           perform   prs-trt-pf2-000      thru prs-trt-pf2-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione overlay di esecuzione Pf2   *
      *                  *---------------------------------------------*
           move      w-prs-trt-pf2        to   w-cod-cod-dcp-of2      .
       acc-050.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcp4010"           to   s-pro                  .
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
           move      "pdcp4000"           to   s-pro                  .
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
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di impostazione Pf2    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-cod-dcp-pf2      .
      *                  *---------------------------------------------*
      *                  * Se tipo accettazione diverso da 'A' : no    *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-tac    not  = "A"
                     go to acc-200.
      *                  *---------------------------------------------*
      *                  * Test se programma di interrogazione su di-  *
      *                  * sponibilita' di magazzino gia' attivo       *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
      *
           if        w-cod-cod-dcp-of2    =    "O"
                     move  "porc500s"     to   s-pro
           else      move  "pmag3010"     to   s-pro                  .
      *
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to acc-200.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura variabile globale di   *
      *                  * i.p.c. 'acoddcp0.[2]'                       *
      *                  *---------------------------------------------*
           move      "GV"                 to   s-ope                  .
           move      "acoddcp0.[2]"       to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se variabile esistente : no abilitazione    *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to acc-200.
      *                  *---------------------------------------------*
      *                  * Abilitazione tasto Pf2                      *
      *                  *---------------------------------------------*
           move      "[2] "               to   v-pfk (15)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico                             *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-tac    =    "N" or
                     w-cod-cod-dcp-tac    =    "A"
                     move  w-cod-cod-dcp-num
                                          to   w-cod-cod-dcp-s01
           else      move  zero           to   w-cod-cod-dcp-s01      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico                         *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-tac    =    "N"
                     move  spaces         to   w-cod-cod-dcp-s02
           else      move  w-cod-cod-dcp-alf
                                          to   w-cod-cod-dcp-s02      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing v-edm                   *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-cod-dcp-s70      .
      *                  *---------------------------------------------*
      *                  * User function-keys eventualmente epurate    *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-cod-dcp-s90      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo accettazione    *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-tac    =    "N"
                     go to acc-500
           else      go to acc-600.
       acc-500.
      *              *-------------------------------------------------*
      *              * Se tipo accettazione 'N'                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione valore numerico editato        *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcp-s01    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione                   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-s90    to   v-ufk                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : continuazione           *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cod-cod-dcp-ope      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-999.
       acc-600.
      *              *-------------------------------------------------*
      *              * Se tipo accettazione 'A' - 'S' - 'L'            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione                   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cod-cod-dcp-tac    =    "S"
                     move  13             to   v-car
           else      move  14             to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-s90    to   v-ufk                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-s02    to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : continuazione           *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cod-cod-dcp-ope      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-999.
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-ope    =    "A+"
                     go to aco-300
           else if   w-cod-cod-dcp-ope    =    "F+"
                     go to aco-200.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Insr                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Reimpostazione, a seconda del tipo di ac-   *
      *                  * cettazione                                  *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-tac    =    "N"
                     go to aco-125
           else      go to aco-150.
       aco-125.
      *                  *---------------------------------------------*
      *                  * Se tipo accettazione 'N'                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione valore numerico editato    *
      *                      *-----------------------------------------*
           move      w-cod-cod-dcp-s01    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-s90    to   v-ufk                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-150.
      *                  *---------------------------------------------*
      *                  * Se tipo accettazione 'A'  'S'  'L'          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cod-cod-dcp-tac    =    "S"
                     move  13             to   v-car
           else      move  14             to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-s90    to   v-ufk                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-s02    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Find                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della variabile di i.p.c. di ritor- *
      *                  * no dall'interrogazione, e deviazione in     *
      *                  * funzione dell'esito della lettura           *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-pro"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     go to aco-220.
       aco-210.
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. non esistente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-100.
       aco-220.
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. esistente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di accet- *
      *                      * tazione                                 *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcp-tac    =    "N"
                     go to aco-230
           else      go to aco-240.
       aco-230.
      *                      *-----------------------------------------*
      *                      * Se tipo di accettazione 'N'             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore selezionato in area di usci- *
      *                          * ta numerica                         *
      *                          *-------------------------------------*
           move      s-num                to   w-cod-cod-dcp-num      .
      *                          *-------------------------------------*
      *                          * Area di uscita alfanumerica a spa-  *
      *                          * ces                                 *
      *                          *-------------------------------------*
           move      spaces               to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Editing del valore in uscita        *
      *                          *-------------------------------------*
           move      w-cod-cod-dcp-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                          *-------------------------------------*
      *                          * Visualizzazione del valore editato  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo operazione : non-continuazione *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Normalizzazione function-key        *
      *                          *-------------------------------------*
           move      spaces               to   v-key                  .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-240.
      *                      *-----------------------------------------*
      *                      * Se tipo di accettazione 'A'  'S '  'L'  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura del record 'dcp' relativo   *
      *                          * al codice numerico ritornato, e de- *
      *                          * viazione a seconda dell'esito della *
      *                          * lettura                             *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      s-num                to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                =    e-not-err
                     go to aco-260.
       aco-250.
      *                          *-------------------------------------*
      *                          * Se record 'dcp' non esistente       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     aco-100.
       aco-260.
      *                          *-------------------------------------*
      *                          * Se record 'dcp' esistente           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-dcp-tac    =    "A"
                     go to aco-280.
       aco-270.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'S'  'L'   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Area di uscita numerica in  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      rf-dcp-num-pro       to   w-cod-cod-dcp-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico conte-  *
      *                                  * nuto nel record in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      rf-dcp-alf-pro       to   w-cod-cod-dcp-alf      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico selezionato    *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-280.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'A'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico conte-  *
      *                                  * nuto nel record in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      rf-dcp-alf-pro       to   w-cod-cod-dcp-alf      .
      *                                  *-----------------------------*
      *                                  * Valore numerico contenuto   *
      *                                  * nel record in area di usci- *
      *                                  * ta                          *
      *                                  *-----------------------------*
           move      rf-dcp-num-pro       to   w-cod-cod-dcp-num      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico selezionato    *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-300.
      *              *-------------------------------------------------*
      *              * Se rientro ne' dopo Insr ne' dopo Find          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt :                            *
      *                  * - Tipo operazione : non-continuazione       *
      *                  * - Uscita                                    *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "DELT"
                     move  "AC"           to   w-cod-cod-dcp-ope
                     go to aco-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore alfanumerico proveniente *
      *                  * dall'impostazione effettivamente eseguita   *
      *                  * nel chiamante                               *
      *                  *---------------------------------------------*
           move      v-alf                to   w-sav-dac-xxx          .
      *                  *---------------------------------------------*
      *                  * Se valore impostato a Spaces                *
      *                  *---------------------------------------------*
           if        w-sav-dac-xxx        not  = spaces
                     go to aco-320.
      *                      *-----------------------------------------*
      *                      * Valore numerico in uscita a zero        *
      *                      *-----------------------------------------*
           move      zero                 to   w-cod-cod-dcp-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico in uscita a Spaces  *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-cod-dcp-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione dell'area destinata al- *
      *                      * l'impostazione del codice a Spaces      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione : non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-320.
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded                     *
      *                  *---------------------------------------------*
           move      w-sav-dac-xxx        to   w-ble-str              .
           move      14                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            =    spaces
                     go to aco-360.
       aco-340.
      *                  *---------------------------------------------*
      *                  * Reimpostazione con default pari al valore   *
      *                  * alfanumerico salvato proveniente dal chia-  *
      *                  * mante                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           if        w-cod-cod-dcp-tac    =    "S"
                     move  13             to   v-car
           else      move  14             to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-s90    to   v-ufk                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-sav-dac-xxx        to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-360.
      *                  *---------------------------------------------*
      *                  * Precablaggio flag di utilizzo selezione     *
      *                  * precedente per sinonimo                     *
      *                  *---------------------------------------------*
           if        w-rbl-sin-min        =    spaces and
                     w-rbl-sin-max        =    spaces
                     move  "#"            to   w-man-x14-fss
           else      move  spaces         to   w-man-x14-fss          .
      *                  *---------------------------------------------*
      *                  * Suddivisione ed analisi codice impostato di *
      *                  * 14 caratteri alfanumerici proveniente dal   *
      *                  * chiamante                                   *
      *                  *---------------------------------------------*
           move      w-sav-dac-xxx        to   w-man-x14-vlr          .
           perform   sac-000              thru sac-999                .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del risultato del-   *
      *                  * la suddivisione ed analisi                  *
      *                  *---------------------------------------------*
           if        w-man-x14-flg        =    "P"
                     go to aco-370
           else if   w-man-x14-flg        =    "F"
                     go to aco-375
           else if   w-man-x14-flg        =    "N"
                     go to aco-380
           else if   w-man-x14-flg        =    "C"
                     go to aco-385
           else if   w-man-x14-flg        =    "K"
                     go to aco-390
           else if   w-man-x14-flg        =    "A"
                     go to aco-400
           else if   w-man-x14-flg        =    "S"
                     go to aco-500
           else if   w-man-x14-flg        =    "?"
                     go to aco-530
           else if   w-man-x14-flg        =    "-" or
                     w-man-x14-flg        =    "I"
                     go to aco-540
           else      go to aco-340.
       aco-370.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'P'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'ese-   *
      *                      * cuzione della ricerca per mezzo di un   *
      *                      * box locale per la selezione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca : 'P'               *
      *                          *-------------------------------------*
           move      "P"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Subroutine di ricerca               *
      *                          *-------------------------------------*
           perform   aco-pdt-000          thru aco-pdt-999            .
      *                          *-------------------------------------*
      *                          * Test sul tipo di uscita             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ext        =    spaces
                     go to aco-373
           else if   w-rbl-tip-ext        =    "N"
                     go to aco-480
           else      go to aco-340.
       aco-373.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-375.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'F'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'ese-   *
      *                      * cuzione della ricerca per mezzo di un   *
      *                      * box locale per la selezione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca : 'F'               *
      *                          *-------------------------------------*
           move      "F"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Subroutine di ricerca               *
      *                          *-------------------------------------*
           perform   aco-fnt-000          thru aco-fnt-999            .
      *                          *-------------------------------------*
      *                          * Test sul tipo di uscita             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ext        =    spaces
                     go to aco-383
           else if   w-rbl-tip-ext        =    "N"
                     go to aco-480
           else      go to aco-340.
       aco-383.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-380.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'N'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di accettazione non e' 'N'   *
      *                      * si torna alla reimpostazione con de-    *
      *                      * fault pari al valore salvato            *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcp-tac    not  = "N"
                     go to aco-340.
      *                      *-----------------------------------------*
      *                      * Se il tipo di accettazione e' 'N' si e- *
      *                      * sce e si ritorna al chiamante           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore impostato in area di uscita  *
      *                          * ta numerica                         *
      *                          *-------------------------------------*
           move      w-man-x14-num        to   w-cod-cod-dcp-num      .
      *                          *-------------------------------------*
      *                          * Area di uscita alfanumerica a spa-  *
      *                          * ces                                 *
      *                          *-------------------------------------*
           move      spaces               to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Editing del valore in uscita        *
      *                          *-------------------------------------*
           move      w-cod-cod-dcp-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                          *-------------------------------------*
      *                          * Visualizzazione del valore editato  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo operazione : non-continuazione *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-385.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'C'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'ese-   *
      *                      * cuzione della ricerca per mezzo di un   *
      *                      * box locale per la selezione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca : 'C'               *
      *                          *-------------------------------------*
           move      "C"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Subroutine di ricerca               *
      *                          *-------------------------------------*
           perform   aco-cli-000          thru aco-cli-999            .
      *                          *-------------------------------------*
      *                          * Test sul tipo di uscita             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ext        =    spaces
                     go to aco-387
           else if   w-rbl-tip-ext        =    "N"
                     go to aco-480
           else      go to aco-340.
       aco-387.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-390.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'K'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'ese-   *
      *                      * cuzione della ricerca per mezzo di un   *
      *                      * box locale per la selezione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca : 'K'               *
      *                          *-------------------------------------*
           move      "K"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Subroutine di ricerca               *
      *                          *-------------------------------------*
           perform   aco-klb-000          thru aco-klb-999            .
      *                          *-------------------------------------*
      *                          * Test sul tipo di uscita             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ext        =    spaces
                     go to aco-395
           else if   w-rbl-tip-ext        =    "N"
                     go to aco-480
           else      go to aco-340.
       aco-395.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'A'            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di accet- *
      *                      * tazione                                 *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcp-tac    =    "A" or
                     w-cod-cod-dcp-tac    =    "L" or
                     w-cod-cod-dcp-tac    =    "N"
                     go to aco-440.
       aco-420.
      *                      *-----------------------------------------*
      *                      * Se tipo accettazione 'S'                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore impostato in area di uscita  *
      *                          * ta alfanumerica                     *
      *                          *-------------------------------------*
           move      w-man-x14-alf        to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Area di uscita numerica a zero      *
      *                          *-------------------------------------*
           move      zero                 to   w-cod-cod-dcp-num      .
      *                          *-------------------------------------*
      *                          * Tipo operazione : non-continuazione *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-999.
       aco-440.
      *                      *-----------------------------------------*
      *                      * Se tipo accettazione 'A', 'L' o 'N'     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su personalizzazione relativa  *
      *                          * ai bar-code                         *
      *                          *-------------------------------------*
           if        w-prs-snx-gbc-sng    not  = "S"
                     go to aco-450.
       aco-445.
      *                          *-------------------------------------*
      *                          * Lettura del record 'dcp' relativo   *
      *                          * al codice alfanumerico impostato,   *
      *                          * per vedere se si tratta di bar-code *
      *                          *-------------------------------------*
           perform   aco-bcd-000          thru aco-bcd-999            .
           if        w-man-x14-bcd        not  = spaces
                     go to aco-460.
       aco-450.
      *                          *-------------------------------------*
      *                          * Se tipo accettazione 'L' : oltre    *
      *                          *-------------------------------------*
           if        w-cod-cod-dcp-tac    not  = "L"
                     go to aco-455.
      *                              *---------------------------------*
      *                              * Valore impostato in area di     *
      *                              * uscita alfanumerica             *
      *                              *---------------------------------*
           move      w-man-x14-alf        to   w-cod-cod-dcp-alf      .
      *                              *---------------------------------*
      *                              * Area di uscita numerica a zero  *
      *                              *---------------------------------*
           move      zero                 to   w-cod-cod-dcp-num      .
      *                              *---------------------------------*
      *                              * Tipo operazione : non-continuaz.*
      *                              *---------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-455.
      *                          *-------------------------------------*
      *                          * Lettura del record 'dcp' relativo   *
      *                          * al codice alfanumerico impostato,   *
      *                          * e deviazione a seconda dell'esito   *
      *                          * della lettura                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Start                           *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ALFPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-man-x14-alf        to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * Se start errata : a record non  *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Read next per il primo record   *
      *                              *---------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * Se At End : a record non esist. *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-480.
      *                              *---------------------------------*
      *                              * Se il codice alfanumerico con-  *
      *                              * tenuto nel record non e' pari a *
      *                              * quello cercato : a record non   *
      *                              * esistente                       *
      *                              *---------------------------------*
           if        rf-dcp-alf-pro       not  = w-man-x14-alf
                     go to aco-480.
       aco-460.
      *                          *-------------------------------------*
      *                          * Se trovato il record 'dcp' cercato  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore impostato in area di u-  *
      *                              * scita alfanumerica              *
      *                              *---------------------------------*
           move      w-man-x14-alf        to   w-cod-cod-dcp-alf      .
      *                              *---------------------------------*
      *                              * Codice prodotto numerico con-   *
      *                              * tenuto in record 'dcp' in area  *
      *                              * di uscita numerica              *
      *                              *---------------------------------*
           move      rf-dcp-num-pro       to   w-cod-cod-dcp-num      .
       aco-465.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-dcp-tac    =    "N"
                     go to aco-475.
       aco-470.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'A'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se flag di impostazione ta- *
      *                                  * sto Pf2 attivo : uscita con *
      *                                  * valore chiave impostata a   *
      *                                  * spazi e tipo operazione F+  *
      *                                  *-----------------------------*
           if        w-cod-cod-dcp-pf2    not  = spaces
                     move  spaces         to   v-key
                     move  "F+"           to   w-cod-cod-dcp-ope
                     go to aco-999.
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Test su personalizzazione   *
      *                                  * bar-code                    *
      *                                  *-----------------------------*
           if        w-prs-snx-gbc-sng    not  = "S"
                     go to aco-473.
      *                                  *-----------------------------*
      *                                  * Test su flag di bar-code    *
      *                                  *-----------------------------*
           if        w-man-x14-bcd        =    spaces
                     go to aco-473.
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico in uscita      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-473.
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-475.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'N'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing del valore numerico *
      *                                  * in uscita                   *
      *                                  *-----------------------------*
           move      w-cod-cod-dcp-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * numerico in uscita editato  *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione : non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-480.
      *                          *-------------------------------------*
      *                          * Se record 'dcp' relativo al codice  *
      *                          * alfanumerico impostato non esisten- *
      *                          * te : a reimpostazione con default   *
      *                          * pari al valore salvato              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione puntini in area *
      *                              * per la descrizione              *
      *                              *---------------------------------*
           if        w-cod-cod-dcp-dln    =    zero
                     go to aco-482.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-482.
      *                              *---------------------------------*
      *                              * Normalizzazione flag di impo-   *
      *                              * stazione tasto Pf2              *
      *                              *---------------------------------*
           if        w-cod-cod-dcp-pf2    not  = spaces
                     move  spaces         to   w-cod-cod-dcp-pf2      .
      *                              *---------------------------------*
      *                              * A reimpostazione con default    *
      *                              * pari al valore salvato          *
      *                              *---------------------------------*
           go to     aco-340.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo 'S'            *
      *                  *---------------------------------------------*
       aco-510.
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'ese-   *
      *                      * cuzione della ricerca per mezzo di un   *
      *                      * box locale per la selezione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di ricerca : 'S'               *
      *                          *-------------------------------------*
           move      "S"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Se la preparazione ha determinato   *
      *                          * l'utilizzo della selezione prece-   *
      *                          * dente, non si precablano max e min  *
      *                          *-------------------------------------*
           if        w-man-x14-fsp        not  = spaces
                     go to aco-520.
      *                          *-------------------------------------*
      *                          * Valore sinonimo iniziale            *
      *                          *-------------------------------------*
           move      w-man-x14-sin        to   w-rbl-sin-min          .
      *                          *-------------------------------------*
      *                          * Valore sinonimo finale              *
      *                          *-------------------------------------*
           move      w-man-x14-sin        to   w-rbl-sin-max          .
      *                          *-------------------------------------*
      *                          * Se valore wildcard diverso da '+' : *
      *                          * oltre                               *
      *                          *-------------------------------------*
           if        w-man-x14-wlc        not  = "+"
                     go to aco-520.
       aco-515.
      *                          *-------------------------------------*
      *                          * Padding del sinonimo finale con 'z' *
      *                          *-------------------------------------*
           move      13                   to   w-rbl-ctr-001          .
       aco-516.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-sin-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-sin-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-516.
       aco-520.
      *                          *-------------------------------------*
      *                          * Alla ricerca per mezzo del box lo-  *
      *                          * cale di selezione                   *
      *                          *-------------------------------------*
           go to     aco-600.
       aco-530.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo '?' per ri-    *
      *                  * chiesta del codice numerico corrispondente  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo di accettazione non e' 'A'   *
      *                      * si torna alla reimpostazione con de-    *
      *                      * fault pari al valore originale          *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcp-tac    not  = "A"
                     go to aco-100.
      *                      *-----------------------------------------*
      *                      * Altrimenti si mostra il codice numeri-  *
      *                      * co corrispondente in un box locale      *
      *                      *-----------------------------------------*
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
      *                          * Visualizzazione box vuoto           *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      13                   to   v-lto                  .
           move      67                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Interno del box                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Titolo                          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      33                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      16                   to   v-pos                  .
           move      "Codice numerico corrispondente : "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Codice numerico editato         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing                     *
      *                                  *-----------------------------*
           move      w-cod-cod-dcp-s01    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * numerico editato            *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      12                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Parentesi quadre                *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Accettazione presa visione          *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      64                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ripristino immagine video           *
      *                          *-------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Quindi si torna alla reimpostazione con *
      *                      * default pari al valore originale        *
      *                      *-----------------------------------------*
           go to     aco-100.
       aco-540.
      *                  *---------------------------------------------*
      *                  * Se la suddivisione ed analisi codice impo-  *
      *                  * stato ha determinato il tipo '-' o 'I'      *
      *                  *---------------------------------------------*
       aco-541.
      *                      *-----------------------------------------*
      *                      * Se il tipo impostazione e' 'I' si si-   *
      *                      * mula una impostazione di descrizione    *
      *                      * pari al codice impostato, se ne deter-  *
      *                      * mina il minimo ed il massimo, e ci si   *
      *                      * riallaccia alla ricerca per descrizio-  *
      *                      * ne                                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-man-x14-flg        not  = "I"
                     go to aco-542.
      *                          *-------------------------------------*
      *                          * Tipo di ricerca : 'I'               *
      *                          *-------------------------------------*
           move      "I"                  to   w-rbl-tip-ric          .
      *                          *-------------------------------------*
      *                          * Preparazione descrizione con il va- *
      *                          * lore iniziale del codice            *
      *                          *-------------------------------------*
           move      w-man-x14-vic        to   w-rbl-des-acc          .
      *                          *-------------------------------------*
      *                          * Riaggancio comune                   *
      *                          *-------------------------------------*
           go to     aco-570.
       aco-542.
      *                      *-----------------------------------------*
      *                      * Tipo di ricerca : 'D'                   *
      *                      *-----------------------------------------*
           move      "D"                  to   w-rbl-tip-ric          .
      *                      *-----------------------------------------*
      *                      * Se non e' specificata una linea per la  *
      *                      * descrizione si ritorna alla reimposta-  *
      *                      * zione con default pari al valore sal-   *
      *                      * vato                                    *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcp-dln    =    zero or
                     w-cod-cod-dcp-dps    =    zero
                     go to aco-340.
       aco-545.
      *                      *-----------------------------------------*
      *                      * Accettazione descrizione per ricerca    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Spaces in comodo per impostazione   *
      *                          * descrizione                         *
      *                          *-------------------------------------*
           move      spaces               to   w-rbl-des-acc          .
      *                          *-------------------------------------*
      *                          * Visualizzazione spaces in area di   *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-550.
      *                          *-------------------------------------*
      *                          * Accettazione descrizione minima di  *
      *                          * ricerca, in uppercase               *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXPD"               to   v-pfk (15)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      w-rbl-des-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Valore accettato in area di comodo  *
      *                          * di destinazione                     *
      *                          *-------------------------------------*
           move      v-alf                to   w-rbl-des-acc          .
       aco-551.
      *                          *-------------------------------------*
      *                          * Se Up                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-555.
      *                              *---------------------------------*
      *                              * Visualizzazione spaces in area  *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-552.
      *                              *---------------------------------*
      *                              * Alla reimpostazione con default *
      *                              * pari al valore salvato          *
      *                              *---------------------------------*
           go to     aco-340.
       aco-555.
      *                          *-------------------------------------*
      *                          * Se Exit                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-560.
      *                              *---------------------------------*
      *                              * Ripristino valori originali     *
      *                              *---------------------------------*
           move      w-cod-cod-dcp-s01    to   w-cod-cod-dcp-num      .
           move      w-cod-cod-dcp-s02    to   w-cod-cod-dcp-alf      .
      *                              *---------------------------------*
      *                              * Tipo operazione a : non conti-  *
      *                              * nuazione                        *
      *                              *---------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-560.
      *                          *-------------------------------------*
      *                          * Se Find                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        v-key                not  = "FIND"
                     go to aco-565.
      *                              *---------------------------------*
      *                              * Se impostazione a Spaces : ri-  *
      *                              * entro ad accettazione descri-   *
      *                              * zione in uppercase              *
      *                              *---------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-550.
      *                              *---------------------------------*
      *                              * Normalizzazione del valore im-  *
      *                              * postato in formato privo di     *
      *                              * spaces e di caratteri diversi   *
      *                              * da A..Z - 0..9, e salvataggio   *
      *                              * del risultato                   *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-rcr-tot-dcp-vnr      .
      *                              *---------------------------------*
      *                              * Se il valore normalizzato e' a  *
      *                              * spaces : rientro ad accettazio- *
      *                              * ne descrizione in uppercase     *
      *                              *---------------------------------*
           if        w-rcr-tot-dcp-vnr    =    spaces
                     go to aco-550.
      *                              *---------------------------------*
      *                              * Se tutto Ok : ad esecuzione ri- *
      *                              * cerca totale                    *
      *                              *---------------------------------*
           go to     aco-800.
       aco-565.
      *                          *-------------------------------------*
      *                          * Se Select                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        v-key                not  = "SLCT"
                     go to aco-567.
      *                              *---------------------------------*
      *                              * Accettazione di 5 campi alfa-   *
      *                              * numerici                        *
      *                              *---------------------------------*
           perform   aco-slc-000          thru aco-slc-999            .
      *                              *---------------------------------*
      *                              * Se tutto Ok : ad esecuzione ri- *
      *                              * cerca totale                    *
      *                              *---------------------------------*
           if        w-rbl-des-fl5        not  = spaces
                     go to aco-800.
      *                              *---------------------------------*
      *                              * Altrimenti : ad accettazione    *
      *                              *---------------------------------*
           go to     aco-550.
       aco-567.
      *                          *-------------------------------------*
      *                          * Se Expand                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test                            *
      *                              *---------------------------------*
           if        v-key                not  = "EXPD"
                     go to aco-568.
      *                              *---------------------------------*
      *                              * Video di aiuto                  *
      *                              *---------------------------------*
           perform   aco-exp-000          thru aco-exp-999            .
      *                              *---------------------------------*
      *                              * Ad accettazione                 *
      *                              *---------------------------------*
           go to     aco-550.
       aco-568.
      *                          *-------------------------------------*
      *                          * Se Return                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se Valore impostato a Spaces :  *
      *                              * trattamento come per Up         *
      *                              *---------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-552.
       aco-570.
      *                              *---------------------------------*
      *                              * Preparazione dei parametri per  *
      *                              * l'esecuzione della ricerca per  *
      *                              * mezzo di un box locale per la   *
      *                              * selezione                       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore descrizione iniziale *
      *                                  *-----------------------------*
           move      w-rbl-des-acc        to   w-rbl-des-min          .
       aco-575.
      *                                  *-----------------------------*
      *                                  * Valore descrizione finale   *
      *                                  * con padding di 'z'          *
      *                                  *-----------------------------*
           move      w-rbl-des-acc        to   w-rbl-des-max          .
           move      40                   to   w-rbl-ctr-001          .
       aco-576.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-des-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-des-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-576.
       aco-580.
      *                                  *-----------------------------*
      *                                  * Alla ricerca mediante box   *
      *                                  * locale di selezione         *
      *                                  *-----------------------------*
           go to     aco-600.
       aco-600.
      *                  *---------------------------------------------*
      *                  * Ricerca mediante box locale di selezione    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Contatore records caricati nel buffer a *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
       aco-605.
      *                      *-----------------------------------------*
      *                      * Start                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * ricerca                             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-606
           else if   w-rbl-tip-ric        =    "I"
                     go to aco-607
           else      go to aco-608.
       aco-606.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Sinonimo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Start                *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "SYNPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-sin-min        to   rf-dcp-syn-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * A test esito Start              *
      *                              *---------------------------------*
           go to     aco-610.
       aco-607.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Iniziale codice *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Start                *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ALFPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-des-min        to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * A test esito Start              *
      *                              *---------------------------------*
           go to     aco-610.
       aco-608.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Descrizione     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Start                *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-des-min        to   rf-dcp-des-key         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                              *---------------------------------*
      *                              * A test esito Start              *
      *                              *---------------------------------*
           go to     aco-610.
       aco-610.
      *                      *-----------------------------------------*
      *                      * Test esito Start : se errata a tratta-  *
      *                      * mento per fine file, altrimenti conti-  *
      *                      * nuazione                                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-650.
       aco-615.
      *                      *-----------------------------------------*
      *                      * Read Next                               *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se At End : a trattamento per fine fi-  *
      *                      * le, altrimenti continuazione            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-650.
       aco-620.
      *                      *-----------------------------------------*
      *                      * Test max                                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * ricerca                             *
      *                          *-------------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-621
           else if   w-rbl-tip-ric        =    "I"
                     go to aco-622
           else      go to aco-623.
       aco-621.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Sinonimo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Test : se non supe-  *
      *                              * rato si va' a trattamento per   *
      *                              * fine file, altrimenti si con-   *
      *                              * tinua                           *
      *                              *---------------------------------*
           if        rf-dcp-syn-pro       >    w-rbl-sin-max
                     go to aco-650
           else      go to aco-625.
       aco-622.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Iniziale codice *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Test : se non supe-  *
      *                              * rato si va' a trattamento per   *
      *                              * fine file, altrimenti si con-   *
      *                              * tinua                           *
      *                              *---------------------------------*
           if        rf-dcp-alf-pro       >    w-rbl-des-max
                     go to aco-650
           else      go to aco-625.
       aco-623.
      *                          *-------------------------------------*
      *                          * Se tipo ricerca per Descrizione     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Esecuzione Test : se non supe-  *
      *                              * rato si va' a trattamento per   *
      *                              * fine file, altrimenti si con-   *
      *                              * tinua                           *
      *                              *---------------------------------*
           if        rf-dcp-des-key       >    w-rbl-des-max
                     go to aco-650
           else      go to aco-625.
       aco-625.
      *                      *-----------------------------------------*
      *                      * Incremento numero records caricati nel  *
      *                      * buffer                                  *
      *                      *-----------------------------------------*
           add       1                    to   w-buf-ctr-rec          .
      *                      *-----------------------------------------*
      *                      * Se oltre il max : a trattamento per fi- *
      *                      * ne file                                 *
      *                      *-----------------------------------------*
           if        w-buf-ctr-rec        >    w-buf-ctr-max
                     go to aco-650.
       aco-630.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record letto            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Tipo di match                       *
      *                          *-------------------------------------*
           move      zero                 to   w-buf-tip-mch
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Codice numerico del prodotto        *
      *                          *-------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Codice alfanumerico del prodotto    *
      *                          *-------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Codice sinonimo del prodotto        *
      *                          *-------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-syn-pro
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Descrizione del prodotto            *
      *                          *-------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Chiave di ordinamento               *
      *                          *-------------------------------------*
           perform   aco-pko-000          thru aco-pko-999            .
       aco-635.
      *                      *-----------------------------------------*
      *                      * Se attivo il flag di primo elemento da  *
      *                      * estrarre : si esce dal ciclo di lettura *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcp-fpe    not  = "#"
                     go to aco-640.
           go to     aco-650.
       aco-640.
      *                      *-----------------------------------------*
      *                      * Riciclo a Read Next                     *
      *                      *-----------------------------------------*
           go to     aco-615.
       aco-650.
      *                      *-----------------------------------------*
      *                      * Trattamento per fine file               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero di  *
      *                          * records caricati nel buffer         *
      *                          *-------------------------------------*
           if        w-buf-ctr-rec        =    zero
                     go to aco-480
           else if   w-buf-ctr-rec        =    1
                     go to aco-655
           else if   w-buf-ctr-rec        >    w-buf-ctr-max
                     go to aco-675
           else      go to aco-700.
       aco-655.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer pari a 1                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se in ricerca totale, ma non    *
      *                              * effettuata la selezione : al    *
      *                              * box di scelta                   *
      *                              *---------------------------------*
           if        w-rbl-tip-ric        =    "T"    and
                     w-rcr-tot-dcp-fsl    =    spaces
                     go to aco-675.
      *                              *---------------------------------*
      *                              * Indice elemento da selezionare  *
      *                              * dal buffer a 1                  *
      *                              *---------------------------------*
           move      1                    to   w-buf-ctr-ibx          .
       aco-660.
      *                          *-------------------------------------*
      *                          * Selezione valori dall'elemento del  *
      *                          * buffer di indice (w-buf-ctr-ibx) e  *
      *                          * ritorno al chiamante                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-dcp-tac    =    "N"
                     go to aco-665
           else      go to aco-670.
       aco-665.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'N'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore numerico in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico in area *
      *                                  * di uscita a Spaces          *
      *                                  *-----------------------------*
           move      spaces               to   w-cod-cod-dcp-alf      .
      *                                  *-----------------------------*
      *                                  * Editing del valore numerico *
      *                                  * in uscita                   *
      *                                  *-----------------------------*
           move      w-cod-cod-dcp-num    to   w-edt-num-cod          .
           perform   edn-000              thru edn-999                .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * numerico editato            *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-edt-num-edt        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione a non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-670.
      *                              *---------------------------------*
      *                              * Se tipo accettazione 'A'  'S'   *
      *                              *                      'L'        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore numerico in area di  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-num      .
      *                                  *-----------------------------*
      *                                  * Valore alfanumerico in area *
      *                                  * di uscita                   *
      *                                  *-----------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-alf      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione del valore  *
      *                                  * alfanumerico                *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Tipo operazione a non-con-  *
      *                                  * tinuazione                  *
      *                                  *-----------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Normalizzazione funct-key   *
      *                                  *-----------------------------*
           move      spaces               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-675.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer maggiore del massimo nume-   *
      *                          * ro di records caricabili            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di ricerca                      *
      *                              *---------------------------------*
           if        w-rbl-tip-ric        =    "S"
                     go to aco-681
           else if   w-rbl-tip-ric        =    "I"
                     go to aco-682
           else      go to aco-683.
       aco-681.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Sinoni-  *
      *                              * mo                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'tip-int'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'syn-min'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "syn-min"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      13                   to   s-car                  .
           move      w-rbl-sin-min        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'syn-max'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "syn-max"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      13                   to   s-car                  .
           move      w-rbl-sin-max        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * A completamento comune      *
      *                                  *-----------------------------*
           go to     aco-684.
       aco-682.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Iniziale *
      *                              * codice                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'tip-int'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "I"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'cod-min'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-min"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      14                   to   s-car                  .
           move      w-rbl-des-min        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'cod-max'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-max"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      14                   to   s-car                  .
           move      w-rbl-des-max        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * A completamento comune      *
      *                                  *-----------------------------*
           go to     aco-684.
       aco-683.
      *                              *---------------------------------*
      *                              * Se tipo di ricerca per Descri-  *
      *                              * zione                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'tip-int'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "D"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'des-min'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "des-min"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-rbl-des-min        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Preparazione della variabi- *
      *                                  * le i.p.c. 'des-max'         *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "des-max"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-rbl-des-max        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * A completamento comune      *
      *                                  *-----------------------------*
           go to     aco-684.
       aco-684.
      *                              *---------------------------------*
      *                              * Completamento comune pre-uscita *
      *                              * per Find                        *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo operazione a : esecu-  *
      *                                  * zione Find                  *
      *                                  *-----------------------------*
           move      "F+"                 to   w-cod-cod-dcp-ope      .
      *                                  *-----------------------------*
      *                                  * Simulazione tasto Find      *
      *                                  *-----------------------------*
           move      "FIND"               to   v-key                  .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     aco-999.
       aco-700.
      *                          *-------------------------------------*
      *                          * Se numero di records caricati nel   *
      *                          * buffer compreso tra 2 ed il massi-  *
      *                          * mo numero records caricabili        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Eventuale sort                  *
      *                              *---------------------------------*
           perform   aco-srt-000          thru aco-srt-999            .
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * totali nel buffer               *
      *                              *---------------------------------*
           move      w-buf-ctr-rec        to   w-buf-tot-pag          .
           subtract  1                    from w-buf-tot-pag          .
           divide    10                   into w-buf-tot-pag          .
           add       1                    to   w-buf-tot-pag          .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-buf-inx-buf          .
      *                              *---------------------------------*
      *                              * Salvataggio immagine video      *
      *                              *---------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione box vuoto       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "               Selezionare il codice prodotto desi
      -              "derato                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "----------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "----------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina video    *
      *                              * contenente il record attual-    *
      *                              * mente trattato                  *
      *                              *---------------------------------*
           perform   vpg-000              thru vpg-999                .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-705.
      *                              *---------------------------------*
      *                              * Determinazione numero linea a   *
      *                              * video in funzione del numero    *
      *                              * elemento in tabella trattato,   *
      *                              * di indice (w-buf-inx-buf)       *
      *                              *---------------------------------*
           divide    10                   into w-buf-inx-buf
                                        giving w-buf-ctr-005
                                     remainder w-buf-num-lnv          .
           if        w-buf-num-lnv        =    zero
                     move  10             to   w-buf-num-lnv          .
           add       07                   to   w-buf-num-lnv          .
       aco-710.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key alla linea calcolata        *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-inx-buf        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-inx-buf        <    w-buf-ctr-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-pag-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-pag-att        <    w-buf-tot-pag
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-num-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-715.
      *                              *---------------------------------*
      *                              * Deviazione a seconda della fun- *
      *                              * ction key impostata             *
      *                              *---------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-720
           else if   v-key                =    "UP  "
                     go to aco-725
           else if   v-key                =    "DOWN"
                     go to aco-730
           else if   v-key                =    "EXIT"
                     go to aco-735
           else if   v-key                =    "NXSC"
                     go to aco-740
           else if   v-key                =    "PRSC"
                     go to aco-745
           else      go to aco-710.
       aco-720.
      *                              *---------------------------------*
      *                              * Se Do o Slct                    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-buf-inx-buf        to   w-buf-ctr-ibx          .
      *                                  *-----------------------------*
      *                                  * A selezione valori indiriz- *
      *                                  * zati da (w-buf-ctr-ibx) ed  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           go to     aco-660.
       aco-725.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    08
                     go to aco-750
           else      go to aco-705.
       aco-730.
      *                              *---------------------------------*
      *                              * Se Return o Down                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-buf-inx-buf        =    w-buf-ctr-rec
                     go to aco-710.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    17
                     go to aco-750
           else      go to aco-705.
       aco-735.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Se il tipo ricerca e' per   *
      *                                  * Descrizione si ritorna al-  *
      *                                  * l'impostazione della de-    *
      *                                  * scrizione di ricerca mini-  *
      *                                  * ma, altrimenti si ritorna   *
      *                                  * alla impostazione con de-   *
      *                                  * fault del valore salvato    *
      *                                  *-----------------------------*
           if        w-rbl-tip-ric        =    "D"
                     go to aco-545
           else      go to aco-340.
       aco-740.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  10                   by   w-buf-inx-buf          .
           subtract  9                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-750.
       aco-745.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  10                   by   w-buf-inx-buf          .
           subtract  9                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-750.
       aco-750.
      *                              *---------------------------------*
      *                              * Visualizzazione nuova pagina    *
      *                              * video                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione pagina vi-  *
      *                                  * deo contenente il record    *
      *                                  * attualmente trattato        *
      *                                  *-----------------------------*
           perform   vpg-000              thru vpg-999                .
      *                                  *-----------------------------*
      *                                  * A reimpostazione funct-key  *
      *                                  *-----------------------------*
           go to     aco-705.
       aco-800.
      *                  *---------------------------------------------*
      *                  * Ricerca totale                              *
      *                  *---------------------------------------------*
       aco-802.
      *                      *-----------------------------------------*
      *                      * Tipo di ricerca                         *
      *                      *-----------------------------------------*
           move      "T"                  to   w-rbl-tip-ric          .
       aco-804.
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
      *                      * Box superiore                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Box vuoto                           *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      06                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Literal nel box                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "                        Ricerca prodotti in esecuz
      -              "ione                        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Box inferiore                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Box vuoto                           *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Literal nel box                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "Numero prodotti esaminati......:           Prodott
      -              "i selezionati....:          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Literal nel box                     *
      *                          *-------------------------------------*
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
      *                      *-----------------------------------------*
      *                      * Box centrale, parte sinistra            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Box vuoto                           *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      40                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Box centrale, parte destra              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Box vuoto                           *
      *                          *-------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-806.
      *                      *-----------------------------------------*
      *                      * Numero records memorizzati in file re-  *
      *                      * lative di appoggio [rlt] : zero         *
      *                      *-----------------------------------------*
           move      zero                 to   w-rlt-num              .
      *                      *-----------------------------------------*
      *                      * Numero record attualmente in trattamen- *
      *                      * to del file relative di appoggio [rlt]  *
      *                      * a zero                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-rlt-att              .
      *                      *-----------------------------------------*
      *                      * Flag di fine ricerca : No               *
      *                      *-----------------------------------------*
           move      spaces               to   w-rcr-tot-dcp-ffr      .
      *                      *-----------------------------------------*
      *                      * Flag di selezione effettuata : No       *
      *                      *-----------------------------------------*
           move      spaces               to   w-rcr-tot-dcp-fsl      .
      *                      *-----------------------------------------*
      *                      * Contatore records esaminati : a zero    *
      *                      *-----------------------------------------*
           move      zero                 to   w-rcr-tot-dcp-cre      .
      *                      *-----------------------------------------*
      *                      * Contatore records selezionati : a zero  *
      *                      *-----------------------------------------*
           move      zero                 to   w-rcr-tot-dcp-crs      .
      *                      *-----------------------------------------*
      *                      * Contatore records nel buffer : a zero   *
      *                      *-----------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
       aco-808.
      *                      *-----------------------------------------*
      *                      * Start su [dcp] per codice alfanumerico  *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      spaces               to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : a fine trattamento    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-910.
       aco-810.
      *                      *-----------------------------------------*
      *                      * Test se l'utente ha premuto uno dei se- *
      *                      * guenti tasti :                          *
      *                      * - Exit : sempre ammesso                 *
      *                      * - Slct o                                *
      *                      *   Rtrn : solo se in questo momento c'e' *
      *                      *          un record in trattamento del   *
      *                      *          file relative di appoggio      *
      *                      *          [rlt]                          *
      *                      * - Up   o                                *
      *                      * - Prsc : solo se in questo momento c'e' *
      *                      *          un record in trattamento del   *
      *                      *          file relative di appoggio      *
      *                      *          [rlt], che non e' il primo     *
      *                      * - Down o                                *
      *                      * - Nxsc : solo se in questo momento c'e' *
      *                      *          un record in trattamento del   *
      *                      *          file relative di appoggio      *
      *                      *          [rlt], che non e' l'ultimo     *
      *                      *-----------------------------------------*
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
                     go to aco-920.
       aco-812.
      *                      *-----------------------------------------*
      *                      * Se flag di fine ricerca in On : invece  *
      *                      * di eseguire la lettura si ricicla per   *
      *                      * vedere quale tasto funzione digita l'u- *
      *                      * tente                                   *
      *                      *-----------------------------------------*
           if        w-rcr-tot-dcp-ffr    =    "S"
                     go to aco-810.
      *                      *-----------------------------------------*
      *                      * Read Next su [dcp] per codice alfanume- *
      *                      * rico                                    *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se At End : a fine trattamento          *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-910.
       aco-814.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record del file di ap-  *
      *                      * poggio [rlt]                            *
      *                      * vedere quale tasto funzione digita l'u- *
      *                      * tente                                   *
      *                      *-----------------------------------------*
           move      spaces               to   rlt-rec                .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record [dcp] all'inter- *
      *                      * no del record di [rlt]                  *
      *                      *-----------------------------------------*
           move      rf-dcp               to   rlt-rec-rec-dcp        .
       aco-816.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record [pdx], tipo 01,  *
      *                      * per descrizione estesa in lingua ita-   *
      *                      * liana, all'interno del record di [rlt]  *
      *                      *-----------------------------------------*
       aco-817.
      *                          *-------------------------------------*
      *                          * Se il segnale indica che non c'e'   *
      *                          * una descrizione estesa : no buffe-  *
      *                          * rizzazione                          *
      *                          *-------------------------------------*
           if        rf-dcp-des-pdx       not  = 1
                     go to aco-820.
      *                          *-------------------------------------*
      *                          * Inizializzazione indice 1           *
      *                          * Inizializzazione indice 2           *
      *                          *-------------------------------------*
           move      zero                 to   w-rlt-i01              .
           move      zero                 to   w-rlt-i02              .
       aco-818.
      *                          *-------------------------------------*
      *                          * Start su [pdx], tipo record 01      *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      01                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      rf-dcp-num-pro       to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .

           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                          *-------------------------------------*
      *                          * Se Start errata : fine bufferizza-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-820.
       aco-819.
      *                          *-------------------------------------*
      *                          * Read next su [pdx], tipo record 01  *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                          *-------------------------------------*
      *                          * Se At End : fine bufferizzazione    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-820.
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : fine buffe-   *
      *                          * rizzazione                          *
      *                          *-------------------------------------*
           if        rf-pdx-tip-rec       not  = 01             or
                     rf-pdx-cod-arc       not  = zero           or
                     rf-pdx-cod-lng       not  = "I"            or
                     rf-pdx-cod-num       not  = rf-dcp-num-pro or
                     rf-pdx-for-mat       not  = spaces
                     go to aco-820.
      *                          *-------------------------------------*
      *                          * Bufferizzazione linea descrizione   *
      *                          *-------------------------------------*
           add       1                    to   w-rlt-i02              .
           move      rf-pdx-des-pro       to   rlt-rec-ele-x01
                                              (w-rlt-i02)             .
      *                          *-------------------------------------*
      *                          * Se non al massimo : riciclo a let-  *
      *                          * tura linea successiva               *
      *                          *-------------------------------------*
           if        w-rlt-i02            <    10
                     go to aco-819.
       aco-820.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record [pdx], tipo 04,  *
      *                      * per descrizione estesa per il listino,  *
      *                      * all'interno del record di [rlt]         *
      *                      *-----------------------------------------*
       aco-821.
      *                          *-------------------------------------*
      *                          * Se non ricerca totale : si esclude  *
      *                          * il trattamento della descrizione    *
      *                          * per il listino                      *
      *                          *-------------------------------------*
           if        w-prs-trt-pf1        not  = "T"
                     go to aco-830.
       aco-823.
      *                          *-------------------------------------*
      *                          * Inizializzazione indice 1           *
      *                          * Inizializzazione indice 2           *
      *                          *-------------------------------------*
           move      zero                 to   w-rlt-i01              .
           move      zero                 to   w-rlt-i02              .
       aco-824.
      *                          *-------------------------------------*
      *                          * Start su [pdx], tipo record 04      *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TRCLNG    "         to   f-key                  .
           move      04                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      rf-dcp-num-pro       to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      zero                 to   rf-pdx-num-prg         .

           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                          *-------------------------------------*
      *                          * Se Start errata : fine bufferizza-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-830.
       aco-825.
      *                          *-------------------------------------*
      *                          * Read next su [pdx], tipo record 04  *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                          *-------------------------------------*
      *                          * Se At End : fine bufferizzazione    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-830.
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : fine buffe-   *
      *                          * rizzazione                          *
      *                          *-------------------------------------*
           if        rf-pdx-tip-rec       not  = 04             or
                     rf-pdx-cod-arc       not  = zero           or
                     rf-pdx-cod-lng       not  = "I"            or
                     rf-pdx-cod-num       not  = rf-dcp-num-pro or
                     rf-pdx-for-mat       not  = spaces
                     go to aco-830.
      *                          *-------------------------------------*
      *                          * Bufferizzazione linea descrizione   *
      *                          *-------------------------------------*
           add       1                    to   w-rlt-i02              .
           move      rf-pdx-des-pro       to   rlt-rec-ele-x04
                                              (w-rlt-i02)             .
      *                          *-------------------------------------*
      *                          * Se non al massimo : riciclo a let-  *
      *                          * tura linea successiva               *
      *                          *-------------------------------------*
           if        w-rlt-i02            <    10
                     go to aco-825.
       aco-830.
      *                          *-------------------------------------*
      *                          * Incremento e visualizzazione del    *
      *                          * numero records esaminati            *
      *                          *-------------------------------------*
       aco-831.
           add       1                    to   w-rcr-tot-dcp-cre      .
       aco-832.
           if        w-rcr-tot-dcp-cre    not  < 100
                     move  w-rcr-tot-dcp-cre
                                          to   w-rcr-tot-dcp-wr2
                     if    w-rcr-tot-dcp-wr2
                                          =    zero
                           go to aco-833
                     else  go to aco-840.
           if        w-rcr-tot-dcp-cre    not  < 10
                     move  w-rcr-tot-dcp-cre
                                          to   w-rcr-tot-dcp-wr1
                     if    w-rcr-tot-dcp-wr1
                                          =    zero
                           go to aco-833
                     else  go to aco-840.
       aco-833.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rcr-tot-dcp-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-840.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * descrizione ad uso interno          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-dcp-des-pro       =    spaces
                     go to aco-845.
      *                              *---------------------------------*
      *                              * Eventuale match con i 5 campi   *
      *                              * alfanumerici                    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se da effettuare       *
      *                                  *-----------------------------*
           if        w-rbl-des-fl5        =    spaces
                     go to aco-843.
      *                                  *-----------------------------*
      *                                  * Subroutine                  *
      *                                  *-----------------------------*
           perform   aco-slc-tst-000      thru aco-slc-tst-999        .
      *                                  *-----------------------------*
      *                                  * Test su esito               *
      *                                  *-----------------------------*
           if        w-rbl-des-fm5        not  = spaces
                     go to aco-875
           else      go to aco-810.
       aco-843.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-dcp-des-pro       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-dcp-vnr    to   w-atz-1t9-vdn          .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 01                              *
      *                              *---------------------------------*
           if        w-atz-1t9-flg-mch    =    spaces
                     move  01             to   w-rcr-tot-dcp-mch
                     go to aco-875.
       aco-845.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * descrizione estesa in lingua ita-   *
      *                          * liana                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rlt-rec-rec-x01      =    spaces
                     go to aco-850.
       aco-847.
      *                              *---------------------------------*
      *                              * Test sulle 10 linee bufferizza- *
      *                              * te                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Indice 01..10 : a zero      *
      *                                  *-----------------------------*
           move      zero                 to   w-rlt-i01              .
       aco-848.
      *                                  *-----------------------------*
      *                                  * Incremento indice 01..10    *
      *                                  *-----------------------------*
           add       1                    to   w-rlt-i01              .
      *                                  *-----------------------------*
      *                                  * Se oltre il massimo : no    *
      *                                  * bufferizzazione             *
      *                                  *-----------------------------*
           if        w-rlt-i01            >    10
                     go to aco-850.
      *                                  *-----------------------------*
      *                                  * Se linea a spaces : no buf- *
      *                                  * ferizzazione                *
      *                                  *-----------------------------*
           if        rlt-rec-ele-x01
                    (w-rlt-i01)           =    spaces
                     go to aco-850.
      *                                  *-----------------------------*
      *                                  * Normalizzazione A..Z - 0..9 *
      *                                  * e preparazione 2. valore    *
      *                                  * per il match                *
      *                                  *-----------------------------*
           move      rlt-rec-ele-x01
                    (w-rlt-i01)           to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                                  *-----------------------------*
      *                                  * Preparazione 1. valore per  *
      *                                  * il match                    *
      *                                  *-----------------------------*
           move      w-rcr-tot-dcp-vnr    to   w-atz-1t9-vdn          .
      *                                  *-----------------------------*
      *                                  * Match tra i due valori      *
      *                                  *-----------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                                  *-----------------------------*
      *                                  * Se c'e' stato un match : a  *
      *                                  * bufferizzazione con indice  *
      *                                  * match a 11                  *
      *                                  *-----------------------------*
           if        w-atz-1t9-flg-mch    =    spaces
                     move  11             to   w-rcr-tot-dcp-mch
                     go to aco-875.
      *                                  *-----------------------------*
      *                                  * Altrimenti riciclo su linea *
      *                                  * successiva                  *
      *                                  *-----------------------------*
           go to     aco-848.
       aco-850.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * descrizione estesa per il listino   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rlt-rec-rec-x04      =    spaces
                     go to aco-855.
       aco-852.
      *                              *---------------------------------*
      *                              * Test sulle 10 linee bufferizza- *
      *                              * te                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Indice 01..10 : a zero      *
      *                                  *-----------------------------*
           move      zero                 to   w-rlt-i01              .
       aco-853.
      *                                  *-----------------------------*
      *                                  * Incremento indice 01..10    *
      *                                  *-----------------------------*
           add       1                    to   w-rlt-i01              .
      *                                  *-----------------------------*
      *                                  * Se oltre il massimo : no    *
      *                                  * bufferizzazione             *
      *                                  *-----------------------------*
           if        w-rlt-i01            >    10
                     go to aco-855.
      *                                  *-----------------------------*
      *                                  * Se linea a spaces : no buf- *
      *                                  * ferizzazione                *
      *                                  *-----------------------------*
           if        rlt-rec-ele-x04
                    (w-rlt-i01)           =    spaces
                     go to aco-855.
      *                                  *-----------------------------*
      *                                  * Normalizzazione A..Z - 0..9 *
      *                                  * e preparazione 2. valore    *
      *                                  * per il match                *
      *                                  *-----------------------------*
           move      rlt-rec-ele-x04
                    (w-rlt-i01)           to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                                  *-----------------------------*
      *                                  * Preparazione 1. valore per  *
      *                                  * il match                    *
      *                                  *-----------------------------*
           move      w-rcr-tot-dcp-vnr    to   w-atz-1t9-vdn          .
      *                                  *-----------------------------*
      *                                  * Match tra i due valori      *
      *                                  *-----------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                                  *-----------------------------*
      *                                  * Se c'e' stato un match : a  *
      *                                  * bufferizzazione con indice  *
      *                                  * match a 21                  *
      *                                  *-----------------------------*
           if        w-atz-1t9-flg-mch    =    spaces
                     move  21             to   w-rcr-tot-dcp-mch
                     go to aco-875.
      *                                  *-----------------------------*
      *                                  * Altrimenti riciclo su linea *
      *                                  * successiva                  *
      *                                  *-----------------------------*
           go to     aco-853.
       aco-855.
      *                          *-------------------------------------*
      *                          * Se non c'e' stato alcun match : si  *
      *                          * ricicla su anagrafica prodotto suc- *
      *                          * cessiva                             *
      *                          *-------------------------------------*
           go to     aco-810.
       aco-875.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record              *
      *                          *-------------------------------------*
       aco-880.
      *                              *---------------------------------*
      *                              * Visualizzazione, se non gia' e- *
      *                              * seguita, del numero records se- *
      *                              * lezionati                       *
      *                              *---------------------------------*
       aco-881.
           if        w-rcr-tot-dcp-cre    not  < 100
                     move  w-rcr-tot-dcp-cre
                                          to   w-rcr-tot-dcp-wr2
                     if    w-rcr-tot-dcp-wr2
                                          =    zero
                           go to aco-883
                     else  go to aco-882.
           if        w-rcr-tot-dcp-cre    not  < 10
                     move  w-rcr-tot-dcp-cre
                                          to   w-rcr-tot-dcp-wr1
                     if    w-rcr-tot-dcp-wr1
                                          =    zero
                           go to aco-883
                     else  go to aco-882.
           go to     aco-883.
       aco-882.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rcr-tot-dcp-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-883.
      *                          *-------------------------------------*
      *                          * Incremento e visualizzazione del    *
      *                          * numero records selezionati          *
      *                          *-------------------------------------*
           add       1                    to   w-rcr-tot-dcp-crs      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-rcr-tot-dcp-crs    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-885.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer, a meno di non aver raggiunto   *
      *                          * il massimo                          *
      *                          *-------------------------------------*
           if        w-buf-ctr-rec        not  < w-buf-ctr-max
                     go to aco-890.
           add       1                    to   w-buf-ctr-rec          .
       aco-890.
      *                          *-------------------------------------*
      *                          * Bufferizzazione vera e propria, con *
      *                          * indice di tipo match, a meno di non *
      *                          * aver raggiunto il massimo           *
      *                          *-------------------------------------*
           if        w-buf-ctr-rec        not  < w-buf-ctr-max
                     go to aco-895.
      *                          *-------------------------------------*
      *                          * Tipo di match                       *
      *                          *-------------------------------------*
           move      w-rcr-tot-dcp-mch    to   w-buf-tip-mch
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Codice numerico del prodotto        *
      *                          *-------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Codice alfanumerico del prodotto    *
      *                          *-------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Codice sinonimo del prodotto        *
      *                          *-------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-syn-pro
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Descrizione del prodotto            *
      *                          *-------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
      *                          *-------------------------------------*
      *                          * Chiave di ordinamento               *
      *                          *-------------------------------------*
           perform   aco-pko-000          thru aco-pko-999            .
       aco-895.
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
           move      w-rcr-tot-dcp-mch    to   rlt-rec-tip-mch        .
           perform   rlt-put-000          thru rlt-put-999            .
      *                          *-------------------------------------*
      *                          * Se e' il primo record scritto, lo   *
      *                          * si visualizza                       *
      *                          *-------------------------------------*
           if        w-rlt-num            >    1
                     go to aco-900.
           move      1                    to   w-rlt-att              .
           perform   aco-970              thru aco-989                .
       aco-900.
      *                          *-------------------------------------*
      *                          * Riciclo a lettura anagrafica pro-   *
      *                          * dotto successiva                    *
      *                          *-------------------------------------*
           go to     aco-810.
       aco-910.
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
           move      w-rcr-tot-dcp-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Flag di fine ricerca : Si       *
      *                              *---------------------------------*
           move      "S"                  to   w-rcr-tot-dcp-ffr      .
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
           go to     aco-810.
       aco-915.
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
           go to     aco-650.
       aco-920.
      *                          *-------------------------------------*
      *                          * Trattamento tasti funzione asin-    *
      *                          * croni                               *
      *                          *-------------------------------------*
       aco-921.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tasto  *
      *                              * funzione premuto                *
      *                              *---------------------------------*
           if        v-key                =    "UP  " or
                     v-key                =    "PRSC"
                     go to aco-922
           else if   v-key                =    "DOWN" or
                     v-key                =    "NXSC"
                     go to aco-923
           else if   v-key                =    "SLCT" or
                     v-key                =    "RTRN"
                     go to aco-924
           else if   v-key                =    "EXIT"
                     go to aco-925
           else      go to aco-812.
       aco-922.
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
           move      rlt-rec-rec-dcp      to   rf-dcp                 .
      *                                  *-----------------------------*
      *                                  * Visualizzazione record del  *
      *                                  * file relative di appoggio   *
      *                                  * [rlt] attualmente in trat-  *
      *                                  * tamento                     *
      *                                  *-----------------------------*
           perform   aco-970              thru aco-989                .
      *                                  *-----------------------------*
      *                                  * Continuazione lettura       *
      *                                  *-----------------------------*
           go to     aco-812.
       aco-923.
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
           move      rlt-rec-rec-dcp      to   rf-dcp                 .
      *                                  *-----------------------------*
      *                                  * Visualizzazione record del  *
      *                                  * file relative di appoggio   *
      *                                  * [rlt] attualmente in trat-  *
      *                                  * tamento                     *
      *                                  *-----------------------------*
           perform   aco-970              thru aco-989                .
      *                                  *-----------------------------*
      *                                  * Continuazione lettura       *
      *                                  *-----------------------------*
           go to     aco-812.
       aco-924.
      *                              *---------------------------------*
      *                              * Se Slct o Return                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Flag di selezione effettua- *
      *                                  * ta : Si                     *
      *                                  *-----------------------------*
           move      "S"                  to   w-rcr-tot-dcp-fsl      .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-dcp      to   rf-dcp                 .
      *                                  *-----------------------------*
      *                                  * Si esegue una forzatura co- *
      *                                  * me se si fosse rintracciato *
      *                                  * un solo record, pari al re- *
      *                                  * cord selezionato            *
      *                                  *-----------------------------*
           move      1                    to   w-buf-ctr-rec          .
           move      spaces               to   w-buf-key-ord (1)      .
           move      zero                 to   w-buf-tip-mch (1)      .
           move      rf-dcp-num-pro       to   w-buf-cod-num (1)      .
           move      rf-dcp-alf-pro       to   w-buf-cod-alf (1)      .
           move      rf-dcp-syn-pro       to   w-buf-syn-pro (1)      .
           move      rf-dcp-des-pro       to   w-buf-des-pro (1)      .
      *                                  *-----------------------------*
      *                                  * A fine ricerca totale       *
      *                                  *-----------------------------*
           go to     aco-915.
       aco-925.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * A fine ricerca totale       *
      *                                  *-----------------------------*
           go to     aco-915.
       aco-970.
      *                          *-------------------------------------*
      *                          * Visualizzazione record del file     *
      *                          * relative di appoggio [rlt] attual-  *
      *                          * mente in trattamento                *
      *                          *-------------------------------------*
       aco-971.
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-972.
      *                              *---------------------------------*
      *                              * Titolo                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero record, literal      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      "Nr:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Numero record, valore       *
      *                                  *-----------------------------*
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
       aco-974.
      *                              *---------------------------------*
      *                              * Anagrafica [dcp]                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * - Codice alfanumerico pro-  *
      *                                  *   dotto                     *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      rf-dcp-alf-pro       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * - Descrizione interna       *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      37                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      rf-dcp-des-pro (01 : 37)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      rf-dcp-des-pro (38 : 03)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-976.
      *                              *---------------------------------*
      *                              * Anagrafica [pdx], tipo 01, per  *
      *                              * descrizione estesa in lingua    *
      *                              * italiana                        *
      *                              *---------------------------------*
       aco-977.
      *                                  *-----------------------------*
      *                                  * Se tipo match 21, per la    *
      *                                  * descrizione estesa per il   *
      *                                  * listino, no visualizzazione *
      *                                  *-----------------------------*
           if        rlt-rec-tip-mch      =    21
                     go to aco-982.
       aco-978.
      *                                  *-----------------------------*
      *                                  * - Visualizzazione linee     *
      *                                  *-----------------------------*
           move      zero                 to   w-rlt-i01              .
       aco-979.
           add       1                    to   w-rlt-i01              .
           if        w-rlt-i01            >    10
                     go to aco-982.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-rlt-i01            to   v-lin                  .
           add       06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rlt-rec-ele-x01
                    (w-rlt-i01)           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-979.
       aco-982.
      *                              *---------------------------------*
      *                              * Anagrafica [pdx], tipo 04, per  *
      *                              * descrizione per il listino      *
      *                              *---------------------------------*
       aco-983.
      *                                  *-----------------------------*
      *                                  * Se tipo match 11, per la    *
      *                                  * descrizione estesa in lin-  *
      *                                  * gua italiana, no visualiz-  *
      *                                  * zazione                     *
      *                                  *-----------------------------*
           if        rlt-rec-tip-mch      =    11
                     go to aco-987.
       aco-984.
      *                                  *-----------------------------*
      *                                  * - Visualizzazione linee     *
      *                                  *-----------------------------*
           move      zero                 to   w-rlt-i01              .
       aco-985.
           add       1                    to   w-rlt-i01              .
           if        w-rlt-i01            >    10
                     go to aco-987.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-rlt-i01            to   v-lin                  .
           add       06                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rlt-rec-ele-x04
                    (w-rlt-i01)           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-985.
       aco-987.
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-989.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Ricerca per chiave libera (barcode)                       *
      *    *-----------------------------------------------------------*
       aco-bcd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-man-x14-bcd          .
       aco-bcd-100.
      *              *-------------------------------------------------*
      *              * Start su [dcp]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "KLBPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-man-x14-alf        to   rf-dcp-klb-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : a lettura [pdk]           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-bcd-600.
       aco-bcd-200.
      *              *-------------------------------------------------*
      *              * Read next su [dcp]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se At End : a lettura [pdk]                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-bcd-600.
       aco-bcd-300.
      *              *-------------------------------------------------*
      *              * Test max su [dcp]                               *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       not  = w-man-x14-alf
                     go to aco-bcd-600.
       aco-bcd-400.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-man-x14-alf          .
      *              *-------------------------------------------------*
      *              * Flag di bar-code                                *
      *              *-------------------------------------------------*
           move      "#"                  to   w-man-x14-bcd          .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     aco-bcd-900.
       aco-bcd-600.
      *              *-------------------------------------------------*
      *              * Ricerca su [pdk] per il barcode confezione      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-man-x14-alf        to   rf-pdk-alf-pro         .
           move      "B"                  to   rf-pdk-tip-rec         .
           move      zero                 to   rf-pdk-num-pro         .
           move      zero                 to   rf-pdk-cod-arc         .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se errata : a lettura [pdk]                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-bcd-700.
       aco-bcd-620.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdk]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : a lettura [pdk]               *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-bcd-700.
       aco-bcd-630.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-man-x14-alf
                     go to aco-bcd-700.
           if        rf-pdk-tip-rec       not  = "B"
                     go to aco-bcd-700.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-pdk-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-man-x14-alf          .
      *              *-------------------------------------------------*
      *              * Flag di bar-code                                *
      *              *-------------------------------------------------*
           move      "#"                  to   w-man-x14-bcd          .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     aco-bcd-900.
       aco-bcd-700.
      *              *-------------------------------------------------*
      *              * Ricerca su [pdk] per il barcode imballo         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-man-x14-alf        to   rf-pdk-alf-pro         .
           move      "D"                  to   rf-pdk-tip-rec         .
           move      zero                 to   rf-pdk-num-pro         .
           move      zero                 to   rf-pdk-cod-arc         .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se errata : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-bcd-900.
       aco-bcd-720.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdk]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-bcd-900.
       aco-bcd-730.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-man-x14-alf
                     go to aco-bcd-900.
           if        rf-pdk-tip-rec       not  = "D"
                     go to aco-bcd-900.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-pdk-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-man-x14-alf          .
      *              *-------------------------------------------------*
      *              * Flag di bar-code                                *
      *              *-------------------------------------------------*
           move      "#"                  to   w-man-x14-bcd          .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     aco-bcd-900.
   
   
   
   
   
   
   
   
   
   
   
       aco-bcd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-bcd-999.
       aco-bcd-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Preparazione chiave di ordinamento righe bufferizzate     *
      *    *-----------------------------------------------------------*
       aco-pko-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-key-ord
                                              (w-buf-ctr-rec)         .
       aco-pko-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della personalizzazione  *
      *              *-------------------------------------------------*
           if        w-prs-snx-opd        =    "S"
                     go to aco-pko-400.
       aco-pko-200.
      *              *-------------------------------------------------*
      *              * Se non ordinamento supplementare per descri-    *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-pko-900.
       aco-pko-400.
      *              *-------------------------------------------------*
      *              * Se ordinamento supplementare per descrizione    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice sinonimo prodotto                    *
      *                  *---------------------------------------------*
           move      w-buf-syn-pro
                    (w-buf-ctr-rec)       to   w-buf-key-ord-syn-01
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      w-buf-des-pro
                    (w-buf-ctr-rec)       to   w-buf-key-ord-des-01
                                              (w-buf-ctr-rec)         .
      *
           move      w-buf-key-ord-des-01
                    (w-buf-ctr-rec)       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-buf-key-ord-des-01
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-rec)       to   w-buf-key-ord-alf-01
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-pko-900.
       aco-pko-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-pko-999.
       aco-pko-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Ordinamento righe bufferizzate                            *
      *    *-----------------------------------------------------------*
       aco-srt-000.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione relativa              *
      *              *-------------------------------------------------*
           if        w-prs-snx-opd        not  = "S"
                     go to aco-srt-999.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec    <    2
                     go to aco-srt-999.
       aco-srt-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-ctr-001          .
       aco-srt-100.
           add       1                    to   w-buf-ctr-001          .
           if        w-buf-ctr-001        =    w-buf-ctr-rec
                     go to aco-srt-999.
           move      w-buf-ctr-001        to   w-buf-ctr-002
                                               w-buf-ctr-003          .
           move      w-buf-key-ord
                    (w-buf-ctr-001)       to   w-buf-rig-svk          .
       aco-srt-200.
           add       1                    to   w-buf-ctr-002          .
           if        w-buf-ctr-002        >    w-buf-ctr-rec
                     go to aco-srt-300.
           if        w-buf-key-ord
                    (w-buf-ctr-002)       >    w-buf-rig-svk
                     go to aco-srt-200.
           move      w-buf-ctr-002        to   w-buf-ctr-003          .
           move      w-buf-key-ord
                    (w-buf-ctr-002)       to   w-buf-rig-svk          .
           go to     aco-srt-200.
       aco-srt-300.
           move      w-buf-ctr-001        to   w-buf-ctr-004          .          
           if        w-buf-rig-svk        >    w-buf-key-ord
                                              (w-buf-ctr-004)
                     go to aco-srt-100.
           move      w-buf-ele-buf
                    (w-buf-ctr-003)       to   w-buf-ele-buf
                                              (w-buf-ctr-max)         .
           move      w-buf-ele-buf
                    (w-buf-ctr-004)       to   w-buf-ele-buf
                                              (w-buf-ctr-003)         .
           move      w-buf-ele-buf
                    (w-buf-ctr-max)       to   w-buf-ele-buf
                                              (w-buf-ctr-004)         .
           go to     aco-srt-100.
       aco-srt-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di accettazione per codice prodotto assegnato  *
      *    * dal produttore                                            *
      *    *-----------------------------------------------------------*
       aco-pdt-000.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di uscita a spaces                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Se non e' specificata una linea per la de-  *
      *                  * scrizione si ritorna alla reimpostazione    *
      *                  * con default pari al valore salvato          *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-dln    =    zero or
                     w-cod-cod-dcp-dps    =    zero
                     go to aco-pdt-800.
       aco-pdt-010.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione per ricerca        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spaces in comodo per impostazione des-  *
      *                      * crizione                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-rbl-des-acc          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-pdt-020.
      *                      *-----------------------------------------*
      *                      * Accettazione descrizione minima di ri-  *
      *                      * cerca, in uppercase                     *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      w-rbl-des-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato in area di comodo di   *
      *                      * destinazione                            *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-acc          .
       aco-pdt-030.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-pdt-050.
      *                          *-------------------------------------*
      *                          * Visualizzazione spaces in area di   *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-pdt-040.
      *                          *-------------------------------------*
      *                          * Alla reimpostazione con default     *
      *                          * pari al valore salvato              *
      *                          *-------------------------------------*
           go to     aco-pdt-800.
       aco-pdt-050.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-pdt-060.
      *                          *-------------------------------------*
      *                          * Ripristino valori originali         *
      *                          *-------------------------------------*
           move      w-cod-cod-dcp-s01    to   w-cod-cod-dcp-num      .
           move      w-cod-cod-dcp-s02    to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Tipo operazione a : non continua-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-pdt-900.
       aco-pdt-060.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Valore impostato a Spaces :      *
      *                          * trattamento come per Up             *
      *                          *-------------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-pdt-040.
       aco-pdt-070.
      *                          *-------------------------------------*
      *                          * Preparazione dei parametri per      *
      *                          * l'esecuzione della ricerca per      *
      *                          * mezzo di un box locale per la sele- *
      *                          * zione                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore iniziale                 *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-cdp-min          .
      *                              *---------------------------------*
      *                              * Valore finale                   *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-cdp-max          .
      *                              *---------------------------------*
      *                              * Se valore wildcard diverso da   *
      *                              * '+' : oltre                     *
      *                              *---------------------------------*
           if        w-man-x14-wlc        not  = "+"
                     go to aco-pdt-100.
      *                              *---------------------------------*
      *                              * Padding del codice finale       *
      *                              *---------------------------------*
           move      21                   to   w-rbl-ctr-001          .
       aco-pdt-080.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-cdp-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-cdp-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-pdt-080.
       aco-pdt-100.
      *              *-------------------------------------------------*
      *              * Ricerca con box locale di selezione             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore records caricati  *
      *                  * nel buffer                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
      *                  *---------------------------------------------*
      *                  * Start su file [aaq] per codice prodotto     *
      *                  * assegnato dal produttore                    *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CDPPDT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-cdp-min        to   rf-aaq-cdp-pdt         .
           move      01                   to   rf-aaq-tip-mag         .
           move      zero                 to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-pdt-350.
       aco-pdt-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale su file [aaq]               *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-pdt-350.
       aco-pdt-220.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        rf-aaq-cdp-pdt       >    w-rbl-cdp-max
                     go to aco-pdt-350.
           if        rf-aaq-tip-mag       not  = 01
                     go to aco-pdt-350.
       aco-pdt-240.
      *              *-------------------------------------------------*
      *              * Letture addizionali                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura del record [dcp]                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-aaq-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcp-des-pro         .
      *                  *---------------------------------------------*
      *                  * Lettura del record [pdt]                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se codice a zero : ragione sociale a    *
      *                      * spaces                                  *
      *                      *-----------------------------------------*
           if        rf-aaq-cod-pdt       =    zero
                     move  spaces         to   rf-pdt-rag-soc
                     go to aco-pdt-300.
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDT"             to   f-key                  .
           move      rf-aaq-cod-pdt       to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-pdt-rag-soc         .
       aco-pdt-300.
      *              *-------------------------------------------------*
      *              * Incremento numero records caricati nel buffer   *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-ctr-rec          .
      *              *-------------------------------------------------*
      *              * Se oltre il max : a trattamento per fine file   *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        >    w-buf-ctr-max
                     subtract 1           from w-buf-ctr-rec
                     go to aco-pdt-500.
       aco-pdt-330.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record letto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di match                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tip-mch
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice numerico del prodotto                *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione del prodotto                    *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Sinonimo del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-syn-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice del prodotto per il produttore       *
      *                  *---------------------------------------------*
           move      rf-aaq-cdp-pdt       to   w-buf-cdp-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice del produttore                       *
      *                  *---------------------------------------------*
           move      rf-aaq-cod-pdt       to   w-buf-cod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale del produttore              *
      *                  *---------------------------------------------*
           move      rf-pdt-rag-soc       to   w-buf-rod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Chiave di ordinamento                       *
      *                  *---------------------------------------------*
           perform   aco-pko-000          thru aco-pko-999            .
       aco-pdt-335.
      *              *-------------------------------------------------*
      *              * Se attivo il flag di primo elemento da estrarre *
      *              * si esce dal ciclo di lettura                    *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-fpe    not  = "#"
                     go to aco-pdt-340.
           go to     aco-pdt-350.
       aco-pdt-340.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     aco-pdt-200.
       aco-pdt-350.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        =    zero
                     go to aco-pdt-375
           else if   w-buf-ctr-rec        =    1
                     go to aco-pdt-400
           else      go to aco-pdt-500.
       aco-pdt-375.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il codice impostato    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di selezione non effettuata     *
      *                      *-----------------------------------------*
           move      "N"                  to   w-rbl-tip-ext          .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-pdt-900.
       aco-pdt-400.
      *                  *---------------------------------------------*
      *                  * Se un 1 record con il codice impostato      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indice di selezione a 1                 *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-ctr-ibx          .
       aco-pdt-420.
      *                      *-----------------------------------------*
      *                      * Valore numerico in area di uscita       *
      *                      *-----------------------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico in area di uscita   *
      *                      *-----------------------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del valore alfanumerico *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione funct-key               *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-pdt-900.
       aco-pdt-500.
      *                  *---------------------------------------------*
      *                  * Se non piu' di w-buf-ctr-max records con lo *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-ctr-rec        to   w-buf-tot-pag          .
           subtract  1                    from w-buf-tot-pag          .
           divide    6                    into w-buf-tot-pag          .
           add       1                    to   w-buf-tot-pag          .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-inx-buf          .
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
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Titolo nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "               Selezionare il codice prodotto desi
      -              "derato                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura superiore                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineature inferiori                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Codice prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto per produt-  *
      *                      * tore                                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      "Per produttore :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per descrizione prodotto         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Descrizione     :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per produttore                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Produttore      :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   vpg-pdt-000          thru vpg-pdt-999            .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-pdt-520.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           divide    06                   into w-buf-inx-buf
                                        giving w-buf-ctr-005
                                     remainder w-buf-num-lnv          .
           if        w-buf-num-lnv        =    zero
                     move  06             to   w-buf-num-lnv          .
           add       07                   to   w-buf-num-lnv          .
       aco-pdt-540.
      *                      *-----------------------------------------*
      *                      * Visualizzazione record attualmente      *
      *                      * trattato, espanso                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice prodotto                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice prodotto per produttore      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      56                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Descrizione prodotto                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice produttore                   *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-pfc
                    (w-buf-inx-buf)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale produttore          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-rod-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-pdt-600.
      *                      *-----------------------------------------*
      *                      * Accettazione function-key alla linea    *
      *                      * calcolata                               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-inx-buf        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-inx-buf        <    w-buf-ctr-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-pag-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-pag-att        <    w-buf-tot-pag
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-num-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-pdt-615.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della function *
      *                          * key impostata                       *
      *                          *-------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-pdt-620
           else if   v-key                =    "UP  "
                     go to aco-pdt-625
           else if   v-key                =    "DOWN"
                     go to aco-pdt-630
           else if   v-key                =    "EXIT"
                     go to aco-pdt-635
           else if   v-key                =    "NXSC"
                     go to aco-pdt-640
           else if   v-key                =    "PRSC"
                     go to aco-pdt-645
           else      go to aco-pdt-600.
       aco-pdt-620.
      *                              *---------------------------------*
      *                              * Se Do o Slct o return           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-buf-inx-buf        to   w-buf-ctr-ibx          .
      *                                  *-----------------------------*
      *                                  * A selezione valori indiriz- *
      *                                  * zati da (w-buf-ctr-ibx) ed  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           go to     aco-pdt-420.
       aco-pdt-625.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    08
                     go to aco-pdt-650
           else      go to aco-pdt-520.
       aco-pdt-630.
      *                              *---------------------------------*
      *                              * Se Down                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-buf-inx-buf        =    w-buf-ctr-rec
                     go to aco-pdt-600.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    13
                     go to aco-pdt-650
           else      go to aco-pdt-520.
       aco-pdt-635.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione spaces in   *
      *                                  * area di accettazione        *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Segnale di uscita con exit  *
      *                                  *-----------------------------*
           move      "E"                  to   w-rbl-tip-ext          .
      *                                  *-----------------------------*
      *                                  * Ad uscita                   *
      *                                  *-----------------------------*
           go to     aco-pdt-900.
       aco-pdt-640.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-pdt-650.
       aco-pdt-645.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-pdt-650.
       aco-pdt-650.
      *                              *---------------------------------*
      *                              * Visualizzazione nuova pagina    *
      *                              * video                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione pagina vi-  *
      *                                  * deo contenente il record    *
      *                                  * attualmente trattato        *
      *                                  *-----------------------------*
           perform   vpg-pdt-000          thru vpg-pdt-999            .
      *                                  *-----------------------------*
      *                                  * A reimpostazione funct-key  *
      *                                  *-----------------------------*
           go to     aco-pdt-520.
       aco-pdt-800.
      *              *-------------------------------------------------*
      *              * Uscita per Exit                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "E"                  to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-pdt-999.
       aco-pdt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-pdt-999.
       aco-pdt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina video, per box locale di selezio-  *
      *    * ne, contenente il record indirizzato da (w-buf-inx-buf)   *
      *    *                                                           *
      *    * Specifica per l'accettazione codice per il produttore     *
      *    *-----------------------------------------------------------*
       vpg-pdt-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina trattata           *
      *              *-------------------------------------------------*
           move      w-buf-inx-buf        to   w-buf-pag-att          .
           add       5                    to   w-buf-pag-att          .
           divide    6                    into w-buf-pag-att          .
      *              *-------------------------------------------------*
      *              * Determinazione indice primo elemento della pa-  *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-att        to   w-buf-pag-ipe          .
           multiply  6                    by   w-buf-pag-ipe          .
           subtract  5                    from w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Determinazione indice ultimo elemento della pa- *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-ipe        to   w-buf-pag-iue          .
           add       5                    to   w-buf-pag-iue          .
           if        w-buf-pag-iue        >    w-buf-ctr-rec
                     move  w-buf-ctr-rec  to   w-buf-pag-iue          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per numero linea a vi-  *
      *              * deo relativa al primo elemento da visualizzare  *
      *              *-------------------------------------------------*
           move      08                   to   w-buf-i01-lnv          .
       vpg-pdt-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
       vpg-pdt-125.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-pdt-150.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-pdt-175.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il produttore           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      57                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-pdt-300.
      *              *-------------------------------------------------*
      *              * Incremento indice primo elemento della pagina   *
      *              * trattata                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Incremento indice per numero linea a video re-  *
      *              * relativa al primo elemento da visualizzare      *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-i01-lnv          .
      *              *-------------------------------------------------*
      *              * Se non si e' oltre l'ultimo elemento da tratta- *
      *              * re nella pagina si ricicla a visualizzare       *
      *              *-------------------------------------------------*
           if        w-buf-pag-ipe        not  > w-buf-pag-iue
                     go to vpg-pdt-100.
      *              *-------------------------------------------------*
      *              * Se si e' a pagina 1 : si esce                   *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        =    1
                     go to vpg-pdt-600.
      *              *-------------------------------------------------*
      *              * Se non si e' all'ultima pagina 1 : si esce      *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        <    w-buf-tot-pag
                     go to vpg-pdt-600.
       vpg-pdt-400.
      *              *-------------------------------------------------*
      *              * Eventuali linee residue all'interno del box di  *
      *              * selezione a Spaces                              *
      *              *-------------------------------------------------*
           if        w-buf-i01-lnv        >    13
                     go to vpg-pdt-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
           go to     vpg-pdt-400.
       vpg-pdt-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero pagina su pagine totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing pagina attuale                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-pag-att        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-att          .
      *                  *---------------------------------------------*
      *                  * Editing pagine totali                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-tot-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-tot          .
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      15                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Pagina"             to   w-all-str-cat (1)      .
           move      w-buf-lit-att        to   w-all-str-cat (2)      .
           move      "di"                 to   w-all-str-cat (3)      .
           move      w-buf-lit-tot        to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-buf-lit-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-lit-pag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
       vpg-pdt-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di accettazione per codice prodotto assegnato  *
      *    * dal fornitore                                             *
      *    *-----------------------------------------------------------*
       aco-fnt-000.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di uscita a spaces                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Se non e' specificata una linea per la de-  *
      *                  * scrizione si ritorna alla reimpostazione    *
      *                  * con default pari al valore salvato          *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-dln    =    zero or
                     w-cod-cod-dcp-dps    =    zero
                     go to aco-fnt-800.
       aco-fnt-010.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione per ricerca        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spaces in comodo per impostazione des-  *
      *                      * crizione                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-rbl-des-acc          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnt-020.
      *                      *-----------------------------------------*
      *                      * Accettazione descrizione minima di ri-  *
      *                      * cerca, in uppercase                     *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      w-rbl-des-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato in area di comodo di   *
      *                      * destinazione                            *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-acc          .
       aco-fnt-030.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-fnt-050.
      *                          *-------------------------------------*
      *                          * Visualizzazione spaces in area di   *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnt-040.
      *                          *-------------------------------------*
      *                          * Alla reimpostazione con default     *
      *                          * pari al valore salvato              *
      *                          *-------------------------------------*
           go to     aco-fnt-800.
       aco-fnt-050.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-fnt-060.
      *                          *-------------------------------------*
      *                          * Ripristino valori originali         *
      *                          *-------------------------------------*
           move      w-cod-cod-dcp-s01    to   w-cod-cod-dcp-num      .
           move      w-cod-cod-dcp-s02    to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Tipo operazione a : non continua-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-fnt-900.
       aco-fnt-060.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Valore impostato a Spaces :      *
      *                          * trattamento come per Up             *
      *                          *-------------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-fnt-040.
       aco-fnt-070.
      *                          *-------------------------------------*
      *                          * Preparazione dei parametri per      *
      *                          * l'esecuzione della ricerca per      *
      *                          * mezzo di un box locale per la sele- *
      *                          * zione                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore iniziale                 *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-cfn-min          .
      *                              *---------------------------------*
      *                              * Valore finale                   *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-cfn-max          .
      *                              *---------------------------------*
      *                              * Se valore wildcard diverso da   *
      *                              * '+' : oltre                     *
      *                              *---------------------------------*
           if        w-man-x14-wlc        not  = "+"
                     go to aco-fnt-100.
      *                              *---------------------------------*
      *                              * Padding del codice finale       *
      *                              *---------------------------------*
           move      14                   to   w-rbl-ctr-001          .
       aco-fnt-080.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-cfn-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-cfn-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-fnt-080.
       aco-fnt-100.
      *              *-------------------------------------------------*
      *              * Ricerca con box locale di selezione             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore records caricati  *
      *                  * nel buffer                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
      *                  *---------------------------------------------*
      *                  * Start su file [aaf] per codice prodotto     *
      *                  * assegnato dal fornitore                     *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "COPSFN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-cfn-min        to   rf-aaf-cop-sfn         .
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
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnt-350.
       aco-fnt-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale su file [aaf]               *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnt-350.
       aco-fnt-220.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        rf-aaf-cop-sfn       >    w-rbl-cfn-max
                     go to aco-fnt-350.
           if        rf-aaf-tip-mag       not  = 01
                     go to aco-fnt-350.
       aco-fnt-240.
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
      *                  *---------------------------------------------*
      *                  * Lettura del record [dcf]                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se codice a zero : ragione sociale a    *
      *                      * spaces                                  *
      *                      *-----------------------------------------*
           if        rf-aaf-cod-dcf       =    zero
                     move  spaces         to   rf-dcf-rag-soc
                     go to aco-fnt-300.
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      rf-aaf-cod-dcf       to   rf-dcf-cod-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcf-rag-soc         .
       aco-fnt-300.
      *              *-------------------------------------------------*
      *              * Incremento numero records caricati nel buffer   *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-ctr-rec          .
      *              *-------------------------------------------------*
      *              * Se oltre il max : a trattamento per fine file   *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        >    w-buf-ctr-max
                     subtract 1           from w-buf-ctr-rec
                     go to aco-fnt-500.
       aco-fnt-330.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record letto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di match                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tip-mch
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice numerico del prodotto                *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione del prodotto                    *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Sinonimo del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-syn-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice del prodotto per il fornitore        *
      *                  *---------------------------------------------*
           move      rf-aaf-cop-sfn       to   w-buf-cdp-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice del fornitore                        *
      *                  *---------------------------------------------*
           move      rf-aaf-cod-dcf       to   w-buf-cod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale del fornitore               *
      *                  *---------------------------------------------*
           move      rf-dcf-rag-soc       to   w-buf-rod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Chiave di ordinamento                       *
      *                  *---------------------------------------------*
           perform   aco-pko-000          thru aco-pko-999            .
       aco-fnt-335.
      *              *-------------------------------------------------*
      *              * Se attivo il flag di primo elemento da estrarre *
      *              * si esce dal ciclo di lettura                    *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-fpe    not  = "#"
                     go to aco-fnt-340.
           go to     aco-fnt-350.
       aco-fnt-340.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     aco-fnt-200.
       aco-fnt-350.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        =    zero
                     go to aco-fnt-375
           else if   w-buf-ctr-rec        =    1
                     go to aco-fnt-400
           else      go to aco-fnt-500.
       aco-fnt-375.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il codice impostato    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di selezione non effettuata     *
      *                      *-----------------------------------------*
           move      "N"                  to   w-rbl-tip-ext          .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-fnt-900.
       aco-fnt-400.
      *                  *---------------------------------------------*
      *                  * Se un 1 record con il codice impostato      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indice di selezione a 1                 *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-ctr-ibx          .
       aco-fnt-420.
      *                      *-----------------------------------------*
      *                      * Valore numerico in area di uscita       *
      *                      *-----------------------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico in area di uscita   *
      *                      *-----------------------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del valore alfanumerico *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione funct-key               *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-fnt-900.
       aco-fnt-500.
      *                  *---------------------------------------------*
      *                  * Se non piu' di w-buf-ctr-max records con lo *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-ctr-rec        to   w-buf-tot-pag          .
           subtract  1                    from w-buf-tot-pag          .
           divide    6                    into w-buf-tot-pag          .
           add       1                    to   w-buf-tot-pag          .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-inx-buf          .
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
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Titolo nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "               Selezionare il codice prodotto desi
      -              "derato                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura superiore                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineature inferiori                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Codice prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto per fornito- *
      *                      * re                                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      22                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "Codice per fornitore :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per descrizione prodotto         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Descrizione     :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per fornitore                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Fornitore       :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   vpg-fnt-000          thru vpg-fnt-999            .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnt-520.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           divide    06                   into w-buf-inx-buf
                                        giving w-buf-ctr-005
                                     remainder w-buf-num-lnv          .
           if        w-buf-num-lnv        =    zero
                     move  06             to   w-buf-num-lnv          .
           add       07                   to   w-buf-num-lnv          .
       aco-fnt-540.
      *                      *-----------------------------------------*
      *                      * Visualizzazione record attualmente      *
      *                      * trattato, espanso                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice prodotto                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice prodotto per fornitore       *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Descrizione prodotto                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice fornitore                    *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-pfc
                    (w-buf-inx-buf)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale fornitore           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-rod-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnt-600.
      *                      *-----------------------------------------*
      *                      * Accettazione function-key alla linea    *
      *                      * calcolata                               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-inx-buf        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-inx-buf        <    w-buf-ctr-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-pag-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-pag-att        <    w-buf-tot-pag
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-num-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-fnt-615.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della function *
      *                          * key impostata                       *
      *                          *-------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-fnt-620
           else if   v-key                =    "UP  "
                     go to aco-fnt-625
           else if   v-key                =    "DOWN"
                     go to aco-fnt-630
           else if   v-key                =    "EXIT"
                     go to aco-fnt-635
           else if   v-key                =    "NXSC"
                     go to aco-fnt-640
           else if   v-key                =    "PRSC"
                     go to aco-fnt-645
           else      go to aco-fnt-600.
       aco-fnt-620.
      *                              *---------------------------------*
      *                              * Se Do o Slct o return           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-buf-inx-buf        to   w-buf-ctr-ibx          .
      *                                  *-----------------------------*
      *                                  * A selezione valori indiriz- *
      *                                  * zati da (w-buf-ctr-ibx) ed  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           go to     aco-fnt-420.
       aco-fnt-625.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    08
                     go to aco-fnt-650
           else      go to aco-fnt-520.
       aco-fnt-630.
      *                              *---------------------------------*
      *                              * Se Down                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-buf-inx-buf        =    w-buf-ctr-rec
                     go to aco-fnt-600.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    13
                     go to aco-fnt-650
           else      go to aco-fnt-520.
       aco-fnt-635.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione spaces in   *
      *                                  * area di accettazione        *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Ad uscita per Exit          *
      *                                  *-----------------------------*
           go to     aco-fnt-800.
       aco-fnt-640.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-fnt-650.
       aco-fnt-645.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-fnt-650.
       aco-fnt-650.
      *                              *---------------------------------*
      *                              * Visualizzazione nuova pagina    *
      *                              * video                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione pagina vi-  *
      *                                  * deo contenente il record    *
      *                                  * attualmente trattato        *
      *                                  *-----------------------------*
           perform   vpg-fnt-000          thru vpg-fnt-999            .
      *                                  *-----------------------------*
      *                                  * A reimpostazione funct-key  *
      *                                  *-----------------------------*
           go to     aco-fnt-520.
       aco-fnt-800.
      *              *-------------------------------------------------*
      *              * Uscita per Exit                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "E"                  to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-fnt-999.
       aco-fnt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-fnt-999.
       aco-fnt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina video, per box locale di selezio-  *
      *    * ne, contenente il record indirizzato da (w-buf-inx-buf)   *
      *    *                                                           *
      *    * Specifica per l'accettazione codice per il fornitore      *
      *    *-----------------------------------------------------------*
       vpg-fnt-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina trattata           *
      *              *-------------------------------------------------*
           move      w-buf-inx-buf        to   w-buf-pag-att          .
           add       5                    to   w-buf-pag-att          .
           divide    6                    into w-buf-pag-att          .
      *              *-------------------------------------------------*
      *              * Determinazione indice primo elemento della pa-  *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-att        to   w-buf-pag-ipe          .
           multiply  6                    by   w-buf-pag-ipe          .
           subtract  5                    from w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Determinazione indice ultimo elemento della pa- *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-ipe        to   w-buf-pag-iue          .
           add       5                    to   w-buf-pag-iue          .
           if        w-buf-pag-iue        >    w-buf-ctr-rec
                     move  w-buf-ctr-rec  to   w-buf-pag-iue          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per numero linea a vi-  *
      *              * deo relativa al primo elemento da visualizzare  *
      *              *-------------------------------------------------*
           move      08                   to   w-buf-i01-lnv          .
       vpg-fnt-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
       vpg-fnt-125.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-fnt-150.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-fnt-175.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il fornitore            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      63                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-fnt-300.
      *              *-------------------------------------------------*
      *              * Incremento indice primo elemento della pagina   *
      *              * trattata                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Incremento indice per numero linea a video re-  *
      *              * relativa al primo elemento da visualizzare      *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-i01-lnv          .
      *              *-------------------------------------------------*
      *              * Se non si e' oltre l'ultimo elemento da tratta- *
      *              * re nella pagina si ricicla a visualizzare       *
      *              *-------------------------------------------------*
           if        w-buf-pag-ipe        not  > w-buf-pag-iue
                     go to vpg-fnt-100.
      *              *-------------------------------------------------*
      *              * Se si e' a pagina 1 : si esce                   *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        =    1
                     go to vpg-fnt-600.
      *              *-------------------------------------------------*
      *              * Se non si e' all'ultima pagina 1 : si esce      *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        <    w-buf-tot-pag
                     go to vpg-fnt-600.
       vpg-fnt-400.
      *              *-------------------------------------------------*
      *              * Eventuali linee residue all'interno del box di  *
      *              * selezione a Spaces                              *
      *              *-------------------------------------------------*
           if        w-buf-i01-lnv        >    13
                     go to vpg-fnt-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
           go to     vpg-fnt-400.
       vpg-fnt-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero pagina su pagine totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing pagina attuale                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-pag-att        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-att          .
      *                  *---------------------------------------------*
      *                  * Editing pagine totali                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-tot-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-tot          .
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      15                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Pagina"             to   w-all-str-cat (1)      .
           move      w-buf-lit-att        to   w-all-str-cat (2)      .
           move      "di"                 to   w-all-str-cat (3)      .
           move      w-buf-lit-tot        to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-buf-lit-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-lit-pag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
       vpg-fnt-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di accettazione per codice prodotto assegnato  *
      *    * dal cliente                                               *
      *    *-----------------------------------------------------------*
       aco-cli-000.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di uscita a spaces                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Se non e' specificata una linea per la de-  *
      *                  * scrizione si ritorna alla reimpostazione    *
      *                  * con default pari al valore salvato          *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-dln    =    zero or
                     w-cod-cod-dcp-dps    =    zero
                     go to aco-cli-800.
       aco-cli-010.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione per ricerca        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spaces in comodo per impostazione des-  *
      *                      * crizione                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-rbl-des-acc          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-cli-020.
      *                      *-----------------------------------------*
      *                      * Accettazione descrizione minima di ri-  *
      *                      * cerca, in uppercase                     *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      w-rbl-des-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato in area di comodo di   *
      *                      * destinazione                            *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-acc          .
       aco-cli-030.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-cli-050.
      *                          *-------------------------------------*
      *                          * Visualizzazione spaces in area di   *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-cli-040.
      *                          *-------------------------------------*
      *                          * Alla reimpostazione con default     *
      *                          * pari al valore salvato              *
      *                          *-------------------------------------*
           go to     aco-cli-800.
       aco-cli-050.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-cli-060.
      *                          *-------------------------------------*
      *                          * Ripristino valori originali         *
      *                          *-------------------------------------*
           move      w-cod-cod-dcp-s01    to   w-cod-cod-dcp-num      .
           move      w-cod-cod-dcp-s02    to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Tipo operazione a : non continua-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-cli-900.
       aco-cli-060.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Valore impostato a Spaces :      *
      *                          * trattamento come per Up             *
      *                          *-------------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-cli-040.
       aco-cli-070.
      *                          *-------------------------------------*
      *                          * Preparazione dei parametri per      *
      *                          * l'esecuzione della ricerca per      *
      *                          * mezzo di un box locale per la sele- *
      *                          * zione                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore iniziale                 *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-cpc-min          .
      *                              *---------------------------------*
      *                              * Valore finale                   *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-cpc-max          .
      *                              *---------------------------------*
      *                              * Se valore wildcard diverso da   *
      *                              * '+' : oltre                     *
      *                              *---------------------------------*
           if        w-man-x14-wlc        not  = "+"
                     go to aco-cli-100.
      *                              *---------------------------------*
      *                              * Padding del codice finale       *
      *                              *---------------------------------*
           move      40                   to   w-rbl-ctr-001          .
       aco-cli-080.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-cpc-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-cpc-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-cli-080.
       aco-cli-100.
      *              *-------------------------------------------------*
      *              * Ricerca con box locale di selezione             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore records caricati  *
      *                  * nel buffer                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
      *                  *---------------------------------------------*
      *                  * Start su file [pdk] per codice prodotto     *
      *                  * assegnato dal cliente                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "RECALF    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "C"                  to   rf-pdk-tip-rec         .
           move      w-rbl-cpc-min        to   rf-pdk-alf-pro         .
           move      zero                 to   rf-pdk-num-pro         .
           move      zero                 to   rf-pdk-cod-arc         .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-cli-350.
       aco-cli-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale su file [pdk]               *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-cli-350.
       aco-cli-220.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        rf-pdk-tip-rec       not  = "C"
                     go to aco-cli-350.
           if        rf-pdk-alf-pro       >    w-rbl-cpc-max
                     go to aco-cli-350.
       aco-cli-240.
      *              *-------------------------------------------------*
      *              * Letture addizionali                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura del record [dcp]                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-pdk-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se record [dcp] non trovato             *
      *                      *                                         *
      *                      * Attualmente inibito                     *
      *                      *-----------------------------------------*
______*    if        f-sts                not  = e-not-err
______*              move  all "."        to   rf-dcp-des-pro         .
      *                      *-----------------------------------------*
      *                      * Se record [dcp] non trovato: a riciclo  *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-cli-200.
      *                  *---------------------------------------------*
      *                  * Lettura del record [dcc]                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se codice a zero : ragione sociale a    *
      *                      * spaces                                  *
      *                      *-----------------------------------------*
           if        rf-pdk-cod-arc       =    zero
                     move  spaces         to   rf-dcc-rag-soc
                     go to aco-cli-300.
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-pdk-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
       aco-cli-300.
      *              *-------------------------------------------------*
      *              * Incremento numero records caricati nel buffer   *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-ctr-rec          .
      *              *-------------------------------------------------*
      *              * Se oltre il max : a trattamento per fine file   *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        >    w-buf-ctr-max
                     subtract 1           from w-buf-ctr-rec
                     go to aco-cli-500.
       aco-cli-330.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record letto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di match                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tip-mch
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice numerico del prodotto                *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione del prodotto                    *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Sinonimo del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-syn-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice del prodotto per il cliente          *
      *                  *---------------------------------------------*
           move      rf-pdk-alf-pro       to   w-buf-cdp-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice del cliente                          *
      *                  *---------------------------------------------*
           move      rf-pdk-cod-arc       to   w-buf-cod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale del cliente                 *
      *                  *---------------------------------------------*
           move      rf-dcc-rag-soc       to   w-buf-rod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Chiave di ordinamento                       *
      *                  *---------------------------------------------*
           perform   aco-pko-000          thru aco-pko-999            .
       aco-cli-335.
      *              *-------------------------------------------------*
      *              * Se attivo il flag di primo elemento da estrarre *
      *              * si esce dal ciclo di lettura                    *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-fpe    not  = "#"
                     go to aco-cli-340.
           go to     aco-cli-350.
       aco-cli-340.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     aco-cli-200.
       aco-cli-350.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        =    zero
                     go to aco-cli-375
           else if   w-buf-ctr-rec        =    1
                     go to aco-cli-400
           else      go to aco-cli-500.
       aco-cli-375.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il codice impostato    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di selezione non effettuata     *
      *                      *-----------------------------------------*
           move      "N"                  to   w-rbl-tip-ext          .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-cli-900.
       aco-cli-400.
      *                  *---------------------------------------------*
      *                  * Se un 1 record con il codice impostato      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indice di selezione a 1                 *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-ctr-ibx          .
       aco-cli-420.
      *                      *-----------------------------------------*
      *                      * Valore numerico in area di uscita       *
      *                      *-----------------------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico in area di uscita   *
      *                      *-----------------------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del valore alfanumerico *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione funct-key               *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-cli-900.
       aco-cli-500.
      *                  *---------------------------------------------*
      *                  * Se non piu' di w-buf-ctr-max records con lo *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-ctr-rec        to   w-buf-tot-pag          .
           subtract  1                    from w-buf-tot-pag          .
           divide    6                    into w-buf-tot-pag          .
           add       1                    to   w-buf-tot-pag          .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-inx-buf          .
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
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Titolo nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "               Selezionare il codice prodotto desi
      -              "derato                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura superiore                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineature inferiori                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Codice prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto per cliente  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      "Codice del cliente :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per descrizione prodotto         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Descrizione     :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per cliente                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Cliente         :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   vpg-cli-000          thru vpg-cli-999            .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-cli-520.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           divide    06                   into w-buf-inx-buf
                                        giving w-buf-ctr-005
                                     remainder w-buf-num-lnv          .
           if        w-buf-num-lnv        =    zero
                     move  06             to   w-buf-num-lnv          .
           add       07                   to   w-buf-num-lnv          .
       aco-cli-540.
      *                      *-----------------------------------------*
      *                      * Visualizzazione record attualmente      *
      *                      * trattato, espanso                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice prodotto                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice prodotto per cliente         *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Descrizione prodotto                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice cliente                      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-pfc
                    (w-buf-inx-buf)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale cliente             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-rod-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-cli-600.
      *                      *-----------------------------------------*
      *                      * Accettazione function-key alla linea    *
      *                      * calcolata                               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-inx-buf        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-inx-buf        <    w-buf-ctr-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-pag-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-pag-att        <    w-buf-tot-pag
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-num-lnv        to   v-lin                  .
           move      20                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-cli-615.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della function *
      *                          * key impostata                       *
      *                          *-------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-cli-620
           else if   v-key                =    "UP  "
                     go to aco-cli-625
           else if   v-key                =    "DOWN"
                     go to aco-cli-630
           else if   v-key                =    "EXIT"
                     go to aco-cli-635
           else if   v-key                =    "NXSC"
                     go to aco-cli-640
           else if   v-key                =    "PRSC"
                     go to aco-cli-645
           else      go to aco-cli-600.
       aco-cli-620.
      *                              *---------------------------------*
      *                              * Se Do o Slct o return           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-buf-inx-buf        to   w-buf-ctr-ibx          .
      *                                  *-----------------------------*
      *                                  * A selezione valori indiriz- *
      *                                  * zati da (w-buf-ctr-ibx) ed  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           go to     aco-cli-420.
       aco-cli-625.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    08
                     go to aco-cli-650
           else      go to aco-cli-520.
       aco-cli-630.
      *                              *---------------------------------*
      *                              * Se Down                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-buf-inx-buf        =    w-buf-ctr-rec
                     go to aco-cli-600.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    13
                     go to aco-cli-650
           else      go to aco-cli-520.
       aco-cli-635.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione spaces in   *
      *                                  * area di accettazione        *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Ad uscita per Exit          *
      *                                  *-----------------------------*
           go to     aco-cli-800.
       aco-cli-640.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-cli-650.
       aco-cli-645.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-cli-650.
       aco-cli-650.
      *                              *---------------------------------*
      *                              * Visualizzazione nuova pagina    *
      *                              * video                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione pagina vi-  *
      *                                  * deo contenente il record    *
      *                                  * attualmente trattato        *
      *                                  *-----------------------------*
           perform   vpg-cli-000          thru vpg-cli-999            .
      *                                  *-----------------------------*
      *                                  * A reimpostazione funct-key  *
      *                                  *-----------------------------*
           go to     aco-cli-520.
       aco-cli-800.
      *              *-------------------------------------------------*
      *              * Uscita per Exit                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "E"                  to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-cli-999.
       aco-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-cli-999.
       aco-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina video, per box locale di selezio-  *
      *    * ne, contenente il record indirizzato da (w-buf-inx-buf)   *
      *    *                                                           *
      *    * Specifica per l'accettazione codice per il cliente        *
      *    *-----------------------------------------------------------*
       vpg-cli-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina trattata           *
      *              *-------------------------------------------------*
           move      w-buf-inx-buf        to   w-buf-pag-att          .
           add       5                    to   w-buf-pag-att          .
           divide    6                    into w-buf-pag-att          .
      *              *-------------------------------------------------*
      *              * Determinazione indice primo elemento della pa-  *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-att        to   w-buf-pag-ipe          .
           multiply  6                    by   w-buf-pag-ipe          .
           subtract  5                    from w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Determinazione indice ultimo elemento della pa- *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-ipe        to   w-buf-pag-iue          .
           add       5                    to   w-buf-pag-iue          .
           if        w-buf-pag-iue        >    w-buf-ctr-rec
                     move  w-buf-ctr-rec  to   w-buf-pag-iue          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per numero linea a vi-  *
      *              * deo relativa al primo elemento da visualizzare  *
      *              *-------------------------------------------------*
           move      08                   to   w-buf-i01-lnv          .
       vpg-cli-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
       vpg-cli-125.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-cli-150.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-cli-175.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il cliente              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      61                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-cli-300.
      *              *-------------------------------------------------*
      *              * Incremento indice primo elemento della pagina   *
      *              * trattata                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Incremento indice per numero linea a video re-  *
      *              * relativa al primo elemento da visualizzare      *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-i01-lnv          .
      *              *-------------------------------------------------*
      *              * Se non si e' oltre l'ultimo elemento da tratta- *
      *              * re nella pagina si ricicla a visualizzare       *
      *              *-------------------------------------------------*
           if        w-buf-pag-ipe        not  > w-buf-pag-iue
                     go to vpg-cli-100.
      *              *-------------------------------------------------*
      *              * Se si e' a pagina 1 : si esce                   *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        =    1
                     go to vpg-cli-600.
      *              *-------------------------------------------------*
      *              * Se non si e' all'ultima pagina 1 : si esce      *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        <    w-buf-tot-pag
                     go to vpg-cli-600.
       vpg-cli-400.
      *              *-------------------------------------------------*
      *              * Eventuali linee residue all'interno del box di  *
      *              * selezione a Spaces                              *
      *              *-------------------------------------------------*
           if        w-buf-i01-lnv        >    13
                     go to vpg-cli-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
           go to     vpg-cli-400.
       vpg-cli-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero pagina su pagine totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing pagina attuale                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-pag-att        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-att          .
      *                  *---------------------------------------------*
      *                  * Editing pagine totali                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-tot-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-tot          .
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      15                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Pagina"             to   w-all-str-cat (1)      .
           move      w-buf-lit-att        to   w-all-str-cat (2)      .
           move      "di"                 to   w-all-str-cat (3)      .
           move      w-buf-lit-tot        to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-buf-lit-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-lit-pag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
       vpg-cli-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *                                                           *
      *    * Subroutine di accettazione per codice libero              *
      *    *-----------------------------------------------------------*
       aco-klb-000.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di uscita a spaces                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Se non e' specificata una linea per la de-  *
      *                  * scrizione si ritorna alla reimpostazione    *
      *                  * con default pari al valore salvato          *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcp-dln    =    zero or
                     w-cod-cod-dcp-dps    =    zero
                     go to aco-klb-800.
       aco-klb-010.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione per ricerca        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spaces in comodo per impostazione des-  *
      *                      * crizione                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-rbl-des-acc          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-klb-020.
      *                      *-----------------------------------------*
      *                      * Accettazione descrizione minima di ri-  *
      *                      * cerca, in uppercase                     *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      w-rbl-des-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato in area di comodo di   *
      *                      * destinazione                            *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-acc          .
       aco-klb-030.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-klb-050.
      *                          *-------------------------------------*
      *                          * Visualizzazione spaces in area di   *
      *                          * accettazione                        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-klb-040.
      *                          *-------------------------------------*
      *                          * Alla reimpostazione con default     *
      *                          * pari al valore salvato              *
      *                          *-------------------------------------*
           go to     aco-klb-800.
       aco-klb-050.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-klb-060.
      *                          *-------------------------------------*
      *                          * Ripristino valori originali         *
      *                          *-------------------------------------*
           move      w-cod-cod-dcp-s01    to   w-cod-cod-dcp-num      .
           move      w-cod-cod-dcp-s02    to   w-cod-cod-dcp-alf      .
      *                          *-------------------------------------*
      *                          * Tipo operazione a : non continua-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     aco-klb-900.
       aco-klb-060.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se Valore impostato a Spaces :      *
      *                          * trattamento come per Up             *
      *                          *-------------------------------------*
           if        w-rbl-des-acc        =    spaces
                     go to aco-klb-040.
       aco-klb-070.
      *                          *-------------------------------------*
      *                          * Preparazione dei parametri per      *
      *                          * l'esecuzione della ricerca per      *
      *                          * mezzo di un box locale per la sele- *
      *                          * zione                               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore iniziale                 *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-klb-min          .
      *                              *---------------------------------*
      *                              * Valore finale                   *
      *                              *---------------------------------*
           move      w-rbl-des-acc        to   w-rbl-klb-max          .
      *                              *---------------------------------*
      *                              * Se valore wildcard diverso da   *
      *                              * '+' : oltre                     *
      *                              *---------------------------------*
           if        w-man-x14-wlc        not  = "+"
                     go to aco-klb-100.
      *                              *---------------------------------*
      *                              * Padding del codice finale       *
      *                              *---------------------------------*
           move      13                   to   w-rbl-ctr-001          .
       aco-klb-080.
           if        w-rbl-ctr-001        >    zero
                     if    w-rbl-klb-mxx
                          (w-rbl-ctr-001) =    spaces
                           move     "z"   to   w-rbl-klb-mxx
                                              (w-rbl-ctr-001)
                           subtract 1     from w-rbl-ctr-001
                           go to    aco-klb-080.
       aco-klb-100.
      *              *-------------------------------------------------*
      *              * Ricerca con box locale di selezione             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore records caricati  *
      *                  * nel buffer                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-ctr-rec          .
      *                  *---------------------------------------------*
      *                  * Start su file [dcp] per codice libero       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "KLBPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rbl-klb-min        to   rf-dcp-klb-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-klb-350.
       aco-klb-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale su file [dcp]               *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-klb-350.
       aco-klb-220.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       >    w-rbl-klb-max
                     go to aco-klb-350.
       aco-klb-240.
      *              *-------------------------------------------------*
      *              * Letture addizionali                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice a zero : descrizione a spaces     *
      *                  *---------------------------------------------*
           if        rf-dcp-cla-pro       =    zero
                     move  spaces         to   rf-zp1-des-cla
                     go to aco-klb-300.
      *                  *---------------------------------------------*
      *                  * Lettura del record [zp1]                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      rf-dcp-cla-pro       to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-zp1-des-cla         .
       aco-klb-300.
      *              *-------------------------------------------------*
      *              * Incremento numero records caricati nel buffer   *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-ctr-rec          .
      *              *-------------------------------------------------*
      *              * Se oltre il max : a trattamento per fine file   *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        >    w-buf-ctr-max
                     subtract 1           from w-buf-ctr-rec
                     go to aco-klb-500.
       aco-klb-330.
      *              *-------------------------------------------------*
      *              * Bufferizzazione record letto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di match                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tip-mch
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice numerico del prodotto                *
      *                  *---------------------------------------------*
           move      rf-dcp-num-pro       to   w-buf-cod-num
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      rf-dcp-alf-pro       to   w-buf-cod-alf
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione del prodotto                    *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-buf-des-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Sinonimo del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-syn-pro       to   w-buf-syn-pro
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice libero del prodotto                  *
      *                  *---------------------------------------------*
           move      rf-dcp-klb-pro       to   w-buf-cdp-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Codice classe                               *
      *                  *---------------------------------------------*
           move      rf-dcp-cla-pro       to   w-buf-cod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Descrizione classe                          *
      *                  *---------------------------------------------*
           move      rf-zp1-des-cla       to   w-buf-rod-pfc
                                              (w-buf-ctr-rec)         .
      *                  *---------------------------------------------*
      *                  * Chiave di ordinamento                       *
      *                  *---------------------------------------------*
           perform   aco-pko-000          thru aco-pko-999            .
       aco-klb-335.
      *              *-------------------------------------------------*
      *              * Se attivo il flag di primo elemento da estrarre *
      *              * si esce dal ciclo di lettura                    *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcp-fpe    not  = "#"
                     go to aco-klb-340.
           go to     aco-klb-350.
       aco-klb-340.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     aco-klb-200.
       aco-klb-350.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-buf-ctr-rec        =    zero
                     go to aco-klb-375
           else if   w-buf-ctr-rec        =    1
                     go to aco-klb-400
           else      go to aco-klb-500.
       aco-klb-375.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il codice impostato    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di selezione non effettuata     *
      *                      *-----------------------------------------*
           move      "N"                  to   w-rbl-tip-ext          .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-klb-900.
       aco-klb-400.
      *                  *---------------------------------------------*
      *                  * Se un 1 record con il codice impostato      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indice di selezione a 1                 *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-ctr-ibx          .
       aco-klb-420.
      *                      *-----------------------------------------*
      *                      * Valore numerico in area di uscita       *
      *                      *-----------------------------------------*
           move      w-buf-cod-num
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico in area di uscita   *
      *                      *-----------------------------------------*
           move      w-buf-cod-alf
                    (w-buf-ctr-ibx)       to   w-cod-cod-dcp-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del valore alfanumerico *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcp-lin    to   v-lin                  .
           move      w-cod-cod-dcp-pos    to   v-pos                  .
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione funct-key               *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-klb-900.
       aco-klb-500.
      *                  *---------------------------------------------*
      *                  * Se non piu' di w-buf-ctr-max records con lo *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-ctr-rec        to   w-buf-tot-pag          .
           subtract  1                    from w-buf-tot-pag          .
           divide    6                    into w-buf-tot-pag          .
           add       1                    to   w-buf-tot-pag          .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-inx-buf          .
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
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Titolo nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "               Selezionare il codice prodotto desi
      -              "derato                "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura superiore                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineature inferiori                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Codice prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per sinonimo                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      53                   to   v-pos                  .
           move      "Sinonimo :"         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per descrizione prodotto         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Descrizione     :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt per la classe                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "Classe prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   vpg-klb-000          thru vpg-klb-999            .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-klb-520.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           divide    06                   into w-buf-inx-buf
                                        giving w-buf-ctr-005
                                     remainder w-buf-num-lnv          .
           if        w-buf-num-lnv        =    zero
                     move  06             to   w-buf-num-lnv          .
           add       07                   to   w-buf-num-lnv          .
       aco-klb-540.
      *                      *-----------------------------------------*
      *                      * Visualizzazione record attualmente      *
      *                      * trattato, espanso                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice prodotto                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Sinonimo                            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      64                   to   v-pos                  .
           move      w-buf-syn-pro
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Descrizione prodotto                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Codice classe                       *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      19                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-buf-cod-pfc
                    (w-buf-inx-buf)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Descrizione classe                  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-rod-pfc
                    (w-buf-inx-buf)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-klb-600.
      *                      *-----------------------------------------*
      *                      * Accettazione function-key alla linea    *
      *                      * calcolata                               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-inx-buf        >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-inx-buf        <    w-buf-ctr-rec
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-pag-att        >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-pag-att        <    w-buf-tot-pag
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-num-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-klb-615.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della function *
      *                          * key impostata                       *
      *                          *-------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-klb-620
           else if   v-key                =    "UP  "
                     go to aco-klb-625
           else if   v-key                =    "DOWN"
                     go to aco-klb-630
           else if   v-key                =    "EXIT"
                     go to aco-klb-635
           else if   v-key                =    "NXSC"
                     go to aco-klb-640
           else if   v-key                =    "PRSC"
                     go to aco-klb-645
           else      go to aco-klb-600.
       aco-klb-620.
      *                              *---------------------------------*
      *                              * Se Do o Slct o return           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-buf-inx-buf        to   w-buf-ctr-ibx          .
      *                                  *-----------------------------*
      *                                  * A selezione valori indiriz- *
      *                                  * zati da (w-buf-ctr-ibx) ed  *
      *                                  * uscita                      *
      *                                  *-----------------------------*
           go to     aco-klb-420.
       aco-klb-625.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    08
                     go to aco-klb-650
           else      go to aco-klb-520.
       aco-klb-630.
      *                              *---------------------------------*
      *                              * Se Down                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-buf-inx-buf        =    w-buf-ctr-rec
                     go to aco-klb-600.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-buf-num-lnv        =    13
                     go to aco-klb-650
           else      go to aco-klb-520.
       aco-klb-635.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Visualizzazione spaces in   *
      *                                  * area di accettazione        *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcp-dln    to   v-lin                  .
           move      w-cod-cod-dcp-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Segnale di uscita con exit  *
      *                                  *-----------------------------*
           move      "E"                  to   w-rbl-tip-ext          .
      *                                  *-----------------------------*
      *                                  * Ad uscita                   *
      *                                  *-----------------------------*
           go to     aco-klb-900.
       aco-klb-640.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-klb-650.
       aco-klb-645.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-buf-pag-att          .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-buf-pag-att        to   w-buf-inx-buf          .
           multiply  06                   by   w-buf-inx-buf          .
           subtract  5                    from w-buf-inx-buf          .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-klb-650.
       aco-klb-650.
      *                              *---------------------------------*
      *                              * Visualizzazione nuova pagina    *
      *                              * video                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione pagina vi-  *
      *                                  * deo contenente il record    *
      *                                  * attualmente trattato        *
      *                                  *-----------------------------*
           perform   vpg-klb-000          thru vpg-klb-999            .
      *                                  *-----------------------------*
      *                                  * A reimpostazione funct-key  *
      *                                  *-----------------------------*
           go to     aco-klb-520.
       aco-klb-800.
      *              *-------------------------------------------------*
      *              * Uscita per Exit                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "E"                  to   w-rbl-tip-ext          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-klb-999.
       aco-klb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-klb-999.
       aco-klb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina video, per box locale di selezio-  *
      *    * ne, contenente il record indirizzato da (w-buf-inx-buf)   *
      *    *                                                           *
      *    * Specifica per l'accettazione codice libero                *
      *    *-----------------------------------------------------------*
       vpg-klb-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina trattata           *
      *              *-------------------------------------------------*
           move      w-buf-inx-buf        to   w-buf-pag-att          .
           add       5                    to   w-buf-pag-att          .
           divide    6                    into w-buf-pag-att          .
      *              *-------------------------------------------------*
      *              * Determinazione indice primo elemento della pa-  *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-att        to   w-buf-pag-ipe          .
           multiply  6                    by   w-buf-pag-ipe          .
           subtract  5                    from w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Determinazione indice ultimo elemento della pa- *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-ipe        to   w-buf-pag-iue          .
           add       5                    to   w-buf-pag-iue          .
           if        w-buf-pag-iue        >    w-buf-ctr-rec
                     move  w-buf-ctr-rec  to   w-buf-pag-iue          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per numero linea a vi-  *
      *              * deo relativa al primo elemento da visualizzare  *
      *              *-------------------------------------------------*
           move      08                   to   w-buf-i01-lnv          .
       vpg-klb-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
       vpg-klb-125.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-klb-150.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-klb-175.
      *                  *---------------------------------------------*
      *                  * Codice libero prodotto                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      64                   to   v-pos                  .
           move      w-buf-cdp-pfc
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-klb-300.
      *              *-------------------------------------------------*
      *              * Incremento indice primo elemento della pagina   *
      *              * trattata                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Incremento indice per numero linea a video re-  *
      *              * relativa al primo elemento da visualizzare      *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-i01-lnv          .
      *              *-------------------------------------------------*
      *              * Se non si e' oltre l'ultimo elemento da tratta- *
      *              * re nella pagina si ricicla a visualizzare       *
      *              *-------------------------------------------------*
           if        w-buf-pag-ipe        not  > w-buf-pag-iue
                     go to vpg-klb-100.
      *              *-------------------------------------------------*
      *              * Se si e' a pagina 1 : si esce                   *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        =    1
                     go to vpg-klb-600.
      *              *-------------------------------------------------*
      *              * Se non si e' all'ultima pagina 1 : si esce      *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        <    w-buf-tot-pag
                     go to vpg-klb-600.
       vpg-klb-400.
      *              *-------------------------------------------------*
      *              * Eventuali linee residue all'interno del box di  *
      *              * selezione a Spaces                              *
      *              *-------------------------------------------------*
           if        w-buf-i01-lnv        >    13
                     go to vpg-klb-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
           go to     vpg-klb-400.
       vpg-klb-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero pagina su pagine totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing pagina attuale                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-pag-att        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-att          .
      *                  *---------------------------------------------*
      *                  * Editing pagine totali                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-tot-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-tot          .
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      15                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Pagina"             to   w-all-str-cat (1)      .
           move      w-buf-lit-att        to   w-all-str-cat (2)      .
           move      "di"                 to   w-all-str-cat (3)      .
           move      w-buf-lit-tot        to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-buf-lit-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-lit-pag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
       vpg-klb-999.
           exit.

      *    *===========================================================*
      *    * Accettazione                                              *
      *    *                                                           *
      *    * Subroutine di visualizzazione aiuto a video tramite tasto *
      *    * di Expand                                                 *
      *    *-----------------------------------------------------------*
       aco-exp-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione box vuoto                       *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      70                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literals nel box                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "            Metodi disponibili per la ricerca     
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "Da codice                                         
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : =SINONIMO            : +SINONIMO               
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : -Inizio codice                                 
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : F- o F+ {invio}  per codice prodotto Fornitore 
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : P- o P+ {invio}  per codice prodotto Produttore
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : C- o C+ {invio}  per codice prodotto Cliente   
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "Da descrizione                                    
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : -       {invio}  inizio o tutta la descrizione 
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : -       {invio}  parte della descrizione {ricer
      -              "ca}   "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : -       {invio} {expand} (questa videata)      
      -              "      "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      " : -       {invio} {selezione} parti di descrizion
      -              "e     "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      20                   to   v-lin                  .
           move      67                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-exp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-exp-999.
       aco-exp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione                                              *
      *    *                                                           *
      *    * Subroutine di accettazione parti di descrizione per la    *
      *    * ricerca avanzata                                          *
      *    *-----------------------------------------------------------*
       aco-slc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-rbl-des-fl5          .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione box vuoto                       *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      70                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Literals nel box                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "     RICERCA AVANZATA SU DESCRIZIONE AD USO INTERN
      -              "O     "             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      56                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      "--------------------------------------------------
      -              "------"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-slc-050.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-rbl-des-al1          .
           move      spaces               to   w-rbl-des-al2          .
           move      spaces               to   w-rbl-des-al3          .
           move      spaces               to   w-rbl-des-al4          .
           move      spaces               to   w-rbl-des-al5          .
           move      spaces               to   v-key                  .
       aco-slc-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
       aco-slc-110.
      *                  *---------------------------------------------*
      *                  * Campo alfanumerico 1                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      07                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-rbl-des-al1        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato                        *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-al1          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to aco-slc-600
           else if   v-key                =    "EXIT"
                     go to aco-slc-850.
       aco-slc-200.
      *                  *---------------------------------------------*
      *                  * Campo alfanumerico 2                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se campo da accettare              *
      *                      *-----------------------------------------*
           if        w-rbl-des-al1        =    spaces
                     go to aco-slc-850.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      08                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-rbl-des-al2        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato                        *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-al2          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to aco-slc-100
           else if   v-key                =    "DO  "
                     go to aco-slc-600
           else if   v-key                =    "EXIT"
                     go to aco-slc-850.
      *                      *-----------------------------------------*
      *                      * Se a spaces : come per tasto 'Do'       *
      *                      *-----------------------------------------*
           if        w-rbl-des-al2        =    spaces
                     go to aco-slc-600.
       aco-slc-300.
      *                  *---------------------------------------------*
      *                  * Campo alfanumerico 3                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se campo da accettare              *
      *                      *-----------------------------------------*
           if        w-rbl-des-al2        =    spaces
                     go to aco-slc-600.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      09                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-rbl-des-al3        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato                        *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-al3          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to aco-slc-200
           else if   v-key                =    "DO  "
                     go to aco-slc-600
           else if   v-key                =    "EXIT"
                     go to aco-slc-850.
      *                      *-----------------------------------------*
      *                      * Se a spaces : come per tasto 'Do'       *
      *                      *-----------------------------------------*
           if        w-rbl-des-al3        =    spaces
                     go to aco-slc-600.
       aco-slc-400.
      *                  *---------------------------------------------*
      *                  * Campo alfanumerico 4                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se campo da accettare              *
      *                      *-----------------------------------------*
           if        w-rbl-des-al3        =    spaces
                     go to aco-slc-600.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      10                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-rbl-des-al4        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato                        *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-al4          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to aco-slc-300
           else if   v-key                =    "DO  "
                     go to aco-slc-600
           else if   v-key                =    "EXIT"
                     go to aco-slc-850.
      *                      *-----------------------------------------*
      *                      * Se a spaces : come per tasto 'Do'       *
      *                      *-----------------------------------------*
           if        w-rbl-des-al4        =    spaces
                     go to aco-slc-600.
       aco-slc-500.
      *                  *---------------------------------------------*
      *                  * Campo alfanumerico 5                        *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      11                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-rbl-des-al5        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore accettato                        *
      *                      *-----------------------------------------*
           move      v-alf                to   w-rbl-des-al5          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to aco-slc-400
           else if   v-key                =    "DO  "
                     go to aco-slc-600
           else if   v-key                =    "EXIT"
                     go to aco-slc-850.
      *                      *-----------------------------------------*
      *                      * Se a spaces : come per tasto 'Do'       *
      *                      *-----------------------------------------*
           if        w-rbl-des-al5        =    spaces
                     go to aco-slc-600.
       aco-slc-600.
      *              *-------------------------------------------------*
      *              * Controllo valori impostati                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Che non ci siano campi vuoti in mezzo       *
      *                  *---------------------------------------------*
           if       (w-rbl-des-al5        not  = spaces and
                     w-rbl-des-al4        =    spaces ) or
                    (w-rbl-des-al5        not  = spaces and
                     w-rbl-des-al3        =    spaces ) or
                    (w-rbl-des-al5        not  = spaces and
                     w-rbl-des-al2        =    spaces ) or
                    (w-rbl-des-al5        not  = spaces and
                     w-rbl-des-al1        =    spaces )
                     go to     aco-slc-100.
           if       (w-rbl-des-al4        not  = spaces and
                     w-rbl-des-al3        =    spaces ) or
                    (w-rbl-des-al4        not  = spaces and
                     w-rbl-des-al2        =    spaces ) or
                    (w-rbl-des-al4        not  = spaces and
                     w-rbl-des-al1        =    spaces )
                     go to     aco-slc-100.
           if       (w-rbl-des-al3        not  = spaces and
                     w-rbl-des-al2        =    spaces ) or
                    (w-rbl-des-al3        not  = spaces and
                     w-rbl-des-al1        =    spaces )
                     go to     aco-slc-100.
           if       (w-rbl-des-al2        not  = spaces and
                     w-rbl-des-al1        =    spaces )
                     go to     aco-slc-100.
       aco-slc-700.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori impostati                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9 - campo 1       *
      *                  *---------------------------------------------*
           if        w-rbl-des-al1        =    spaces
                     go to aco-slc-850.
           move      w-rbl-des-al1        to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-rbl-des-al1          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9 - campo 2       *
      *                  *---------------------------------------------*
           if        w-rbl-des-al2        =    spaces
                     go to aco-slc-800.
           move      w-rbl-des-al2        to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-rbl-des-al2          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9 - campo 3       *
      *                  *---------------------------------------------*
           if        w-rbl-des-al3        =    spaces
                     go to aco-slc-800.
           move      w-rbl-des-al3        to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-rbl-des-al3          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9 - campo 4       *
      *                  *---------------------------------------------*
           if        w-rbl-des-al4        =    spaces
                     go to aco-slc-800.
           move      w-rbl-des-al4        to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-rbl-des-al4          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9 - campo 5       *
      *                  *---------------------------------------------*
           if        w-rbl-des-al5        =    spaces
                     go to aco-slc-800.
           move      w-rbl-des-al5        to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
           move      w-atz-1t9-vno        to   w-rbl-des-al5          .
       aco-slc-800.
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rbl-des-fl5          .
      *                  *---------------------------------------------*
      *                  * Se 'Do' : uscita diretta                    *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to aco-slc-900.
       aco-slc-810.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      13                   to   v-lin                  .
           move      67                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .

      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della function key *
      *                      * impostata                               *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     move  spaces         to   v-key
                     go to aco-slc-100
           else if   v-key                =    "DO  "
                     go to aco-slc-600
           else if   v-key                =    "EXIT"
                     go to aco-slc-850.
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     aco-slc-810.
       aco-slc-850.
      *              *-------------------------------------------------*
      *              * Uscita per Exit                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-des-fl5          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-slc-900.
       aco-slc-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-slc-999.
       aco-slc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione                                              *
      *    *                                                           *
      *    * Subroutine di confronto eventuale con i 5 campi alfanume- *
      *    * rici accettati                                            *
      *    *-----------------------------------------------------------*
       aco-slc-tst-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flags di uscita                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-rbl-des-fm5          .
      *              *-------------------------------------------------*
      *              * Test su valore campi alfanumerici               *
      *              *-------------------------------------------------*
           if        w-rbl-des-al1        =    spaces and
                     w-rbl-des-al2        =    spaces and
                     w-rbl-des-al3        =    spaces and
                     w-rbl-des-al4        =    spaces and
                     w-rbl-des-al5        =    spaces
                     go to aco-slc-tst-900.
       aco-slc-tst-100.
      *              *-------------------------------------------------*
      *              * Match per il campo 1                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rbl-des-al1        =    spaces
                     go to aco-slc-tst-850.
      *                  *---------------------------------------------*
      *                  * Preparazione 2. valore per il match         *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-rbl-des-al1        to   w-atz-1t9-vdn          .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Se c'e' stato un match : flag attivo        *
      *                  * superato                                    *
      *                  *---------------------------------------------*
           if        w-atz-1t9-flg-mch    not  = spaces
                     go to aco-slc-tst-200.
           move      "#"                  to   w-rbl-des-fm5          .
       aco-slc-tst-200.
      *              *-------------------------------------------------*
      *              * Match per il campo 2                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rbl-des-al2        =    spaces
                     go to aco-slc-tst-800.
      *                  *---------------------------------------------*
      *                  * Preparazione 2. valore per il match         *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-rbl-des-al2        to   w-atz-1t9-vdn          .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Test sul risultato del match                *
      *                  *---------------------------------------------*
           if        w-atz-1t9-flg-mch    not  = spaces
                     go to aco-slc-tst-850.
       aco-slc-tst-300.
      *              *-------------------------------------------------*
      *              * Match per il campo 3                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rbl-des-al3        =    spaces
                     go to aco-slc-tst-800.
      *                  *---------------------------------------------*
      *                  * Preparazione 2. valore per il match         *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-rbl-des-al3        to   w-atz-1t9-vdn          .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Test sul risultato del match                *
      *                  *---------------------------------------------*
           if        w-atz-1t9-flg-mch    not  = spaces
                     go to aco-slc-tst-850.
       aco-slc-tst-400.
      *              *-------------------------------------------------*
      *              * Match per il campo 4                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rbl-des-al4        =    spaces
                     go to aco-slc-tst-800.
      *                  *---------------------------------------------*
      *                  * Preparazione 2. valore per il match         *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-rbl-des-al4        to   w-atz-1t9-vdn          .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Test sul risultato del match                *
      *                  *---------------------------------------------*
           if        w-atz-1t9-flg-mch    not  = spaces
                     go to aco-slc-tst-850.
       aco-slc-tst-500.
      *              *-------------------------------------------------*
      *              * Match per il campo 5                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rbl-des-al5        =    spaces
                     go to aco-slc-tst-800.
      *                  *---------------------------------------------*
      *                  * Preparazione 2. valore per il match         *
      *                  *---------------------------------------------*
           move      rf-dcp-des-pro       to   w-atz-1t9-vdn          .
           perform   nor-atz-1t9-000      thru nor-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-rbl-des-al5        to   w-atz-1t9-vdn          .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   mch-atz-1t9-000      thru mch-atz-1t9-999        .
      *                  *---------------------------------------------*
      *                  * Test sul risultato del match                *
      *                  *---------------------------------------------*
           if        w-atz-1t9-flg-mch    not  = spaces
                     go to aco-slc-tst-850.
       aco-slc-tst-800.
      *              *-------------------------------------------------*
      *              * Test sul flag                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-rbl-des-fm5        not  = spaces
                     move  01             to   w-rcr-tot-dcp-mch      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-slc-tst-900.
       aco-slc-tst-850.
      *              *-------------------------------------------------*
      *              * Uscita per esito negativo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-rbl-des-fm5          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-slc-tst-900.
       aco-slc-tst-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-slc-tst-999.
       aco-slc-tst-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina video, per box locale di selezio-  *
      *    * ne, contenente il record indirizzato da (w-buf-inx-buf)   *
      *    *-----------------------------------------------------------*
       vpg-000.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina trattata           *
      *              *-------------------------------------------------*
           move      w-buf-inx-buf        to   w-buf-pag-att          .
           add       9                    to   w-buf-pag-att          .
           divide    10                   into w-buf-pag-att          .
      *              *-------------------------------------------------*
      *              * Determinazione indice primo elemento della pa-  *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-att        to   w-buf-pag-ipe          .
           multiply  10                   by   w-buf-pag-ipe          .
           subtract  9                    from w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Determinazione indice ultimo elemento della pa- *
      *              * gina trattata                                   *
      *              *-------------------------------------------------*
           move      w-buf-pag-ipe        to   w-buf-pag-iue          .
           add       9                    to   w-buf-pag-iue          .
           if        w-buf-pag-iue        >    w-buf-ctr-rec
                     move  w-buf-ctr-rec  to   w-buf-pag-iue          .
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per numero linea a vi-  *
      *              * deo relativa al primo elemento da visualizzare  *
      *              *-------------------------------------------------*
           move      08                   to   w-buf-i01-lnv          .
       vpg-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione linea                           *
      *              *-------------------------------------------------*
       vpg-125.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-buf-cod-alf
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-150.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-buf-des-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-175.
      *                  *---------------------------------------------*
      *                  * Codice prodotto sinonimo                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal esplicativo                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      63                   to   v-pos                  .
           if        w-buf-syn-pro
                    (w-buf-pag-ipe)       =    spaces
                     move  spaces         to   v-alf
           else      move  "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-200.
      *                      *-----------------------------------------*
      *                      * Sinonimo                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      64                   to   v-pos                  .
           move      w-buf-syn-pro
                    (w-buf-pag-ipe)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vpg-300.
      *              *-------------------------------------------------*
      *              * Incremento indice primo elemento della pagina   *
      *              * trattata                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pag-ipe          .
      *              *-------------------------------------------------*
      *              * Incremento indice per numero linea a video re-  *
      *              * relativa al primo elemento da visualizzare      *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-i01-lnv          .
      *              *-------------------------------------------------*
      *              * Se non si e' oltre l'ultimo elemento da tratta- *
      *              * re nella pagina si ricicla a visualizzare       *
      *              *-------------------------------------------------*
           if        w-buf-pag-ipe        not  > w-buf-pag-iue
                     go to vpg-100.
      *              *-------------------------------------------------*
      *              * Se si e' a pagina 1 : si esce                   *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        =    1
                     go to vpg-600.
      *              *-------------------------------------------------*
      *              * Se non si e' all'ultima pagina 1 : si esce      *
      *              *-------------------------------------------------*
           if        w-buf-pag-att        <    w-buf-tot-pag
                     go to vpg-600.
       vpg-400.
      *              *-------------------------------------------------*
      *              * Eventuali linee residue all'interno del box di  *
      *              * selezione a Spaces                              *
      *              *-------------------------------------------------*
           if        w-buf-i01-lnv        >    17
                     go to vpg-600.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      72                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-buf-i01-lnv        to   v-lin                  .
           move      05                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
           go to     vpg-400.
       vpg-600.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero pagina su pagine totali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing pagina attuale                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-pag-att        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-att          .
      *                  *---------------------------------------------*
      *                  * Editing pagine totali                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-buf-tot-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-buf-lit-tot          .
      *                  *---------------------------------------------*
      *                  * Preparazione literal                        *
      *                  *---------------------------------------------*
           move      15                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Pagina"             to   w-all-str-cat (1)      .
           move      w-buf-lit-att        to   w-all-str-cat (2)      .
           move      "di"                 to   w-all-str-cat (3)      .
           move      w-buf-lit-tot        to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-buf-lit-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      19                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-lit-pag        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-i01-lnv          .
       vpg-999.
           exit.

      *    *===========================================================*
      *    * Editing del valore numerico, contenuto in w-edt-num-cod,  *
      *    * ritornando il valore editato in un formato di 14 carat-   *
      *    * teri in w-edt-num-edt                                     *
      *    *-----------------------------------------------------------*
       edn-000.
      *              *-------------------------------------------------*
      *              * Se valore zero : uscita con Spaces              *
      *              *-------------------------------------------------*
           if        w-edt-num-cod        =    zero
                     move  spaces         to   w-edt-num-edt
                     go to edn-999.
      *              *-------------------------------------------------*
      *              * Esecuzione editing                              *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-edt-num-cod        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Valore editato in area di ritorno               *
      *              *-------------------------------------------------*
           move      spaces               to   w-edt-num-edt          .
           string    "("        delimited by   size
                     v-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-edt-num-edt          .
       edn-999.
           exit.

      *    *===========================================================*
      *    * Test se blanks embedded in w-ble-str                      *
      *    *-----------------------------------------------------------*
       ble-000.
           move      spaces               to   w-ble-flg              .
           if        w-ble-str            =    spaces
                     go to ble-999.
           if        w-ble-chr (1)        =    spaces
                     move  "#"            to   w-ble-flg
                     go to ble-999.
           move      1                    to   w-ble-ctr              .
       ble-100.
           add       1                    to   w-ble-ctr              .
           if        w-ble-ctr            >    w-ble-max
                     go to ble-999.
           if        w-ble-chr
                    (w-ble-ctr)           not  = spaces
                     go to ble-100.
       ble-200.
           add       1                    to   w-ble-ctr              .
           if        w-ble-ctr            >    w-ble-max
                     go to ble-999.
           if        w-ble-chr
                    (w-ble-ctr)           =    spaces
                     go to ble-200.
           move      "#"                  to   w-ble-flg              .
       ble-999.
           exit.

      *    *===========================================================*
      *    * Suddivisione ed analisi codice impostato di 14 caratteri  *
      *    *-----------------------------------------------------------*
       sac-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           move      spaces               to   w-man-x14-wlc          .
           move      spaces               to   w-man-x14-vic          .
           move      spaces               to   w-man-x14-fsp          .
       sac-020.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice casa pro-  *
      *              * duttrice, singolo codice                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "P"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "-"
                     go to sac-030.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-030.
           move      "P"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-030.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice casa pro-  *
      *              * duttrice, piu' codici                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "P"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "+"
                     go to sac-040.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-040.
           move      "P"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-040.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice del forni- *
      *              * tore preferenziale, singolo codice              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "F"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "-"
                     go to sac-050.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-050.
           move      "F"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-050.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice del forni- *
      *              * tore preferenziale, piu' codici                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "F"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "+"
                     go to sac-060.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-060.
           move      "F"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-060.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice del clien- *
      *              * te, singolo codice                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "C"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "-"
                     go to sac-070.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-070.
           move      "C"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-070.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice del clien- *
      *              * te, piu' codici                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "C"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "+"
                     go to sac-080.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-080.
           move      "C"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-080.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice libero,    *
      *              * singolo codice                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "K"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "-"
                     go to sac-090.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-090.
           move      "K"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sac-999.
       sac-090.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per codice libero,    *
      *              * piu' codici                                     *
      *              *-------------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "K"  or
                     w-man-x14-vlr
                    (02 : 01)             not  = "+"
                     go to sac-100.
           if        w-man-x14-vlr
                    (03 : 12)             not  = spaces
                     go to sac-100.
           move      "K"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 01)             to   w-man-x14-wlc          .
           go to     sac-999.
       sac-100.
      *              *-------------------------------------------------*
      *              * Test se richiesta ricerca per descrizione       *
      *              *-------------------------------------------------*
           if        w-man-x14-vlr        not  = "-"
                     go to sac-200.
           move      "-"                  to   w-man-x14-flg          .
           go to     sac-999.
       sac-200.
      *              *-------------------------------------------------*
      *              * Test se codici che iniziano per ..              *
      *              *-------------------------------------------------*
           if        w-man-x14-vlr
                    (01 : 01)             not  = "-"
                     go to sac-300.
           if        w-man-x14-vlr
                    (02 : 13)             =    spaces
                     go to sac-300.
           move      "I"                  to   w-man-x14-flg          .
           move      w-man-x14-vlr
                    (02 : 13)             to   w-man-x14-vic          .
           go to     sac-999.
       sac-300.
      *              *-------------------------------------------------*
      *              * Test se sinonimo                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-tss    tallying w-man-x14-i01
                                      for all  w-man-x14-cha (1)      .
           if        w-man-x14-i01        =    zero
                     go to sac-350.
           if        w-man-x14-vlr
                    (02 : 13)             not  = spaces
                     go to sac-320.
           if        w-man-x14-fss        not  = spaces
                     go to sac-350.
            move     "#"                  to   w-man-x14-fsp          .
       sac-320.
            move     "S"                  to   w-man-x14-flg          .
            move     w-man-x14-vlr
                    (01 : 01)             to   w-man-x14-wlc          .
            if       w-man-x14-fsp        not  = spaces
                     go to sac-999.
            move     w-man-x14-vlr
                    (02 : 13)             to   w-man-x14-sin          .
            go to    sac-999.
       sac-350.
      *              *-------------------------------------------------*
      *              * Test se richiesta di codice numerico            *
      *              *-------------------------------------------------*
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-trc    tallying w-man-x14-i01
                                      for all  w-man-x14-cha (1)      .
           if        w-man-x14-i01        =    zero
                     go to sac-400.
           if        w-man-x14-vlr
                    (02 : 13)             not  = spaces
                     go to sac-400.
            move     "?"                  to   w-man-x14-flg          .
            move     w-man-x14-vlr
                    (01 : 01)             to   w-man-x14-wlc          .
            move     w-man-x14-vlr
                    (02 : 13)             to   w-man-x14-sin          .
            go to    sac-999.
       sac-400.
      *              *-------------------------------------------------*
      *              * Test se codice prodotto numerico                *
      *              *-------------------------------------------------*
           if        w-man-x14-cha (1)    not  = "("
                     go to sac-500.
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-vlr    tallying w-man-x14-i01
                                      for all  ")"                    .
           if        w-man-x14-i01        =    1
                     go to sac-410.
       sac-405.
           move      "#"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           go to     sac-999.
       sac-410.
           move      zero                 to   w-man-x14-i01          .
           inspect   w-man-x14-vlr    tallying w-man-x14-i01
                     for   characters   before initial ")"            .
           if        w-man-x14-i01        not  > 1
                     go to sac-405.
           move      zero                 to   w-man-x14-i02          .
           inspect   w-man-x14-vlr    tallying w-man-x14-i02
                                          for  all   " "              .
           add       w-man-x14-i01        to   w-man-x14-i02          .
           if        w-man-x14-i02        not  = 13
                     go to sac-405.
       sac-415.
           move      zero                 to   w-man-x14-n14          .
           move      1                    to   w-man-x14-i01          .
       sac-420.
           add       1                    to   w-man-x14-i01          .
           if        w-man-x14-cha
                    (w-man-x14-i01)       =    ")"
                     go to sac-425.
           if        w-man-x14-cha
                    (w-man-x14-i01)       <    "0" or
                     w-man-x14-cha
                    (w-man-x14-i01)       >    "9"
                     go to sac-405.
           multiply  10                   by   w-man-x14-n14          .
           add       w-man-x14-chn
                    (w-man-x14-i01)       to   w-man-x14-n14          .
           go to     sac-420.
       sac-425.
           if        w-man-x14-n14        =    zero
                     go to sac-405.
           if        w-man-x14-n14        >    9999999
                     go to sac-405.
           move      "N"                  to   w-man-x14-flg          .
           move      w-man-x14-n14        to   w-man-x14-num          .
           move      spaces               to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
           go to     sac-999.
       sac-500.
      *              *-------------------------------------------------*
      *              * Test se codice prodotto alfanumerico            *
      *              *-------------------------------------------------*
           if        w-man-x14-cha (1)    not  < "0" and
                     w-man-x14-cha (1)    not  > "9"
                     go to sac-525
           else if   w-man-x14-cha (1)    not  < "A" and
                     w-man-x14-cha (1)    not  > "Z"
                     go to sac-525
           else      go to sac-405.
       sac-525.
           move      "A"                  to   w-man-x14-flg          .
           move      zero                 to   w-man-x14-num          .
           move      w-man-x14-vlr        to   w-man-x14-alf          .
           move      spaces               to   w-man-x14-sin          .
       sac-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione di un campo alfabetico in un valore privo *
      *    * di spaces e di caratteri non compresi tra i limiti A..Z   *
      *    * oppure 0..9                                               *
      *    *-----------------------------------------------------------*
       nor-atz-1t9-000.
      *              *-------------------------------------------------*
      *              * Trasformazione in uppercase del valore di ori-  *
      *              * gine                                            *
      *              *-------------------------------------------------*
           move      w-atz-1t9-vdn        to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-atz-1t9-vdn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare valore di destina-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-atz-1t9-vno          .
      *              *-------------------------------------------------*
      *              * Indice su valore di origine a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-atz-1t9-inx-vdn      .
      *              *-------------------------------------------------*
      *              * Indice su valore di destinazione a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   w-atz-1t9-inx-vno      .
       nor-atz-1t9-100.
      *              *-------------------------------------------------*
      *              * Incremento indice su valore di origine          *
      *              *-------------------------------------------------*
           add       1                    to   w-atz-1t9-inx-vdn      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-atz-1t9-inx-vdn    >    40
                     go to nor-atz-1t9-999.
      *              *-------------------------------------------------*
      *              * Se il carattere di origine in esame e' compreso *
      *              * tra A..Z, lo si sposta nella destinazione e si  *
      *              * ricicla sul carattere di origine successivo     *
      *              *-------------------------------------------------*
           if        w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  < "A" and
                     w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  > "Z"
                     add   1              to   w-atz-1t9-inx-vno
                     move  w-atz-1t9-vdn-chr
                          (w-atz-1t9-inx-vdn)
                                          to   w-atz-1t9-vno-chr
                                              (w-atz-1t9-inx-vno)
                     go to nor-atz-1t9-100.
      *              *-------------------------------------------------*
      *              * Se il carattere di origine in esame e' compreso *
      *              * tra 0..9, lo si sposta nella destinazione e si  *
      *              * ricicla sul carattere di origine successivo     *
      *              *-------------------------------------------------*
           if        w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  < "0" and
                     w-atz-1t9-vdn-chr
                    (w-atz-1t9-inx-vdn)   not  > "9"
                     add   1              to   w-atz-1t9-inx-vno
                     move  w-atz-1t9-vdn-chr
                          (w-atz-1t9-inx-vdn)
                                          to   w-atz-1t9-vno-chr
                                              (w-atz-1t9-inx-vno)
                     go to nor-atz-1t9-100.
      *              *-------------------------------------------------*
      *              * In ogni altro caso si ignora il carattere di o- *
      *              * rigine e si ricicla sul carattere di origine    *
      *              * successivo                                      *
      *              *-------------------------------------------------*
           go to     nor-atz-1t9-100.
       nor-atz-1t9-999.
           exit.

      *    *===========================================================*
      *    * Match tra due valori normalizzati A..Z - 0..9             *
      *    *-----------------------------------------------------------*
       mch-atz-1t9-000.
           move      zero                 to   w-atz-1t9-inx-vno      .
           inspect   w-atz-1t9-vdn    tallying w-atz-1t9-inx-vno
                                  for trailing spaces                 .
           move      40                   to   w-atz-1t9-inx-vdn      .
           subtract  w-atz-1t9-inx-vno    from w-atz-1t9-inx-vdn      .
           move      zero                 to   w-atz-1t9-ctr-mch      .
           inspect   w-atz-1t9-vno    tallying w-atz-1t9-ctr-mch
                                       for all w-atz-1t9-vdn
                                              (1 : w-atz-1t9-inx-vdn) .
           if        w-atz-1t9-ctr-mch    =    zero
                     move  "N"            to   w-atz-1t9-flg-mch
           else      move  spaces         to   w-atz-1t9-flg-mch      .
       mch-atz-1t9-999.
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
      *              *-------------------------------------------------*
      *              * Preparazione status di uscita                   *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
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
                     go to rlt-opn-400. 
      *                  *---------------------------------------------*
      *                  * Attesa di 1 secondo                         *
      *                  *---------------------------------------------*
           move      "W1"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * A open                                      *
      *                  *---------------------------------------------*
           go to     rlt-opn-100.
       rlt-opn-400.
      *              *-------------------------------------------------*
      *              * Test su status di uscita                        *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to rlt-opn-999.
       rlt-opn-800.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per Fatal Error          *
      *              *-------------------------------------------------*
           move      "FE"                 to   s-ope                  .
           move      f-rlt-nam            to   s-nam                  .
           move      f-rlt-pat            to   s-pat                  .
           move      f-rlt-sts            to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

