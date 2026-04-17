       Identification Division.
       Program-Id.                                 page3005           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    con                 *
      *                                   Fase:    age300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/05/94    *
      *                       Ultima revisione:    NdK del 13/12/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Generazione automatica dei conteggi provvi- *
      *                    gionali a fronte delle fatture emesse.      *
      *                                                                *
      *                    Modulo per la lettura dei documenti fattu-  *
      *                    ra.                                         *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "OP".                *
      *                                                                *
      *                       l-age-300-tip-exe : Tipo di esecuzione   *
      *                                           programma.           *
      *                                                                *
      *                       l-age-300-age-min : Codici agente da se- *
      *                       l-age-300-age-max   lezionare.           *
      *                                           Se a zero significa  *
      *                                           tutti gli agenti.    *
      *                                                                *
      *                       l-age-300-dat-min : Data minima documen- *
      *                                           ti fattura da sele-  *
      *                                           zionare.             *
      *                                           Se a zero significa  *
      *                                           da inizio file.      *
      *                                                                *
      *                       l-age-300-dat-max : Data massima docu-   *
      *                                           mentiti fattura da   *
      *                                           selezionare.         *
      *                                           Se a zero significa  *
      *                                           fino a fine file.    *
      *                                                                *
      *                       l-age-300-cod-zfi : Tipo documento fat-  *
      *                                           tura da selezionare. *
      *                                           Se a spaces signi-   *
      *                                           fica tutti i tipi    *
      *                                           documento.           *
      *                                                                *
      *                       l-age-300-tip-vpa : Tipo vendita per     *
      *                                           l'agente da selezio- *
      *                                           nare.                *
      *                                           Se a zero, significa *
      *                                           tutti i tipi di      *
      *                                           vendita.             *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita in  *
      *                                           ogni caso a spaces.  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "CL".                *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita in  *
      *                                           ogni caso a spaces.  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GT"  Lettura sequenziale archivio [fit] o [fir]               *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "GT".                *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita     *
      *                                            -  spaces : Ok      *
      *                                            -  #      : Eof     *
      *                                                                *
      *                       l-age-300-tip-rec : Tipo record          *
      *                                            -  "fit "   oppure  *
      *                                            -  "fir "           *
      *                                                                *
      *                       l-age-300-rot-age : Codice agente letto  *
      *                                                                *
      *                       l-age-300-rot-prt : Protocollo fattura   *
      *                                           letto                *
      *                                                                *
      *                       l-age-300-rot-prg : Progressivo nel pro- *
      *                                           tocollo fattura      *
      *                                           letto                *
      *                                                                *
      *                       l-age-300-rec-fit : Dati record [fit]    *
      *                                                                *
      *                       l-age-300-rec-fir : Dati record [fir]    *
      *                                                                *
      *                       l-age-300-aux-rec : Dati ausiliari al    *
      *                                           record               *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GN"  Lettura sequenziale archivio [fit] non selezionato       *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "GN".                *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita     *
      *                                            -  spaces : Ok      *
      *                                            -  #      : Eof     *
      *                                                                *
      *                       l-age-300-tip-rec : Tipo record, ovvero  *
      *                                           motivo che ha causa- *
      *                                           to la non-selezione  *
      *                                                                *
      *                                           A001 : Codice agen-  *
      *                                                  te in fattu-  *
      *                                                  ra a zero     *
      *                                           D001 : Codice docu-  *
      *                                                  mento in fat- *
      *                                                  tura a spaces *
      *                                           F001 : Flag signifi- *
      *                                                  cativita' per *
      *                                                  provvigioni   *
      *                                                  in testata    *
      *                                                  fattura a No  *
      *                                                                *
      *                       l-age-300-rec-fit : Dati record [fit]    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "UP"  Aggiornamento documento fattura con flag di elaborazio-  *
      *       ne per agenti                                            *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "UP".                *
      *                                                                *
      *                       l-age-300-prt-fit : Numero protocollo    *
      *                                           del documento fat-   *
      *                                           tura da aggiornare.  *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita in  *
      *                                           ogni caso a spaces.  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "NC"  Normalizzazione conteggio provvigionale [gpc]            *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "PC".                *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita in  *
      *                                           ogni caso a spaces.  *
      *                                                                *
      *                       l-age-300-rec-gpc : Dati record [gpc],   *
      *                                           normalizzato.        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "PC"  Scrittura conteggio provvigionale [gpc]                  *
      *                                                                *
      *              Input  : l-age-300-tip-ope : "PC".                *
      *                                                                *
      *                       l-age-300-rec-gpc : Dati record [gpc],   *
      *                                           riempito con tutti   *
      *                                           i dati relativi al   *
      *                                           conteggio, ad esclu- *
      *                                           sione di:            *
      *                                            - rf-gpc-ide-dat    *
      *                                            - rf-gpc-ide-ute    *
      *                                            - rf-gpc-ide-fas    *
      *                                            - rf-gpc-num-ctg    *
      *                                                                *
      *                                                                *
      *              Output : l-age-300-sts-exi : Status di uscita in  *
      *                                           ogni caso a spaces.  *
      *                                                                *
      *                       l-age-300-rec-gpc : Dati record [gpc],   *
      *                                           completato con i da- *
      *                                           ti non passati.      *
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

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

       Special-Names.       Decimal-Point is comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      *    *===========================================================*
      *    * File Control [seq]                                        *
      *    *-----------------------------------------------------------*
           select            seq   assign to input-output   f-seq-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-seq-sts .

      *    *===========================================================*
      *    * File Control [sq2]                                        *
      *    *-----------------------------------------------------------*
           select            sq2   assign to input-output   f-sq2-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-sq2-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Subchiave 1 : Per agente                          *
      *            *---------------------------------------------------*
               10  srt-k01.
      *                *-----------------------------------------------*
      *                * Flag di codice diverso da zero ma non trovato *
      *                * in archivio agenti                            *
      *                *                                               *
      *                *   - 1 : Esistente                             *
      *                *   - 5 : Non esistente                         *
      *                *                                               *
      *                * Solo per tipo ordinamento agente :            *
      *                *                                               *
      *                * - Per nominativo                              *
      *                * - Per mnemonico                               *
      *                *-----------------------------------------------*
                   15  srt-k01-flg-eon    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Nominativo per l'agente                       *
      *                *                                               *
      *                * Solo per tipo ordinamento agente :            *
      *                *                                               *
      *                * - Per nominativo                              *
      *                *-----------------------------------------------*
                   15  srt-k01-ron-age    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Mnemonico per l'agente                        *
      *                *                                               *
      *                * Solo per tipo ordinamento agente :            *
      *                *                                               *
      *                * - Per mnemonico                               *
      *                *-----------------------------------------------*
                   15  srt-k01-mne-age    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Codice agente                                 *
      *                *                                               *
      *                * Per tutti i tipi ordinamento agente           *
      *                *-----------------------------------------------*
                   15  srt-k01-cod-age    pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Subchiave 2 : Per data e numero documento         *
      *            *---------------------------------------------------*
               10  srt-k02.
      *                *-----------------------------------------------*
      *                * Data documento                                *
      *                *-----------------------------------------------*
                   15  srt-k02-dat-doc    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero documento                              *
      *                *-----------------------------------------------*
                   15  srt-k02-num-doc    pic  9(06)                  .
      *                *-----------------------------------------------*
      *                * Numero protocollo movimento di fatture clien- *
      *                * ti.                                           *
      *                *-----------------------------------------------*
                   15  srt-k02-prt-fcl    pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Per ospitare un intero 'rf-fit'                   *
      *            *---------------------------------------------------*
               10  srt-dat-rec-fit.
                   15  filler  occurs 2048
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per ospitare un intera area ausiliaria a 'rf-fit' *
      *            *---------------------------------------------------*
               10  srt-dat-aux-fit.
      *                *-----------------------------------------------*
      *                * Codice del subagente                          *
      *                *-----------------------------------------------*
                   15  srt-dat-aux-sub    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice del super-agente                       *
      *                *-----------------------------------------------*
                   15  srt-dat-aux-sup    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Terna di % di provvigione destinate al super- *
      *                * agente                                        *
      *                *-----------------------------------------------*
                   15  srt-dat-aux-pvg
                               occurs 03  pic  9(02)v9(01)            .
      *                *-----------------------------------------------*
      *                * Percentuale di provvigione unica destinata al *
      *                * super-agente                                  *
      *                *-----------------------------------------------*
                   15  srt-dat-aux-pun    pic  9(02)v9(01)            .

      *    *===========================================================*
      *    * File Description [seq]                                    *
      *    *-----------------------------------------------------------*
       fd  seq  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  seq-rec.
      *        *-------------------------------------------------------*
      *        * Tipo record                                           *
      *        *  - fit  : Record testata fattura                      *
      *        *  - fir  : Record riga    fattura                      *
      *        *-------------------------------------------------------*
           05  seq-tip-rec                pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Dati record, a seconda del tipo record                *
      *        *-------------------------------------------------------*
           05  seq-dat-rec.
               10  seq-chr-rec
                             occurs 2048  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati ausiliari, a seconda del tipo record             *
      *        *-------------------------------------------------------*
           05  seq-aux-rec.
      *            *---------------------------------------------------*
      *            * Codice del subagente                              *
      *            *---------------------------------------------------*
               10  seq-aux-sub            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice del super-agente                           *
      *            *---------------------------------------------------*
               10  seq-aux-sup            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Terna di % di provvigione destinate al super-     *
      *            * agente                                            *
      *            *---------------------------------------------------*
               10  seq-aux-pvg occurs 03  pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * % di provvigione unica destinata al super-agente  *
      *            *---------------------------------------------------*
               10  seq-aux-pun            pic  9(02)v9(01)            .

      *    *===========================================================*
      *    * File Description [sq2]                                    *
      *    *-----------------------------------------------------------*
       fd  sq2  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  sq2-rec.
      *        *-------------------------------------------------------*
      *        * Tipo record, ovvero motivo della non-selezione        *
      *        *  - A001 : Codice agente in fattura a zero             *
      *        *  - D001 : Tipo documento in fattura a spaces          *
      *        *  - F001 : Flag di significativita' delle provvigioni  *
      *        *           in fattura a No                             *
      *        *-------------------------------------------------------*
           05  sq2-tip-rec.
               10  sq2-tip-rec-cls        pic  x(01)                  .
               10  sq2-tip-rec-cod        pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati record                                           *
      *        *-------------------------------------------------------*
           05  sq2-dat-rec.
               10  sq2-chr-rec
                             occurs 2048  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [seq]                *
      *    *-----------------------------------------------------------*
       01  f-seq.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-seq-nam                  pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-seq-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-seq-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [seq]                *
      *    *-----------------------------------------------------------*
       01  f-sq2.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-sq2-nam                  pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-sq2-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-sq2-sts                  pic  x(02) value "00"       .

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
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [fix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffix"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [ags]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfags"                          .
      *        *-------------------------------------------------------*
      *        * [gpc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpc"                          .
      *        *-------------------------------------------------------*
      *        * [rnnumgpc]                                            *
      *        *-------------------------------------------------------*
           copy      "pgm/age/num/rec/rnnumgpc"                       .

      *    *===========================================================*
      *    * Record logico ausiliario per [fit]                        *
      *    *-----------------------------------------------------------*
       01  rx-fit.
      *        *-------------------------------------------------------*
      *        * Codice del sub-agente                                 *
      *        *-------------------------------------------------------*
           05  rx-fit-cod-sub             pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice del super-agente                               *
      *        *-------------------------------------------------------*
           05  rx-fit-cod-sup             pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Terna di % di provvigione per il super-agente         *
      *        *-------------------------------------------------------*
           05  rx-fit-pdp-sup  occurs 03  pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * % di provvigione unica per il super-agente            *
      *        *-------------------------------------------------------*
           05  rx-fit-ppu-sup             pic  9(02)v9(01)            .

      *    *===========================================================*
      *    * Work per l'esecuzione della overlay                       *
      *    *-----------------------------------------------------------*
       01  w-aux-age-300.
      *        *-------------------------------------------------------*
      *        * Salvataggio parametri in input della open             *
      *        *-------------------------------------------------------*
           05  w-aux-pin.
      *            *---------------------------------------------------*
      *            * Tipo di esecuzione del programma                  *
      *            *---------------------------------------------------*
               10  w-aux-pin-tip-exe      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codici agente                                     *
      *            *---------------------------------------------------*
               10  w-aux-pin-age-min      pic  9(07)                  .
               10  w-aux-pin-age-max      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo ordinamento per agenti                       *
      *            *---------------------------------------------------*
               10  w-aux-pin-tor-age      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Nominativo o ragione sociale per agenti           *
      *            *---------------------------------------------------*
               10  w-aux-pin-ron-age      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Data minima documenti fattura                     *
      *            *---------------------------------------------------*
               10  w-aux-pin-dat-min      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data massima documenti fattura                    *
      *            *---------------------------------------------------*
               10  w-aux-pin-dat-max      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Tipo documento fattura                            *
      *            *---------------------------------------------------*
               10  w-aux-pin-cod-zfi      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Tipo vendita                                      *
      *            *---------------------------------------------------*
               10  w-aux-pin-tip-vpa      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Opzioni per stampa anomalie per records non sele- *
      *            * zionati                                           *
      *            *---------------------------------------------------*
               10  w-aux-pin-ops-ans      pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio parametri per update                      *
      *        *-------------------------------------------------------*
           05  w-aux-upd.
      *            *---------------------------------------------------*
      *            * Numero protocollo record [fit]                    *
      *            *---------------------------------------------------*
               10  w-aux-upd-prt-fit      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Salvataggio record [fit]                          *
      *            *---------------------------------------------------*
               10  w-aux-upd-sav-fit.
                   15  filler occurs 2048
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per composizione record di sort                  *
      *        *-------------------------------------------------------*
           05  w-aux-srt.
      *            *---------------------------------------------------*
      *            * Numero ciclo per fattura                          *
      *            *  - Ciclo 01 : Per l'agente vero e proprio         *
      *            *  - Ciclo 02 : Per l'eventuale super-agente        *
      *            *---------------------------------------------------*
               10  w-aux-srt-num-cic      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per comodi locali                                *
      *        *-------------------------------------------------------*
           05  w-aux-war.
      *            *---------------------------------------------------*
      *            * Contatore generico 00..99                         *
      *            *---------------------------------------------------*
               10  w-aux-war-ctr-001      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di attribuzione numero conteggio per *
      *    * la gestione provvigioni agenti                            *
      *    *-----------------------------------------------------------*
       01  w-num-gpc.
           05  w-num-gpc-num-gpc          pic  9(09)                  .
           05  w-num-gpc-val-pre          pic  9(09)                  .
           05  w-num-gpc-val-pos          pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica agenti            *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)     value spaces .
               10  w-let-arc-age-cod      pic  9(07)     value zero   .
               10  w-let-arc-age-nom      pic  x(20)     value spaces .
               10  w-let-arc-age-rag      pic  x(40)     value spaces .
               10  w-let-arc-age-via      pic  x(40)     value spaces .
               10  w-let-arc-age-loc      pic  x(40)     value spaces .
               10  w-let-arc-age-mne      pic  x(10)     value spaces .
               10  w-let-arc-age-exc      pic  9(07)     value zero   .

      *    *===========================================================*
      *    * Work area per determinazioni varie                        *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione % unica da max 3 %                 *
      *        *-------------------------------------------------------*
           05  w-det-per-uni.
      *            *---------------------------------------------------*
      *            * Le tre %                                          *
      *            *---------------------------------------------------*
               10  w-det-per-uni-001      pic   9(02)v9(01)           .
               10  w-det-per-uni-002      pic   9(02)v9(01)           .
               10  w-det-per-uni-003      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * La percentuale unica                              *
      *            *---------------------------------------------------*
               10  w-det-per-uni-per      pic   9(02)v9(01)           .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-per-uni-w01      pic   9(07)v9(01)           .
               10  w-det-per-uni-w02      pic   9(07)v9(01)           .
               10  w-det-per-uni-w03      pic   9(07)v9(01)           .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area comune per programmi della serie page3000       *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/page3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-age-300              .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        l-age-300-tip-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   l-age-300-tip-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   l-age-300-tip-ope    =    "GT"
                     perform   get-000    thru get-999
           else if   l-age-300-tip-ope    =    "GN"
                     perform   gno-000    thru gno-999
           else if   l-age-300-tip-ope    =    "UP"
                     perform   upd-000    thru upd-999
           else if   l-age-300-tip-ope    =    "NC"
                     perform   nco-000    thru nco-999
           else if   l-age-300-tip-ope    =    "PC"
                     perform   pco-000    thru pco-999                .
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
      *              * Status di uscita a spaces                       *
      *              *-------------------------------------------------*
           move      spaces               to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Salvataggio parametri passati nella Open        *
      *              *-------------------------------------------------*
           perform   sav-prm-inp-000      thru sav-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Open files per l'esecuzione del programma       *
      *              *-------------------------------------------------*
           perform   opn-fls-exe-000      thru opn-fls-exe-999        .
      *              *-------------------------------------------------*
      *              * Open output file sequenziale di appoggio        *
      *              *-------------------------------------------------*
           perform   opn-out-seq-000      thru opn-out-seq-999        .
      *              *-------------------------------------------------*
      *              * Open output file sequenziale-2 di appoggio      *
      *              *-------------------------------------------------*
           perform   opn-out-sq2-000      thru opn-out-sq2-999        .
      *              *-------------------------------------------------*
      *              * Generazione files sequenziali di appoggio       *
      *              *-------------------------------------------------*
           perform   gen-seq-app-000      thru gen-seq-app-999        .
      *              *-------------------------------------------------*
      *              * Close file sequenziale di appoggio              *
      *              *-------------------------------------------------*
           perform   cls-fil-seq-000      thru cls-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Close file sequenziale-2 di appoggio            *
      *              *-------------------------------------------------*
           perform   cls-fil-sq2-000      thru cls-fil-sq2-999        .
      *              *-------------------------------------------------*
      *              * Open input file sequenziale di appoggio         *
      *              *-------------------------------------------------*
           perform   opn-inp-seq-000      thru opn-inp-seq-999        .
      *              *-------------------------------------------------*
      *              * Open input file sequenziale-2 di appoggio       *
      *              *-------------------------------------------------*
           perform   opn-inp-sq2-000      thru opn-inp-sq2-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione ciclo di lettura               *
      *              *-------------------------------------------------*
           perform   ini-ccl-get-000      thru ini-ccl-get-999        .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a spaces                       *
      *              *-------------------------------------------------*
           move      spaces               to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Close files per l'esecuzione del programma      *
      *              *-------------------------------------------------*
           perform   cls-fls-exe-000      thru cls-fls-exe-999        .
      *              *-------------------------------------------------*
      *              * Close file sequenziale di appoggio              *
      *              *-------------------------------------------------*
           perform   cls-fil-seq-000      thru cls-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Close file sequenziale-2 di appoggio            *
      *              *-------------------------------------------------*
           perform   cls-fil-sq2-000      thru cls-fil-sq2-999        .
      *              *-------------------------------------------------*
      *              * Delete file sequenziale di appoggio             *
      *              *-------------------------------------------------*
           perform   del-fil-seq-000      thru del-fil-seq-999        .
      *              *-------------------------------------------------*
      *              * Delete file sequenziale-2 di appoggio           *
      *              *-------------------------------------------------*
           perform   del-fil-sq2-000      thru del-fil-sq2-999        .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale file di appoggio sequenziale          *
      *    *-----------------------------------------------------------*
       get-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a spaces                       *
      *              *-------------------------------------------------*
           move      spaces               to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Lettura record dal file sequenziale di appoggio *
      *              * con eventuale set del flag di fine file         *
      *              *-------------------------------------------------*
           read      seq    at end
                            move  "#"     to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Trattamento per fine file o per il tipo record  *
      *              * letto dal file sequenziale di appoggio          *
      *              *-------------------------------------------------*
           if        l-age-300-sts-exi    =    "#"
                     perform   trt-seq-eof-000
                                          thru trt-seq-eof-999
           else if   seq-tip-rec          =    "fit "
                     perform   trt-seq-fit-000
                                          thru trt-seq-fit-999
           else      perform   trt-seq-fir-000
                                          thru trt-seq-fir-999        .
       get-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale file di appoggio sequenziale-2, rela- *
      *    * tivo ai records fattura non selezionati                   *
      *    *-----------------------------------------------------------*
       gno-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a spaces                       *
      *              *-------------------------------------------------*
           move      spaces               to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Lettura record dal file sequenziale di appoggio *
      *              * con eventuale set del flag di fine file         *
      *              *-------------------------------------------------*
           read      sq2    at end
                            move  "#"     to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Trattamento per fine file o per il tipo record  *
      *              * letto dal file sequenziale di appoggio          *
      *              *-------------------------------------------------*
           if        l-age-300-sts-exi    =    "#"
                     perform   trt-sq2-eof-000
                                          thru trt-sq2-eof-999
           else      perform   trt-sq2-fit-000
                                          thru trt-sq2-fit-999        .
       gno-999.
           exit.

      *    *===========================================================*
      *    * Update record [fit] con flag elaborazione agenti          *
      *    *-----------------------------------------------------------*
       upd-000.
      *              *-------------------------------------------------*
      *              * Status di uscita a spaces                       *
      *              *-------------------------------------------------*
           move      spaces               to   l-age-300-sts-exi      .
      *              *-------------------------------------------------*
      *              * Salvataggio numero protocollo [fit] del docu-   *
      *              * mento da aggiornare                             *
      *              *-------------------------------------------------*
           move      l-age-300-prt-fit    to   w-aux-upd-prt-fit      .
      *              *-------------------------------------------------*
      *              * Salvataggio record [fit] attuale                *
      *              *-------------------------------------------------*
           move      rf-fit               to   w-aux-upd-sav-fit      .
      *              *-------------------------------------------------*
      *              * Se numero protocollo a zero : no aggiornamento  *
      *              *-------------------------------------------------*
           if        w-aux-upd-prt-fit    =    zero
                     go to upd-800.
       upd-100.
      *              *-------------------------------------------------*
      *              * Lettura record [fit], con lock                  *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT"             to   f-key                  .
           move      w-aux-upd-prt-fit    to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Se errore : ad unlock                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to upd-500.
       upd-200.
      *              *-------------------------------------------------*
      *              * Se il flag di elaborazione agenti e' gia' in On *
      *              * si omette l'update                              *
      *              *-------------------------------------------------*
           if        rf-fit-flg-blx (2)   not  = spaces
                     go to upd-500.
       upd-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento flag elaborazione agenti          *
      *              *-------------------------------------------------*
           move      "#"                  to   rf-fit-flg-blx (2)     .
       upd-400.
      *              *-------------------------------------------------*
      *              * Update record [fit]                             *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Se errore : riciclo a ritentare                 *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to upd-100.
       upd-500.
      *              *-------------------------------------------------*
      *              * Unlock record [fit]                             *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       upd-800.
      *              *-------------------------------------------------*
      *              * Ripristino record [fit]                         *
      *              *-------------------------------------------------*
           move      w-aux-upd-sav-fit    to   rf-fit                 .
       upd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     upd-999.
       upd-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione conteggio provvigionale [gpc]             *
      *    *-----------------------------------------------------------*
       nco-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [gpc]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
       nco-100.
      *              *-------------------------------------------------*
      *              * Record [gpc] in area di uscita                  *
      *              *-------------------------------------------------*
           move      rf-gpc               to   l-age-300-rec-gpc      .
       nco-999.
           exit.

      *    *===========================================================*
      *    * Scrittura conteggio provvigionale [gpc]                   *
      *    *-----------------------------------------------------------*
       pco-000.
      *              *-------------------------------------------------*
      *              * Record in ingresso in area [gpc]                *
      *              *-------------------------------------------------*
           move      l-age-300-rec-gpc    to   rf-gpc                 .
       pco-100.
      *              *-------------------------------------------------*
      *              * Completamento dei campi                         *
      *              *  - rf-gpc-ide-dat                               *
      *              *  - rf-gpc-ide-ute                               *
      *              *  - rf-gpc-ide-fas                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta a segreteria                      *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * In campi di destinazione                    *
      *                  *---------------------------------------------*
           move      s-dat                to   rf-gpc-ide-dat         .
           move      s-ute                to   rf-gpc-ide-ute         .
           move      s-fas                to   rf-gpc-ide-fas         .
       pco-200.
      *              *-------------------------------------------------*
      *              * Completamento del campo                         *
      *              *  - rf-gpc-num-gpc                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attribuzione nuovo numero di conteggio      *
      *                  *---------------------------------------------*
           perform   att-num-gpc-000      thru att-num-gpc-999        .
      *                  *---------------------------------------------*
      *                  * In campo di destinazione                    *
      *                  *---------------------------------------------*
           move      w-num-gpc-num-gpc    to   rf-gpc-num-ctg         .
       pco-300.
      *              *-------------------------------------------------*
      *              * Scrittura record [gpc]                          *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
       pco-400.
      *              *-------------------------------------------------*
      *              * Record [gpc] in area di uscita                  *
      *              *-------------------------------------------------*
           move      rf-gpc               to   l-age-300-rec-gpc      .
       pco-999.
           exit.

      *    *===========================================================*
      *    * Salvataggio dei parametri in input                        *
      *    *-----------------------------------------------------------*
       sav-prm-inp-000.
      *              *-------------------------------------------------*
      *              * Tipo di esecuzione                              *
      *              *-------------------------------------------------*
           move      l-age-300-tip-exe    to   w-aux-pin-tip-exe      .
      *              *-------------------------------------------------*
      *              * Codici agente                                   *
      *              *-------------------------------------------------*
           move      l-age-300-age-min    to   w-aux-pin-age-min      .
           move      l-age-300-age-max    to   w-aux-pin-age-max      .
      *              *-------------------------------------------------*
      *              * Tipo ordinamento per agenti                     *
      *              *-------------------------------------------------*
           move      l-age-300-tor-age    to   w-aux-pin-tor-age      .
      *              *-------------------------------------------------*
      *              * Nominativo o ragione sociale per agenti         *
      *              *-------------------------------------------------*
           move      l-age-300-ron-age    to   w-aux-pin-ron-age      .
      *              *-------------------------------------------------*
      *              * Data minima documenti fattura                   *
      *              *-------------------------------------------------*
           move      l-age-300-dat-min    to   w-aux-pin-dat-min      .
      *              *-------------------------------------------------*
      *              * Data massima documenti fattura                  *
      *              *-------------------------------------------------*
           move      l-age-300-dat-max    to   w-aux-pin-dat-max      .
      *              *-------------------------------------------------*
      *              * Tipo documento fattura                          *
      *              *-------------------------------------------------*
           move      l-age-300-cod-zfi    to   w-aux-pin-cod-zfi      .
      *              *-------------------------------------------------*
      *              * Tipo vendita                                    *
      *              *-------------------------------------------------*
           move      l-age-300-tip-vpa    to   w-aux-pin-tip-vpa      .
      *              *-------------------------------------------------*
      *              * Opzioni per stampa anomalie per records non se- *
      *              * lezionati                                       *
      *              *-------------------------------------------------*
           move      l-age-300-ops-ans    to   w-aux-pin-ops-ans      .
       sav-prm-inp-999.
           exit.

      *    *===========================================================*
      *    * Open files per l'esecuzione del programma                 *
      *    *-----------------------------------------------------------*
       opn-fls-exe-000.
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * [ags]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *              *-------------------------------------------------*
      *              * [gpc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
      *              *-------------------------------------------------*
      *              * [numgpc]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
       opn-fls-exe-999.
           exit.

      *    *===========================================================*
      *    * Close files per l'esecuzione del programma                *
      *    *-----------------------------------------------------------*
       cls-fls-exe-000.
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * [ags]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *              *-------------------------------------------------*
      *              * [gpc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
      *              *-------------------------------------------------*
      *              * [numgpc]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
       cls-fls-exe-999.
           exit.

      *    *===========================================================*
      *    * Open output file sequenziale di appoggio                  *
      *    *-----------------------------------------------------------*
       opn-out-seq-000.
      *              *-------------------------------------------------*
      *              * Richiesta alla segreteria di un pathname unico  *
      *              * per files temporanei                            *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione area ausiliaria per controllo i-o  *
      *              * su [seq]                                        *
      *              *-------------------------------------------------*
           move      "seq "               to   f-seq-nam              .
           move      s-pat                to   f-seq-pat              .
           move      "00"                 to   f-seq-sts              .
      *              *-------------------------------------------------*
      *              * Open output                                     *
      *              *-------------------------------------------------*
           open      output seq                                       .
       opn-out-seq-999.
           exit.

      *    *===========================================================*
      *    * Open input file sequenziale di appoggio                   *
      *    *-----------------------------------------------------------*
       opn-inp-seq-000.
      *              *-------------------------------------------------*
      *              * Open input                                      *
      *              *-------------------------------------------------*
           open      input seq                                        .
       opn-inp-seq-999.
           exit.

      *    *===========================================================*
      *    * Close file sequenziale di appoggio                        *
      *    *-----------------------------------------------------------*
       cls-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     seq                                              .
       cls-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Delete file sequenziale di appoggio                       *
      *    *-----------------------------------------------------------*
       del-fil-seq-000.
      *              *-------------------------------------------------*
      *              * Se il file name o il file pathname e' a spaces  *
      *              * non si esegue il delete                         *
      *              *-------------------------------------------------*
           if        f-seq-nam            =    spaces or
                     f-seq-pat            =    spaces
                     go to del-fil-seq-900.
       del-fil-seq-100.
      *              *-------------------------------------------------*
      *              * Richiesta di delete alla segreteria             *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-seq-pat            to   s-pat                  .
           move      "S"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-fil-seq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-fil-seq-999.
       del-fil-seq-999.
           exit.

      *    *===========================================================*
      *    * Open output file sequenziale-2 di appoggio                *
      *    *-----------------------------------------------------------*
       opn-out-sq2-000.
      *              *-------------------------------------------------*
      *              * Richiesta alla segreteria di un pathname unico  *
      *              * per files temporanei                            *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Preparazione area ausiliaria per controllo i-o  *
      *              * su [sq2]                                        *
      *              *-------------------------------------------------*
           move      "sq2 "               to   f-sq2-nam              .
           move      s-pat                to   f-sq2-pat              .
           move      "00"                 to   f-sq2-sts              .
      *              *-------------------------------------------------*
      *              * Open output                                     *
      *              *-------------------------------------------------*
           open      output sq2                                       .
       opn-out-sq2-999.
           exit.

      *    *===========================================================*
      *    * Open input file sequenziale-2 di appoggio                 *
      *    *-----------------------------------------------------------*
       opn-inp-sq2-000.
      *              *-------------------------------------------------*
      *              * Open input                                      *
      *              *-------------------------------------------------*
           open      input sq2                                        .
       opn-inp-sq2-999.
           exit.

      *    *===========================================================*
      *    * Close file sequenziale-2 di appoggio                      *
      *    *-----------------------------------------------------------*
       cls-fil-sq2-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     sq2                                              .
       cls-fil-sq2-999.
           exit.

      *    *===========================================================*
      *    * Delete file sequenziale-2 di appoggio                     *
      *    *-----------------------------------------------------------*
       del-fil-sq2-000.
      *              *-------------------------------------------------*
      *              * Se il file name o il file pathname e' a spaces  *
      *              * non si esegue il delete                         *
      *              *-------------------------------------------------*
           if        f-sq2-nam            =    spaces or
                     f-sq2-pat            =    spaces
                     go to del-fil-sq2-900.
       del-fil-sq2-100.
      *              *-------------------------------------------------*
      *              * Richiesta di delete alla segreteria             *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-sq2-pat            to   s-pat                  .
           move      "S"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-fil-sq2-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-fil-sq2-999.
       del-fil-sq2-999.
           exit.

      *    *===========================================================*
      *    * Generazione file sequenziale di appoggio                  *
      *    *-----------------------------------------------------------*
       gen-seq-app-000.
      *              *-------------------------------------------------*
      *              * Esecuzione sort                                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   gen-seq-app-inp-000
                                          thru gen-seq-app-inp-999
                     output procedure     is   gen-seq-app-out-000
                                          thru gen-seq-app-out-999    .
       gen-seq-app-999.
           exit.

      *    *===========================================================*
      *    * Generazione file sequenziale di appoggio                  *
      *    *                                                           *
      *    * Input procedure                                           *
      *    *-----------------------------------------------------------*
       gen-seq-app-inp-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [fit] per data registrazione, *
      *              * in base alla data minima documento              *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-pin-dat-min    to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-num-doc         .
           move      spaces               to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : ad uscita                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to gen-seq-app-inp-900.
       gen-seq-app-inp-100.
      *              *-------------------------------------------------*
      *              * Next su [fit]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Se fine file : ad uscita                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to gen-seq-app-inp-900.
       gen-seq-app-inp-150.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare del record del file *
      *              * sequenziale-2 di appoggio, per i records [fit]  *
      *              * non selezionati                                 *
      *              *-------------------------------------------------*
           move      spaces               to   sq2-rec                .
       gen-seq-app-inp-200.
      *              *-------------------------------------------------*
      *              * Test max su [fit], se non superato : ad uscita  *
      *              *-------------------------------------------------*
       gen-seq-app-inp-210.
      *                  *---------------------------------------------*
      *                  * Su data documento massima                   *
      *                  *---------------------------------------------*
           if        w-aux-pin-dat-max    =    zero
                     go to gen-seq-app-inp-220.
           if        rf-fit-dat-doc       >    w-aux-pin-dat-max
                     go to gen-seq-app-inp-900.
       gen-seq-app-inp-220.
      *                  *---------------------------------------------*
      *                  * Fine test max                               *
      *                  *---------------------------------------------*
           go to     gen-seq-app-inp-300.
       gen-seq-app-inp-300.
      *              *-------------------------------------------------*
      *              * Selezione su [fit], se non superato:            *
      *              *  - Scrittura records su file sequenziale-2 di   *
      *              *    appoggio, solo a seconda del motivo della    *
      *              *    non-selezione                                *
      *              *  - Riciclo a lettura record [fit] successivo    *
      *              *-------------------------------------------------*
       gen-seq-app-inp-310.
      *                  *---------------------------------------------*
      *                  * Su flag di elaborazione agenti              *
      *                  *---------------------------------------------*
           if        rf-fit-flg-blx (2)   not  = spaces
                     go to gen-seq-app-inp-850.
       gen-seq-app-inp-315.
      *                  *---------------------------------------------*
      *                  * Su tipo documento                           *
      *                  *                                             *
      *                  * Esclusione delle Fatture Pro-Forma          *
      *                  *---------------------------------------------*
           if        rf-fit-tip-doc       =    04
                     go to gen-seq-app-inp-850.
       gen-seq-app-inp-320.
      *                  *---------------------------------------------*
      *                  * Su codice agente                            *
      *                  *---------------------------------------------*
           if        rf-fit-cod-age       =    zero
                     move  "A001"         to   sq2-tip-rec
                     go to gen-seq-app-inp-800.
           if        w-aux-pin-age-min    =    zero and
                     w-aux-pin-age-max    =    zero
                     go to gen-seq-app-inp-330.
           if        rf-fit-cod-age       <    w-aux-pin-age-min or
                     rf-fit-cod-age       >    w-aux-pin-age-max
                     go to gen-seq-app-inp-850.
       gen-seq-app-inp-330.
      *                  *---------------------------------------------*
      *                  * Su tipo documento                           *
      *                  *---------------------------------------------*
           if        rf-fit-cod-tmo       =    spaces
                     move  "D001"         to   sq2-tip-rec
                     go to gen-seq-app-inp-800.
           if        w-aux-pin-cod-zfi    =    spaces
                     go to gen-seq-app-inp-340.
           if        rf-fit-cod-tmo       not  = w-aux-pin-cod-zfi
                     go to gen-seq-app-inp-850.
       gen-seq-app-inp-340.
      *                  *---------------------------------------------*
      *                  * Su tipo di vendita per l'agente             *
      *                  *---------------------------------------------*
           if        w-aux-pin-tip-vpa    =    zero
                     go to gen-seq-app-inp-350.
           if        rf-fit-tip-vpa       not  = w-aux-pin-tip-vpa
                     go to gen-seq-app-inp-850.
       gen-seq-app-inp-350.
      *                  *---------------------------------------------*
      *                  * Su flag di testata sulla significativita'   *
      *                  * delle provvigioni                           *
      *                  *---------------------------------------------*
           if        rf-fit-fsp-doc       =    02
                     move  "F001"         to   sq2-tip-rec
                     go to gen-seq-app-inp-800.
       gen-seq-app-inp-380.
      *                  *---------------------------------------------*
      *                  * Fine selezioni                              *
      *                  *---------------------------------------------*
           go to     gen-seq-app-inp-400.
       gen-seq-app-inp-400.
      *              *-------------------------------------------------*
      *              * Rilascio al sort, eseguito in due cicli, il 1.  *
      *              * ciclo per l'agente vero e proprio, il 2. ciclo  *
      *              * per il super-agente                             *
      *              *-------------------------------------------------*
       gen-seq-app-inp-410.
      *                  *---------------------------------------------*
      *                  * Determinazione dei dati relativi al record  *
      *                  * ausiliario a [fit], per il super-agente     *
      *                  *---------------------------------------------*
           perform   det-aux-fit-000      thru det-aux-fit-999        .
       gen-seq-app-inp-420.
      *                  *---------------------------------------------*
      *                  * Numero ciclo a zero                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-srt-num-cic      .
       gen-seq-app-inp-450.
      *                  *---------------------------------------------*
      *                  * Incremento numero ciclo                     *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-srt-num-cic      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del numero ciclo       *
      *                  *---------------------------------------------*
           if        w-aux-srt-num-cic    =    01
                     go to gen-seq-app-inp-460
           else if   w-aux-srt-num-cic    =    02
                     go to gen-seq-app-inp-470
           else      go to gen-seq-app-inp-480.
       gen-seq-app-inp-460.
      *                  *---------------------------------------------*
      *                  * Se ciclo 01                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     gen-seq-app-inp-500.
       gen-seq-app-inp-470.
      *                  *---------------------------------------------*
      *                  * Se ciclo 02                                 *
      *                  *---------------------------------------------*
       gen-seq-app-inp-472.
      *                      *-----------------------------------------*
      *                      * Se non c'era super-agente si incrementa *
      *                      * il numero di ciclo                      *
      *                      *-----------------------------------------*
           if        rx-fit-cod-sup       =    zero
                     go to gen-seq-app-inp-750.
      *                      *-----------------------------------------*
      *                      * Codice super-agente al posto del codice *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           move      rx-fit-cod-sup       to   rf-fit-cod-age         .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     gen-seq-app-inp-500.
       gen-seq-app-inp-480.
      *                  *---------------------------------------------*
      *                  * Se oltre il ciclo 02                        *
      *                  *---------------------------------------------*
       gen-seq-app-inp-482.
      *                      *-----------------------------------------*
      *                      * Riciclo al record [fit] successivo      *
      *                      *-----------------------------------------*
           go to     gen-seq-app-inp-850.
       gen-seq-app-inp-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica agente, o super-agente   *
      *                  * se nel secondo ciclo                        *
      *                  *---------------------------------------------*
           move      rf-fit-cod-age       to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
       gen-seq-app-inp-600.
      *                  *---------------------------------------------*
      *                  * Composizione record di sort                 *
      *                  *---------------------------------------------*
       gen-seq-app-inp-610.
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare record sort *
      *                      *-----------------------------------------*
           move      spaces               to   srt-rec                .
       gen-seq-app-inp-630.
      *                      *-----------------------------------------*
      *                      * Composizione dell'area del record di    *
      *                      * sort relativa alla subchiave 1 : per    *
      *                      * agente                                  *
      *                      *-----------------------------------------*
       gen-seq-app-inp-632.
      *                          *-------------------------------------*
      *                          * Flag di codice diverso da zero ma   *
      *                          * non trovato in archivio agenti      *
      *                          *                                     *
      *                          * Se ordinamento agenti per codice    *
      *                          * viene forzato a zero                *
      *                          *                                     *
      *                          * Se ordinamento agenti per nominati- *
      *                          * vo o mnemonico viene posto a secon- *
      *                          * da del valore del codice agente e   *
      *                          * dell'esistenza o meno dell'anagra-  *
      *                          * fica                                *
      *                          *-------------------------------------*
           if        l-age-300-tor-age    =    02
                     move  zero           to   srt-k01-flg-eon
           else if   w-let-arc-age-flg    =    spaces
                     move  1              to   srt-k01-flg-eon
           else      move  5              to   srt-k01-flg-eon        .
       gen-seq-app-inp-634.
      *                          *-------------------------------------*
      *                          * Nominativo per l'agente             *
      *                          *                                     *
      *                          * Se ordinamento agenti per codice o  *
      *                          * per mnemonico viene forzato a spa-  *
      *                          * ces                                 *
      *                          *                                     *
      *                          * Se ordinamento agenti per nominati- *
      *                          * vo viene posto al valore provenien- *
      *                          * te dall'anagrafica agente, e a se-  *
      *                          * conda della personalizzazione che   *
      *                          * indica se esporre il nome o la ra-  *
      *                          * gione sociale                       *
      *                          *-------------------------------------*
           if        l-age-300-tor-age    =    02 or
                     l-age-300-tor-age    =    03
                     move  spaces         to   srt-k01-ron-age
           else if   l-age-300-ron-age    =    "R"
                     move  w-let-arc-age-rag
                                          to   srt-k01-ron-age
           else      move  w-let-arc-age-nom
                                          to   srt-k01-ron-age        .
       gen-seq-app-inp-636.
      *                          *-------------------------------------*
      *                          * Mnemonico per l'agente              *
      *                          *                                     *
      *                          * Se ordinamento agenti per codice o  *
      *                          * per nominativo viene forzato a spa- *
      *                          * ces                                 *
      *                          *                                     *
      *                          * Se ordinamento agenti per mnemoni-  *
      *                          * co, viene posto al valore prove-    *
      *                          * niente dall'anagrafica agente       *
      *                          *-------------------------------------*
           if        l-age-300-tor-age    =    01 or
                     l-age-300-tor-age    =    02
                     move  spaces         to   srt-k01-mne-age
           else      move  w-let-arc-age-mne
                                          to   srt-k01-mne-age        .
       gen-seq-app-inp-638.
      *                          *-------------------------------------*
      *                          * Codice agente                       *
      *                          *-------------------------------------*
           move      rf-fit-cod-age       to   srt-k01-cod-age        .
       gen-seq-app-inp-650.
      *                      *-----------------------------------------*
      *                      * Composizione dell'area del record di    *
      *                      * sort relativa alla subchiave 2 : per    *
      *                      * data e numero documento                 *
      *                      *-----------------------------------------*
       gen-seq-app-inp-652.
      *                          *-------------------------------------*
      *                          * Data documento                      *
      *                          *-------------------------------------*
           move      rf-fit-dat-doc       to   srt-k02-dat-doc        .
       gen-seq-app-inp-654.
      *                          *-------------------------------------*
      *                          * Numero documento                    *
      *                          *-------------------------------------*
           move      rf-fit-num-doc       to   srt-k02-num-doc        .
       gen-seq-app-inp-656.
      *                          *-------------------------------------*
      *                          * Numero protocollo fatture clienti   *
      *                          *-------------------------------------*
           move      rf-fit-num-prt       to   srt-k02-prt-fcl        .
       gen-seq-app-inp-670.
      *                      *-----------------------------------------*
      *                      * Composizione dell'area dati             *
      *                      *-----------------------------------------*
           move      rf-fit               to   srt-dat-rec-fit        .
      *                      *-----------------------------------------*
      *                      * Composizione dell'area ausiliaria       *
      *                      * - Codice del sub-agente                 *
      *                      * - Codice del super-agente               *
      *                      * - Terna di % di provvigione destinate   *
      *                      *   al super-agente                       *
      *                      * - Percentuale di provvigione unica de-  *
      *                      *   stinata al super-agente               *
      *                      *-----------------------------------------*
           if        w-aux-srt-num-cic    =    02
                     move  rx-fit-cod-sub to   srt-dat-aux-sub
                     move  rx-fit-cod-sup to   srt-dat-aux-sup
                     move  rx-fit-pdp-sup (1)
                                          to   srt-dat-aux-pvg (1)
                     move  rx-fit-pdp-sup (2)
                                          to   srt-dat-aux-pvg (2)
                     move  rx-fit-pdp-sup (3)
                                          to   srt-dat-aux-pvg (3)
                     move  rx-fit-ppu-sup to   srt-dat-aux-pun
           else      move  zero           to   srt-dat-aux-sub
                     move  zero           to   srt-dat-aux-sup
                     move  zero           to   srt-dat-aux-pvg (1)
                     move  zero           to   srt-dat-aux-pvg (2)
                     move  zero           to   srt-dat-aux-pvg (3)
                     move  zero           to   srt-dat-aux-pun        .
       gen-seq-app-inp-700.
      *                  *---------------------------------------------*
      *                  * Rilascio del record al sort                 *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
       gen-seq-app-inp-750.
      *                  *---------------------------------------------*
      *                  * A ciclo successivo                          *
      *                  *---------------------------------------------*
           go to     gen-seq-app-inp-450.
       gen-seq-app-inp-800.
      *              *-------------------------------------------------*
      *              * Scrittura del record [fit] non selezionato sul  *
      *              * file sequenziale-2 di appoggio                  *
      *              *-------------------------------------------------*
       gen-seq-app-inp-805.
      *                  *---------------------------------------------*
      *                  * Se non si e' in simulazione non si scrive   *
      *                  * mai il record non-selezionato               *
      *                  *---------------------------------------------*
           if        w-aux-pin-tip-exe    not  = 01
                     go to gen-seq-app-inp-820.
       gen-seq-app-inp-810.
      *                  *---------------------------------------------*
      *                  * Se il tipo di anomalia per non-selezione    *
      *                  * non e' contemplato tra quelli da stampare   *
      *                  * non si scrive il record non-selezionato     *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-war-ctr-001      .
           inspect   w-aux-pin-ops-ans
                                      tallying w-aux-war-ctr-001
                                      for all  sq2-tip-rec-cls        .
           if        w-aux-war-ctr-001    not  > zero
                     go to gen-seq-app-inp-820.
       gen-seq-app-inp-815.
      *                  *---------------------------------------------*
      *                  * Completamento con record [fit]              *
      *                  *---------------------------------------------*
           move      rf-fit               to   sq2-dat-rec            .
      *                  *---------------------------------------------*
      *                  * Scrittura record effettiva                  *
      *                  *---------------------------------------------*
           write     sq2-rec                                          .
       gen-seq-app-inp-820.
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     gen-seq-app-inp-850.
       gen-seq-app-inp-850.
      *              *-------------------------------------------------*
      *              * Riciclo al record [fit] successivo              *
      *              *-------------------------------------------------*
           go to     gen-seq-app-inp-100.
       gen-seq-app-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gen-seq-app-inp-999.
       gen-seq-app-inp-999.
           exit.

      *    *===========================================================*
      *    * Generazione file sequenziale di appoggio                  *
      *    *                                                           *
      *    * Output procedure                                          *
      *    *-----------------------------------------------------------*
       gen-seq-app-out-000.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt], se  *
      *              * fine file : ad uscita                           *
      *              *-------------------------------------------------*
           return    srt    at end
                            go to gen-seq-app-out-900.
       gen-seq-app-out-100.
      *              *-------------------------------------------------*
      *              * Record [fit] contenuto nell' area dati del sort *
      *              * in area di ridefinizione                        *
      *              *-------------------------------------------------*
           move      srt-dat-rec-fit      to   rf-fit                 .
      *              *-------------------------------------------------*
      *              * Record dati ausiliari a [fit] in area di ride-  *
      *              * finizione                                       *
      *              *-------------------------------------------------*
           move      srt-dat-aux-sub      to   rx-fit-cod-sub         .
           move      srt-dat-aux-sup      to   rx-fit-cod-sup         .
           move      srt-dat-aux-pvg (1)  to   rx-fit-pdp-sup (1)     .
           move      srt-dat-aux-pvg (2)  to   rx-fit-pdp-sup (2)     .
           move      srt-dat-aux-pvg (3)  to   rx-fit-pdp-sup (3)     .
           move      srt-dat-aux-pun      to   rx-fit-ppu-sup         .
       gen-seq-app-out-200.
      *              *-------------------------------------------------*
      *              * Scrittura record [fit] su sequenziale di appog- *
      *              * gio                                             *
      *              *-------------------------------------------------*
       gen-seq-app-out-210.
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo record                             *
      *                      *-----------------------------------------*
           move      "fit "               to   seq-tip-rec            .
      *                      *-----------------------------------------*
      *                      * Dati record                             *
      *                      *-----------------------------------------*
           move      rf-fit               to   seq-dat-rec            .
      *                      *-----------------------------------------*
      *                      * Dati ausiliari al record                *
      *                      *-----------------------------------------*
           move      rx-fit-cod-sub       to   seq-aux-sub            .
           move      rx-fit-cod-sup       to   seq-aux-sup            .
           move      rx-fit-pdp-sup (1)   to   seq-aux-pvg (1)        .
           move      rx-fit-pdp-sup (2)   to   seq-aux-pvg (2)        .
           move      rx-fit-pdp-sup (3)   to   seq-aux-pvg (3)        .
           move      rx-fit-ppu-sup       to   seq-aux-pun            .
       gen-seq-app-out-220.
      *                  *---------------------------------------------*
      *                  * Scrittura record effettiva                  *
      *                  *---------------------------------------------*
           write     seq-rec                                          .
       gen-seq-app-out-250.
      *              *-------------------------------------------------*
      *              * Start su archivio [fir] per righe relative alla *
      *              * testata                                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-fit-num-prt       to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a lettura testata successiva  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to gen-seq-app-out-550.
       gen-seq-app-out-300.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Read Next su [fir]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Se fine file : a lettura testata successiva     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to gen-seq-app-out-550.
       gen-seq-app-out-350.
      *              *-------------------------------------------------*
      *              * Test max su [fir], se non superato : a lettura  *
      *              * testata successiva                              *
      *              *-------------------------------------------------*
       gen-seq-app-out-360.
      *                  *---------------------------------------------*
      *                  * Su numero protocollo                        *
      *                  *---------------------------------------------*
           if        rf-fir-num-prt       not  = rf-fit-num-prt
                     go to gen-seq-app-out-550.
       gen-seq-app-out-370.
      *                  *---------------------------------------------*
      *                  * Fine test max                               *
      *                  *---------------------------------------------*
           go to     gen-seq-app-out-400.
       gen-seq-app-out-400.
      *              *-------------------------------------------------*
      *              * Selezione su [fir], se non superato : riciclo   *
      *              * a lettura record successivo                     *
      *              *-------------------------------------------------*
       gen-seq-app-out-410.
      *                  *---------------------------------------------*
      *                  * Nessuna selezione                           *
      *                  *---------------------------------------------*
           go to     gen-seq-app-out-450.
       gen-seq-app-out-450.
      *              *-------------------------------------------------*
      *              * Scrittura record [fir] su sequenziale di appog- *
      *              * gio                                             *
      *              *-------------------------------------------------*
       gen-seq-app-out-460.
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo record                             *
      *                      *-----------------------------------------*
           move      "fir "               to   seq-tip-rec            .
      *                      *-----------------------------------------*
      *                      * Dati record                             *
      *                      *-----------------------------------------*
           move      rf-fir               to   seq-dat-rec            .
      *                      *-----------------------------------------*
      *                      * Dati ausiliari al record                *
      *                      *-----------------------------------------*
           move      zero                 to   seq-aux-sub            .
           move      zero                 to   seq-aux-sup            .
           move      zero                 to   seq-aux-pvg (1)        .
           move      zero                 to   seq-aux-pvg (2)        .
           move      zero                 to   seq-aux-pvg (3)        .
           move      zero                 to   seq-aux-pun            .
       gen-seq-app-out-470.
      *                  *---------------------------------------------*
      *                  * Scrittura record effettiva                  *
      *                  *---------------------------------------------*
           write     seq-rec                                          .
       gen-seq-app-out-500.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura record [fir] successivo       *
      *              *-------------------------------------------------*
           go to     gen-seq-app-out-300.
       gen-seq-app-out-550.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura record [fit] successivo       *
      *              *-------------------------------------------------*
           go to     gen-seq-app-out-000.
       gen-seq-app-out-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gen-seq-app-out-999.
       gen-seq-app-out-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la determinazione dei dati relativi al re- *
      *    * cord ausiliario a [fit]                                   *
      *    *-----------------------------------------------------------*
       det-aux-fit-000.
      *              *-------------------------------------------------*
      *              * Dati relativi al super-agente                   *
      *              *-------------------------------------------------*
       det-aux-fit-025.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice sub-agente                       *
      *                      *-----------------------------------------*
           move      rf-fit-cod-age       to   rx-fit-cod-sub         .
      *                      *-----------------------------------------*
      *                      * Codice super-agente                     *
      *                      *-----------------------------------------*
           move      zero                 to   rx-fit-cod-sup         .
      *                      *-----------------------------------------*
      *                      * Terna di % di provvigione per il super- *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           move      zero                 to   rx-fit-pdp-sup (1)     .
           move      zero                 to   rx-fit-pdp-sup (2)     .
           move      zero                 to   rx-fit-pdp-sup (3)     .
      *                      *-----------------------------------------*
      *                      * % di provvigione unica per il super-    *
      *                      * agente                                  *
      *                      *-----------------------------------------*
           move      zero                 to   rx-fit-ppu-sup         .
       det-aux-fit-050.
      *                  *---------------------------------------------*
      *                  * Determinazione del super-agente relativo    *
      *                  * all'agente con riferimento alla data del-   *
      *                  * la fattura                                  *
      *                  *---------------------------------------------*
       det-aux-fit-075.
      *                      *-----------------------------------------*
      *                      * Start su archivio legami per data tra   *
      *                      * agenti e super-agenti                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "AGESUP    "         to   f-key                  .
           move      rf-fit-cod-age       to   rf-ags-cod-age         .
           move      zero                 to   rf-ags-num-prg         .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *                      *-----------------------------------------*
      *                      * Se start errata : a fine determinazione *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-aux-fit-225.
       det-aux-fit-100.
      *                      *-----------------------------------------*
      *                      * Read Next su archivio legami per data   *
      *                      * tra agenti e super-agenti               *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofags"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ags                 .
      *                      *-----------------------------------------*
      *                      * Se fine file : a fine determinazione    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-aux-fit-225.
       det-aux-fit-125.
      *                      *-----------------------------------------*
      *                      * Test Max su archivio legami per data    *
      *                      * tra agenti e super-agenti               *
      *                      *-----------------------------------------*
           if        rf-ags-cod-age       not  = rf-fit-cod-age
                     go to det-aux-fit-225.
       det-aux-fit-150.
      *                      *-----------------------------------------*
      *                      * Selezione su archivio legami per data   *
      *                      * tra agenti e super-agenti               *
      *                      *-----------------------------------------*
           if        rf-ags-dat-val       >    rf-fit-dat-doc
                     go to det-aux-fit-100.
       det-aux-fit-175.
      *                      *-----------------------------------------*
      *                      * Memorizzazione codice super-agente      *
      *                      *-----------------------------------------*
           move      rf-ags-sup-age       to   rx-fit-cod-sup         .
      *                      *-----------------------------------------*
      *                      * Memorizzazione terna di % di provvi-    *
      *                      * gioni associata                         *
      *                      *-----------------------------------------*
           move      rf-ags-per-pvg (1)   to   rx-fit-pdp-sup (1)     .
           move      rf-ags-per-pvg (2)   to   rx-fit-pdp-sup (2)     .
           move      rf-ags-per-pvg (3)   to   rx-fit-pdp-sup (3)     .
       det-aux-fit-200.
      *                      *-----------------------------------------*
      *                      * Riciclo su elemento successivo          *
      *                      *-----------------------------------------*
           go to     det-aux-fit-100.
       det-aux-fit-225.
      *                      *-----------------------------------------*
      *                      * Determinazione della % di provvigione   *
      *                      * unica per il super-agente               *
      *                      *-----------------------------------------*
           move      rx-fit-pdp-sup (1)   to   w-det-per-uni-001      .
           move      rx-fit-pdp-sup (2)   to   w-det-per-uni-002      .
           move      rx-fit-pdp-sup (3)   to   w-det-per-uni-003      .
           perform   det-per-uni-000      thru det-per-uni-999        .
           move      w-det-per-uni-per    to   rx-fit-ppu-sup         .
       det-aux-fit-250.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-aux-fit-999.
       det-aux-fit-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione ciclo di lettura                         *
      *    *-----------------------------------------------------------*
       ini-ccl-get-000.
       ini-ccl-get-999.
           exit.

      *    *===========================================================*
      *    * Trattamento record letto da file sequenziale di appoggio  *
      *    * di tipo [fit]                                             *
      *    *-----------------------------------------------------------*
       trt-seq-fit-000.
      *              *-------------------------------------------------*
      *              * Tipo record in uscita                           *
      *              *-------------------------------------------------*
           move      "fit "               to   l-age-300-tip-rec      .
      *              *-------------------------------------------------*
      *              * Parametri per rottura in uscita                 *
      *              *  - Codice agente                                *
      *              *  - Numero protocollo fattura                    *
      *              *  - Numero riga nel protocollo fattura a zero    *
      *              *-------------------------------------------------*
           move      seq-dat-rec          to   rf-fit                 .
           move      rf-fit-cod-age       to   l-age-300-rot-age      .
           move      rf-fit-num-prt       to   l-age-300-rot-prt      .
           move      zero                 to   l-age-300-rot-prg      .
      *              *-------------------------------------------------*
      *              * Dati record in uscita                           *
      *              *-------------------------------------------------*
           move      seq-dat-rec          to   l-age-300-rec-fit      .
      *              *-------------------------------------------------*
      *              * Dati ausiliari al record in uscita              *
      *              *-------------------------------------------------*
           move      seq-aux-sub          to   l-age-300-aux-sub      .
           move      seq-aux-sup          to   l-age-300-aux-sup      .
           move      seq-aux-pvg (1)      to   l-age-300-aux-pvg (1)  .
           move      seq-aux-pvg (2)      to   l-age-300-aux-pvg (2)  .
           move      seq-aux-pvg (3)      to   l-age-300-aux-pvg (3)  .
           move      seq-aux-pun          to   l-age-300-aux-pun      .
       trt-seq-fit-999.
           exit.

      *    *===========================================================*
      *    * Trattamento record letto da file sequenziale di appoggio  *
      *    * di tipo [fir]                                             *
      *    *-----------------------------------------------------------*
       trt-seq-fir-000.
      *              *-------------------------------------------------*
      *              * Tipo record in uscita                           *
      *              *-------------------------------------------------*
           move      "fir "               to   l-age-300-tip-rec      .
      *              *-------------------------------------------------*
      *              * Parametri per rottura in uscita                 *
      *              *  - Codice agente                                *
      *              *  - Numero protocollo fattura                    *
      *              *  - Numero riga nel protocollo fattura           *
      *              *-------------------------------------------------*
           move      rf-fit-cod-age       to   l-age-300-rot-age      .
           move      rf-fit-num-prt       to   l-age-300-rot-prt      .
           move      seq-dat-rec          to   rf-fir                 .
           move      rf-fir-num-prg       to   l-age-300-rot-prg      .
      *              *-------------------------------------------------*
      *              * Dati record in uscita                           *
      *              *-------------------------------------------------*
           move      seq-dat-rec          to   l-age-300-rec-fir      .
      *              *-------------------------------------------------*
      *              * Dati ausiliari al record in uscita              *
      *              *-------------------------------------------------*
           move      zero                 to   l-age-300-aux-sub      .
           move      zero                 to   l-age-300-aux-sup      .
           move      zero                 to   l-age-300-aux-pvg (1)  .
           move      zero                 to   l-age-300-aux-pvg (2)  .
           move      zero                 to   l-age-300-aux-pvg (3)  .
           move      zero                 to   l-age-300-aux-pun      .
       trt-seq-fir-999.
           exit.

      *    *===========================================================*
      *    * Trattamento per fine file sequenziale di appoggio         *
      *    *-----------------------------------------------------------*
       trt-seq-eof-000.
       trt-seq-eof-999.
           exit.

      *    *===========================================================*
      *    * Trattamento record letto da file sequenziale-2 di appog-  *
      *    * gio, sempre relativo ad un record [fit]                   *
      *    *-----------------------------------------------------------*
       trt-sq2-fit-000.
      *              *-------------------------------------------------*
      *              * Tipo record in uscita                           *
      *              *-------------------------------------------------*
           move      sq2-tip-rec          to   l-age-300-tip-rec      .
      *              *-------------------------------------------------*
      *              * Dati record in uscita                           *
      *              *-------------------------------------------------*
           move      sq2-dat-rec          to   l-age-300-rec-fit      .
       trt-sq2-fit-999.
           exit.

      *    *===========================================================*
      *    * Trattamento per fine file sequenziale-2 di appoggio       *
      *    *-----------------------------------------------------------*
       trt-sq2-eof-000.
       trt-sq2-eof-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero conteggio per la gestione  *
      *    * provvigioni agenti                                        *
      *    *-----------------------------------------------------------*
       att-num-gpc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [numgpc]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-num-gpc-400.
       att-num-gpc-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *                  *---------------------------------------------*
      *                  * Unlock record                               *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-num-gpc-000.
       att-num-gpc-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore pre-incremento           *
      *                  *---------------------------------------------*
           move      rn-num-gpc-num-gpc   to   w-num-gpc-val-pre      .
      *                  *---------------------------------------------*
      *                  * Preparazione valore post-incremento         *
      *                  *---------------------------------------------*
           move      w-num-gpc-val-pre    to   w-num-gpc-val-pos      .
           add       1                    to   w-num-gpc-val-pos      .
           if        w-num-gpc-val-pos    =    zero
                     move  1              to   w-num-gpc-val-pos      .
      *                  *---------------------------------------------*
      *                  * Preparazione numero in uscita               *
      *                  *---------------------------------------------*
           move      w-num-gpc-val-pos    to   w-num-gpc-num-gpc      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero movimento              *
      *                  *---------------------------------------------*
           move      w-num-gpc-num-gpc    to   rn-num-gpc-num-gpc     .
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito dell'opera- *
      *                  * zione di update                             *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-num-gpc-600.
       att-num-gpc-500.
      *                  *---------------------------------------------*
      *                  * Se errore in operazione di update           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     att-num-gpc-000.
       att-num-gpc-600.
      *                  *---------------------------------------------*
      *                  * Se nessun errore in operazione di update    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpc             .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     att-num-gpc-999.
       att-num-gpc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio anagrafica agenti             *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    w-let-arc-age-exc
                     go to let-arc-age-900.
       let-arc-age-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [age] relativamente all'agente *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-age-200
           else      go to let-arc-age-300.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente esistente                  *
      *              *-------------------------------------------------*
       let-arc-age-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-age-via-age       to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-age-loc-age       to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      rf-age-cod-mne       to   w-let-arc-age-mne      .
       let-arc-age-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente non esistente              *
      *              *-------------------------------------------------*
       let-arc-age-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Se codice agente a zero                         *
      *              *-------------------------------------------------*
       let-arc-age-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente su valore precedente          *
      *                  *---------------------------------------------*
           move      w-let-arc-age-cod    to   w-let-arc-age-exc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Determinazione % unica da max 3 % distinte                *
      *    *                                                           *
      *    * Input  : w-det-per-uni-001     = 1.a percentuale          *
      *    *          w-det-per-uni-002     = 2.a percentuale          *
      *    *          w-det-per-uni-003     = 3.a percentuale          *
      *    *                                                           *
      *    * Output : w-det-per-uni-per     = Unica percentuale        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-per-uni-000.
           move      100                  to   w-det-per-uni-w01      .
           move      zero                 to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-001    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-002    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           multiply  w-det-per-uni-003    by   w-det-per-uni-w01
                                        giving w-det-per-uni-w03      .
           divide    100                  into w-det-per-uni-w03      .
           subtract  w-det-per-uni-w03    from w-det-per-uni-w01      .
           add       w-det-per-uni-w03    to   w-det-per-uni-w02      .
      *
           move      w-det-per-uni-w02    to   w-det-per-uni-per      .
       det-per-uni-999.
           exit.

