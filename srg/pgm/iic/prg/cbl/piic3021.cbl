       Identification Division.
       Program-Id.                                 piic3021           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    iic                 *
      *                                Settore:    mov                 *
      *                                   Fase:    iic302              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 19/01/93    *
      *                       Versione attuale:    NdK del 02/10/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione per il programma piic3020 :      *
      *                                                                *
      *                    Stampa brogliaccio movimenti Iva intraco-   *
      *                    munitaria.                                  *
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
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "iic"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mov"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "iic302"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "piic3021"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    BROGLIACCIO MOVIMENTI IVA INTRA     "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

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
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-rec-ric-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-rec-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No record richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No stampa                                      *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [iit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiit"                          .
      *        *-------------------------------------------------------*
      *        * [iir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiir"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .
      *        *-------------------------------------------------------*
      *        * [gxp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxp"                          .

      *    *===========================================================*
      *    * Tabella tipi operazione per gestione Iva intracomunitaria *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.tab"                   .

      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data movimenti minima da ricercare                    *
      *        *-------------------------------------------------------*
           05  rr-dtm-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data movimenti massima da ricercare                   *
      *        *-------------------------------------------------------*
           05  rr-dtm-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo operazione da ricercare                          *
      *        *-------------------------------------------------------*
           05  rr-top-iic                 pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Tipo operazione da ricercare, descrizione             *
      *        *-------------------------------------------------------*
           05  rr-top-iic-des             pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Codice ISO dello stato CEE da ricercare               *
      *        *-------------------------------------------------------*
           05  rr-iso-cee                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice ISO dello stato CEE da ricercare, descrizione  *
      *        *-------------------------------------------------------*
           05  rr-iso-cee-des             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice della nomenclatura combinata della merce       *
      *        *-------------------------------------------------------*
           05  rr-cdn-cdm                 pic  9(08)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxn]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxn.
               10  w-let-arc-gxn-flg      pic  x(01)                  .
               10  w-let-arc-gxn-cod      pic  x(03)                  .
               10  w-let-arc-gxn-des      pic  x(20)                  .
               10  w-let-arc-gxn-cee      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxp.
               10  w-let-arc-gxp-flg      pic  x(01)                  .
               10  w-let-arc-gxp-cod      pic  x(03)                  .
               10  w-let-arc-gxp-des      pic  x(25)                  .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per esecuzione stampa                           *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Sub-work per intestazione pagina                      *
      *        *-------------------------------------------------------*
           05  w-stp-int.
      *            *---------------------------------------------------*
      *            * Numero totale di intestazioni eseguite            *
      *            *---------------------------------------------------*
               10  w-stp-int-num-int      pic  9(05)       value zero .
      *            *---------------------------------------------------*
      *            * Titolo per lo stampato, allineato a sinistra      *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stp      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina da stampare                         *
      *            *---------------------------------------------------*
               10  w-stp-int-num-pag      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Data di stampa                                    *
      *            *---------------------------------------------------*
               10  w-stp-int-dat-stp      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale azienda allineata a sinistra      *
      *            *---------------------------------------------------*
               10  w-stp-int-rag-azi      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza della ragione sociale azienda, allinea- *
      *            * ta a sinistra, senza considerare gli spazi in co- *
      *            * da                                                *
      *            *---------------------------------------------------*
               10  w-stp-int-rag-azl      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza del titolo per lo stampato, allineato a *
      *            * sinistra, senza considerare gli spazi in coda     *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stl      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Colonna di stampa per il titolo dello stampato    *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stc      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Necessarieta' di una o due linee di stampa per il *
      *            * titolo dello stampato : 'U' o 'D'                 *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-uod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero caratteri di slittamento a destra per data *
      *            * e pagina                                          *
      *            *---------------------------------------------------*
               10  w-stp-int-ncs-dep      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-int-wct-c01      pic  9(03)                  .
               10  w-stp-int-wct-c02      pic  9(03)                  .
               10  w-stp-int-wct-c03      pic  9(03)                  .
               10  w-stp-int-wct-c04      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodi per editing                                *
      *            *---------------------------------------------------*
               10  w-stp-int-wed-e01      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per editing data di registrazione e numero   *
      *        * protocollo di riferimento per la contabilita'         *
      *        *-------------------------------------------------------*
           05  w-stp-dep.
      *            *---------------------------------------------------*
      *            * Area editata completa                             *
      *            *---------------------------------------------------*
               10  w-stp-dep-edt-000      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Area editata numero protocollo                    *
      *            *---------------------------------------------------*
               10  w-stp-dep-edt-001      pic  x(07)                  .
      *            *---------------------------------------------------*
      *            * Area editata data registrazione                   *
      *            *---------------------------------------------------*
               10  w-stp-dep-edt-002      pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa righe di [iir]                    *
      *        *-------------------------------------------------------*
           05  w-stp-iir.
      *            *---------------------------------------------------*
      *            * Contatore righe esaminate                         *
      *            *---------------------------------------------------*
               10  w-stp-iir-ctr-iir      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Flag di stampa numero riga                        *
      *            *---------------------------------------------------*
               10  w-stp-iir-flg-snr      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per stampa campi espansi                             *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Natura della transazione                   *
      *        *-------------------------------------------------------*
           05  w-exp-cdn-dtr.
               10  w-exp-cdn-dtr-inx      pic  9(02)                  .
               10  w-exp-cdn-dtr-num      pic  9(02)       value 10   .
               10  w-exp-cdn-dtr-lun      pic  9(02)       value 44   .
               10  w-exp-cdn-dtr-tbl.
                   15  filler             pic  x(44) value
                       "(Non definita)                              " .
                   15  filler             pic  x(44) value
                       "1. Acquisto o vendita                       " .
                   15  filler             pic  x(44) value
                       "2. Restituzione o sostituzione di merci     " .
                   15  filler             pic  x(44) value
                       "3. Aiuti governativi, privati, ..           " .
                   15  filler             pic  x(44) value
                       "4. Operazione in vista di una lavorazione .." .
                   15  filler             pic  x(44) value
                       "5. Operazione successiva ad una lavoraz. .. " .
                   15  filler             pic  x(44) value
                       "6. Movimento senza trasferimento di propr.  " .
                   15  filler             pic  x(44) value
                       "7. Operazione a titolo di un programma ..   " .
                   15  filler             pic  x(44) value
                       "8. Fornitura di materiali e macchinari ..   " .
                   15  filler             pic  x(44) value
                       "9. Altre transazioni                        " .
               10  w-exp-cdn-dtr-tbx redefines
                   w-exp-cdn-dtr-tbl.
                   15  w-exp-cdn-dtr-ele
                                occurs 09 pic  x(44)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Condizioni di consegna                     *
      *        *-------------------------------------------------------*
           05  w-exp-cod-cdc.
               10  w-exp-cod-cdc-inx      pic  9(02)                  .
               10  w-exp-cod-cdc-num      pic  9(02)       value 14   .
               10  w-exp-cod-cdc-lun      pic  9(02)       value 44   .
               10  w-exp-cod-cdc-tbl.
                   15  filler             pic  x(44) value
                       "(Non definita)                              " .
                   15  filler             pic  x(44) value
                       "E. Franco fabbrica                          " .
                   15  filler             pic  x(44) value
                       "F. Franco vettore                           " .
                   15  filler             pic  x(44) value
                       "F. Franco sotto bordo                       " .
                   15  filler             pic  x(44) value
                       "F. Franco a bordo                           " .
                   15  filler             pic  x(44) value
                       "C. Costo e nolo                             " .
                   15  filler             pic  x(44) value
                       "C. Costo, assicurazione, nolo               " .
                   15  filler             pic  x(44) value
                       "C. Nolo/porto pagato fino a ...             " .
                   15  filler             pic  x(44) value
                       "C. Nolo/porto e assic. pagati fino a ...    " .
                   15  filler             pic  x(44) value
                       "D. Reso frontiera                           " .
                   15  filler             pic  x(44) value
                       "D. Reso franco bordo nave a destino         " .
                   15  filler             pic  x(44) value
                       "D. Reso franco banchina                     " .
                   15  filler             pic  x(44) value
                       "D. Reso non sdoganato                       " .
                   15  filler             pic  x(44) value
                       "D. Reso sdoganato                           " .
               10  w-exp-cod-cdc-tbx redefines
                   w-exp-cod-cdc-tbl.
                   15  w-exp-cod-cdc-ele
                                occurs 13 pic  x(44)                  .
      *        *-------------------------------------------------------*
      *        * Work per : Codice del modo di trasporto               *
      *        *-------------------------------------------------------*
           05  w-exp-cod-mdt.
               10  w-exp-cod-mdt-inx      pic  9(02)                  .
               10  w-exp-cod-mdt-num      pic  9(02)       value  9   .
               10  w-exp-cod-mdt-lun      pic  9(02)       value 44   .
               10  w-exp-cod-mdt-tbl.
                   15  filler             pic  x(44) value
                       "(Non definito)                              " .
                   15  filler             pic  x(44) value
                       "1. Trasporto marittimo                      " .
                   15  filler             pic  x(44) value
                       "2. Trasporto ferroviario                    " .
                   15  filler             pic  x(44) value
                       "3. Trasporto stradale                       " .
                   15  filler             pic  x(44) value
                       "4. Trasporto aereo                          " .
                   15  filler             pic  x(44) value
                       "5. Spedizioni postali                       " .
                   15  filler             pic  x(44) value
                       "7. Installazioni fisse di trasporto         " .
                   15  filler             pic  x(44) value
                       "8. Trasporto per vie d'acqua                " .
                   15  filler             pic  x(44) value
                       "9. Propulsione propria                      " .
               10  w-exp-cod-mdt-tbx redefines
                   w-exp-cod-mdt-tbl.
                   15  w-exp-cod-mdt-ele
                                occurs 08 pic  x(44)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Lettura record richieste                        *
      *              *-------------------------------------------------*
           perform   let-rec-ric-000      thru let-rec-ric-999        .
           if        w-cnt-let-rec-ric    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Lettura parametri di selezione stampa           *
      *              *-------------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to main-900.
       main-300.
      *              *-------------------------------------------------*
      *              * Open files per routine di stampa                *
      *              *-------------------------------------------------*
           perform   prn-opn-fls-000      thru prn-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione eventuale sort preliminare           *
      *              *-------------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se sort eseguito oppure no *
      *              *-------------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to main-400
           else      go to main-500.
       main-400.
      *              *-------------------------------------------------*
      *              * Se sort non eseguito                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di report-program                     *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     main-600.
       main-500.
      *              *-------------------------------------------------*
      *              * Se sort eseguito                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     main-600.
       main-600.
      *              *-------------------------------------------------*
      *              * Close files per routine di stampa               *
      *              *-------------------------------------------------*
           perform   prn-cls-fls-000      thru prn-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo stampa                     *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "N"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm
           else      move  spaces         to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura record richieste                                  *
      *    *-----------------------------------------------------------*
       let-rec-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-rec-ric      .
      *              *-------------------------------------------------*
      *              * Test se programma senza richieste               *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to let-rec-ric-999.
      *              *-------------------------------------------------*
      *              * Richiesta tipo funzionamento a segreteria       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Inizio lettura record richieste                 *
      *              *-------------------------------------------------*
           move      "OI"                 to   b-ope                  .
           move      s-fun                to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric
                     go to let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Estrazione segmenti da 255  bytes da record ri- *
      *              * chieste                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stu-pnt-stu      .
       let-rec-ric-100.
           move      "GT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           if        b-rsc                not  = spaces
                     go to let-rec-ric-200.
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    b-chr
                     delimited by size    into rr
                                  with pointer w-cnt-stu-pnt-stu      .
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-rec-ric-100.
       let-rec-ric-200.
      *              *-------------------------------------------------*
      *              * Fine lettura record richieste                   *
      *              *-------------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric      .
       let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Cancel modulo trattamento richieste             *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
       let-rec-ric-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Se errori                           *
      *                          *-------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
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
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No record richieste                          *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No stampa                                    *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Open files                         *
      *    *-----------------------------------------------------------*
       prn-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [iir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
       prn-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : No                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-exe-rou-srt      .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       stp-srt-inp-000.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-str-ini-100.
      *              *-------------------------------------------------*
      *              * Start su [iit] per data                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rr-dtm-min           to   rf-iit-dat-reg         .
           move      zero                 to   rf-iit-num-prt         .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della start     *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prn-str-ini-300
           else      go to prn-str-ini-200.
       prn-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se start errata                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-300.
      *              *-------------------------------------------------*
      *              * Se start Ok                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun movimento Iva intracomunitaria entro i limi
      -              "ti assegnati ! "    to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-let-seq-100.
      *              *-------------------------------------------------*
      *              * Next su [iit] per data                          *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prn-let-seq-300
           else      go to prn-let-seq-200.
       prn-let-seq-200.
      *              *-------------------------------------------------*
      *              * Se fine file                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-300.
      *              *-------------------------------------------------*
      *              * Se lettura Ok                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-let-seq-999.
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-100.
      *              *-------------------------------------------------*
      *              * Se oltre data massima : superamento             *
      *              *-------------------------------------------------*
           if        rr-dtm-max           =    zero
                     go to prn-tst-max-200.
           if        rf-iit-dat-reg       >    rr-dtm-max
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-tst-max-999.
       prn-tst-max-200.
      *              *-------------------------------------------------*
      *              * Fine test max                                   *
      *              *-------------------------------------------------*
           go to     prn-tst-max-999.
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Selezione su tipo operazione                    *
      *              *-------------------------------------------------*
       prn-sel-rec-105.
      *                  *---------------------------------------------*
      *                  * Se il tipo operazione da ricercare e' pari  *
      *                  * a zero : nessuna selezione                  *
      *                  *---------------------------------------------*
           if        rr-top-iic           =    zero
                     go to prn-sel-rec-200.
       prn-sel-rec-110.
      *                  *---------------------------------------------*
      *                  * Test e selezione                            *
      *                  *---------------------------------------------*
           if        rf-iit-top-iic       not  = rr-top-iic
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Selezione su codice ISO dello stato CEE         *
      *              *-------------------------------------------------*
       prn-sel-rec-205.
      *                  *---------------------------------------------*
      *                  * Se il codice ISO dello stato CEE da ricer-  *
      *                  * care e' pari a spaces : nessuna selezione   *
      *                  *---------------------------------------------*
           if        rr-iso-cee           =    spaces
                     go to prn-sel-rec-300.
       prn-sel-rec-210.
      *                  *---------------------------------------------*
      *                  * Test e selezione                            *
      *                  *---------------------------------------------*
           if        rf-iit-iso-opc       not  = rr-iso-cee
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-300.
      *              *-------------------------------------------------*
      *              * Selezione su codice nomenclatura combinata      *
      *              *-------------------------------------------------*
       prn-sel-rec-305.
      *                  *---------------------------------------------*
      *                  * Se codice nomenclatura combinata a zero :   *
      *                  * nessuna selezione                           *
      *                  *---------------------------------------------*
           if        rr-cdn-cdm           =    zero
                     go to prn-sel-rec-400.
       prn-sel-rec-310.
      *                  *---------------------------------------------*
      *                  * Start su [iir]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-iit-dat-reg       to   rf-iir-dat-reg         .
           move      rf-iit-num-prt       to   rf-iir-num-prt         .
           move      zero                 to   rf-iir-num-prg         .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : selezione fallita         *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-315.
      *                  *---------------------------------------------*
      *                  * Read Next su [iir]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : selezione fallita            *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-320.
      *                  *---------------------------------------------*
      *                  * Test Max [iir], se non superato : selezione *
      *                  * fallita                                     *
      *                  *---------------------------------------------*
           if        rf-iir-dat-reg       not  = rf-iit-dat-reg or
                     rf-iir-num-prt       not  = rf-iit-num-prt
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-325.
      *                  *---------------------------------------------*
      *                  * Se la riga contiene il codice nomenclatura  *
      *                  * combinata cercato la selezione e' superata, *
      *                  * altrimenti si ricicla al record successivo  *
      *                  * della stessa registrazione                  *
      *                  *---------------------------------------------*
           if        rf-iir-cdn-cdm       =    rr-cdn-cdm or
                     rf-iir-cnc-pre       =    rr-cdn-cdm
                     go to prn-sel-rec-400
           else      go to prn-sel-rec-315.
       prn-sel-rec-400.
      *              *-------------------------------------------------*
      *              * Fine selezioni                                  *
      *              *-------------------------------------------------*
           go to     prn-sel-rec-999.
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Lineette di separazione finali                  *
      *              *-------------------------------------------------*
       prn-fin-cic-101.
      *                  *---------------------------------------------*
      *                  * Se linee residue non sufficienti : nessuna  *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        p-res                >    2
                     go to prn-fin-cic-102
           else      go to prn-fin-cic-200.
       prn-fin-cic-102.
      *                  *---------------------------------------------*
      *                  * Due interlinee                              *
      *                  *---------------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      027                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-200.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-cic-999.
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Testata movimento                               *
      *              *-------------------------------------------------*
       prn-liv-det-120.
      *                  *---------------------------------------------*
      *                  * Lineette di separazione                     *
      *                  *---------------------------------------------*
       prn-liv-det-121.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    5
                     go to prn-liv-det-122.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-122.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      027                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-140.
      *                  *---------------------------------------------*
      *                  * Tipo operazione, numero, data               *
      *                  *---------------------------------------------*
       prn-liv-det-141.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-142.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-142.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal tipo operazione                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      17                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "Tipo operazione :"  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Codice tipo operazione                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      019                  to   p-pos                  .
           move      rf-iit-top-iic       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione tipo operazione             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura codice tipo operazione      *
      *                          *-------------------------------------*
           move      "C"                  to   w-top-iic-tab-tle      .
           move      rf-iit-top-iic       to   w-top-iic-tab-top      .
           perform   top-iic-let-000      thru top-iic-let-999        .
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      50                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      025                  to   p-pos                  .
           move      w-top-iic-tab-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal numero operazione               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      092                  to   p-pos                  .
           move      "Nr. operazione :"   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero operazione                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      "<B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      109                  to   p-pos                  .
           move      rf-iit-num-prt       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal data operazione                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      119                  to   p-pos                  .
           move      "Del :"              to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data operazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      125                  to   p-pos                  .
           move      rf-iit-dat-reg       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-160.
      *                  *---------------------------------------------*
      *                  * Cliente o Fornitore                         *
      *                  *---------------------------------------------*
       prn-liv-det-161.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-180.
       prn-liv-det-162.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-163.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-163.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-164.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se Cliente o For-  *
      *                      * nitore                                  *
      *                      *-----------------------------------------*
           if        rf-iit-tip-arc       =    "C"
                     go to prn-liv-det-165
           else if   rf-iit-tip-arc       =    "F"
                     go to prn-liv-det-166
           else      go to prn-liv-det-167.
       prn-liv-det-165.
      *                      *-----------------------------------------*
      *                      * Se Cliente                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice cliente ...................................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Codice cliente                      *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Lettura anagrafica cliente          *
      *                          *-------------------------------------*
           move      rf-iit-cod-arc       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                          *-------------------------------------*
      *                          * Ragione sociale cliente             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-cli-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-180.
       prn-liv-det-166.
      *                      *-----------------------------------------*
      *                      * Se Fornitore                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice fornitore .................................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Codice fornitore                    *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Lettura anagrafica fornitore        *
      *                          *-------------------------------------*
           move      rf-iit-cod-arc       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                          *-------------------------------------*
      *                          * Ragione sociale fornitore           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-fnt-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-180.
       prn-liv-det-167.
      *                      *-----------------------------------------*
      *                      * Se tipo archivio non riconosciuto       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice operatore comunitario .....................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Codice                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      all   "."            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-180.
       prn-liv-det-180.
      *                  *---------------------------------------------*
      *                  * Cliente o Fornitore precedente              *
      *                  *---------------------------------------------*
       prn-liv-det-181.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-200.
      *                      *-----------------------------------------*
      *                      * Se codice archivio precedente a zero :  *
      *                      * no stampa                               *
      *                      *-----------------------------------------*
           if        rf-iit-arc-pre       =    zero
                     go to prn-liv-det-200.
      *                      *-----------------------------------------*
      *                      * Se codice archivio precedente pari a    *
      *                      * quello attuele : no stampa              *
      *                      *-----------------------------------------*
           if        rf-iit-arc-pre       =    rf-iit-cod-arc
                     go to prn-liv-det-200.
       prn-liv-det-182.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-183.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-183.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-184.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se Cliente o For-  *
      *                      * nitore                                  *
      *                      *-----------------------------------------*
           if        rf-iit-tip-arc       =    "C"
                     go to prn-liv-det-185
           else if   rf-iit-tip-arc       =    "F"
                     go to prn-liv-det-186
           else      go to prn-liv-det-187.
       prn-liv-det-185.
      *                      *-----------------------------------------*
      *                      * Se Cliente                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "                                                  
      -              "Ex :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Codice cliente                      *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-arc-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Lettura anagrafica cliente          *
      *                          *-------------------------------------*
           move      rf-iit-arc-pre       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                          *-------------------------------------*
      *                          * Ragione sociale cliente             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-cli-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-200.
       prn-liv-det-186.
      *                      *-----------------------------------------*
      *                      * Se Fornitore                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "                                                  
      -              "Ex :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Codice fornitore                    *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-arc-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Lettura anagrafica fornitore        *
      *                          *-------------------------------------*
           move      rf-iit-arc-pre       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
      *                          *-------------------------------------*
      *                          * Ragione sociale fornitore           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-fnt-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-200.
       prn-liv-det-187.
      *                      *-----------------------------------------*
      *                      * Se tipo archivio non riconosciuto       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "                                                  
      -              "Ex :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Codice                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-arc-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      all   "."            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-200.
       prn-liv-det-200.
      *                  *---------------------------------------------*
      *                  * Codice ISO dello stato CEE dell'operatore   *
      *                  * comunitario                                 *
      *                  *---------------------------------------------*
       prn-liv-det-201.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-220.
       prn-liv-det-202.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-203.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-203.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-204.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ar-   *
      *                          * chivio                              *
      *                          *-------------------------------------*
           if        rf-iit-tip-arc       =    "C"
                     go to prn-liv-det-205
           else if   rf-iit-tip-arc       =    "F"
                     go to prn-liv-det-206
           else      go to prn-liv-det-207.
       prn-liv-det-205.
      *                          *-------------------------------------*
      *                          * Se cliente                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal per cliente             *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice ISO dello stato CEE del cliente ...........
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-208.
       prn-liv-det-206.
      *                          *-------------------------------------*
      *                          * Se fornitore                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal per fornitore           *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice ISO dello stato CEE del fornitore .........
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-208.
       prn-liv-det-207.
      *                          *-------------------------------------*
      *                          * Se tipo archivio non riconosciuto   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal generico                *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice ISO dello stato CEE dell'operatore ........
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-208.
       prn-liv-det-208.
      *                      *-----------------------------------------*
      *                      * Codice ISO                              *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-iso-opc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-209.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica codice ISO           *
      *                      *-----------------------------------------*
           move      rf-iit-iso-opc       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione codice ISO                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-gxn-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-220.
      *                  *---------------------------------------------*
      *                  * Codice ISO dello stato CEE dell'operatore   *
      *                  * comunitario precedente                      *
      *                  *---------------------------------------------*
       prn-liv-det-221.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-240.
      *                      *-----------------------------------------*
      *                      * Se codice ISO precedente a spaces : no  *
      *                      * stampa                                  *
      *                      *-----------------------------------------*
           if        rf-iit-iso-ocp       =    spaces
                     go to prn-liv-det-240.
      *                      *-----------------------------------------*
      *                      * Se codice ISO precedente pari a quello  *
      *                      * attuale : no stampa                     *
      *                      *-----------------------------------------*
           if        rf-iit-iso-ocp       =    rf-iit-iso-opc
                     go to prn-liv-det-240.
       prn-liv-det-222.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-223.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-223.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-224.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "                                                  
      -              "Ex :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-225.
      *                      *-----------------------------------------*
      *                      * Codice ISO                              *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-iso-ocp       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-226.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica codice ISO           *
      *                      *-----------------------------------------*
           move      rf-iit-iso-ocp       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione codice ISO                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-gxn-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-240.
      *                  *---------------------------------------------*
      *                  * Codice Iva del Cliente e Fornitore          *
      *                  *---------------------------------------------*
       prn-liv-det-241.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-260.
       prn-liv-det-242.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-243.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-243.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-244.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se Cliente o For-  *
      *                      * nitore                                  *
      *                      *-----------------------------------------*
           if        rf-iit-tip-arc       =    "C"
                     go to prn-liv-det-245
           else if   rf-iit-tip-arc       =    "F"
                     go to prn-liv-det-246
           else      go to prn-liv-det-247.
       prn-liv-det-245.
      *                      *-----------------------------------------*
      *                      * Se Cliente                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice Iva del cliente ...........................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Partita Iva                         *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cdi-opi       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-260.
       prn-liv-det-246.
      *                      *-----------------------------------------*
      *                      * Se Fornitore                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice Iva del fornitore .........................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Partita Iva                         *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cdi-opi       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-260.
       prn-liv-det-247.
      *                      *-----------------------------------------*
      *                      * Se tipo archivio non riconosciuto       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Codice Iva dell'operatore comunitario ............
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Partita Iva                         *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cdi-opi       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     prn-liv-det-260.
       prn-liv-det-260.
      *                  *---------------------------------------------*
      *                  * Codice Iva del Cliente e Fornitore prece-   *
      *                  * dente                                       *
      *                  *---------------------------------------------*
       prn-liv-det-261.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-280.
      *                      *-----------------------------------------*
      *                      * Se codice Iva precedente a spaces : no  *
      *                      * stampa                                  *
      *                      *-----------------------------------------*
           if        rf-iit-cdi-ocp       =    spaces
                     go to prn-liv-det-280.
      *                      *-----------------------------------------*
      *                      * Se codice Iva precedente pari a quello  *
      *                      * attuale : no stampa                     *
      *                      *-----------------------------------------*
           if        rf-iit-cdi-ocp       =    rf-iit-cdi-opi
                     go to prn-liv-det-280.
       prn-liv-det-262.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-263.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-263.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-264.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "                                                  
      -              "Ex :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Partita Iva                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cdi-ocp       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-280.
      *                  *---------------------------------------------*
      *                  * Natura della transazione                    *
      *                  *---------------------------------------------*
       prn-liv-det-281.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-300.
       prn-liv-det-282.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-283.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-283.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-284.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Natura della transazione .........................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      44                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cdn-dtr       to   w-exp-cdn-dtr-inx      .
           if        w-exp-cdn-dtr-inx    =    zero
                     move  spaces         to   p-alf
           else if   w-exp-cdn-dtr-inx    >    w-exp-cdn-dtr-num
                     move  all "."        to   p-alf
           else      move  w-exp-cdn-dtr-ele
                          (w-exp-cdn-dtr-inx)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-300.
      *                  *---------------------------------------------*
      *                  * Natura della transazione precedente         *
      *                  *---------------------------------------------*
       prn-liv-det-301.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-320.
      *                      *-----------------------------------------*
      *                      * Se natura della transazione precedente  *
      *                      * a zero : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-cdn-dtp       =    zero
                     go to prn-liv-det-320.
      *                      *-----------------------------------------*
      *                      * Se natura della transazione precedente  *
      *                      * pari a quella attuale : no stampa       *
      *                      *-----------------------------------------*
           if        rf-iit-cdn-dtp       =    rf-iit-cdn-dtr
                     go to prn-liv-det-320.
       prn-liv-det-302.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-303.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-303.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-304.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "                                                  
      -              "Ex :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      44                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cdn-dtp       to   w-exp-cdn-dtr-inx      .
           if        w-exp-cdn-dtr-inx    =    zero
                     move  spaces         to   p-alf
           else if   w-exp-cdn-dtr-inx    >    w-exp-cdn-dtr-num
                     move  all "."        to   p-alf
           else      move  w-exp-cdn-dtr-ele
                          (w-exp-cdn-dtr-inx)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-320.
      *                  *---------------------------------------------*
      *                  * Condizioni di consegna                      *
      *                  *---------------------------------------------*
       prn-liv-det-321.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002
                     go to prn-liv-det-340.
       prn-liv-det-322.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-323.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-323.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-324.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Condizioni di consegna ...........................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      44                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cod-cdc       to   w-exp-cod-cdc-inx      .
      *
           if        w-exp-cod-cdc-inx    =    01
                     move  1              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    11
                     move  2              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    12
                     move  3              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    13
                     move  4              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    21
                     move  5              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    22
                     move  6              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    23
                     move  7              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    24
                     move  8              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    31
                     move  9              to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    32
                     move  10             to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    33
                     move  11             to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    34
                     move  12             to   w-exp-cod-cdc-inx
           else if   w-exp-cod-cdc-inx    =    35
                     move  13             to   w-exp-cod-cdc-inx
           else      move  zero           to   w-exp-cod-cdc-inx      .
      *
           if        w-exp-cod-cdc-inx    =    zero
                     move  spaces         to   p-alf
           else if   w-exp-cod-cdc-inx    >    w-exp-cod-cdc-num
                     move  all "."        to   p-alf
           else      move  w-exp-cod-cdc-ele
                          (w-exp-cod-cdc-inx)
                                          to   p-alf                  .
      *
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-340.
      *                  *---------------------------------------------*
      *                  * Modo di trasporto                           *
      *                  *---------------------------------------------*
       prn-liv-det-341.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002
                     go to prn-liv-det-360.
       prn-liv-det-342.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-343.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-343.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-344.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Modo di trasporto ................................
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      44                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-cod-mdt       to   w-exp-cod-mdt-inx      .
           if        w-exp-cod-mdt-inx    =    01
                     move  01             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    02
                     move  02             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    03
                     move  03             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    04
                     move  04             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    05
                     move  05             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    07
                     move  06             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    08
                     move  07             to   w-exp-cod-mdt-inx
           else if   w-exp-cod-mdt-inx    =    09
                     move  08             to   w-exp-cod-mdt-inx
           else      move  zero           to   w-exp-cod-mdt-inx      .
           if        w-exp-cod-mdt-inx    =    zero
                     move  spaces         to   p-alf
           else if   w-exp-cod-mdt-inx    >    w-exp-cod-mdt-num
                     move  all "."        to   p-alf
           else      move  w-exp-cod-mdt-ele
                          (w-exp-cod-mdt-inx)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-360.
      *                  *---------------------------------------------*
      *                  * Codice ISO dello Stato membro CEE di prove- *
      *                  * nienza o di destinazione delle merci        *
      *                  *---------------------------------------------*
       prn-liv-det-361.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002
                     go to prn-liv-det-380.
       prn-liv-det-362.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-363.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-363.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-364.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
       prn-liv-det-365.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ope-  *
      *                          * razione                             *
      *                          *-------------------------------------*
           if        rf-iit-top-iic       =    0001
                     go to prn-liv-det-366
           else      go to prn-liv-det-367.
       prn-liv-det-366.
      *                          *-------------------------------------*
      *                          * Se acquisto                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal                         *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Stato CEE di provenienza delle merci .............
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-368.
       prn-liv-det-367.
      *                          *-------------------------------------*
      *                          * Se cessione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal                         *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Stato CEE di destinazione delle merci ............
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-368.
       prn-liv-det-368.
      *                      *-----------------------------------------*
      *                      * Codice dello stato CEE                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-iso-pod       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-369.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica codice ISO           *
      *                      *-----------------------------------------*
           move      rf-iit-iso-pod       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione codice ISO                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-gxn-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-380.
      *                  *---------------------------------------------*
      *                  * Provincia italiana di origine o destina-    *
      *                  * zione delle merci                           *
      *                  *---------------------------------------------*
       prn-liv-det-381.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002
                     go to prn-liv-det-400.
       prn-liv-det-382.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-383.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-383.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-384.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
       prn-liv-det-385.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo ope-  *
      *                          * razione                             *
      *                          *-------------------------------------*
           if        rf-iit-top-iic       =    0001
                     go to prn-liv-det-386
           else      go to prn-liv-det-387.
       prn-liv-det-386.
      *                          *-------------------------------------*
      *                          * Se acquisto                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal                         *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Provincia italiana di destinazione delle merci ...
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-388.
       prn-liv-det-387.
      *                          *-------------------------------------*
      *                          * Se cessione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Literal                         *
      *                              *---------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Provincia italiana di origine delle merci ........
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     prn-liv-det-388.
       prn-liv-det-388.
      *                      *-----------------------------------------*
      *                      * Codice della provincia                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-pvc-ood       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-389.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica provincia            *
      *                      *-----------------------------------------*
           move      rf-iit-pvc-ood       to   w-let-arc-gxp-cod      .
           perform   let-arc-gxp-000      thru let-arc-gxp-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione provincia                   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      25                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-gxp-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Corrispettivo dell'operazione in valuta     *
      *                  * estera, senza decimali                      *
      *                  *---------------------------------------------*
       prn-liv-det-401.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0011
                     go to prn-liv-det-420.
       prn-liv-det-402.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-403.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-403.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-404.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Corrispettivo operazione in valuta estera ........
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Importo                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-tco-eiv       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-420.
      *                  *---------------------------------------------*
      *                  * Riferimenti alla registrazione contabile    *
      *                  *---------------------------------------------*
       prn-liv-det-421.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-440.
       prn-liv-det-422.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-423.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-423.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-424.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Movimento contabile di riferimento ...............
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-425.
      *                      *-----------------------------------------*
      *                      * Protocollo e data registrazione         *
      *                      *-----------------------------------------*
       prn-liv-det-426.
      *                          *-------------------------------------*
      *                          * Se entrambi a zero : no editing     *
      *                          *-------------------------------------*
           if        rf-iit-dtr-cge       =    zero and
                     rf-iit-prt-cge       =    zero
                     go to prn-liv-det-440.
       prn-liv-det-427.
      *                          *-------------------------------------*
      *                          * Preparazione area editata completa  *
      *                          *-------------------------------------*
       prn-liv-det-428.
      *                              *---------------------------------*
      *                              * Abblencamento iniziale          *
      *                              *---------------------------------*
           move      spaces               to   w-stp-dep-edt-000      .
      *                              *---------------------------------*
      *                              * Editing numero protocollo       *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      rf-iit-prt-cge       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-dep-edt-001      .
      *                              *---------------------------------*
      *                              * Editing data registrazione      *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-iit-dtr-cge       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-dep-edt-002      .
      *                              *---------------------------------*
      *                              * Deviazione a seconda dei valori *
      *                              * presenti                        *
      *                              *---------------------------------*
           if        rf-iit-dtr-cge       not  = zero and
                     rf-iit-prt-cge       not  = zero
                     go to prn-liv-det-429
           else if   rf-iit-dtr-cge       not  = zero
                     go to prn-liv-det-430
           else      go to prn-liv-det-431.
       prn-liv-det-429.
      *                              *---------------------------------*
      *                              * Se entrambi i valori presenti   *
      *                              *---------------------------------*
           string    "Protocollo "
                                delimited by   size
                     w-stp-dep-edt-001
                                delimited by   spaces
                     " del "
                                delimited by   size
                     w-stp-dep-edt-002
                                delimited by   spaces
                                          into w-stp-dep-edt-000      .
           go to     prn-liv-det-432.
       prn-liv-det-430.
      *                              *---------------------------------*
      *                              * Se presente solo la data        *
      *                              *---------------------------------*
           string    "Del "
                                delimited by   size
                     w-stp-dep-edt-001
                                delimited by   spaces
                                          into w-stp-dep-edt-000      .
           go to     prn-liv-det-432.
       prn-liv-det-431.
      *                              *---------------------------------*
      *                              * Se presente solo il protocollo  *
      *                              *---------------------------------*
           string    "Protocollo "
                                delimited by   size
                     w-stp-dep-edt-001
                                delimited by   spaces
                                          into w-stp-dep-edt-000      .
           go to     prn-liv-det-432.
       prn-liv-det-432.
      *                          *-------------------------------------*
      *                          * Stampa area editata completa        *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      w-stp-dep-edt-000    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-440.
      *                  *---------------------------------------------*
      *                  * Utente - Programma - Data inserimento o mo- *
      *                  * difica                                      *
      *                  *---------------------------------------------*
       prn-liv-det-441.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-442.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-442.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-443.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "Utente - Programma - Data inserimento o modifica .
      -              ".. :"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-444.
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iit-ide-ute       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-445.
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      073                  to   p-pos                  .
           move      rf-iit-ide-fas       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-446.
      *                      *-----------------------------------------*
      *                      * Data                                    *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      083                  to   p-pos                  .
           move      rf-iit-ide-dat       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-460.
      *                  *---------------------------------------------*
      *                  * Literal di inizio per righe corpo           *
      *                  *---------------------------------------------*
       prn-liv-det-461.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-462.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-462.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-463.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      54                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "     Merci relative al movimento                  
      -              "    "               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-480.
      *                  *---------------------------------------------*
      *                  * Fine testata : a righe corpo                *
      *                  *---------------------------------------------*
           go to     prn-liv-det-500.
       prn-liv-det-500.
      *              *-------------------------------------------------*
      *              * Righe movimento                                 *
      *              *-------------------------------------------------*
       prn-liv-det-520.
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore righe esaminate  *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-iir-ctr-iir      .
       prn-liv-det-540.
      *                  *---------------------------------------------*
      *                  * Start su [iir]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-iit-dat-reg       to   rf-iir-dat-reg         .
           move      rf-iit-num-prt       to   rf-iir-num-prt         .
           move      zero                 to   rf-iir-num-prg         .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine righe                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-900.
       prn-liv-det-560.
      *                  *---------------------------------------------*
      *                  * Read Next su [iir]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iir                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine righe                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-900.
      *                  *---------------------------------------------*
      *                  * Test max su [iir], se non superato : fine   *
      *                  * righe                                       *
      *                  *---------------------------------------------*
           if        rf-iir-dat-reg       not  = rf-iit-dat-reg or
                     rf-iir-num-prt       not  = rf-iit-num-prt
                     go to prn-liv-det-900.
       prn-liv-det-580.
      *                  *---------------------------------------------*
      *                  * Incremento contatore righe esaminate        *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-iir-ctr-iir      .
       prn-liv-det-600.
      *                  *---------------------------------------------*
      *                  * Numero riga                                 *
      *                  *---------------------------------------------*
       prn-liv-det-601.
      *                      *-----------------------------------------*
      *                      * Flag di stampa numero riga in On        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-stp-iir-flg-snr      .
       prn-liv-det-602.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-603.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-603.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra aperta                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      008                  to   p-pos                  .
           move      "["                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero riga                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      009                  to   p-pos                  .
           move      w-stp-iir-ctr-iir    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra chiusa                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      012                  to   p-pos                  .
           move      "]"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-620.
      *                  *---------------------------------------------*
      *                  * Anno di riferimento per la rettifica        *
      *                  *---------------------------------------------*
       prn-liv-det-621.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-640.
       prn-liv-det-622.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-624.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-623.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-623.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-624.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Anno di riferimento per la rettifica ...... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Anno di riferimento                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-ann-rif       to   p-num                  .
           add       1900                 to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-640.
      *                  *---------------------------------------------*
      *                  * Mese di riferimento per la rettifica        *
      *                  *---------------------------------------------*
       prn-liv-det-641.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-660.
       prn-liv-det-642.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-644.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-643.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-643.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-644.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Mese di riferimento per la rettifica ...... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mese di riferimento                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-mes-rif       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-660.
      *                  *---------------------------------------------*
      *                  * Ammontare in valuta base                    *
      *                  *---------------------------------------------*
       prn-liv-det-661.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-680.
       prn-liv-det-662.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-664.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-663.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-663.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-664.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Ammontare in      ......................... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      030                  to   p-pos                  .
           move      c-des                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ammontare                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-aml-ope       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-680.
      *                  *---------------------------------------------*
      *                  * Ammontare precedente in valuta base         *
      *                  *---------------------------------------------*
       prn-liv-det-681.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-700.
      *                      *-----------------------------------------*
      *                      * Se ammontare precedente a zero : no     *
      *                      * stampa                                  *
      *                      *-----------------------------------------*
           if        rf-iir-aml-pre       =    zero
                     go to prn-liv-det-700.
      *                      *-----------------------------------------*
      *                      * Se ammontare precedente pari a quello   *
      *                      * attuale : no stampa                     *
      *                      *-----------------------------------------*
           if        rf-iir-aml-pre       =    rf-iir-aml-ope
                     go to prn-liv-det-700.
       prn-liv-det-682.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-684.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-683.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-683.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-684.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "                                         Ex :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ammontare                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-aml-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-700.
      *                  *---------------------------------------------*
      *                  * Ammontare in valuta                         *
      *                  *---------------------------------------------*
       prn-liv-det-701.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0011
                     go to prn-liv-det-720.
       prn-liv-det-702.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-704.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-703.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-703.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-704.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Ammontare in valuta ....................... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ammontare                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-avl-ope       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-720.
      *                  *---------------------------------------------*
      *                  * Ammontare in valuta precedente              *
      *                  *---------------------------------------------*
       prn-liv-det-721.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0011
                     go to prn-liv-det-740.
      *                      *-----------------------------------------*
      *                      * Se ammontare in valuta precedente a     *
      *                      * zero : no stampa                        *
      *                      *-----------------------------------------*
           if        rf-iir-avl-pre       =    zero
                     go to prn-liv-det-740.
      *                      *-----------------------------------------*
      *                      * Se ammontare in valuta precedente pari  *
      *                      * a quello attuale : no stampa            *
      *                      *-----------------------------------------*
           if        rf-iir-avl-pre       =    rf-iir-avl-ope
                     go to prn-liv-det-740.
       prn-liv-det-722.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-724.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-723.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-723.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-724.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "                                         Ex :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ammontare                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-avl-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-740.
      *                  *---------------------------------------------*
      *                  * Nomenclatura combinata della merce          *
      *                  *---------------------------------------------*
       prn-liv-det-741.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-760.
       prn-liv-det-742.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-744.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-743.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-743.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-744.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Nomenclatura combinata della merce ........ :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Nomenclatura combinata                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-cdn-cdm       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-760.
      *                  *---------------------------------------------*
      *                  * Nomenclatura combinata della merce prece-   *
      *                  * dente                                       *
      *                  *---------------------------------------------*
       prn-liv-det-761.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-780.
      *                      *-----------------------------------------*
      *                      * Se codice della natura combinata pre-   *
      *                      * cedente a zero : no stampa              *
      *                      *-----------------------------------------*
           if        rf-iir-cnc-pre       =    zero
                     go to prn-liv-det-780.
      *                      *-----------------------------------------*
      *                      * Se codice della natura combinata pre-   *
      *                      * cedente pari a quello attuale : no      *
      *                      * stampa                                  *
      *                      *-----------------------------------------*
           if        rf-iir-cnc-pre       =    rf-iir-cdn-cdm
                     go to prn-liv-det-780.
       prn-liv-det-762.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-764.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-763.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-763.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-764.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "                                         Ex :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Nomenclatura combinata                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-cnc-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-780.
      *                  *---------------------------------------------*
      *                  * Massa netta in chilogrammi della merce      *
      *                  *---------------------------------------------*
       prn-liv-det-781.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002
                     go to prn-liv-det-800.
       prn-liv-det-782.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-784.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-783.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-783.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-784.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Massa netta in chilogrammi della merce .... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Massa netta                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-mas-net       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-800.
      *                  *---------------------------------------------*
      *                  * Quantita' in unita' di misura supplementare *
      *                  *---------------------------------------------*
       prn-liv-det-801.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002
                     go to prn-liv-det-820.
       prn-liv-det-802.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-804.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-803.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-803.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-804.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Quantita' in unita' di misura supplem. .... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Quantita' in unita' di misura supple-   *
      *                      * mentare                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-qen-ums       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-820.
      *                  *---------------------------------------------*
      *                  * Valore statistico in valuta base            *
      *                  *---------------------------------------------*
       prn-liv-det-821.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001 and
                     rf-iit-top-iic       not  = 0002 and
                     rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-840.
       prn-liv-det-822.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-824.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-823.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-823.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-824.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Valore statistico in      ................. :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      038                  to   p-pos                  .
           move      c-des                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore statistico                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-vls-iml       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-840.
      *                  *---------------------------------------------*
      *                  * Valore statistico precedente in valuta base *
      *                  *---------------------------------------------*
       prn-liv-det-841.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0011 and
                     rf-iit-top-iic       not  = 0012
                     go to prn-liv-det-860.
      *                      *-----------------------------------------*
      *                      * Se valore statistico precedente a zero  *
      *                      * : no stampa                             *
      *                      *-----------------------------------------*
           if        rf-iir-vls-pre       =    zero
                     go to prn-liv-det-860.
      *                      *-----------------------------------------*
      *                      * Se valore statistico precedente pari a  *
      *                      * quello attuale : no stampa              *
      *                      *-----------------------------------------*
           if        rf-iir-vls-pre       =    rf-iir-vls-iml
                     go to prn-liv-det-860.
       prn-liv-det-842.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-844.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    1
                     go to prn-liv-det-843.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-843.
      *                      *-----------------------------------------*
      *                      * Una interlinea                          *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-844.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "                                         Ex :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore statistico                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GB"                to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-vls-pre       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-860.
      *                  *---------------------------------------------*
      *                  * Codice ISO dello stato di origine           *
      *                  *---------------------------------------------*
       prn-liv-det-861.
      *                      *-----------------------------------------*
      *                      * Se tipo operazione che non prevede il   *
      *                      * valore : no stampa                      *
      *                      *-----------------------------------------*
           if        rf-iit-top-iic       not  = 0001
                     go to prn-liv-det-880.
       prn-liv-det-862.
      *                      *-----------------------------------------*
      *                      * Test se linee residue sufficienti ed e- *
      *                      * ventuale reintestazione con controllo   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il flag di stampa numero riga e' *
      *                          * in On : no test ne' interlinea      *
      *                          *-------------------------------------*
           if        w-stp-iir-flg-snr    not  = spaces
                     move  spaces         to   w-stp-iir-flg-snr
                     go to prn-liv-det-864.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-863.
      *                          *-------------------------------------*
      *                          * Reintestazione                      *
      *                          *-------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                          *-------------------------------------*
      *                          * Controllo                           *
      *                          *-------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
       prn-liv-det-863.
      *                      *-----------------------------------------*
      *                      * Due interlinee                          *
      *                      *-----------------------------------------*
           move      "L+"                 to   p-ope                  .
           move      2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-864.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      45                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      "Codice ISO dello stato di origine ......... :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-865.
      *                      *-----------------------------------------*
      *                      * Codice ISO                              *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      rf-iir-iso-org       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-866.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica codice ISO           *
      *                      *-----------------------------------------*
           move      rf-iir-iso-org       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione codice ISO                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-let-arc-gxn-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-880.
      *                  *---------------------------------------------*
      *                  * Fine singola riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura riga successiva               *
      *                      *-----------------------------------------*
           go to     prn-liv-det-560.
       prn-liv-det-900.
      *                  *---------------------------------------------*
      *                  * Fine righe                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'intestazione della pagina                *
      *    *-----------------------------------------------------------*
       stp-int-pag-000.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio assoluto                  *
      *              *-------------------------------------------------*
       stp-int-pag-010.
      *                  *---------------------------------------------*
      *                  * Se non e' l'inizio in assoluto : nessuna a- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-stp-int-num-int    >    zero
                     go to stp-int-pag-100.
       stp-int-pag-020.
      *                  *---------------------------------------------*
      *                  * Preparazione titolo stampato, allineato a   *
      *                  * sinistra                                    *
      *                  *---------------------------------------------*
           move      "BROGLIACCIO MOVIMENTI I.V.A. INTRACOMUNITARIA     
      -              "                              "
                                          to   w-stp-int-tit-stp      .
       stp-int-pag-050.
      *                  *---------------------------------------------*
      *                  * Numero pagina da stampare a zero            *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-num-pag      .
      *                  *---------------------------------------------*
      *                  * Preparazione data di stampa                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-stp-int-dat-stp      .
      *                  *---------------------------------------------*
      *                  * Preparazione ragione sociale azienda        *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-stp-int-rag-azi      .
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza della ragio- *
      *                  * ne sociale azienda allineata a sinistra,    *
      *                  * senza considerare gli spazi in coda         *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-rag-azi
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
           move      40                   to   w-stp-int-rag-azl      .
           subtract  w-stp-int-wct-c01    from w-stp-int-rag-azl      .
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza del titolo   *
      *                  * dello stampato allineato a sinistra, senza  *
      *                  * considerare gli spazi in coda               *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-tit-stp
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
           move      80                   to   w-stp-int-tit-stl      .
           subtract  w-stp-int-wct-c01    from w-stp-int-tit-stl      .
      *                  *---------------------------------------------*
      *                  * Determinazione della posizione di stampa    *
      *                  * per il titolo dello stampato                *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-stp-int-tit-stc      .
           subtract  w-stp-int-tit-stl    from w-stp-int-tit-stc      .
           divide    2                    into w-stp-int-tit-stc      .
           add       1                    to   w-stp-int-tit-stc      .
      *                  *---------------------------------------------*
      *                  * Determinazione se necessarie una o due ri-  *
      *                  * ghe per il titolo dello stampato            *
      *                  *---------------------------------------------*
           move      w-stp-int-rag-azl    to   w-stp-int-wct-c01      .
           add       2                    to   w-stp-int-wct-c01      .
           move      w-stp-int-tit-stc    to   w-stp-int-wct-c02      .
           move      w-stp-int-tit-stc    to   w-stp-int-wct-c03      .
           add       w-stp-int-tit-stl    to   w-stp-int-wct-c03      .
           subtract  1                    from w-stp-int-wct-c03      .
           move      p-sel-als-sel        to   w-stp-int-wct-c04      .
           subtract  27                   from w-stp-int-wct-c04      .
           if        w-stp-int-wct-c02    >    w-stp-int-wct-c01 and
                     w-stp-int-wct-c03    <    w-stp-int-wct-c04
                     move  "U"            to   w-stp-int-tit-uod
           else      move  "D"            to   w-stp-int-tit-uod      .
       stp-int-pag-100.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio pagina                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina da stampare        *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-int-num-pag      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero caratteri di slitta-  *
      *                  * mento a destra per data e pagina in funzio- *
      *                  * ne del numero pagina                        *
      *                  *---------------------------------------------*
           if        w-stp-int-num-pag    >    9999
                     move  zero           to   w-stp-int-ncs-dep
           else if   w-stp-int-num-pag    >    999
                     move  1              to   w-stp-int-ncs-dep
           else if   w-stp-int-num-pag    >    99
                     move  2              to   w-stp-int-ncs-dep
           else if   w-stp-int-num-pag    >    9
                     move  3              to   w-stp-int-ncs-dep
           else      move  4              to   w-stp-int-ncs-dep      .
       stp-int-pag-200.
      *              *-------------------------------------------------*
      *              * Esecuzione intestazione pagina                  *
      *              *-------------------------------------------------*
       stp-int-pag-250.
      *                  *---------------------------------------------*
      *                  * Avanzamento pagina                          *
      *                  *---------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si pone in *
      *                  * On il flag di interruzione forzata e si e-  *
      *                  * sce immediatamente                          *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move  "#"            to   w-cnt-prn-flg-int
                     go to stp-int-pag-999.
       stp-int-pag-300.
      *                  *---------------------------------------------*
      *                  * Intestazione vera e propria                 *
      *                  *---------------------------------------------*
       stp-int-pag-325.
      *                      *-----------------------------------------*
      *                      * Linea di '=' iniziale                   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-350.
      *                      *-----------------------------------------*
      *                      * Ragione sociale azienda                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-int-rag-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal 'Data :'                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  25                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data di stampa                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  18                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      w-stp-int-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal 'Pag.'                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  08                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero pagina                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      5                    to   p-car                  .
           subtract  w-stp-int-ncs-dep    from p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  04                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      w-stp-int-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Se stampa su una linea si passa diret-  *
      *                      * tamente a stampare il titolo centrale   *
      *                      *-----------------------------------------*
           if        w-stp-int-tit-uod    =    "U"
                     go to stp-int-pag-375.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-375.
      *                      *-----------------------------------------*
      *                      * Titolo centrale                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-int-tit-stl    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-int-tit-stc    to   p-pos                  .
           move      w-stp-int-tit-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Linea di '-' finale                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-400.
      *                  *---------------------------------------------*
      *                  * Sub-intestazione                            *
      *                  *---------------------------------------------*
       stp-int-pag-425.
      *                      *-----------------------------------------*
      *                      * Se il numero pagina da stampare non e'  *
      *                      * pari a 1 : no sub-intestazione          *
      *                      *-----------------------------------------*
           if        w-stp-int-num-pag    not  = 1
                     go to stp-int-pag-500.
       stp-int-pag-450.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale per lasciare   *
      *                      * una interlinea di separazione           *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal 'Periodo di riferimento :'      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      24                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      045                  to   p-pos                  .
           move      "Periodo di riferimento :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data minima                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      070                  to   p-pos                  .
           move      rr-dtm-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Separazione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      079                  to   p-pos                  .
           move      "-"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data massima                            *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      081                  to   p-pos                  .
           move      rr-dtm-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-500.
      *              *-------------------------------------------------*
      *              * Fine stampa intestazione                        *
      *              *-------------------------------------------------*
       stp-int-pag-525.
      *                  *---------------------------------------------*
      *                  * Posizionamento verticale per lasciare una   *
      *                  * interlinea di separazione                   *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-550.
      *                  *---------------------------------------------*
      *                  * Incremento numero totale di intestazioni e- *
      *                  * seguite                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-int-num-int      .
       stp-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
       let-arc-fnt-100.
      *              *-------------------------------------------------*
      *              * Se codice fornitore a zero                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-cod    not  = zero
                     go to let-arc-fnt-200.
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       let-arc-fnt-300.
      *              *-------------------------------------------------*
      *              * Se codice fornitore non esistente               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-fnt-400.
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
       let-arc-cli-100.
      *              *-------------------------------------------------*
      *              * Se codice cliente a zero                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-cod    not  = zero
                     go to let-arc-cli-200.
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       let-arc-cli-300.
      *              *-------------------------------------------------*
      *              * Se codice cliente non esistente                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-cli-400.
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [gxn]                             *
      *    *-----------------------------------------------------------*
       let-arc-gxn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-gxn-cod    =    spaces
                     go to let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODNAZ    "         to   f-key                  .
           move      w-let-arc-gxn-cod    to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxn-400.
       let-arc-gxn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxn-des-naz       to   w-let-arc-gxn-des      .
           move      rf-gxn-snx-cee       to   w-let-arc-gxn-cee      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-gxn-999.
       let-arc-gxn-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxn-flg      .
           move      all   "."            to   w-let-arc-gxn-des      .
           move      spaces               to   w-let-arc-gxn-cee      .
           go to     let-arc-gxn-999.
       let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-des      .
           move      spaces               to   w-let-arc-gxn-cee      .
       let-arc-gxn-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [gxp]                             *
      *    *-----------------------------------------------------------*
       let-arc-gxp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-gxp-cod    =    spaces
                     go to let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-let-arc-gxp-cod    to   rf-gxp-cod-prv         .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxp-400.
       let-arc-gxp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxp-des-prv       to   w-let-arc-gxp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-gxp-999.
       let-arc-gxp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxp-flg      .
           move      all   "."            to   w-let-arc-gxp-des      .
           go to     let-arc-gxp-999.
       let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-des      .
       let-arc-gxp-999.
           exit.

      *    *===========================================================*
      *    * Lettura tabella tipi operazione per gestione Iva intra-   *
      *    * comunitaria                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.rtn"                   .
