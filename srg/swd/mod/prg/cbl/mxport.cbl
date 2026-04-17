       Identification Division.
       Program-Id.                                 mxport             .
      *================================================================*
      *                                                                *
      *                    Modulo per export dati in formato 'csv'     *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/01/22    *
      *                       Ultima revisione:    NdK del 28/02/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo per export dati in formato 'csv' e   *
      *                    in formato 'html' per l'utente              *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *                              Note                              *
      *                                                                *
      * -  Viene utilizzata l'area di interfaccia 'x'.                 *
      *                                                                *
      *   (NOTE: da ampliare per path di comodo da chiamante)          *
      *                                                                *
      *    --------------------------------------------------------    *
      *                                                                *
      * -  Attualmente richiamato da:                                  *
      *                                                                *
      *    - page4601 : Stampa situazione maturazioni                  *
      *                                                                *
      *    - pbfo3201 : Stampa elenco documenti di entrata             *
      *                                                                *
      *    - pbol2201 : Stampa giro clienti                            *
      *                                                                *
      *    - pcge3251 : Stampa mastrini sottoconti                     *
      *    - pcge3401 : Stampa bilanci infrannuali                     *
      *    - pcge3501 : Stampa bilanci infrannuali                     *
      *    - pcge4301 : Stampa situazione contabile Clienti            *
      *    - pcge5251 : Stampa partitario Fornitori ad uso interno     *
      *    - pcge5301 : Stampa situazione contabile Fornitori          *
      *    - pcge800b : Stampa bilanci                                 *
      *                                                                *
      *    - pdcc340a : Stampe prezzi netti concordati Clienti         *
      *    - pdcc4023 : Stampa anagrafiche Clienti                     *
      *    - pdcc6101 : Stampa Etichette Clienti                       *
      *                                                                *
      *    - pdcc4023 : Stampa anagrafiche Fornitori                   *
      *                                                                *
      *    - pdcp340a : Stampa prezzi di listino prodotti              *
      *                                                                *
      *    - pfat3301 : Lista dettagliata documenti clienti            *
      *                                                                *
      *    - pffo3401 : Stampa elenco documenti di entrata materie p.  *
      *                                                                *
      *    - pgep5001 : Lista scadenze in portafoglio per data         *
      *    - pgep6601 : Stampa clienti con maggiore esposizione        *
      *    - pgep6901 : Lista scadenze riscosse per cliente            *
      *                                                                *
      *    - pmag3061 : Stampa brogliaccio movimenti di magazzino      *
      *    - pmag4121 : Disponibilita' prodotti di vendita             *
      *    - pmag4601 : Situazione giacenze di proprieta'              *
      *                                                                *
      *    - porc3301 : Portafoglio ordini clienti                     *
      *    - porc3601 : Portafoglio ordini clienti per Agente          *
      *                                                                *
      *    - porf3301 : Portafoglio ordini per fornitore               *
      *                                                                *
      *    - prda3301 : Stampa certificazione annuale ritenute         *
      *                                                                *
      *    - psaf2001 : Stampa statistica Fornitori                    *
      *    - psaf2501 : Stampa statistica Fornitori                    *
      *    - psaf8001 : Stampa statistica Fornitori                    *
      *                                                                *
      *    - pscf5001 : Lista scadenze fornitori per data              *
      *                                                                *
      *    - psvf2001 : Stampa statistica Clienti (tramite svf900)     *
      *    - psvf2501 : Stampa statistica Clienti     "       "        *
      *    - psvf3501 : Stampa statistica Clienti     "       "        *
      *    - psvf4251 : Stampa statistica Clienti     "       "        *
      *    - psvf5001 : Stampa statistica Clienti     "       "        *
      *    - psvf7201 : Stampa statistica Clienti     "       "        *
      *    - psvf7501 : Stampa statistica Clienti     "       "        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *                          Interfaccia                           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Open        Open file di export                                *
      *                                                                *
      *             Input  : x-ope = "OP"                              *
      *                                                                *
      *                      x-tex = Tipo file di export               *
      *                                                                *
      *                      x-sep = Tipo separatore                   *
      *                                                                *
      *                      x-pat = Pathname del file di export       *
      *                             (se a spazi, assegnazione dal)     *
      *                              modulo di segreteria)             *
      *                                                                *
      *                      x-nam = Filename opzionale                *
      *                                                                *
      *             Output : x-sts = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - 1x     : Errore                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Close       Close file di export                               *
      *                                                                *
      *             Input  : x-ope = "CL"                              *
      *                                                                *
      *             Output : x-msg = Eventuale messaggio               *
      *                                                                *
      *                      x-sts = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - 2x     : Errore                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Print       Inserimento all'interno del file di export di un   *
      * Field       campo che non e' l'ultimo del record               *
      *                                                                *
      *             Input  : x-ope = "PF"                              *
      *                                                                *
      *                      x-tip = come per display di mvideo        *
      *                                                                *
      *                      x-car = come per display di mvideo        *
      *                                                                *
      *                      x-ldt = come per display di mvideo        *
      *                                                                *
      *                      x-dec = come per display di mvideo        *
      *                                                                *
      *                      x-sgn = come per display di mvideo        *
      *                                                                *
      *                      x-edm = come per display di mvideo        *
      *                                                                *
      *                      x-msk = come per display di mvideo        *
      *                                                                *
      *                      x-alf = come per display di mvideo        *
      *                                                                *
      *                      x-txt = come per display di mvideo        *
      *                                                                *
      *                      x-num = come per display di mvideo        *
      *                                                                *
      *                      x-dat = come per display di mvideo        *
      *                                                                *
      *             Output : x-sts = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - 3x     : Errore                 *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Print       Inserimento all'interno del file di export di un   *
      * Record      campo che e' l'ultimo del record                   *
      *                                                                *
      *             Input  : x-ope = "PR"                              *
      *                                                                *
      *             Output : x-sts = Status di uscita                  *
      *                              - Spaces : OK                     *
      *                              - 4x     : Errore                 *
      *                                                                *
      *             -------------------------------------------------- *
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
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential, con pathname lungo               *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/j"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mopsys" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Work-area per l'esecuzione dell'export                    *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Funzione Open                                         *
      *        *-------------------------------------------------------*
           05  w-exe-opn.
      *            *---------------------------------------------------*
      *            * Flag di open in corso                             *
      *            *---------------------------------------------------*
               10  w-exe-opn-flg-opn      pic  9(01) value zero       .
      *            *---------------------------------------------------*
      *            * File pathname relativo                            *
      *            *---------------------------------------------------*
               10  w-exe-opn-fil-nam      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * File pathname completo                            *
      *            *---------------------------------------------------*
               10  w-exe-opn-fil-pat      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * File pathname completo 'HTML'                     *
      *            *---------------------------------------------------*
               10  w-exe-opn-fil-htm      pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Funzione PR                                           *
      *        *-------------------------------------------------------*
           05  w-exe-fpr.
      *            *---------------------------------------------------*
      *            * Contatore elementi emessi                         *
      *            *---------------------------------------------------*
               10  w-exe-fpr-ctr-ele      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Contatore campi in riga                           *
      *            *---------------------------------------------------*
               10  w-exe-fpr-ctr-fld      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Costanti esadecimali                                  *
      *        *-------------------------------------------------------*
           05  w-exe-esa.
      *            *---------------------------------------------------*
      *            * Tab                                               *
      *            *---------------------------------------------------*
               10  w-exe-esa-TAB          pic  x(01) value H"09"      .
      *            *---------------------------------------------------*
      *            * Line Feed                                         *
      *            *---------------------------------------------------*
               10  w-exe-esa-LFx          pic  x(01) value H"0A"      .
      *            *---------------------------------------------------*
      *            * Carriage Return                                   *
      *            *---------------------------------------------------*
               10  w-exe-esa-CRx          pic  x(01) value H"0D"      .

      *    *===========================================================*
      *    * Work-area per editing ed allineamenti                     *
      *    *-----------------------------------------------------------*
       01  w-edt.
      *        *-------------------------------------------------------*
      *        * Valore editato, tramite chiamata a mvideo             *
      *        *-------------------------------------------------------*
           05  w-edt-mvi.
               10  filler occurs 132      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore editato, allineato a sinistra                  *
      *        *-------------------------------------------------------*
           05  w-edt-asx.
               10  filler occurs 132      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore editato, allineato a sinistra per 'HTML'       *
      *        *-------------------------------------------------------*
           05  w-edt-htm.
               10  filler occurs 132      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di output                                     *
      *        *-------------------------------------------------------*
           05  w-edt-str.
               10  w-edt-chr  occurs 1024 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Lunghezza massima                                     *
      *        *-------------------------------------------------------*
           05  w-edt-max                  pic  9(04) value 1024       .
      *        *-------------------------------------------------------*
      *        * Lunghezza del valore editato allineato a sinistra     *
      *        *-------------------------------------------------------*
           05  w-edt-lun                  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Switch per allineamento colonna                       *
      *        *-------------------------------------------------------*
           05  w-edt-swc.
               10  w-edt-all  occurs 256  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi di lavoro                                      *
      *        *-------------------------------------------------------*
           05  w-edt-i01                  pic  9(03)                  .
           05  w-edt-i02                  pic  9(03)                  .
           05  w-edt-c01                  pic  9(05)                  .
           05  w-edt-c02                  pic  9(05)                  .
           05  w-edt-dat.
               10  w-edt-dat-gio          pic  9(02)                  .
               10  filler                 pic  x(01) value "/"        .
               10  w-edt-dat-mes          pic  9(02)                  .
               10  filler                 pic  x(01) value "/"        .
               10  w-edt-dat-saa          pic  9(04)                  .
           05  w-edt-st1                  pic  x(40)                  .
           05  w-edt-st2                  pic  x(40)                  .

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
      *    * Area di comunicazione per modulo                "mxport"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/x"                                  .

      ******************************************************************
       Procedure Division                using x                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        x-ope                =    "OP"
                     perform   opn-000    thru opn-999
           else if   x-ope                =    "CL"
                     perform   cls-000    thru cls-999
           else if   x-ope                =    "PF"
                     perform   prf-000    thru prf-999
           else if   x-ope                =    "PR"
                     perform   prr-000    thru prr-999                .
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
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione messaggio di uscita             *
      *              *-------------------------------------------------*
           move      spaces               to   x-msg                  .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare area editing        *
      *              *-------------------------------------------------*
           move      spaces               to   x-tip                  .
           move      zero                 to   x-car                  .
           move      zero                 to   x-ldt                  .
           move      zero                 to   x-dec                  .
           move      spaces               to   x-sgn                  .
           move      spaces               to   x-edm                  .
           move      spaces               to   x-msk                  .
           move      spaces               to   x-alf                  .
           move      spaces               to   x-txt                  .
           move      zero                 to   x-num                  .
           move      zero                 to   x-dat                  .
      *              *-------------------------------------------------*
      *              * Se open gia' in corso : uscita senza nessuna a- *
      *              * zione, ma con status di uscita a Ko             *
      *              *-------------------------------------------------*
           if        w-exe-opn-flg-opn    not  = zero
                     move  "11"           to   x-sts
                     go to opn-999.
       opn-100.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri passati e controllo       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * File pathname                               *
      *                  *---------------------------------------------*
           move      x-pat                to   w-exe-opn-fil-pat      .
      *                  *---------------------------------------------*
      *                  * Se pathname a spaces : uscita con codice    *
      *                  * di errore                                   *
      *                  *---------------------------------------------*
           if        w-exe-opn-fil-pat    not  =    spaces
                     go to opn-500.
      *                  *---------------------------------------------*
      *                  * Pathname da segreteria                      *
      *                  *---------------------------------------------*
           move      "UE"                 to   s-ope                  .
           move      "csv "               to   s-nam                  .
           move      x-nam                to   s-tip                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                     .
      *                  *---------------------------------------------*
      *                  * Pathname determinati                        *
      *                  *---------------------------------------------*
           move      s-pat                to   w-exe-opn-fil-pat      .
           move      s-alf                to   w-exe-opn-fil-nam      .
       opn-500.
      *              *-------------------------------------------------*
      *              * Apertura del file in output                     *
      *              *-------------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      "seq "               to   g-nam                  .
           move      w-exe-opn-fil-pat    to   g-pat                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Flag di open in corso                           *
      *              *-------------------------------------------------*
           move      1                    to   w-exe-opn-flg-opn      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore linee generate        *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-fpr-ctr-ele      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore campi                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-fpr-ctr-fld      .
      *              *-------------------------------------------------*
      *              * Normalizzazione stringa di output               *
      *              *-------------------------------------------------*
           move      all spaces           to   w-edt-str              .
       opn-600.
      *              *-------------------------------------------------*
      *              * Inizializzazione 'HTML'                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se previsto dal tipo esportazione      *
      *                  *---------------------------------------------*
           if        x-tex                not  = 02
                     go to opn-900.
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   htm-ini-000          thru htm-ini-999            .
       opn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-999. 
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Se open non in corso : uscita senza nessuna a-  *
      *              * zione, ma con status di uscita a Ko             *
      *              *-------------------------------------------------*
           if        w-exe-opn-flg-opn    =    zero
                     move  "21"           to   x-sts
                     go to cls-999.
       cls-100.
      *              *-------------------------------------------------*
      *              * Chiusura del file in output                     *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *              *-------------------------------------------------*
      *              * Flag di open non piu' in corso                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-opn-flg-opn      .
       cls-200.
      *              *-------------------------------------------------*
      *              * Prima linea di messaggio                        *
      *              *-------------------------------------------------*
           move      spaces               to   x-ms1                  .
           string    "Preparato archivio              : "
                                delimited by   size
                     "["        delimited by   size
                     w-exe-opn-fil-nam
                                delimited by   spaces
                     "]"        delimited by   size
                                          into x-ms1                  .
       cls-300.
      *              *-------------------------------------------------*
      *              * Seconda linea di messaggio                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing contatore                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-exe-fpr-ctr-ele    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio per contatore elementi            *
      *                  *---------------------------------------------*
           move      spaces               to   x-ms2                  .
           string    "Linee generate nr.              : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into x-ms2                  .
       cls-400.
      *              *-------------------------------------------------*
      *              * Chiusura 'HTML'                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se previsto dal tipo esportazione      *
      *                  *---------------------------------------------*
           if        x-tex                not  = 02
                     go to cls-900.
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           perform   htm-chi-000          thru htm-chi-999            .
       cls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cls-999. 
       cls-999.
           exit.

      *    *===========================================================*
      *    * Print Field                                               *
      *    *-----------------------------------------------------------*
       prf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Se open non in corso : uscita senza nessuna a-  *
      *              * zione, ma con status di uscita a Ko             *
      *              *-------------------------------------------------*
           if        w-exe-opn-flg-opn    =    zero
                     move  "31"           to   x-sts
                     go to prf-999.
       prf-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore campi                      *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-fpr-ctr-fld      .
      *              *-------------------------------------------------*
      *              * Trattamento switch di allineamento              *
      *              *-------------------------------------------------*
           if        w-exe-fpr-ctr-ele    =    zero
                     move  "L"            to   w-edt-all
                                              (w-exe-fpr-ctr-fld)     .
      *              *-------------------------------------------------*
      *              * Editing del campo                               *
      *              *-------------------------------------------------*
           perform   edt-000              thru edt-999                .
      *              *-------------------------------------------------*
      *              * Output del campo                                *
      *              *-------------------------------------------------*
           perform   ofl-000              thru ofl-999                .
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           move      x-sep                to   w-edt-asx              .
           move      01                   to   w-edt-lun              .
           perform   ofl-000              thru ofl-999                .
       prf-800.
      *              *-------------------------------------------------*
      *              * Emissione singolo campo per generazione 'HTML'  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se previsto dal tipo esportazione      *
      *                  *---------------------------------------------*
           if        x-tex                not  = 02
                     go to prf-900.
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   htm-fld-000          thru htm-fld-999            .
       prf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prf-999.
       prf-999.
           exit.

      *    *===========================================================*
      *    * Print Record                                              *
      *    *-----------------------------------------------------------*
       prr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Incremento contatore elementi                   *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-fpr-ctr-ele      .
      *              *-------------------------------------------------*
      *              * Carattere di fine riga                          *
      *              *-------------------------------------------------*
           move      w-exe-esa-CRx        to   w-edt-asx              .
           move      01                   to   w-edt-lun              .
           perform   ofl-000              thru ofl-999                .
      *              *-------------------------------------------------*
      *              * Stringa in uscita                               *
      *              *-------------------------------------------------*
           move      w-edt-str            to   g-rec                  .
      *              *-------------------------------------------------*
      *              * Scrittura su sequenziale                        *
      *              *-------------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       prr-300.
      *              *-------------------------------------------------*
      *              * Scrittura fine riga 'HTML'                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se previsto dal tipo esportazione      *
      *                  *---------------------------------------------*
           if        x-tex                not  = 02
                     go to prr-600.
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   htm-cri-000          thru htm-cri-999            .
       prr-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore campi                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-fpr-ctr-fld      .
       prr-800.
      *              *-------------------------------------------------*
      *              * Normalizzazione stringa di output               *
      *              *-------------------------------------------------*
           move      all spaces           to   w-edt-str              .
       prr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to    prr-999. 
       prr-999.
           exit.

      *    *===========================================================*
      *    * Editing del campo                                         *
      *    *-----------------------------------------------------------*
       edt-000.
      *              *-------------------------------------------------*
      *              * Editing, richiamando il modulo 'mvideo', a se-  *
      *              * conda del tipo campo                            *
      *              *-------------------------------------------------*
           if        x-tip                =    "H"
                     go to edt-050
           else if   x-tip                =    "A" or
                     x-tip                =    "U" or
                     x-tip                =    "L"
                     go to edt-100
           else if   x-tip                =    "T"
                     go to edt-200
           else if   x-tip                =    "E"
                     go to edt-300
           else if   x-tip                =    "C"
                     go to edt-400
           else if   x-tip                =    "N" or
                     x-tip                =    "V"
                     go to edt-500
           else if   x-tip                =    "P"
                     go to edt-600
           else if   x-tip                =    "D"
                     go to edt-700.
       edt-010.
      *              *-------------------------------------------------*
      *              * Se tipo campo errato : uscita con codice di er- *
      *              * rore                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di errore                            *
      *                  *---------------------------------------------*
           move      "EE"                 to   x-sts                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     edt-999.
       edt-050.
      *              *-------------------------------------------------*
      *              * Tipo campo : "H" - header per campi numerici    *
      *              *                                                 *
      *              * N.B.: uguale ai campi "A" ma serve a differen-  *
      *              *       ziare gli "headers" HTML per i numeri in  *
      *              *       modo che vengano allineati a destra       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo di esportazione                *
      *                  *---------------------------------------------*
           if        x-tex                =    01
                     go to edt-060.
      *                  *---------------------------------------------*
      *                  * Trattamento switch di allineamento          *
      *                  *---------------------------------------------*
           if        w-exe-fpr-ctr-ele    =    zero
                     move  "R"            to   w-edt-all
                                              (w-exe-fpr-ctr-fld)     .
       edt-060.
      *                  *---------------------------------------------*
      *                  * Tipo campo: come "A"                        *
      *                  *---------------------------------------------*
           move      "A"                  to   x-tip                  .
      *                  *---------------------------------------------*
      *                  * A trattamento campo di testo                *
      *                  *---------------------------------------------*
           go to     edt-100.
       edt-100.
      *              *-------------------------------------------------*
      *              * Tipo campo : "A" "U" "L"                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      x-tip                to   v-tip                  .
           move      x-car                to   v-car                  .
           move      x-alf                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-200.
      *              *-------------------------------------------------*
      *              * Tipo campo : "T"                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      x-tip                to   v-tip                  .
           move      x-car                to   v-car                  .
           move      x-txt                to   v-txt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-300.
      *              *-------------------------------------------------*
      *              * Tipo campo : "E"                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      x-tip                to   v-tip                  .
           move      x-car                to   v-car                  .
           move      x-ldt                to   v-ldt                  .
           move      x-txt                to   v-txt                  .
           move      x-num                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-400.
      *              *-------------------------------------------------*
      *              * Tipo campo : "C"                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      x-tip                to   v-tip                  .
           move      x-car                to   v-car                  .
           move      x-ldt                to   v-ldt                  .
           move      x-edm                to   v-edm                  .
           move      x-alf                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-500.
      *              *-------------------------------------------------*
      *              * Tipo campo : "N" "V"                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      x-tip                to   v-tip                  .
           move      x-car                to   v-car                  .
           move      x-dec                to   v-dec                  .
           move      x-sgn                to   v-sgn                  .
           move      x-edm                to   v-edm                  .
           move      spaces               to   v-msk                  .
           move      x-num                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Se zero                                     *
      *                  *---------------------------------------------*
           if        v-edt                =    spaces
                     move  "0"            to   w-edt-mvi              .
           if        x-tip                =    "V"    and
                     v-edt                =    spaces
                     move  "0,00"         to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-600.
      *              *-------------------------------------------------*
      *              * Tipo campo : "P"                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      x-tip                to   v-tip                  .
           move      x-car                to   v-car                  .
           move      x-edm                to   v-edm                  .
           move      x-num                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-700.
      *              *-------------------------------------------------*
      *              * Tipo campo : "D"                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se data a zero                         *
      *                  *---------------------------------------------*
           if        x-dat                =    zero
                     move  spaces         to   w-edt-mvi
                     go to edt-800.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      x-gio                to   w-edt-dat-gio          .
           move      x-mes                to   w-edt-dat-mes          .
           move      x-saa                to   w-edt-dat-saa          .
           add       1900                 to   w-edt-dat-saa          .
           move      w-edt-dat            to   w-edt-mvi              .
      *                  *---------------------------------------------*
      *                  * Ad allineamento                             *
      *                  *---------------------------------------------*
           go to     edt-800.
       edt-800.
      *              *-------------------------------------------------*
      *              * Allineamento a sinistra della stringa editata,  *
      *              * e calcolo della lunghezza risultante            *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      w-edt-mvi            to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   w-edt-asx              .
           move      w-all-str-alf        to   w-edt-htm              .
      *
           move      w-edt-asx            to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
           move      w-all-str-lun        to   w-edt-lun              .
      *              *-------------------------------------------------*
      *              * Conversione punti e virgola in virgole          *
      *              *-------------------------------------------------*
           inspect   w-edt-asx  replacing all  ";"
                                          by   ","                    .
       edt-999.
           exit.

      *    *===========================================================*
      *    * Subroutines comuni ai programmi della serie 'xpg7xx'      *
      *    *                                                           *
      *    *  - Composizione singolo campo                             *
      *    *-----------------------------------------------------------*

      *    *===========================================================*
      *    * Composizione singolo campo                                *
      *    *-----------------------------------------------------------*
       ofl-000.
      *              *-------------------------------------------------*
      *              * Test che il contatore non sia a zero            *
      *              *-------------------------------------------------*
           if        w-edt-lun            =    zero
                     move  1              to   w-edt-lun              .
      *              *-------------------------------------------------*
      *              * Scansione per assemblaggio                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-edt-c01              .
           inspect   w-edt-str        tallying w-edt-c01
                                  for trailing spaces                 .
           move      w-edt-max            to   w-edt-c02              .
           subtract  w-edt-c01            from w-edt-c02              .
      *              *-------------------------------------------------*
      *              * Se fine riga, si pone il carattere CR sopra al- *
      *              * l'utlimo ';'                                    *
      *              *-------------------------------------------------*
           if        x-ope                =    "PR"
                     go to ofl-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-edt-c02          .
       ofl-200.
      *              *-------------------------------------------------*
      *              * Scrittura su stringa                            *
      *              *-------------------------------------------------*
           if        w-edt-c02            =    zero or
                     w-edt-lun            =    zero
                     go to ofl-900.
           move      w-edt-asx            to   w-edt-str
                                              (w-edt-c02 :
                                               w-edt-lun)             .
       ofl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ofl-999.
       ofl-999.
           exit.

      *    *===========================================================*
      *    * Generazione 'HTML'                                        *
      *    *                                                           *
      *    * Subroutine di inizializzazione                            *
      *    *-----------------------------------------------------------*
       htm-ini-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Pathname da segreteria                      *
      *                  *---------------------------------------------*
           move      "UE"                 to   s-ope                  .
           move      "html"               to   s-nam                  .
           move      x-nam                to   s-tip                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                     .
      *                  *---------------------------------------------*
      *                  * Pathname determinati                        *
      *                  *---------------------------------------------*
           move      s-pat                to   w-exe-opn-fil-htm      .
       htm-ini-050.
      *              *-------------------------------------------------*
      *              * Apertura del file in output                     *
      *              *-------------------------------------------------*
           move      "OO"                 to   j-ope                  .
           move      "seq "               to   j-nam                  .
           move      w-exe-opn-fil-htm    to   j-pat                  .
           call      "swd/mod/prg/obj/mcvoju"
                                         using j                      .
       htm-ini-800.
      *              *-------------------------------------------------*
      *              * Normalizzazione switch per allineamento campi   *
      *              *-------------------------------------------------*
           move      all spaces           to   w-edt-swc              .
       htm-ini-100.
      *              *-------------------------------------------------*
      *              * Apertura html                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * 'HTML'                                      *
      *                  *---------------------------------------------*
           move      "<!DOCTYPE html>"    to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<html>"             to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<head>"             to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *                  *---------------------------------------------*
      *                  * Title                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Estrazione del nome file                *
      *                      *-----------------------------------------*
           move      w-exe-opn-fil-nam    to   w-all-str-alf          .
           move      "/"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *
           move      w-all-str-cat (3)    to   w-all-str-alf          .
           move      "."                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                      *-----------------------------------------*
      *                      * Output del nome file                    *
      *                      *-----------------------------------------*
           move      spaces               to   j-rec                  .
      *
           string    "<title>"  delimited by   size
                     w-all-str-cat (1)
                                delimited by   spaces
                     "</title>" delimited by   size
                                          into j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-ini-300.
      *              *-------------------------------------------------*
      *              * Style                                           *
      *              *-------------------------------------------------*
           move      "<!-- STYLE -->"     to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "jquery.dataTables.min.css"
                                          to   w-edt-htm              .
           perform   htm-css-000          thru htm-css-999            .
      *
           move      "buttons.dataTables.min.css"
                                          to   w-edt-htm              .
           perform   htm-css-000          thru htm-css-999            .
      *              *-------------------------------------------------*
      *              * Javascript                                      *
      *              *-------------------------------------------------*
           move      "<!-- JAVASCRIPT -->"
                                          to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "jquery_3.5.1.js"    to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "jquery.dataTables.min.js"
                                          to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "dataTables.buttons.min.js"
                                          to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "jszip.min.js"       to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "buttons.html5.min.js"
                                          to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "buttons.print.min.js"
                                          to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "moment.min.js"      to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "datetime-moment.js" to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *
           move      "t_datatables.js"    to   w-edt-htm              .
           perform   htm-jsc-000          thru htm-jsc-999            .
      *              *-------------------------------------------------*
      *              * Head                                            *
      *              *-------------------------------------------------*
           move      "</head>"            to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-ini-400.
      *              *-------------------------------------------------*
      *              * Body                                            *
      *              *-------------------------------------------------*
           move      "<body>"             to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<center>"           to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-ini-500.
      *              *-------------------------------------------------*
      *              * Table                                           *
      *              *-------------------------------------------------*
           move      "<table id='mxport' class='display nowrap' style='w
      -              "idth:100%'>"        to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-ini-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-ini-999.
       htm-ini-999.
           exit.

      *    *===========================================================*
      *    * Generazione 'HTML'                                        *
      *    *                                                           *
      *    * Subroutine di chiusura                                    *
      *    *-----------------------------------------------------------*
       htm-chi-000.
      *              *-------------------------------------------------*
      *              * Chiusura tbody                                  *
      *              *-------------------------------------------------*
           move      "</tbody>"           to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *              *-------------------------------------------------*
      *              * Footer                                          *
      *              *-------------------------------------------------*
______*    perform   htm-foo-000          thru htm-foo-999            .
      *              *-------------------------------------------------*
      *              * Chiusura table                                  *
      *              *-------------------------------------------------*
           move      "</table>"           to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "</center>"          to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<br>"               to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<hr size=3>"        to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<center>"           to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<address>"          to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<font size=3>&copy TANGRAM - wip</font>" 
                                          to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "</address>"         to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "</center>"          to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "</body>"            to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "</html>"            to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-chi-800.
      *              *-------------------------------------------------*
      *              * Close del file in output                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mcvoju"
                                         using j                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvoju"                         .
       htm-chi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-chi-999.
       htm-chi-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine di scrittura singolo campo                     *
      *    *-----------------------------------------------------------*
       htm-fld-000.
      *              *-------------------------------------------------*
      *              * Test se primo campo della riga                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-fpr-ctr-fld    >    1
                     go to htm-fld-100.
      *                  *---------------------------------------------*
      *                  * Inizializzazione riga 'HTML'                *
      *                  *---------------------------------------------*
           perform   htm-iri-000          thru htm-iri-999            .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su numero riga                     *
      *                      *-----------------------------------------*
           if        w-exe-fpr-ctr-ele    =    zero
                     go to htm-fld-100.
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-exe-fpr-ctr-ele    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Output                                  *
      *                      *-----------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      v-edt                to   w-all-str-cat (2)      .
      *
           if        w-exe-fpr-ctr-ele    >    zero
                     move  "<td align=right>"
                                          to   w-all-str-cat (1)
                     move  "</td>"        to   w-all-str-cat (3)
           else      move  "<th class='sorter-thousands'>"
                                          to   w-all-str-cat (1)
                     move  "</th>"        to   w-all-str-cat (3)      .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-fld-100.
      *              *-------------------------------------------------*
      *              * Test su switch di allineamento                  *
      *              *-------------------------------------------------*
           if        w-edt-all
                    (w-exe-fpr-ctr-fld)   =    "R"
                     go to htm-fld-200
           else      go to htm-fld-400.
       htm-fld-200.
      *              *-------------------------------------------------*
      *              * Campo allineato a destra                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su campo 'header'                      *
      *                  *---------------------------------------------*
           if        w-exe-fpr-ctr-ele    >    zero
                     go to htm-fld-240
           else      go to htm-fld-220.
       htm-fld-220.
      *                  *---------------------------------------------*
      *                  * Campo 'header'                              *
      *                  *---------------------------------------------*
           move      "<th class='sorter-thousands'>" 
                                          to   w-edt-st1              .
           move      "</th>"              to   w-edt-st2              .
           perform   htm-asf-000          thru htm-asf-999            .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     htm-fld-900.
       htm-fld-240.
      *                  *---------------------------------------------*
      *                  * Campo normale                               *
      *                  *---------------------------------------------*
           move      "<td align=right>"   to   w-edt-st1              .
           move      "</td>"              to   w-edt-st2              .
           perform   htm-asf-000          thru htm-asf-999            .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     htm-fld-900.
       htm-fld-400.
      *              *-------------------------------------------------*
      *              * Campo allineato a sinistra                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su campo 'header'                      *
      *                  *---------------------------------------------*
           if        w-exe-fpr-ctr-ele    >    zero
                     go to htm-fld-440
           else      go to htm-fld-420.
       htm-fld-420.
      *                  *---------------------------------------------*
      *                  * Campo 'header'                              *
      *                  *---------------------------------------------*
           move      "<th>"               to   w-edt-st1              .
           move      "</th>"              to   w-edt-st2              .
           perform   htm-asf-000          thru htm-asf-999            .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     htm-fld-900.
       htm-fld-440.
      *                  *---------------------------------------------*
      *                  * Campo normale                               *
      *                  *---------------------------------------------*
           move      "<td>"               to   w-edt-st1              .
           move      "</td>"              to   w-edt-st2              .
           perform   htm-asf-000          thru htm-asf-999            .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     htm-fld-900.
       htm-fld-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-fld-999.
       htm-fld-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine di assemblaggio singolo campo                  *
      *    *-----------------------------------------------------------*
       htm-asf-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-edt-st1            to   w-all-str-cat (1)      .
           move      w-edt-htm            to   w-all-str-cat (2)      .
           move      w-edt-st2            to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-asf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-asf-999.
       htm-asf-999.
           exit.

      *    *===========================================================*
      *    * Generazione 'HTML'                                        *
      *    *                                                           *
      *    * Subroutine per script 'Javascript'                        *
      *    *-----------------------------------------------------------*
       htm-jsc-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<script src='../tmp/jsc/"
                                          to   w-all-str-cat (1)      .
           move      w-edt-htm            to   w-all-str-cat (2)      .
           move      "'></script>"        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-jsc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-jsc-999.
       htm-jsc-999.
           exit.

      *    *===========================================================*
      *    * Generazione 'HTML'                                        *
      *    *                                                           *
      *    * Subroutine per 'Style' CSS                                *
      *    *-----------------------------------------------------------*
       htm-css-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<link rel='stylesheet' href='../tmp/css/"
                                          to   w-all-str-cat (1)      .
           move      w-edt-htm            to   w-all-str-cat (2)      .
           move      "'>"                 to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-css-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-css-999.
       htm-css-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine di inizializzazione riga                       *
      *    *-----------------------------------------------------------*
       htm-iri-000.
      *              *-------------------------------------------------*
      *              * Test se riga di intestazione                    *
      *              *-------------------------------------------------*
           if        w-exe-fpr-ctr-ele    >    zero
                     go to htm-iri-200.
      *              *-------------------------------------------------*
      *              * Riga intestazione                               *
      *              *-------------------------------------------------*
           move      "<thead>"            to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
           move      "<tr>"               to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
           move      "<th class='sorter-thousands'>NR</th>"
                                          to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     htm-iri-900.
       htm-iri-200.
      *              *-------------------------------------------------*
      *              * Riga normale                                    *
      *              *-------------------------------------------------*
           move      "<tr>"               to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-iri-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-iri-999.
       htm-iri-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine di chiusura riga                               *
      *    *-----------------------------------------------------------*
       htm-cri-000.
      *              *-------------------------------------------------*
      *              * Test se riga di intestazione                    *
      *              *-------------------------------------------------*
           if        w-exe-fpr-ctr-ele    >    1
                     go to htm-cri-200.
      *              *-------------------------------------------------*
      *              * Riga intestazione                               *
      *              *-------------------------------------------------*
           move      "</tr>"              to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "</thead>"           to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *
           move      "<tbody>"            to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-cri-900.
       htm-cri-200.
      *              *-------------------------------------------------*
      *              * Riga normale                                    *
      *              *-------------------------------------------------*
           move      "</tr>"              to   j-rec                  .
           perform   htm-ptn-000          thru htm-ptn-999            .
       htm-cri-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-cri-999.
       htm-cri-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine di scrittura                                   *
      *    *-----------------------------------------------------------*
       htm-ptn-000.
      *              *-------------------------------------------------*
      *              * Scrittura su sequenziale                        *
      *              *-------------------------------------------------*
           move      "PN"                 to   j-ope                  .
           call      "swd/mod/prg/obj/mcvoju"
                                         using j                      .
       htm-ptn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     htm-ptn-999.
       htm-ptn-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
