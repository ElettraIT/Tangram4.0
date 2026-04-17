       Identification Division.
       Program-Id.                                 pcge300z           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    mov                 *
      *                                   Fase:    cge300z             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/12/89    *
      *                       Ultima revisione:    NdK del 13/05/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Programma di aggiornamento automatico CGE   *
      *                                                                *
      *================================================================*
      *                                                                *
      *              *-------------------------------------------------*
      *              * Open modulo aggiornamento contabilita' genera-  *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
      *    perform   mdl-agg-cge-opn-000  thru mdl-agg-cge-opn-999    .*
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "OP"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "NO" - Normalizzazione work-area testata e riga movimento      *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "NO"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "N+" - Normalizzazione work-area sola riga movimento           *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "N+"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "WR" - Scrittura testata e prima riga movimento                *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "WR"                       *
      *                 l-cge-300-dat-reg = data registrazione         *
      *                 l-cge-300-num-prt = zero se deve essere pre-   *
      *                                     levato automaticamente ;   *
      *                                     altrimenti il numero pro-  *
      *                                     tocollo da utilizzare ; in *
      *                                     questo caso viene eseguito *
      *                                     automaticamente il delete  *
      *                                     della eventuale registra-  *
      *                                     zione preesistente con tal *
      *                                     numero di protocollo       *
      *                 l-cge-300-cod-cau = codice causale             *
      *                 l-cge-300-des-cau = opzionale; se spaces viene *
      *                                     inserita la descrizione    *
      *                                     letta da tabella causali   *
      *                 l-cge-300-snx-dts = si/no data scadenza        *
      *                                      - N : No                  *
      *                                      - S : Si                  *
      *                 l-cge-300-dat-sca = data scadenza              *
      *                 l-cge-300-dat-doc = opzionale                  *
      *                 l-cge-300-num-doc = opzionale; obbligatorio se *
      *                                     dat-doc presente; viene    *
      *                                     in ogni caso allineato a   *
      *                                     sinistra con soppressione  *
      *                                     di spazi iniziali          *
      *                 l-cge-300-cod-num = opzionale;                 *
      *                 l-cge-300-prt-iva = obbligatorio se registra-  *
      *                                     zione iva fornitori        *
      *                 l-cge-300-cod-pdc = obbligatorio se tip-arc    *
      *                                     uguale a "G"; facoltativo  *
      *                                     se tip-arc uguale a "C" o  *
      *                                     "F" (in tal caso viene let-*
      *                                     to il sottoconto di conta- *
      *                                     bilita' associato al clien-*
      *                                     te o fornitore)            *
      *                 l-cge-300-tip-arc = obbligatorio               *
      *                 l-cge-300-cod-arc = obbligatorio se tip-arc u- *
      *                                     guale "C" o "F"            *
      *                 l-cge-300-com-rig = opzionale                  *
      *                 l-cge-300-dat-rif = opzionale                  *
      *                 l-cge-300-num-rif = opzionale; obbligatorio se *
      *                                     dat-rif presente           *
      *                 l-cge-300-dar-ave = obbligatorio               *
      *                 l-cge-300-imp-mov = obbligatorio               *
      *                 l-cge-300-cst-iva = obbligatorio se registra-  *
      *                                     zione iva                  *
      *                 l-cge-300-tot-doc = opzionale; se zero viene   *
      *                                     assunta la somma degli im- *
      *                                     ponibili + imposte         *
      *                 l-cge-300-flg-rfp = opzionale                  *
      *                 l-cge-300-sgl-num = opzionale                  *
      *                 l-cge-300-dat-acq = opzionale, solo se tip-arc *
      *                                     "F"                        *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "WP" - Scrittura testata e prima riga movimento                *
      *                                                                *
      *        Input  : Come funzione 'WR', ma :                       *
      *                 - il numero protocollo deve essere preparato   *
      *                   dal chiamante                                *
      *                 - non viene eseguito preventivamente alcun de- *
      *                   lete della registrazione preesistente        *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "W+" - Scrittura di righe movimento successive alla prima      *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "W+"                       *
      *                 l-cge-300-cod-pdc = come per operazione "WR"   *
      *                 l-cge-300-tip-arc = come per operazione "WR"   *
      *                 l-cge-300-cod-arc = come per operazione "WR"   *
      *                 l-cge-300-com-rig = come per operazione "WR"   *
      *                 l-cge-300-dat-rif = come per operazione "WR"   *
      *                 l-cge-300-num-rif = come per operazione "WR"   *
      *                 l-cge-300-dar-ave = come per operazione "WR"   *
      *                 l-cge-300-imp-mov = come per operazione "WR"   *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DR" - Richiesta data registrazione minima di CGE, per movi-   *
      *        menti di generale, clienti, fornitori, iva              *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "DR"                       *
      *                                                                *
      *        Output : l-cge-300-dat-reg = data registrazione minima  *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DN" - Richiesta data registrazione minima di CGE, per movi-   *
      *        menti di generale, clienti, fornitori, ma non iva       *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "DN"                       *
      *                                                                *
      *        Output : l-cge-300-dat-reg = data registrazione minima  *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DA" - Richiesta ultima data di stampa registro Iva acquisti   *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "DA"                       *
      *                 l-cge-300-cod-num = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-dat-reg = data di stampa             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DV" - Richiesta ultima data di stampa registro Iva vendite    *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "DV"                       *
      *                 l-cge-300-cod-num = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-dat-reg = data di stampa             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DC" - Richiesta ultima data di stampa registro Iva corrispet. *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "DC"                       *
      *                 l-cge-300-cod-num = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-dat-reg = data di stampa             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "TM" - Test se un movimento di CGE e' modificabile o annulla-  *
      *        bile                                                    *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "TM"                       *
      *                 l-cge-300-dat-reg = obbligatorio               *
      *                 l-cge-300-num-prt = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: movimento modifi-  *
      *                                             cabile o annulla-  *
      *                                             bile               *
      *                                     ?     : movimento cancel-  *
      *                                             lato o inesistente *
      *                                     #     : non modificabile o *
      *                                             annullabile        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "NP" - Richiesta numero protocollo di CGE                      *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "NP"                       *
      *                 l-cge-300-dat-reg = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-num-prt = Nuovo numero protocollo    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DE" - Delete movimento di CGE                                 *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "DE"                       *
      *                 l-cge-300-dat-reg = obbligatorio               *
      *                 l-cge-300-num-prt = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "RT" - Lettura testata movimento registrato                    *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "RT"                       *
      *                 l-cge-300-dat-reg = obbligatorio               *
      *                 l-cge-300-num-prt = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                 l-cge-300-xxx-xxx = tutti i campi di testata   *
      *                                     riempiti                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "CN" - Controllo di unicita' numero documento fornitore        *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "CN"                       *
      *                 l-cge-300-tip-arc = obbligatorio               *
      *                 l-cge-300-cod-arc = obbligatorio               *
      *                 l-cge-300-dat-doc = obbligatorio               *
      *                 l-cge-300-num-doc = obbligatorio               *
      *                 l-cge-300-num-prt = zero se non serve per il   *
      *                                     controllo                  *
      *                                                                *
      *        Output : l-cge-300-exi-sts = spaces: non trovato        *
      *                                     #     : trovato            *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "PI" - Attribuzione protocollo Iva                             *
      *       (Da utilizzare nei batch)                                *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "PI"                       *
      *                 l-cge-300-dat-reg = obbligatorio               *
      *                 l-cge-300-cod-num = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-prt-iva = protocollo Iva             *
      *                 l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "CP" - Controllo di congruenza Protocollo Iva                  *
      *                                                                *
      *        Input  : l-cge-300-tip-ope = "CP"                       *
      *                 l-cge-300-num-prt = obbligatorio               *
      *                 l-cge-300-tip-arc = obbligatorio               *
      *                 l-cge-300-dat-reg = obbligatorio               *
      *                 l-cge-300-cod-num = obbligatorio               *
      *                 l-cge-300-prt-iva = obbligatorio               *
      *                                                                *
      *        Output : l-cge-300-prt-iva = protocollo Iva             *
      *                 l-cge-300-dat-reg = data registrazione         *
      *                 l-cge-300-exi-sts = spaces: tutto OK           *
      *                                     #     : errore             *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record file                                               *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [mgi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgi"                          .
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .

      *    *===========================================================*
      *    * Record file numerazioni                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [datbil]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rndatbil"                       .
      *        *-------------------------------------------------------*
      *        * [giocon]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rngiocon"                       .
      *        *-------------------------------------------------------*
      *        * [pargen]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnpargen"                       .
      *        *-------------------------------------------------------*
      *        * [parcli]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnparcli"                       .
      *        *-------------------------------------------------------*
      *        * [parfnt]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnparfnt"                       .
      *        *-------------------------------------------------------*
      *        * [dtrcge]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rndtrcge"                       .
      *        *-------------------------------------------------------*
      *        * [prtcge]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnprtcge"                       .
      *        *-------------------------------------------------------*
      *        * [prtivf]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rnprtivf"                       .
      *        *-------------------------------------------------------*
      *        * [givacq]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rngivacq"                       .
      *        *-------------------------------------------------------*
      *        * [givven]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rngivven"                       .
      *        *-------------------------------------------------------*
      *        * [givcor]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/num/rec/rngivcor"                       .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Flag di data registrazione minima determinata, per    *
      *        * generale, clienti, fornitori, iva                     *
      *        *-------------------------------------------------------*
           05  w-nol-dtr-flg              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Data registrazione minima per generale, clienti, for- *
      *        * nitori, iva                                           *
      *        *-------------------------------------------------------*
           05  w-nol-dtr-min              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di data registrazione minima determinata, per    *
      *        * generale, clienti, fornitori, ma non iva              *
      *        *-------------------------------------------------------*
           05  w-nol-dtn-flg              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Data registrazione minima per generale, clienti, for- *
      *        * nitori, ma non iva                                    *
      *        *-------------------------------------------------------*
           05  w-nol-dtn-min              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento iva                                    *
      *        *-------------------------------------------------------*
           05  w-nol-tip-iva              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No movimento di bilancio                           *
      *        *-------------------------------------------------------*
           05  w-nol-snx-mob              pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per manipolazione protocollo iva per giornale    *
      *        *-------------------------------------------------------*
           05  w-wrk-pig.
               10  w-wrk-pig-i01          pic  9(02)                  .
               10  w-wrk-pig-i02          pic  9(02)                  .
               10  w-wrk-pig-alf.
                   15  w-wrk-pig-chr
                               occurs 10  pic  x(01)                  .
               10  w-wrk-pig-num          pic  9(11)                  .
               10  w-wrk-pig-red redefines
                   w-wrk-pig-num.
                   15  w-wrk-pig-cif
                               occurs 11  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di riga movimento                           *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-rig              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo 'I'                               *
      *        *-------------------------------------------------------*
           05  I                          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det anno di esercizio                        *
      *        *-------------------------------------------------------*
           05  w-det-ese-cge.
               10  w-det-ese-cge-mce      pic  9(02) value zero       .
               10  w-det-ese-cge-dtr      pic  9(07)                  .
               10  w-det-ese-cge-esa      pic  9(03)                  .
               10  w-det-ese-cge-esm      pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per controllo di congruenza fra data e numero   *
      *    * su [mgi]                                                  *
      *    *-----------------------------------------------------------*
       01  w-cdn-mgi.
      *        *-------------------------------------------------------*
      *        * Work area locale                                      *
      *        *-------------------------------------------------------*
           05  w-cdn-mgi-wrk.
      *            *---------------------------------------------------*
      *            * Comodo per data per Start                         *
      *            *---------------------------------------------------*
               10  w-cdn-mgi-wrk-wds      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data                                              *
      *            *---------------------------------------------------*
               10  w-cdn-mgi-inp-dat      pic  9(07)                  .
               10  w-cdn-mgi-inp-dat-r redefines
                   w-cdn-mgi-inp-dat.
                   15  w-cdn-mgi-inp-saa  pic  9(03)                  .
                   15  w-cdn-mgi-inp-mes  pic  9(02)                  .
                   15  w-cdn-mgi-inp-gio  pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Agg                               *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Work per Agg [mgs]                                    *
      *        *-------------------------------------------------------*
           05  w-agg-mgs.
               10  w-agg-mgs-ese-bil      pic  9(03)                  .
               10  w-agg-mgs-dat-reg      pic  9(07)                  .
               10  w-agg-mgs-tip-arc      pic  x(01)                  .
               10  w-agg-mgs-cod-arc      pic  9(07)                  .
               10  w-agg-mgs-cod-pdc      pic  9(07)                  .
               10  w-agg-mgs-tip-agg      pic  x(01)                  .
               10  w-agg-mgs-snx-mob      pic  x(01)                  .
               10  w-agg-mgs-dar-ave      pic  x(01)                  .
               10  w-agg-mgs-imp-mov      pic s9(13)                  .

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
      *    * Area di comunicazione per movimento di contabilita'       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgl"                   .

      ******************************************************************
       Procedure Division                using l-cge-300              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   l-cge-300-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Write testata e prima riga movimento        *
      *                  *---------------------------------------------*
           if        l-cge-300-tip-ope    =    "WR" or
                     l-cge-300-tip-ope    =    "WP"
                     perform wtr-000      thru wtr-999
      *                  *---------------------------------------------*
      *                  * Write righe movimento successive alla prima *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "W+"
                     perform wrg-000      thru wrg-999
      *                  *---------------------------------------------*
      *                  * Rilascio numero protocollo di CGE           *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "NP"
                     perform rnp-000      thru rnp-999
      *                  *---------------------------------------------*
      *                  * Test su movimento                           *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "TM"
                     perform tsm-000      thru tsm-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione testata e prima riga movi-  *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "NO"
                     perform ntr-000      thru ntr-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione righe movimento successive  *
      *                  * alla prima                                  *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "N+"
                     perform nrg-000      thru nrg-999
      *                  *---------------------------------------------*
      *                  * Delete movimento                            *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "DE"
                     perform del-000      thru del-999
      *                  *---------------------------------------------*
      *                  * Richiesta data registrazione CGE, per gene- *
      *                  * rale, clienti, fornitori, iva               *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "DR"
                     perform drm-000      thru drm-999
      *                  *---------------------------------------------*
      *                  * Richiesta data registrazione CGE, per gene- *
      *                  * rale, clienti, fornitori, ma non iva        *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "DN"
                     perform dnm-000      thru dnm-999
      *                  *---------------------------------------------*
      *                  * Richiesta data ultima stampa registro Iva   *
      *                  * acquisti                                    *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "DA"
                     perform dsa-000      thru dsa-999
      *                  *---------------------------------------------*
      *                  * Richiesta data ultima stampa registro Iva   *
      *                  * vendite                                     *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "DV"
                     perform dsv-000      thru dsv-999
      *                  *---------------------------------------------*
      *                  * Richiesta data ultima stampa registro Iva   *
      *                  * corrispettivi                               *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "DC"
                     perform dsc-000      thru dsc-999
      *                  *---------------------------------------------*
      *                  * Lettura testata movimento                   *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "RT"
                     perform rtm-000      thru rtm-999
      *                  *---------------------------------------------*
      *                  * Controllo unicita' documento fornitore      *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "CN"
                     perform cud-000      thru cud-999
      *                  *---------------------------------------------*
      *                  * Determinazione protocollo Iva fornitore     *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "PI"
                     perform pif-000      thru pif-999
      *                  *---------------------------------------------*
      *                  * Controllo di congruenza Protocollo Iva      *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "CP"
                     perform cdn-000      thru cdn-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-cge-300-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999                .
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
      *              *-------------------------------------------------*
      *              * Lettura numerazioni                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dati relativi alla chiusura bilancio        *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indatbil"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-bil             .
      *              *-------------------------------------------------*
      *              * Apertura file principali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mgt] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mgr] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mgi] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mgs] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Apertura file ausiliari                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura file                         [cli] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [fnt] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [zcc] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * Apertura file numerazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura numerazione               [prtcge] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *                  *---------------------------------------------*
      *                  * [prtivf]                                    *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
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
      *              * Chiusura file principali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mgt] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mgr] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mgi] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mgs] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *              *-------------------------------------------------*
      *              * Chiusura file ausiliari                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [cli] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [fnt] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [zcc] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *              *-------------------------------------------------*
      *              * Chiusura file numerazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura numerazione               [prtcge] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *                  *---------------------------------------------*
      *                  * [prtivf]                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
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
                     move  spaces         to   l-cge-300-exi-sts
           else      move  "#"            to   l-cge-300-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione sola riga movimento                       *
      *    *-----------------------------------------------------------*
       nrg-000.
           move      zero                 to   l-cge-300-cod-pdc      .
           move      spaces               to   l-cge-300-tip-arc      .
           move      zero                 to   l-cge-300-cod-arc      .
           move      spaces               to   l-cge-300-com-rig      .
           move      zero                 to   l-cge-300-dat-rif      .
           move      spaces               to   l-cge-300-num-rif      .
           move      spaces               to   l-cge-300-dar-ave      .
           move      zero                 to   l-cge-300-imp-mov      .
       nrg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione link per testata e per riga               *
      *    *-----------------------------------------------------------*
       ntr-000.
      *        *-------------------------------------------------------*
      *        * Normalizzazione link completa                         *
      *        *-------------------------------------------------------*
           move      spaces               to   l-cge-300              .
           move      zero                 to   l-cge-300-dat-reg      .
           move      zero                 to   l-cge-300-num-prt      .
           move      zero                 to   l-cge-300-cod-cau      .
           move      spaces               to   l-cge-300-des-cau      .
           move      spaces               to   l-cge-300-snx-dts      .
           move      zero                 to   l-cge-300-dat-sca      .
           move      zero                 to   l-cge-300-dat-doc      .
           move      spaces               to   l-cge-300-num-doc      .
           move      zero                 to   l-cge-300-cod-num      .
           move      zero                 to   l-cge-300-prt-iva      .
           move      zero                 to   l-cge-300-cod-pdc      .
           move      spaces               to   l-cge-300-tip-arc      .
           move      zero                 to   l-cge-300-cod-arc      .
           move      spaces               to   l-cge-300-com-rig      .
           move      zero                 to   l-cge-300-dat-rif      .
           move      spaces               to   l-cge-300-num-rif      .
           move      spaces               to   l-cge-300-dar-ave      .
           move      zero                 to   l-cge-300-imp-mov      .
           move      zero                 to   I                      .
       ntr-100.
           add       1                    to   I                      .
           if        I                    >    6
                     go to  ntr-200.
           move      zero                 to   l-cge-300-ibl-iva (I)  .
           move      zero                 to   l-cge-300-cod-iva (I)  .
           move      zero                 to   l-cge-300-imp-iva (I)  .
           go to     ntr-100.
       ntr-200.
           move      zero                 to   l-cge-300-tot-doc      .
      *        *-------------------------------------------------------*
      *        * Normalizzazione area no-link completa                 *
      *        *-------------------------------------------------------*
           move      spaces               to   w-nol-tip-iva          .
           move      spaces               to   w-nol-snx-mob          .
       ntr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura testata e prima riga movimento                  *
      *    *-----------------------------------------------------------*
       wtr-000.
      *              *-------------------------------------------------*
      *              * Lettura tabella causali e determinazione para-  *
      *              * metri ad essa associati                         *
      *              *-------------------------------------------------*
           perform   let-cod-cau-000      thru let-cod-cau-999        .
      *              *-------------------------------------------------*
      *              * Lettura archivio clienti o fornitori per deter- *
      *              * minazione sottoconto associato                  *
      *              *-------------------------------------------------*
           perform   let-cod-cof-000      thru let-cod-cof-999        .
      *              *-------------------------------------------------*
      *              * Se numero protocollo gia' esistente vuol dire   *
      *              * che e' stato passato in link-area               *
      *              *-------------------------------------------------*
           if        l-cge-300-num-prt    =    zero
                     go to  wtr-100.
      *                  *---------------------------------------------*
      *                  * Richiamo operazione di delete registrazione,*
      *                  * ma solo se tipo operazione 'WR' e non 'WP'  *
      *                  *---------------------------------------------*
           if        l-cge-300-tip-ope    =    "WR"
                     perform   del-000    thru del-999                .
           go to     wtr-200.
       wtr-100.
      *              *-------------------------------------------------*
      *              * Attribuzione numero protocollo                  *
      *              *-------------------------------------------------*
           perform   att-num-prt-000      thru att-num-prt-999        .
       wtr-200.
      *              *-------------------------------------------------*
      *              * Trattamento file [mgr]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore numero riga mo-  *
      *                  * vimento                                     *
      *                  *---------------------------------------------*
           move      00100                to   w-wrk-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Write record [mgr]                          *
      *                  *---------------------------------------------*
           perform   wrt-rec-mgr-000      thru wrt-rec-mgr-999        .
      *              *-------------------------------------------------*
      *              * Trattamento file [mgt]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Write record [mgt]                          *
      *                  *---------------------------------------------*
           perform   wrt-rec-mgt-000      thru wrt-rec-mgt-999        .
      *                  *---------------------------------------------*
      *                  * Write record [mgi]                          *
      *                  *---------------------------------------------*
           perform   wrt-rec-mgi-000      thru wrt-rec-mgi-999        .
       wtr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura riga movimento successiva alla prima            *
      *    *-----------------------------------------------------------*
       wrg-000.
      *              *-------------------------------------------------*
      *              * Lettura archivio clienti o fornitori per deter- *
      *              * minazione sottoconto associato                  *
      *              *-------------------------------------------------*
           perform   let-cod-cof-000      thru let-cod-cof-999        .
      *              *-------------------------------------------------*
      *              * Trattamento file [mgr]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore numero riga movimento  *
      *                  *---------------------------------------------*
           add       00100                to   w-wrk-ctr-rig          .
      *                      *-----------------------------------------*
      *                      * Test se oltre il numero massimo consen- *
      *                      * tito                                    *
      *                      *-----------------------------------------*
           if        w-wrk-ctr-rig        not  < 99900
                     go to  wrg-900.
      *                  *---------------------------------------------*
      *                  * Write record [mgr]                          *
      *                  *---------------------------------------------*
           perform   wrt-rec-mgr-000      thru wrt-rec-mgr-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     wrg-999.
       wrg-900.
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo numero di righe per *
      *                      * registrazione                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Registrazione con numero righe oltre il massimo co
      -              "nsentito !"         to   l-cge-300-msg-exi      .
      *                          *-------------------------------------*
      *                          * Delete intera registrazione         *
      *                          *-------------------------------------*
           perform   del-000              thru del-999                .
      *                          *-------------------------------------*
      *                          * Segnale di errore                   *
      *                          *-------------------------------------*
           move      "#"                  to   l-cge-300-exi-sts      .
       wrg-999.
           exit.

      *    *===========================================================*
      *    * Data registrazione minima di CGE, per generale, clienti,  *
      *    * fornitori, iva                                            *
      *    *-----------------------------------------------------------*
       drm-000.
      *              *-------------------------------------------------*
      *              * Determinazione data registrazione minima        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se data gia' determinata               *
      *                  *---------------------------------------------*
           if        w-nol-dtr-flg        not  = spaces
                     go to drm-800.
      *                  *---------------------------------------------*
      *                  * Flag di data determinata                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-nol-dtr-flg          .
      *                  *---------------------------------------------*
      *                  * Lettura numerazione data registrazione CGE  *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indtrcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dtr-cge             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indtrcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dtr-cge             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indtrcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dtr-cge             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/indtrcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dtr-cge             .
      *                  *---------------------------------------------*
      *                  * Se data a zero : non si calcola il giorno   *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           move      rn-dtr-cge-ult-dat   to   s-dat                  .
           if        s-dat                =    zero
                     go to drm-500.
      *                  *---------------------------------------------*
      *                  * Calcolo del giorno successivo               *
      *                  *---------------------------------------------*
           move      rn-dtr-cge-ult-dat   to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to drm-500.
           move      1                    to   s-gio                  .
           add       1                    to   s-mes                  .
           if        s-mes                not  > 12
                     go to drm-500.
           move      1                    to   s-mes                  .
           add       1                    to   s-saa                  .
       drm-500.
      *                  *---------------------------------------------*
      *                  * Data determinata in work di salvataggio     *
      *                  *---------------------------------------------*
           move      s-dat                to   w-nol-dtr-min          .
       drm-800.
      *              *-------------------------------------------------*
      *              * Data registrazione minima in area di uscita     *
      *              *-------------------------------------------------*
           move      w-nol-dtr-min        to   l-cge-300-dat-reg      .
       drm-999.
           exit.

      *    *===========================================================*
      *    * Data registrazione minima di CGE, per generale, clienti,  *
      *    * fornitori, ma non iva                                     *
      *    *-----------------------------------------------------------*
       dnm-000.
      *              *-------------------------------------------------*
      *              * Determinazione data registrazione minima        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se data gia' determinata               *
      *                  *---------------------------------------------*
           if        w-nol-dtn-flg        not  = spaces
                     go to dnm-800.
      *                  *---------------------------------------------*
      *                  * Flag di data determinata                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-nol-dtn-flg          .
       dnm-100.
      *                  *---------------------------------------------*
      *                  * Numerazione per : giornale contabile        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura numerazione                     *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingiocon"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-gio-con             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione data                         *
      *                  *---------------------------------------------*
           move      rn-gio-con-dat-stp   to   w-nol-dtn-min          .
       dnm-200.
      *                  *---------------------------------------------*
      *                  * Numerazione per : partitario di generale    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura numerazione                     *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inpargen"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-gen             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inpargen"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-gen             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inpargen"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-gen             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inpargen"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-gen             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione data                         *
      *                  *---------------------------------------------*
           if        rn-par-gen-dat-fin   >    w-nol-dtn-min
                     move  rn-par-gen-dat-fin
                                          to   w-nol-dtn-min          .
       dnm-300.
      *                  *---------------------------------------------*
      *                  * Numerazione per : partitario clienti        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura numerazione                     *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-cli             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-cli             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-cli             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-cli             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione data                         *
      *                  *---------------------------------------------*
           if        rn-par-cli-dat-fin   >    w-nol-dtn-min
                     move  rn-par-cli-dat-fin
                                          to   w-nol-dtn-min          .
       dnm-400.
      *                  *---------------------------------------------*
      *                  * Numerazione per : partitario fornitori      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura numerazione                     *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparfnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-fnt             .
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparfnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-fnt             .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparfnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-fnt             .
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inparfnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-par-fnt             .
      *                  *---------------------------------------------*
      *                  * Memorizzazione data                         *
      *                  *---------------------------------------------*
           if        rn-par-fnt-dat-fin   >    w-nol-dtn-min
                     move  rn-par-fnt-dat-fin
                                          to   w-nol-dtn-min          .
       dnm-600.
      *                  *---------------------------------------------*
      *                  * Calcolo del giorno successivo               *
      *                  *---------------------------------------------*
           if        w-nol-dtn-min        =    zero
                     go to dnm-800.
           move      w-nol-dtn-min        to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to dnm-650.
           move      1                    to   s-gio                  .
           add       1                    to   s-mes                  .
           if        s-mes                not  > 12
                     go to dnm-650.
           move      1                    to   s-mes                  .
           add       1                    to   s-saa                  .
       dnm-650.
           move      s-dat                to   w-nol-dtn-min          .
       dnm-800.
      *              *-------------------------------------------------*
      *              * Data registrazione minima in area di uscita     *
      *              *-------------------------------------------------*
           move      w-nol-dtn-min        to   l-cge-300-dat-reg      .
       dnm-999.
           exit.

      *    *===========================================================*
      *    * Data ultima stampa registro Iva acquisti                  *
      *    *-----------------------------------------------------------*
       dsa-000.
      *              *-------------------------------------------------*
      *              * Apertura numerazione                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivacq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-acq             .
      *              *-------------------------------------------------*
      *              * Normalizzazione numerazione                     *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivacq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-acq             .
      *              *-------------------------------------------------*
      *              * Lettura file numerazioni giornali iva acquisti  *
      *              *-------------------------------------------------*
           move      l-cge-300-cod-num    to   rn-giv-acq-cod-num     .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivacq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-acq             .
           if        f-sts                not  = e-not-err
                     move   zero          to   rn-giv-acq-dat-stp     .
           move      rn-giv-acq-dat-stp   to   l-cge-300-dat-reg      .
      *              *-------------------------------------------------*
      *              * Chiusura numerazione                            *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivacq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-acq             .
       dsa-999.
           exit.

      *    *===========================================================*
      *    * Data ultima stampa registro Iva vendite                   *
      *    *-----------------------------------------------------------*
       dsv-000.
      *              *-------------------------------------------------*
      *              * Apertura numerazione                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivven"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-ven             .
      *              *-------------------------------------------------*
      *              * Normalizzazione numerazione                     *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivven"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-ven             .
      *              *-------------------------------------------------*
      *              * Lettura file numerazioni giornali iva vendite   *
      *              *-------------------------------------------------*
           move      l-cge-300-cod-num    to   rn-giv-ven-cod-num     .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivven"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-ven             .
           if        f-sts                not  = e-not-err
                     move   zero          to   rn-giv-ven-dat-stp     .
           move      rn-giv-ven-dat-stp   to   l-cge-300-dat-reg      .
      *              *-------------------------------------------------*
      *              * Chiusura numerazione                            *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivven"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-ven             .
       dsv-999.
           exit.

      *    *===========================================================*
      *    * Data ultima stampa registro Iva corrispettivi             *
      *    *-----------------------------------------------------------*
       dsc-000.
      *              *-------------------------------------------------*
      *              * Apertura numerazione                            *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivcor"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-cor             .
      *              *-------------------------------------------------*
      *              * Normalizzazione numerazione                     *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivcor"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-cor             .
      *              *-------------------------------------------------*
      *              * Lettura file numerazioni giornali iva corrisp.  *
      *              *-------------------------------------------------*
           move      l-cge-300-cod-num    to   rn-giv-cor-cod-num     .
           move      "RD"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivcor"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-cor             .
           if        f-sts                not  = e-not-err
                     move   zero          to   rn-giv-cor-dat-stp     .
           move      rn-giv-cor-dat-stp   to   l-cge-300-dat-reg      .
      *              *-------------------------------------------------*
      *              * Chiusura numerazione                            *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/ingivcor"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-giv-cor             .
       dsc-999.
           exit.

      *    *===========================================================*
      *    * Lettura testata movimento contabile                       *
      *    *-----------------------------------------------------------*
       rtm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       rtm-100.
      *              *-------------------------------------------------*
      *              * Lettura preliminare                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record file [mgt]                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      l-cge-300-dat-reg    to   rf-mgt-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgt-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                      *-----------------------------------------*
      *                      * Se record non trovato uscita con segna- *
      *                      * le di registrazione non trovata         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   l-cge-300-exi-sts
                     go to  rtm-999.
       rtm-200.
      *              *-------------------------------------------------*
      *              * Valori da testata ad output                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice causale                              *
      *                  *---------------------------------------------*
           move      rf-mgt-cod-cau       to   l-cge-300-cod-cau      .
      *                  *---------------------------------------------*
      *                  * Descrizione causale                         *
      *                  *---------------------------------------------*
           move      rf-mgt-des-cau       to   l-cge-300-des-cau      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      rf-mgt-dat-doc       to   l-cge-300-dat-doc      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      rf-mgt-num-doc       to   l-cge-300-num-doc      .
      *                  *---------------------------------------------*
      *                  * Codice numerazione per giornale iva         *
      *                  *---------------------------------------------*
           move      rf-mgt-cod-num       to   l-cge-300-cod-num      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo iva                       *
      *                  *---------------------------------------------*
           move      rf-mgt-prt-iva       to   l-cge-300-prt-iva      .
       rtm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rtm-999.
       rtm-999.
           exit.

      *    *===========================================================*
      *    * Controllo unicita' documento fornitore                    *
      *    *-----------------------------------------------------------*
       cud-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Su tipo archivio                            *
      *                  *---------------------------------------------*
           if        l-cge-300-tip-arc    not  = "F"
                     go to cud-900.
      *                  *---------------------------------------------*
      *                  * Su codice archivio                          *
      *                  *---------------------------------------------*
           if        l-cge-300-cod-arc    =    zero
                     go to cud-900.
      *                  *---------------------------------------------*
      *                  * Su data documento                           *
      *                  *---------------------------------------------*
           if        l-cge-300-dat-doc    =    zero
                     go to cud-900.
      *                  *---------------------------------------------*
      *                  * Su numero documento                         *
      *                  *---------------------------------------------*
           if        l-cge-300-num-doc    =    spaces
                     go to cud-900.
       cud-100.
      *              *-------------------------------------------------*
      *              * Start su file [mgr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "TARDOC"             to   f-key                  .
           move      l-cge-300-tip-arc    to   rf-mgr-tip-arc         .
           move      l-cge-300-dat-doc    to   rf-mgr-dat-doc         .
           move      l-cge-300-num-doc    to   rf-mgr-num-doc         .
           move      l-cge-300-cod-arc    to   rf-mgr-cod-arc         .
           move      zero                 to   rf-mgr-dat-reg         .
           move      zero                 to   rf-mgr-num-prt         .
           move      zero                 to   rf-mgr-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Se start non valida : ad uscita             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cud-900.
       cud-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [mgr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cud-900.
       cud-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il massimo                        *
      *              *-------------------------------------------------*
           if        rf-mgr-tip-arc       not  = l-cge-300-tip-arc or
                     rf-mgr-dat-doc       not  = l-cge-300-dat-doc or
                     rf-mgr-num-doc       not  = l-cge-300-num-doc or
                     rf-mgr-cod-arc       not  = l-cge-300-cod-arc
                     go to cud-900.
       cud-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [mgr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        l-cge-300-num-prt    =    zero
                     go to cud-500.
           if        rf-mgr-num-prt       =    l-cge-300-num-prt
                     go to cud-200.
       cud-500.
      *                  *---------------------------------------------*
      *                  * Test su tipo movimento Iva                  *
      *                  *---------------------------------------------*
           if        rf-mgr-tip-iva       not  = "0" and
                     rf-mgr-tip-iva       not  = "4" and
                     rf-mgr-tip-iva       not  = "5" and
                     rf-mgr-tip-iva       not  = "D" and
                     rf-mgr-tip-iva       not  = "E"
                     go to cud-200.
       cud-600.
      *              *-------------------------------------------------*
      *              * Set del flag a 'trovato'                        *
      *              *-------------------------------------------------*
           move      "#"                  to   l-cge-300-exi-sts      .
       cud-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cud-999.
       cud-999.
           exit.

      *    *===========================================================*
      *    * Determinazione protocollo Iva fornitore                   *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : l-cge-300-tip-ope = "PI"                         *
      *    *          l-cge-300-dat-reg = obbligatoria                 *
      *    *          l-cge-300-cod-num = obbligatorio                 *
      *    *                                                           *
      *    * Output : l-cge-300-prt-iva = protocollo Iva               *
      *    *          l-cge-300-exi-sts = spaces: tutto OK             *
      *    *                              #     : errore               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       pif-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      zero                 to   l-cge-300-prt-iva      .
       pif-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
      *              *-------------------------------------------------*
      *              * Preparazione parametri                          *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   s-dat                  .
           move      s-saa                to   rn-prt-ivf-saa-dtr     .
           move      l-cge-300-cod-num    to   rn-prt-ivf-cod-num     .
      *              *-------------------------------------------------*
      *              * Lettura numerazione                             *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to pif-300.
       pif-200.
      *              *-------------------------------------------------*
      *              * Se numerazione non esistente la si crea norma-  *
      *              * lizzata a zero                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione protocollo                  *
      *                  *---------------------------------------------*
           move      zero                 to   rn-prt-ivf-num-prt     .
      *                  *---------------------------------------------*
      *                  * Scrittura protocollo                        *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
      *                  *---------------------------------------------*
      *                  * A rilettura                                 *
      *                  *---------------------------------------------*
           go to     pif-100.
       pif-300.
      *              *-------------------------------------------------*
      *              * Se numerazione esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero protocollo                *
      *                  *---------------------------------------------*
           add       1                    to   rn-prt-ivf-num-prt     .
           if        rn-prt-ivf-num-prt   =    zero
                     move  1              to   rn-prt-ivf-num-prt     .
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
      *                  *---------------------------------------------*
      *                  * Se errori ripete l'intera operazione        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pif-100.
      *                  *---------------------------------------------*
      *                  * Unlock                                      *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtivf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-ivf             .
       pif-800.
      *              *-------------------------------------------------*
      *              * Valore determinato                              *
      *              *-------------------------------------------------*
           move      rn-prt-ivf-num-prt   to   l-cge-300-prt-iva      .
           if        rn-prt-ivf-num-prt   =    zero
                     move  "#"            to   l-cge-300-exi-sts      .
       pif-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pif-999.
       pif-999.
           exit.

      *    *===========================================================*
      *    * Test su modificabilita' movimento di CGE                  *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : l-cge-300-tip-ope = "TM"                         *
      *    *          l-cge-300-dat-reg = obbligatoria                 *
      *    *          l-cge-300-num-prt = obbligatorio                 *
      *    *                                                           *
      *    * Output : l-cge-300-exi-sts = spaces: movimento modifica-  *
      *    *                                      bile o annullabile   *
      *    *                              ?     : movimento cancellato *
      *    *                                      o inesistente        *
      *    *                              #     : movimento non modi-  *
      *    *                                      ficabile o annulla-  *
      *    *                                      bile                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       tsm-000.
      *              *-------------------------------------------------*
      *              * Lettura record file [mgt]                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      l-cge-300-dat-reg    to   rf-mgt-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgt-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record non trovato uscita con segna- *
      *                      * le di registrazione non trovata         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "?"           to   l-cge-300-exi-sts
                     go to  tsm-999.
      *                      *-----------------------------------------*
      *                      * Se flag di stampa giornale contabile    *
      *                      * non a spazi: segnale di registrazione   *
      *                      * non modificabile                        *
      *                      *-----------------------------------------*
           if        rf-mgt-flg-gio       not  = spaces
                     move   "#"           to   l-cge-300-exi-sts
                     go to  tsm-999.
       tsm-200.
      *              *-------------------------------------------------*
      *              * Test su flag di stampa giornale iva             *
      *              *-------------------------------------------------*
           if        rf-mgt-tip-iva       =    "0" or
                     rf-mgt-tip-iva       =    spaces
                     go to  tsm-999.
      *              *-------------------------------------------------*
      *              * Lettura record file [mgi]                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATPRT"             to   f-key                  .
           move      l-cge-300-dat-reg    to   rf-mgi-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record non trovato uscita con segna- *
      *                      * le di registrazione non modificabile    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move   "#"           to   l-cge-300-exi-sts
                     go to  tsm-999.
      *                      *-----------------------------------------*
      *                      * Se flag di stampa giornale iva non a    *
      *                      * spazi: segnale di registrazione non     *
      *                      * modificabile                            *
      *                      *-----------------------------------------*
           if        rf-mgi-flg-gio       not  = spaces
                     move   "#"           to   l-cge-300-exi-sts
                     go to  tsm-999.
       tsm-999.
           exit.

      *    *===========================================================*
      *    * Rilascio numero protocollo di CGE                         *
      *    *-----------------------------------------------------------*
       rnp-000.
      *              *-------------------------------------------------*
      *              * Attribuzione numero protocollo                  *
      *              *-------------------------------------------------*
           perform   att-num-prt-000      thru att-num-prt-999        .
       rnp-999.
           exit.

      *    *===========================================================*
      *    * Delete movimento di CGE                                   *
      *    *-----------------------------------------------------------*
       del-000.
      *              *-------------------------------------------------*
      *              * Cancellazione righe                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mgr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATREG"             to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      l-cge-300-dat-reg    to   rf-mgr-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgr-num-prt         .
           move      zero                 to   rf-mgr-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                      *-----------------------------------------*
      *                      * Se start non valida : a cancellazione   *
      *                      * testata                                 *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-500.
       del-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [mgr]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *                      *-----------------------------------------*
      *                      * Se 'at end' : a cancellazione testata   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  del-500.
      *                      *-----------------------------------------*
      *                      * Test se oltre il massimo                *
      *                      *-----------------------------------------*
           if        rf-mgr-dat-reg       not  = l-cge-300-dat-reg or
                     rf-mgr-num-prt       not  = l-cge-300-num-prt
                     go to  del-500.
      *                  *---------------------------------------------*
      *                  * Delete record file [mgr]                    *
      *                  *---------------------------------------------*
           perform   del-rec-mgr-000      thru del-rec-mgr-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura riga corpo successiva     *
      *                  *---------------------------------------------*
           go to     del-100.
       del-500.
      *              *-------------------------------------------------*
      *              * Cancellazione testata file [mgt]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Delete record [mgt]                         *
      *                  *---------------------------------------------*
           perform   del-rec-mgt-000      thru del-rec-mgt-999        .
      *                  *---------------------------------------------*
      *                  * Delete record [mgi]                         *
      *                  *---------------------------------------------*
           perform   del-rec-mgi-000      thru del-rec-mgi-999        .
       del-999.
           exit.

      *    *===========================================================*
      *    * Controllo di congruenza tra data e numero per [mgi]       *
      *    *-----------------------------------------------------------*
       cdn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   l-cge-300-exi-sts      .
      *              *-------------------------------------------------*
      *              * Data registrazione in data di comodo            *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   w-cdn-mgi-inp-dat      .
      *              *-------------------------------------------------*
      *              * Test su valori in input                         *
      *              *-------------------------------------------------*
           if        l-cge-300-tip-arc    =    spaces or
                     l-cge-300-dat-reg    =    zero   or
                     l-cge-300-prt-iva    =    zero
                     go to cdn-900.
       cdn-100.
      *              *=================================================*
      *              * Test di congruenza con il numero successivo     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   cdn-tns-000          thru cdn-tns-999            .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        l-cge-300-exi-sts    not  = spaces
                     go to cdn-900.
       cdn-500.
      *              *=================================================*
      *              * Test di congruenza con il numero precedente     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   cdn-tnp-000          thru cdn-tnp-999            .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        l-cge-300-exi-sts    not  = spaces
                     go to cdn-900.
       cdn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cdn-999.
       cdn-999.
           exit.

      *    *===========================================================*
      *    * Controllo di congruenza tra data e numero per [mgi]       *
      *    *                                                           *
      *    * Subroutine per Test di congruenza con numero successivo   *
      *    *-----------------------------------------------------------*
       cdn-tns-000.
      *              *-------------------------------------------------*
      *              * Data per start pari a data passata              *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   w-cdn-mgi-wrk-wds      .
       cdn-tns-100.
      *              *-------------------------------------------------*
      *              * Start su [mgi]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DATDOC    "         to   f-key                  .
           move      l-cge-300-tip-arc    to   rf-mgi-tip-rec         .
           move      w-cdn-mgi-wrk-wds    to   rf-mgi-dat-reg         .
           move      l-cge-300-cod-num    to   rf-mgi-cod-num         .
      *
           if        w-cdn-mgi-wrk-wds    =    l-cge-300-dat-reg
                     move  l-cge-300-prt-iva
                                          to   rf-mgi-prt-iva
           else      move  zero           to   rf-mgi-prt-iva         .
      *
           move      zero                 to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdn-tns-900.
       cdn-tns-200.
      *              *-------------------------------------------------*
      *              * Read Next su [mgi]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Se 'At End' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdn-tns-900.
       cdn-tns-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo record                         *
      *                  *---------------------------------------------*
           if        rf-mgi-tip-rec       not  = l-cge-300-tip-arc
                     go to cdn-tns-900.
       cdn-tns-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice numerazione iva della registra-   *
      *                  * zione pari a quella passata, continuazione  *
      *                  * test                                        *
      *                  *---------------------------------------------*
           if        rf-mgi-cod-num       =    l-cge-300-cod-num
                     go to cdn-tns-520.
      *                  *---------------------------------------------*
      *                  * Se cambio di numerazione iva                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se inferiore a quella passata, a Start  *
      *                      * con data pari a quella della registra-  *
      *                      * zione letta                             *
      *                      *-----------------------------------------*
           if        rf-mgi-cod-num       <    l-cge-300-cod-num
                     move  rf-mgi-dat-reg to   w-cdn-mgi-wrk-wds
                     go to cdn-tns-100.
      *                      *-----------------------------------------*
      *                      * Se superiore a quella passata           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione giorno successivo    *
      *                          * alla data registrazione letta       *
      *                          *-------------------------------------*
           move      rf-mgi-dat-reg       to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                =    spaces
                     go to cdn-tns-420.
           move      1                    to   s-gio                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     move  1              to   s-mes
                     add   1              to   s-saa                  .
       cdn-tns-420.
      *                          *-------------------------------------*
      *                          * A Start con data pari a quella del  *
      *                          * giorno successivo alla data regi-   *
      *                          * strazione letta                     *
      *                          *-------------------------------------*
           move      s-dat                to   w-cdn-mgi-wrk-wds
      *                          *-------------------------------------*
      *                          * A Start                             *
      *                          *-------------------------------------*
           go to     cdn-tns-100.
       cdn-tns-520.
      *                  *---------------------------------------------*
      *                  * Test su numero protocollo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se numero protocollo di contabilita'    *
      *                      * letto pari a quello passato : riciclo   *
      *                      *-----------------------------------------*
           if        l-cge-300-num-prt    not  = zero and
                     rf-mgi-num-prt       =    l-cge-300-num-prt
                     go to cdn-tns-200.
      *                      *-----------------------------------------*
      *                      * Se data registrazione letta appartenen- *
      *                      * te all'anno successivo a quello della   *
      *                      * data passata : test superato            *
      *                      *-----------------------------------------*
           move      rf-mgi-dat-reg       to   s-dat                  .
           if        s-saa                >    w-cdn-mgi-inp-saa
                     go to cdn-tns-900.
      *                      *-----------------------------------------*
      *                      * Se numero protocollo letto non maggiore *
      *                      * di quello passato : test non superato   *
      *                      *-----------------------------------------*
           if        rf-mgi-prt-iva       not  > l-cge-300-prt-iva
                     move  rf-mgi-prt-iva to   l-cge-300-prt-iva
                     move  rf-mgi-dat-reg to   l-cge-300-dat-reg
                     move  "#"            to   l-cge-300-exi-sts
                     go to cdn-tns-900.
       cdn-tns-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cdn-tns-999.
       cdn-tns-999.
           exit.

      *    *===========================================================*
      *    * Controllo di congruenza tra data e numero per [mgi]       *
      *    *                                                           *
      *    * Subroutine per Test di congruenza con numero precedente   *
      *    *-----------------------------------------------------------*
       cdn-tnp-000.
      *              *-------------------------------------------------*
      *              * Data per start pari a data passata              *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   w-cdn-mgi-wrk-wds      .
       cdn-tnp-100.
      *              *-------------------------------------------------*
      *              * Start su [mgi]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NG"                 to   f-cfr                  .
           move      "DATDOC    "         to   f-key                  .
           move      l-cge-300-tip-arc    to   rf-mgi-tip-rec         .
           move      w-cdn-mgi-wrk-wds    to   rf-mgi-dat-reg         .
           move      l-cge-300-cod-num    to   rf-mgi-cod-num         .
           if        w-cdn-mgi-wrk-wds    =    l-cge-300-dat-reg
                     move  l-cge-300-prt-iva
                                          to   rf-mgi-prt-iva
           else      move  99999999999    to   rf-mgi-prt-iva         .
           move      9999999              to   rf-mgi-num-prt         .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdn-tnp-900.
       cdn-tnp-200.
      *              *-------------------------------------------------*
      *              * Read Previous su [mgi]                          *
      *              *-------------------------------------------------*
           move      "RP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *                  *---------------------------------------------*
      *                  * Se 'At End' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdn-tnp-900.
       cdn-tnp-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo record                         *
      *                  *---------------------------------------------*
           if        rf-mgi-tip-rec       not  = l-cge-300-tip-arc
                     go to cdn-tnp-999.
       cdn-tnp-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se codice numerazione iva della registra-   *
      *                  * zione pari a quella passata, continuazione  *
      *                  * test                                        *
      *                  *---------------------------------------------*
           if        rf-mgi-cod-num       =    l-cge-300-cod-num
                     go to cdn-tnp-520.
      *                  *---------------------------------------------*
      *                  * Se cambio di numerazione iva                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se superiore a quella passata, a Start  *
      *                      * con data pari a quella della registra-  *
      *                      * zione letta                             *
      *                      *-----------------------------------------*
           if        rf-mgi-cod-num       >    l-cge-300-cod-num
                     move  rf-mgi-dat-reg to   w-cdn-mgi-wrk-wds
                     go to cdn-tnp-100.
      *                      *-----------------------------------------*
      *                      * Se inferiore a quella passata           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione giorno precedente    *
      *                          * alla data registrazione letta       *
      *                          *-------------------------------------*
           move      rf-mgi-dat-reg       to   s-dat                  .
       cdn-tnp-420.
           subtract  1                    from s-gio                  .
           if        s-gio                =    zero
                     move  31             to   s-gio
                     subtract 1           from s-mes                  .
           if        s-mes                =    zero
                     move  12             to   s-mes
                     subtract 1           from s-saa                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     go to cdn-tnp-420.
      *                          *-------------------------------------*
      *                          * A Start con data pari a quella del  *
      *                          * giorno precedente alla data regi-   *
      *                          * strazione letta                     *
      *                          *-------------------------------------*
           move      s-dat                to   w-cdn-mgi-wrk-wds
      *                          *-------------------------------------*
      *                          * A Start                             *
      *                          *-------------------------------------*
           go to     cdn-tnp-100.
       cdn-tnp-520.
      *                  *---------------------------------------------*
      *                  * Test su numero protocollo                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se numero protocollo di contabilita'    *
      *                      * letto pari a quello passato : riciclo   *
      *                      *-----------------------------------------*
           if        l-cge-300-num-prt    not  = zero and
                     rf-mgi-num-prt       =    l-cge-300-num-prt
                     go to cdn-tnp-200.
      *                      *-----------------------------------------*
      *                      * Se data registrazione letta appartenen- *
      *                      * te all'anno precedente a quello della   *
      *                      * data passata : test superato            *
      *                      *-----------------------------------------*
           move      rf-mgi-dat-reg       to   s-dat                  .
           if        s-saa                <    w-cdn-mgi-inp-saa
                     go to cdn-tnp-900.
      *                      *-----------------------------------------*
      *                      * Se numero protocollo letto non minore   *
      *                      * di quello passato : test non superato   *
      *                      *-----------------------------------------*
           if        rf-mgi-prt-iva       not  < l-cge-300-prt-iva
                     move  rf-mgi-prt-iva to   l-cge-300-prt-iva
                     move  rf-mgi-dat-reg to   l-cge-300-dat-reg
                     move  "#"            to   l-cge-300-exi-sts   
                     go to cdn-tnp-900.
       cdn-tnp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cdn-tnp-999.
       cdn-tnp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio clienti o fornitori per de-   *
      *    * terminazione sottoconto associato                         *
      *    *-----------------------------------------------------------*
       let-cod-cof-000.
      *              *-------------------------------------------------*
      *              * Se tipo archivio di generale : uscita           *
      *              *-------------------------------------------------*
           if        l-cge-300-tip-arc    =    "G"
                     go to  let-cod-cof-999.
      *              *-------------------------------------------------*
      *              * Se sottoconto gia' esistente : uscita           *
      *              *-------------------------------------------------*
           if        l-cge-300-cod-pdc    not  = zero
                     go to  let-cod-cof-999.
      *              *-------------------------------------------------*
      *              * Se archivio clienti                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        l-cge-300-tip-arc    =    "F"
                     go to  let-cod-cof-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      l-cge-300-cod-arc    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           move      rf-cli-cod-cge       to   l-cge-300-cod-pdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-cod-cof-999.
       let-cod-cof-100.
      *              *-------------------------------------------------*
      *              * Se archivio fornitori                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      l-cge-300-cod-arc    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           move      rf-fnt-cod-cge       to   l-cge-300-cod-pdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-cod-cof-999.
       let-cod-cof-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura causale e determinazione parametri ad  *
      *    * essa associati                                            *
      *    *-----------------------------------------------------------*
       let-cod-cau-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri connessi alla causale *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
       let-cod-cau-100.
      *              *-------------------------------------------------*
      *              * Lettura causale                                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCAU    "         to   f-key                  .
           move      l-cge-300-cod-cau    to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
       let-cod-cau-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento parametri connessi alla causale   *
      *              *-------------------------------------------------*
       let-cod-cau-600.
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
       let-cod-cau-610.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del valore passato *
      *                      * in link-area                            *
      *                      *-----------------------------------------*
           if        l-cge-300-des-cau    =    spaces
                     go to let-cod-cau-620
           else      go to let-cod-cau-680.
       let-cod-cau-620.
      *                      *-----------------------------------------*
      *                      * Se valore passato a spaces              *
      *                      *-----------------------------------------*
       let-cod-cau-622.
      *                          *-------------------------------------*
      *                          * Si forza la descrizione dal valore  *
      *                          * letto dalla tabella delle causali   *
      *                          * contabili                           *
      *                          *-------------------------------------*
           move      rf-zcc-des-cau       to   l-cge-300-des-cau      .
       let-cod-cau-624.
      *                          *-------------------------------------*
      *                          * Se non e' stato richiesto l'editing *
      *                          * della data scadenza si continua     *
      *                          * senza alcuna ulteriore azione       *
      *                          *-------------------------------------*
           if        l-cge-300-snx-dts    not  = "S"
                     go to let-cod-cau-660.
      *                          *-------------------------------------*
      *                          * Se l'attuale descrizione, letta da  *
      *                          * tabella, e' a spaces : si continua  *
      *                          * senza alcuna ulteriore azione       *
      *                          *-------------------------------------*
           if        l-cge-300-des-cau    =    spaces
                     go to let-cod-cau-660.
       let-cod-cau-626.
      *                          *-------------------------------------*
      *                          * Assemblaggio data di scadenza con   *
      *                          * descrizione causale                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing della data scadenza     *
      *                              *---------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      l-cge-300-dat-sca    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Assemblaggio                    *
      *                              *---------------------------------*
           move      160                  to   w-all-str-lun          .
           move      3                    to   w-all-str-num          .
           move      l-cge-300-des-cau    to   w-all-str-cat (1)      .
      *
           if        l-cge-300-dat-sca    =    zero
                     move  "a vista"      to   w-all-str-cat (2)
                     move  spaces         to   w-all-str-cat (3)
           else      move  "al"           to   w-all-str-cat (2)      .
                     move  v-edt          to   w-all-str-cat (3)      .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                              *---------------------------------*
      *                              * Aggiornamento descrizione cau-  *
      *                              * sale cosi' modificata           *
      *                              *---------------------------------*
           move      w-all-str-alf        to   l-cge-300-des-cau      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     let-cod-cau-660.
       let-cod-cau-660.
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     let-cod-cau-700.
       let-cod-cau-680.
      *                      *-----------------------------------------*
      *                      * Se valore passato diverso da spaces     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Nessuna operazione : si conserva il *
      *                          * valore passato e non si esegue al-  *
      *                          * cun test se editare la data scaden- *
      *                          * za oppure no                        *
      *                          *-------------------------------------*
           go to     let-cod-cau-700.
       let-cod-cau-700.
      *                  *---------------------------------------------*
      *                  * Tipo movimento iva                          *
      *                  *---------------------------------------------*
           move      rf-zcc-tip-moi       to   w-nol-tip-iva          .
       let-cod-cau-800.
      *                  *---------------------------------------------*
      *                  * Si/No movimento di bilancio                 *
      *                  *---------------------------------------------*
           move      rf-zcc-snx-bil       to   w-nol-snx-mob          .
       let-cod-cau-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero protocollo CGE             *
      *    *-----------------------------------------------------------*
       att-num-prt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *              *-------------------------------------------------*
      *              * Preparazione chiave per lettura tabella numera- *
      *              * zioni [prtcge]                                  *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   s-dat                  .
           move      s-saa                to   rn-prt-cge-saa-dtr     .
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [prtcge]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-num-prt-100.
      *                  *---------------------------------------------*
      *                  * Record non trovato                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione dati                    *
      *                      *-----------------------------------------*
           move      zero                 to   rn-prt-cge-num-prt     .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     att-num-prt-000.
       att-num-prt-100.
      *                  *---------------------------------------------*
      *                  * Record trovato                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero protocollo         *
      *                      *-----------------------------------------*
           add       1                    to   rn-prt-cge-num-prt     .
           if        rn-prt-cge-num-prt   =    zero
                     move  1              to   rn-prt-cge-num-prt     .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione numero protocollo       *
      *                      *-----------------------------------------*
           move      rn-prt-cge-num-prt   to   l-cge-300-num-prt      .
      *                      *-----------------------------------------*
      *                      * Update record                           *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
      *                      *-----------------------------------------*
      *                      * Se errori ripete l'intera operazione    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-num-prt-000.
      *                      *-----------------------------------------*
      *                      * Unlock                                  *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/num/ioc/obj/inprtcge"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-prt-cge             .
       att-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [mgr]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mgr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   rf-mgr-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgr-num-prt         .
           move      w-wrk-ctr-rig        to   rf-mgr-num-prg         .
           move      l-cge-300-cod-pdc    to   rf-mgr-cod-pdc         .
           move      l-cge-300-tip-arc    to   rf-mgr-tip-arc         .
           if        rf-mgr-tip-arc       =    "G"
                     move  zero           to   rf-mgr-cod-arc
           else      move  l-cge-300-cod-arc
                                          to   rf-mgr-cod-arc         .
           move      l-cge-300-cod-cau    to   rf-mgr-cod-cau         .
           move      w-nol-snx-mob        to   rf-mgr-snx-mob         .
           move      w-nol-tip-iva        to   rf-mgr-tip-iva         .
           move      l-cge-300-com-rig    to   rf-mgr-com-rig         .
           move      l-cge-300-dat-doc    to   rf-mgr-dat-doc         .
           move      l-cge-300-num-doc    to   rf-mgr-num-doc         .
           if        rf-mgr-tip-arc       =    "G"
                     move  zero           to   rf-mgr-dat-rif
                     move  spaces         to   rf-mgr-num-rif
                     go to cmp-rec-mgr-600.
       cmp-rec-mgr-200.
           if        l-cge-300-dat-rif    not  = zero
                     move  l-cge-300-dat-rif
                                          to   rf-mgr-dat-rif
                     go to cmp-rec-mgr-400.
           if        l-cge-300-dat-doc    not  = zero
                     move  l-cge-300-dat-doc
                                          to   rf-mgr-dat-rif
                     go to cmp-rec-mgr-400.
           move      l-cge-300-dat-reg    to   rf-mgr-dat-rif         .
       cmp-rec-mgr-400.
           if        l-cge-300-num-rif    not  = spaces
                     move  l-cge-300-num-rif
                                          to   rf-mgr-num-rif
                     go to cmp-rec-mgr-600.
           if        l-cge-300-num-doc    not  = spaces
                     move  l-cge-300-num-doc
                                          to   rf-mgr-num-rif
                     go to cmp-rec-mgr-600.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      l-cge-300-num-prt    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   rf-mgr-num-rif         .
       cmp-rec-mgr-600.
           move      l-cge-300-dar-ave    to   rf-mgr-dar-ave         .
           move      l-cge-300-imp-mov    to   rf-mgr-imp-mov         .
           move      spaces               to   rf-mgr-flg-pge         .
           move      spaces               to   rf-mgr-flg-pcf         .
       cmp-rec-mgr-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [mgr]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-mgr-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento [mgs] corrispondente              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In piu'                                     *
      *                  *---------------------------------------------*
           move      rn-dat-bil-ese-bil   to   w-agg-mgs-ese-bil      .
           move      l-cge-300-dat-reg    to   w-agg-mgs-dat-reg      .
           move      l-cge-300-tip-arc    to   w-agg-mgs-tip-arc      .
           move      l-cge-300-cod-arc    to   w-agg-mgs-cod-arc      .
           move      l-cge-300-cod-pdc    to   w-agg-mgs-cod-pdc      .
           move      "+"                  to   w-agg-mgs-tip-agg      .
           move      w-nol-snx-mob        to   w-agg-mgs-snx-mob      .
           move      l-cge-300-dar-ave    to   w-agg-mgs-dar-ave      .
           move      l-cge-300-imp-mov    to   w-agg-mgs-imp-mov      .
           perform   agg-con-000-000      thru agg-con-000-999        .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mgr-000      thru cmp-rec-mgr-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       wrt-rec-mgr-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [mgr]                                *
      *    *-----------------------------------------------------------*
       del-rec-mgr-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento [mgs] corrispondente              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In meno                                     *
      *                  *---------------------------------------------*
           move      rn-dat-bil-ese-bil   to   w-agg-mgs-ese-bil      .
           move      l-cge-300-dat-reg    to   w-agg-mgs-dat-reg      .
           move      rf-mgr-tip-arc       to   w-agg-mgs-tip-arc      .
           move      rf-mgr-cod-arc       to   w-agg-mgs-cod-arc      .
           move      rf-mgr-cod-pdc       to   w-agg-mgs-cod-pdc      .
           move      "-"                  to   w-agg-mgs-tip-agg      .
           move      rf-mgr-snx-mob       to   w-agg-mgs-snx-mob      .
           move      rf-mgr-dar-ave       to   w-agg-mgs-dar-ave      .
           move      rf-mgr-imp-mov       to   w-agg-mgs-imp-mov      .
           perform   agg-con-000-000      thru agg-con-000-999        .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgr                 .
       del-rec-mgr-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [mgt]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mgt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-mgt-ide-dat         .
           move      s-ute                to   rf-mgt-ide-ute         .
           move      s-fas                to   rf-mgt-ide-fas         .
           move      l-cge-300-dat-reg    to   rf-mgt-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgt-num-prt         .
           move      l-cge-300-cod-cau    to   rf-mgt-cod-cau         .
           move      w-nol-snx-mob        to   rf-mgt-snx-mob         .
           move      w-nol-tip-iva        to   rf-mgt-tip-iva         .
           move      l-cge-300-des-cau    to   rf-mgt-des-cau         .
           move      l-cge-300-dat-doc    to   rf-mgt-dat-doc         .
           move      l-cge-300-num-doc    to   rf-mgt-num-doc         .
           move      l-cge-300-cod-num    to   rf-mgt-cod-num         .
           move      l-cge-300-prt-iva    to   rf-mgt-prt-iva         .
           move      spaces               to   rf-mgt-flg-gio         .
       cmp-rec-mgt-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [mgt]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-mgt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mgt-000      thru cmp-rec-mgt-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       wrt-rec-mgt-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [mgt]                                *
      *    *-----------------------------------------------------------*
       del-rec-mgt-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave primaria                    *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   rf-mgt-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgt-num-prt         .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgt                 .
       del-rec-mgt-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [mgi]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-mgi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           if        l-cge-300-flg-rfp    not  = "S" and
                     l-cge-300-flg-rfp    not  = "R" and
                     l-cge-300-flg-rfp    not  = "E"
                     move  spaces         to   l-cge-300-flg-rfp      .
           if        l-cge-300-tip-arc    not  = "F"
                     move  zero           to   l-cge-300-dat-acq      .
       cmp-rec-mgi-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           if        w-nol-tip-iva        =    "1" or
                     w-nol-tip-iva        =    "2" or
                     w-nol-tip-iva        =    "A" or
                     w-nol-tip-iva        =    "B"
                     move  "C"            to   rf-mgi-tip-rec
           else if   w-nol-tip-iva        =    "3"
                     move  "V"            to   rf-mgi-tip-rec
           else if   w-nol-tip-iva        =    "4" or
                     w-nol-tip-iva        =    "5" or
                     w-nol-tip-iva        =    "6" or
                     w-nol-tip-iva        =    "D" or
                     w-nol-tip-iva        =    "E"
                     move  "F"            to   rf-mgi-tip-rec         .
           move      l-cge-300-dat-reg    to   rf-mgi-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgi-num-prt         .
           move      l-cge-300-dat-doc    to   rf-mgi-dat-doc         .
           move      l-cge-300-num-doc    to   rf-mgi-num-doc         .
           move      l-cge-300-cod-num    to   rf-mgi-cod-num         .
           if        rf-mgi-tip-rec       =    "F"
                     move  l-cge-300-prt-iva
                                          to   rf-mgi-prt-iva
                     go to cmp-rec-mgi-400.
           if        rf-mgi-tip-rec       =    "V"
                     move  zero           to   rf-mgi-prt-iva
                     go to cmp-rec-mgi-400.
           move      rf-mgi-num-doc       to   w-wrk-pig-alf          .
           move      zero                 to   w-wrk-pig-num          .
           move      11                   to   w-wrk-pig-i01          .
           move      11                   to   w-wrk-pig-i02          .
       cmp-rec-mgi-200.
           subtract  1                    from w-wrk-pig-i01          .
           if        w-wrk-pig-i01        =    zero
                     go to cmp-rec-mgi-300.
           if        w-wrk-pig-chr
                    (w-wrk-pig-i01)       <    "0" or
                     w-wrk-pig-chr
                    (w-wrk-pig-i01)       >    "9"
                     go to cmp-rec-mgi-200.
           move      w-wrk-pig-chr
                    (w-wrk-pig-i01)       to   w-wrk-pig-cif
                                              (w-wrk-pig-i02)         .
           subtract  1                    from w-wrk-pig-i02          .
           go to     cmp-rec-mgi-200.
       cmp-rec-mgi-300.
           move      w-wrk-pig-num        to   rf-mgi-prt-iva         .
       cmp-rec-mgi-400.
           move      l-cge-300-cod-cau    to   rf-mgi-cod-cau         .
           move      w-nol-tip-iva        to   rf-mgi-tip-iva         .
           move      zero                 to   I                      .
       cmp-rec-mgi-800.
           add       1                    to   I                      .
           if        I                    >    6
                     go to cmp-rec-mgi-900.
           move      l-cge-300-ibl-iva (I)
                                          to   rf-mgi-ibl-iva (I)     .
           move      l-cge-300-cod-iva (I)
                                          to   rf-mgi-cod-iva (I)     .
           move      l-cge-300-imp-iva (I)
                                          to   rf-mgi-imp-iva (I)     .
           go to     cmp-rec-mgi-800.
       cmp-rec-mgi-900.
           if        l-cge-300-tot-doc    not  = zero
                     move   l-cge-300-tot-doc
                                          to   rf-mgi-tot-doc
                     go to  cmp-rec-mgi-950.
           move      zero                 to   I                      .
       cmp-rec-mgi-920.
           add       1                    to   I                      .
           if        I                    >    6
                     go to cmp-rec-mgi-950.
           add       l-cge-300-ibl-iva (I)
                                          to   rf-mgi-tot-doc         .
           add       l-cge-300-imp-iva (I)
                                          to   rf-mgi-tot-doc         .
           go to     cmp-rec-mgi-920.
       cmp-rec-mgi-950.
           move      l-cge-300-cod-arc    to   rf-mgi-cod-arc         .
           move      l-cge-300-flg-rfp    to   rf-mgi-flg-rfp         .
           move      l-cge-300-sgl-num    to   rf-mgi-sgl-num         .
           move      l-cge-300-dat-acq    to   rf-mgi-dat-acq         .
           move      spaces               to   rf-mgi-flg-gio         .
       cmp-rec-mgi-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [mgi]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-mgi-000.
      *              *-------------------------------------------------*
      *              * Se movimento non iva : uscita                   *
      *              *-------------------------------------------------*
           if        w-nol-tip-iva        =    "0" or
                     w-nol-tip-iva        =    spaces
                     go to wrt-rec-mgi-999.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-mgi-000      thru cmp-rec-mgi-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       wrt-rec-mgi-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione record [mgi]                                *
      *    *-----------------------------------------------------------*
       del-rec-mgi-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave primaria                    *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   rf-mgi-dat-reg         .
           move      l-cge-300-num-prt    to   rf-mgi-num-prt         .
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgi                 .
       del-rec-mgi-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento [mgs] in funzione di w-agg-mgs              *
      *    *-----------------------------------------------------------*
       agg-con-000-000.
      *              *-------------------------------------------------*
      *              * Se movimento di chiusura e apertura bilancio    *
      *              * non si esegue aggiornamento del [mgs]           *
      *              *-------------------------------------------------*
           if        w-nol-snx-mob        =    "X"
                     go to agg-con-000-999.
      *              *-------------------------------------------------*
      *              * Determinazione anno di esercizio contabilita'   *
      *              *-------------------------------------------------*
           move      w-agg-mgs-dat-reg    to   w-det-ese-cge-dtr      .
           perform   det-ese-cge-000      thru det-ese-cge-999        .
      *              *-------------------------------------------------*
      *              * Parte relativa a cliente o fornitore            *
      *              *-------------------------------------------------*
           if        w-agg-mgs-tip-arc    =    "G"
                     go to agg-con-000-200.
       agg-con-000-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Lettura con lock                            *
      *                  *---------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "ESECOD"             to   f-key                  .
           if        w-agg-mgs-snx-mob    =    "S"
                     move   w-agg-mgs-ese-bil
                                          to   rf-mgs-ann-ese
           else      move   w-det-ese-cge-esa
                                          to   rf-mgs-ann-ese         .
           move      w-agg-mgs-tip-arc    to   rf-mgs-tip-rec         .
           move      w-agg-mgs-cod-arc    to   rf-mgs-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Se non trovato si scrive il record nor- *
      *                      * malizzato e si ritorna a leggere        *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to  agg-con-000-100.
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
           go to     agg-con-000-050.
       agg-con-000-100.
      *                      *-----------------------------------------*
      *                      * Aggiornamento dati record [mgs]         *
      *                      *-----------------------------------------*
           perform   agg-con-000-500      thru agg-con-000-899        .
      *                      *-----------------------------------------*
      *                      * Update record [mgs]                     *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Unlock file [mgs]                       *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
       agg-con-000-200.
      *              *-------------------------------------------------*
      *              * Parte relativa a sottoconto di generale         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                  *---------------------------------------------*
      *                  * Lettura con lock                            *
      *                  *---------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "ESECOD"             to   f-key                  .
           if        w-agg-mgs-snx-mob    =    "S"
                     move   w-agg-mgs-ese-bil
                                          to   rf-mgs-ann-ese
           else      move   w-det-ese-cge-esa
                                          to   rf-mgs-ann-ese         .
           move      "G"                  to   rf-mgs-tip-rec         .
           move      w-agg-mgs-cod-pdc    to   rf-mgs-cod-con         .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Se non trovato si scrive il record nor- *
      *                      * malizzato e si ritorna a leggere        *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to  agg-con-000-250.
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
           go to     agg-con-000-200.
       agg-con-000-250.
      *                      *-----------------------------------------*
      *                      * Aggiornamento dati record [mgs]         *
      *                      *-----------------------------------------*
           perform   agg-con-000-500      thru agg-con-000-899        .
      *                      *-----------------------------------------*
      *                      * Update record [mgs]                     *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Unlock file [mgs]                       *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofmgs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mgs                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-con-000-999.
       agg-con-000-500.
      *              *-------------------------------------------------*
      *              * Subroutine interna di aggiornamento record      *
      *              *-------------------------------------------------*
           if        w-agg-mgs-snx-mob    =    "S"
                     go to agg-con-000-700.
      *                  *---------------------------------------------*
      *                  * Se aggiornamento in "+"                     *
      *                  *---------------------------------------------*
           if        w-agg-mgs-tip-agg    =    "-"
                     go to agg-con-000-600.
           if        w-agg-mgs-dar-ave    =    "A"
                     add      w-agg-mgs-imp-mov
                                          to   rf-mgs-ave-mes
                                              (w-det-ese-cge-esm)
           else      add      w-agg-mgs-imp-mov
                                          to   rf-mgs-dar-mes
                                              (w-det-ese-cge-esm)     .
           go to     agg-con-000-899.
       agg-con-000-600.
      *                  *---------------------------------------------*
      *                  * Se aggiornamento in "-"                     *
      *                  *---------------------------------------------*
           if        w-agg-mgs-dar-ave     =    "A"
                     subtract w-agg-mgs-imp-mov
                                          from rf-mgs-ave-mes
                                               (w-det-ese-cge-esm)
           else      subtract w-agg-mgs-imp-mov
                                          from rf-mgs-dar-mes
                                               (w-det-ese-cge-esm)    .
           go to     agg-con-000-899.
       agg-con-000-700.
      *              *-------------------------------------------------*
      *              * Se movimento di bilancio                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se aggiornamento in "+"                     *
      *                  *---------------------------------------------*
           if        w-agg-mgs-tip-agg    =    "-"
                     go to agg-con-000-800.
           if        w-agg-mgs-dar-ave    =    "A"
                     add      w-agg-mgs-imp-mov
                                          to   rf-mgs-ave-bil
           else      add      w-agg-mgs-imp-mov
                                          to   rf-mgs-dar-bil         .
           go to     agg-con-000-899.
       agg-con-000-800.
      *                  *---------------------------------------------*
      *                  * Se aggiornamento in "-"                     *
      *                  *---------------------------------------------*
           if        w-agg-mgs-dar-ave    =    "A"
                     subtract w-agg-mgs-imp-mov
                                          from rf-mgs-ave-bil
           else      subtract w-agg-mgs-imp-mov
                                          from rf-mgs-dar-bil         .
       agg-con-000-899.
           exit.
       agg-con-000-999.
           exit.

      *    *===========================================================*
      *    * Determinazione anno di esercizio per contabilita'         *
      *    *-----------------------------------------------------------*
       det-ese-cge-000.
      *              *-------------------------------------------------*
      *              * Se non e' ancora stato fatto si legge la scelta *
      *              * del mese di chiusura esercizio                  *
      *              *-------------------------------------------------*
           if        w-det-ese-cge-mce    not  = zero
                     go to det-ese-cge-300.
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[mes-chi]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-det-ese-cge-mce
           else      move  12             to   w-det-ese-cge-mce      .
           if        w-det-ese-cge-mce    <    01 or
                     w-det-ese-cge-mce    >    12
                     move  12             to   w-det-ese-cge-mce      .
       det-ese-cge-300.
      *              *-------------------------------------------------*
      *              * Determinazione anno di esercizio                *
      *              *-------------------------------------------------*
           move      w-det-ese-cge-dtr    to   s-dat                  .
           if        w-det-ese-cge-mce    =    12             or
                     s-mes                >    w-det-ese-cge-mce
                     move  s-saa          to   w-det-ese-cge-esa
                     go to det-ese-cge-600.
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-det-ese-cge-esa      .
       det-ese-cge-600.
      *              *-------------------------------------------------*
      *              * Determinazione mese di esercizio                *
      *              *-------------------------------------------------*
           move      s-mes                to   w-det-ese-cge-esm      .
           if        w-det-ese-cge-mce    =    12
                     go to det-ese-cge-999.
           if        s-mes                >    w-det-ese-cge-mce
                     subtract w-det-ese-cge-mce
                                          from w-det-ese-cge-esm
                     go to det-ese-cge-999.
           if        s-mes                <    w-det-ese-cge-mce
                     add   12             to   w-det-ese-cge-esm
                     subtract w-det-ese-cge-mce
                                          from w-det-ese-cge-esm
                     go to det-ese-cge-999.
           move      12                   to   w-det-ese-cge-esm      .
       det-ese-cge-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
