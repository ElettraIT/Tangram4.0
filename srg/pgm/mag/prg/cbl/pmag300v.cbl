       Identification Division.
       Program-Id.                                 pmag300v           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mag                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/11/98    *
      *                       Ultima revisione:    NdK del 13/05/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione valori gestione magazzino         *
      *                                                                *
      * ============================================================== *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : d-vun-mag-tip-ope = "OP"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : d-vun-mag-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : d-vun-mag-tip-ope = "C?"                       *
      *                                                                *
      *        Output : d-vun-mag-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "VM" - Determinazione valore unitario di magazzino alla data   *
      *                                                                *
      *        Input  : d-vun-mag-tip-ope = "VM"                       *
      *                 d-vun-mag-tip-val = Tipo di valorizzazione di  *
      *                                     magazzino                  *
      *                                     - 0001 : Costo medio pon-  *
      *                                              derato dell'anno  *
      *                                     - 0002 : Costo ultimo      *
      *                                     - 0003 : Costo standard    *
      *                                     - 0004 : Costo Lifo alla   *
      *                                              data (dinamico)   *
      *                                     - 0014 : Costo Lifo da     *
      *                                              scaglioni annuali *
      *                                              scheda esercizio  *
      *                                              in corso          *
      *                                     - 0005 : Costo Medio Con-  *
      *                                              tinuo             *
      *                 d-vun-mag-dat-val = Data di valorizzazione (se *
      *                                     zero si considera la data  *
      *                                     attuale)                   *
      *                 d-vun-mag-tip-mag = Tipo codice di magazzino   *
      *                 d-vun-mag-num-mag = Codice numerico di magaz-  *
      *                                     zino                       *
      *                 d-vun-mag-gia-prp = Giacenza di proprieta' al- *
      *                                     la data di valorizzazione  *
      *                                     (solo in caso di valoriz-  *
      *                                     zazione 0004, Lifo)        *
      *                                                                *
      *        Output : d-vun-mag-exi-sts = spaces: Valore determinato *
      *                                     #     : Valore non deter-  *
      *                                             minato per mancan- *
      *                                             za di dati         *
      *                                     N     : Determinazione non *
      *                                             eseguibile per ri- *
      *                                             manenze finali ne- *
      *                                             gative             *
      *                                             (solo se L.i.f.o.) *
      *                                     Z     : Determinazione e-  *
      *                                             seguita ma rima-   *
      *                                             nenza finale zero  *
      *                                             (solo se L.i.f.o.) *
      *                                                                *
      *                 d-vun-mag-val-uni = Valore unitario alla data  *
      *                                                                *
      *                 d-vun-mag-dat-val = Data reale a cui si rife-  *
      *                                     risce il valore unitario   *
      *                                                                *
      *                 Dati in output addizionali (solo se L.i.f.o.): *
      *                                                                *
      *                 d-vun-mag-lif-val = Valore totale L.i.f.o.     *
      *                                                                *
      *                 d-vun-mag-lif-nsc = Numero di scaglioni che    *
      *                                     concorrono a formare il    *
      *                                     valore della rimanenza     *
      *                                     (00.11)                    *
      *                                                                *
      *                 per ogni scaglione :                           *
      *                                                                *
      *                 d-vun-mag-lif-qta : Quantita'                  *
      *                                                                *
      *                 d-vun-mag-lif-vun : Valore unitario            *
      *                                                                *
      *                 d-vun-mag-lif-tvu : Tipo di valore unitario    *
      *                                      - P : Costo medio ponde-  *
      *                                            rato                *
      *                                      - N : Valore normale del  *
      *                                            bene nell'ultimo    *
      *                                            trimestre           *
      *                                                                *
      *                 d-vun-mag-lif-ann : Anno, di quattro cifre     *
      *                                                                *
      *                 d-vun-mag-lif-riv : Flag di rivalutazione      *
      *                                      - Spaces : No             *
      *                                      - S      : Si             *
      *                                                                *
      * "VT" - Determinazione valore totale                            *
      *                                                                *
      *        Input  : d-vun-mag-tip-ope = "VT"                       *
      *                                                                *
      *                 d-vun-mag-tip-mag = Decimali gestione magaz.   *
      *                                                                *
      *                 d-vun-mag-num-mes = Decimali prezzo            *
      *                                                                *
      *                 d-vun-mag-gia-prp = Quantita' per il calcolo   *
      *                                                                *
      *                 d-vun-mag-val-uni = Costo unitario             *
      *                                                                *
      *                                                                *
      *        Output : d-vun-mag-lif-val = Valore totale determinato  *
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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mmv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmv"                          .
      *        *-------------------------------------------------------*
      *        * [mmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmc"                          .
      *        *-------------------------------------------------------*
      *        * [mmr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .
      *        *-------------------------------------------------------*
      *        * [datife]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/num/rec/rndatife"                       .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No considerazione costo a inizio anno              *
      *        *                                                       *
      *        * - 'S' : Si                                            *
      *        * - 'N' : No                                            *
      *        *-------------------------------------------------------*
           05  w-prs-val-ini              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area gestionale [mag]                                 *
      *        *-------------------------------------------------------*
           05  w-prs-arg-mag.
      *            *---------------------------------------------------*
      *            * Si/No espressione dei costi unitari in unita' di  *
      *            * misura diverse                                    *
      *            *                                                   *
      *            * - N : No                                          *
      *            * - K : Costo riferito a 1000 unita'                *
      *            * - C : Costo riferito a 100  unita'                *
      *            * - D : Costo riferito a 10   unita'                *
      *            *---------------------------------------------------*
               10  w-prs-arg-mag-ecu      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area date e numerazioni                              *
      *    *-----------------------------------------------------------*
       01  w-dnu.
      *        *-------------------------------------------------------*
      *        * Da record numerazioni [datife]                        *
      *        *-------------------------------------------------------*
           05  w-dnu-ife.
      *            *---------------------------------------------------*
      *            * Flag di avanzamento inventario fine esercizio     *
      *            * - Spaces : Completamente eseguito                 *
      *            * - #      : In progresso                           *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Data apertura periodo inventario fine esercizio   *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-dti      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data chiusura periodo inventario fine esercizio   *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-dtf      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Anno di esercizio dell'inventario di fine eser-   *
      *            * cizio : s.aa                                      *
      *            *---------------------------------------------------*
               10  w-dnu-ife-ife-ese      pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazione valore                        *
      *        *-------------------------------------------------------*
           05  w-det-vum.
      *            *---------------------------------------------------*
      *            * Data di cui e' stato richiesto il valore          *
      *            *---------------------------------------------------*
               10  w-det-vum-dat-val      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data attuale                                      *
      *            *---------------------------------------------------*
               10  w-det-vum-dat-att      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Esercizio di riferimento : s.aa                   *
      *            *                                                   *
      *            * Calcolato automaticamente dalla routine di de-    *
      *            * terminazione in funzione della data richiesta     *
      *            *---------------------------------------------------*
               10  w-det-vum-ann-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese nell'esercizio di riferimento : 01..12       *
      *            *                                                   *
      *            * Calcolato automaticamente dalla routine di de-    *
      *            * terminazione in funzione della data richiesta     *
      *            *---------------------------------------------------*
               10  w-det-vum-mes-ese      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Mese di chiusura esercizio, calcolato automatica- *
      *            * mente dalla routine di determinazione alla prima  *
      *            * esecuzione della routine stessa                   *
      *            *---------------------------------------------------*
               10  w-det-vum-mes-che      pic  9(02)       value zero .
      *            *---------------------------------------------------*
      *            * Anno di esercizio precedente a quello relativo    *
      *            * alla data in cui e' stato richiesto il valore     *
      *            *---------------------------------------------------*
               10  w-det-vum-ann-prc      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Data di fine esercizio attuale                    *
      *            *---------------------------------------------------*
               10  w-det-vum-dat-fie      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Work-area locale per anno di esercizio            *
      *            *---------------------------------------------------*
               10  w-det-vum-wrk-ann      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Work-area locale per contatore                    *
      *            *---------------------------------------------------*
               10  w-det-vum-wrk-ctr      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione valore : Costo medio pondera- *
      *        * to dell'anno                                          *
      *        *-------------------------------------------------------*
           05  w-det-csm-pnd.
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-csm-pnd-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Work per progressivo quantita'                    *
      *            *---------------------------------------------------*
               10  w-det-csm-pnd-wpq      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Work per progressivo valore                       *
      *            *---------------------------------------------------*
               10  w-det-csm-pnd-wpv      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Work per valore costo medio ponderato             *
      *            *---------------------------------------------------*
               10  w-det-csm-pnd-cst      pic  9(11)v9(06)            .
               10  w-det-csm-pnd-vun      pic  9(11)v9(06)            .
      *            *---------------------------------------------------*
      *            * Work per scansione esercizi                       *
      *            *---------------------------------------------------*
               10  w-det-csm-pnd-ese      pic  9(03)                  .
               10  w-det-csm-pnd-wri      pic s9(10)v9(03)            .
               10  w-det-csm-pnd-wci      pic s9(11)                  .
               10  w-det-csm-pnd-wvw      pic s9(11)v9(03)            .
               10  w-det-csm-pnd-wcd      pic  9(07)                  .
               10  w-det-csm-pnd-ct0      pic  9(02)                  .
               10  w-det-csm-pnd-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione valore : Costo ultimo         *
      *        *-------------------------------------------------------*
           05  w-det-cst-ult.
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-cst-ult-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Work per data costo ultimo                        *
      *            *---------------------------------------------------*
               10  w-det-cst-ult-dcu      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Work per valore costo ultimo                      *
      *            *---------------------------------------------------*
               10  w-det-cst-ult-ucu      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione valore : Costo standard       *
      *        *-------------------------------------------------------*
           05  w-det-cst-std.
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-cst-std-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Work per data costo standard                      *
      *            *---------------------------------------------------*
               10  w-det-cst-std-dst      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Work per valore costo standard                    *
      *            *---------------------------------------------------*
               10  w-det-cst-std-ust      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione valore : Costo L.i.f.o.       *
      *        *-------------------------------------------------------*
           05  w-det-cst-lif.
      *            *---------------------------------------------------*
      *            * Dati memorizzati dalla routine di determinazione  *
      *            * del costo medio ponderato, richiamata da quella   *
      *            * di determinazione del costo L.i.f.o.              *
      *            *---------------------------------------------------*
               10  w-det-cst-lif-buf.
      *                *-----------------------------------------------*
      *                * Flag di lettura record [mmv] anno in corso    *
      *                *  - Spaces : Esistente                         *
      *                *  - #      : Non esistente                     *
      *                *-----------------------------------------------*
                   15  w-det-cst-lif-flg  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Work per valore costo standard                *
      *                *-----------------------------------------------*
                   15  w-det-cst-lif-mmv.
                       20  filler  occurs 3072
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione valore : Costo medio continuo *
      *        *-------------------------------------------------------*
           05  w-det-csm-con.
      *            *---------------------------------------------------*
      *            * Comodo per data ultimo rilevamento                *
      *            *---------------------------------------------------*
               10  w-det-csm-con-dur      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Comodo per giacenza                               *
      *            *---------------------------------------------------*
               10  w-det-csm-con-gia      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Comodo per costo medio continuo                   *
      *            *---------------------------------------------------*
               10  w-det-csm-con-cmc      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per salvataggio valore giacenza            *
      *            *---------------------------------------------------*
               10  w-det-csm-con-sgi      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Comodo per salvataggio costo medio continuo       *
      *            *---------------------------------------------------*
               10  w-det-csm-con-scm      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per quantita' in giacenza significativa    *
      *            *---------------------------------------------------*
               10  w-det-csm-con-qgs      pic  9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Comodo per valore giacenza significativo          *
      *            *---------------------------------------------------*
               10  w-det-csm-con-vgs      pic  9(14)v9(04)            .
      *            *---------------------------------------------------*
      *            * Comodo per campi numerici                         *
      *            *---------------------------------------------------*
               10  w-det-csm-con-w01      pic s9(11)                  .
               10  w-det-csm-con-w02      pic s9(14)v9(04)            .

      *    *===========================================================*
      *    * Work per la routine di determinazione della composizione  *
      *    * del valore L.I.F.O.                                       *
      *    *-----------------------------------------------------------*
       01  w-dcv-lif.
      *        *-------------------------------------------------------*
      *        * Valori in input                                       *
      *        *-------------------------------------------------------*
           05  w-dcv-lif-inp.
      *            *---------------------------------------------------*
      *            * Anno di esercizio attuale                         *
      *            *---------------------------------------------------*
               10  w-dcv-lif-ann-ese      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Anno di esercizio richiesto                       *
      *            *---------------------------------------------------*
               10  w-dcv-lif-ann-ric      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Dati relativi all'esercizio attuale e ai 10 eser- *
      *            * cizi precedenti                                   *
      *            *   - 01 : Esercizio attuale                        *
      *            *   - 02 : Ultimo esercizio precedente              *
      *            *   - 03 : Penultimo esercizio precedente           *
      *            *   - ..                                            *
      *            *   - 11 : Decimo esercizio precedente              *
      *            *---------------------------------------------------*
               10  w-dcv-lif-dat-ese occurs 11.
      *                *-----------------------------------------------*
      *                * Rimanenza finale dell'esercizio               *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-rim-ese  pic s9(08)v9(03)            .
      *                *-----------------------------------------------*
      *                * Costo medio ponderato finale dell'esercizio   *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-ump-ese  pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Valore normale del bene nell'ultimo trimestre *
      *                * dell'esercizio                                *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-vnb-ese  pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Flag di rivalutazione                         *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-fdr-ese  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori in output                                      *
      *        *-------------------------------------------------------*
           05  w-dcv-lif-out.
      *            *---------------------------------------------------*
      *            * Flag di uscita                                    *
      *            *   - Spaces : Determinazione eseguita              *
      *            *   - N      : Determinazione non eseguibile per    *
      *            *              rimanenze finali negative            *
      *            *   - Z      : Rimanenza finale a zero, percio'     *
      *            *              determinazione non eseguita          *
      *            *---------------------------------------------------*
               10  w-dcv-lif-flg-exi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Rimanenza finale                                  *
      *            *---------------------------------------------------*
               10  w-dcv-lif-rim-fin      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore della rimanenza                            *
      *            *---------------------------------------------------*
               10  w-dcv-lif-val-fin      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per calcolo costo della rimanenza          *
      *            *---------------------------------------------------*
               10  w-dcv-lif-rim-cst      pic  9(11)v9(02)            .
      *            *---------------------------------------------------*
      *            * Numero di scaglioni che concorrono a formare il   *
      *            * valore della rimanenza                            *
      *            *---------------------------------------------------*
               10  w-dcv-lif-num-vlf      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Dati relativi agli 11 esercizi che contribuiscono *
      *            * a formare il valore della rimanenza finale        *
      *            *---------------------------------------------------*
               10  w-dcv-lif-dat-vlf occurs 11.
      *                *-----------------------------------------------*
      *                * Quantita'                                     *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-qta-vlf  pic s9(08)v9(03)            .
      *                *-----------------------------------------------*
      *                * Valore unitario                               *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-vun-vlf  pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Tipo di valore unitario                       *
      *                *  - P : Costo medio ponderato                  *
      *                *  - N : Valore normale del bene nell'ultimo    *
      *                *        trimestre                              *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-tpv-vlf  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Anno, di quattro cifre                        *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-ann-vlf  pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Flag di rivalutazione                         *
      *                *-----------------------------------------------*
                   15  w-dcv-lif-riv-vlf  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori di work locale                                 *
      *        *-------------------------------------------------------*
           05  w-dcv-lif-wrk.
      *            *---------------------------------------------------*
      *            * Salvataggio data di valorizzazione                *
      *            *---------------------------------------------------*
               10  w-dcv-lif-sav-ddv      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Indici e contatori                                *
      *            *---------------------------------------------------*
               10  w-dcv-lif-inx-i0i      pic  9(02)                  .
               10  w-dcv-lif-inx-j0j      pic  9(02)                  .
               10  w-dcv-lif-inx-i1i      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per operazioni aritmetiche                 *
      *            *---------------------------------------------------*
               10  w-dcv-lif-cal-s11      pic s9(11)                  .
               10  w-dcv-lif-cal-s04      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Differenza tra la rimanenza finale di ogni eser-  *
      *            * cizio con la differenza finale dell'esercizio     *
      *            * precedente                                        *
      *            *---------------------------------------------------*
               10  w-dcv-lif-dif-rim
                               occurs 11  pic s9(08)v9(03)            .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione valore unitario  *
      *    * di magazzino                                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dvunmag0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-vun-mag              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-vun-mag-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione valore unitario di magazzino *
      *                  *---------------------------------------------*
           if        d-vun-mag-tip-ope    =    "VM"
                     perform dvm-000      thru dvm-999
      *                  *---------------------------------------------*
      *                  * Determinazione valore totale                *
      *                  *---------------------------------------------*
           else if   d-vun-mag-tip-ope    =    "VT"
                     perform dvt-000      thru dvt-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-vun-mag-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-vun-mag-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-vun-mag-tip-ope    =    "C?"
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
      *              * Lettura numerazione [datife]                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura                                    *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RD"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *                  *---------------------------------------------*
      *                  * Spostamento in area di comodo               *
      *                  *---------------------------------------------*
           move      rn-dat-ife-flg-ife   to   w-dnu-ife-ife-flg      .
           move      rn-dat-ife-ini-ife   to   w-dnu-ife-ife-dti      .
           move      rn-dat-ife-fin-ife   to   w-dnu-ife-ife-dtf      .
           move      rn-dat-ife-ese-ife   to   w-dnu-ife-ife-ese      .
      *                  *---------------------------------------------*
      *                  * Chiusura                                    *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/num/ioc/obj/indatife"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-dat-ife             .
      *              *-------------------------------------------------*
      *              * Apertura file principali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmv] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmc] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmc                 .
      *                  *---------------------------------------------*
      *                  * Apertura file                         [mmr] *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
       opn-800.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No considerazione valore iniziale        *
      *                  *---------------------------------------------*
           perform   prs-val-ini-000      thru prs-val-ini-999        .
      *                  *---------------------------------------------*
      *                  * Si/No espressione dei costi unitari in      *
      *                  * unita' di misura diverse                    *
      *                  *---------------------------------------------*
           perform   prs-snx-ecu-000      thru prs-snx-ecu-999        .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No considerazione valore   *
      *    *                             iniziale                      *
      *    *-----------------------------------------------------------*
       prs-val-ini-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mag/cos/mag300v[val-ini]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-val-ini
           else      move  spaces         to   w-prs-val-ini          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-val-ini        not  = "S"
                     move  "N"            to   w-prs-val-ini          .
       prs-val-ini-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No espressione dei costi   *
      *    * unitari in unita' di misura diverse                       *
      *    *-----------------------------------------------------------*
       prs-snx-ecu-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mag/cos[snx-ecu]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-arg-mag-ecu
           else      move  spaces         to   w-prs-arg-mag-ecu      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N" or
                     w-prs-arg-mag-ecu    =    "K" or
                     w-prs-arg-mag-ecu    =    "C" or
                     w-prs-arg-mag-ecu    =    "D"
                     go to prs-snx-ecu-999.
           move      "N"                  to   w-prs-arg-mag-ecu      .
       prs-snx-ecu-999.
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
      *                  * Chiusura file                         [mmv] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mmc] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmc                 .
      *                  *---------------------------------------------*
      *                  * Chiusura file                         [mmr] *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
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
                     move  spaces         to   d-vun-mag-exi-sts
           else      move  "#"            to   d-vun-mag-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario di magazzino               *
      *    *-----------------------------------------------------------*
       dvm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   d-vun-mag-val-uni      .
      *              *-------------------------------------------------*
      *              * Determinazione mese chiusura esercizio per la   *
      *              * gestione magazzino, mutuandolo dalla contabi-   *
      *              * lita' generale                                  *
      *              *-------------------------------------------------*
           if        w-det-vum-mes-che    not  = zero
                     go to dvm-010.
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[mes-chi]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-det-vum-mes-che
           else      move  12             to   w-det-vum-mes-che      .
           if        w-det-vum-mes-che    <    01 or
                     w-det-vum-mes-che    >    12
                     move  12             to   w-det-vum-mes-che      .
       dvm-010.
      *              *-------------------------------------------------*
      *              * Data valore in work di comodo                   *
      *              *-------------------------------------------------*
           move      d-vun-mag-dat-val    to   w-det-vum-dat-val      .
      *              *-------------------------------------------------*
      *              * Determinazione data attuale                     *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-det-vum-dat-att      .
      *              *-------------------------------------------------*
      *              * Aggiustamento data in cui e' richiesto il saldo *
      *              *-------------------------------------------------*
           if        w-det-vum-dat-val    not  = zero
                     go to dvm-020.
      *              *-------------------------------------------------*
      *              * Richiesta data attuale al segretario            *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-det-vum-dat-val      .
       dvm-020.
      *              *-------------------------------------------------*
      *              * Determinazione anno di esercizio relativo alla  *
      *              * data in cui e' stato richiesto il saldo         *
      *              *-------------------------------------------------*
           move      w-det-vum-dat-val    to   s-dat                  .
           if        w-det-vum-mes-che    =    12             or
                     s-mes                >    w-det-vum-mes-che
                     move  s-saa          to   w-det-vum-ann-ese
                     go to dvm-030.
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-det-vum-ann-ese      .
       dvm-030.
      *              *-------------------------------------------------*
      *              * Determinazione mese di esercizio relativo alla  *
      *              * data in cui e' stato richiesto il saldo         *
      *              *-------------------------------------------------*
           move      s-mes                to   w-det-vum-mes-ese      .
           if        w-det-vum-mes-che    =    12
                     go to dvm-040.
           if        s-mes                >    w-det-vum-mes-che
                     subtract w-det-vum-mes-che
                                          from w-det-vum-mes-ese
                     go to dvm-040.
           if        s-mes                <    w-det-vum-mes-che
                     add   12             to   w-det-vum-mes-ese
                     subtract w-det-vum-mes-che
                                          from w-det-vum-mes-ese
                     go to dvm-040.
           move      12                   to   w-det-vum-mes-ese      .
       dvm-040.
      *              *-------------------------------------------------*
      *              * Determinazione anno di esercizio precedente a   *
      *              * quello relativo alla data in cui e' stato ri-   *
      *              * chiesto il saldo                                *
      *              *-------------------------------------------------*
           move      zero                 to   s-dat                  .
           move      w-det-vum-ann-ese    to   s-saa                  .
           subtract  1                    from s-saa                  .
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-det-vum-ann-prc      .
       dvm-050.
      *              *-------------------------------------------------*
      *              * Determinazione data di fine esercizio attuale   *
      *              *-------------------------------------------------*
           move      w-det-vum-ann-ese    to   s-saa                  .
           move      w-det-vum-mes-che    to   s-mes                  .
           move      31                   to   s-gio                  .
       dvm-052.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to dvm-052.
           move      s-dat                to   w-det-vum-dat-fie      .
       dvm-075.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo valorizzazione  *
      *              * richiesta                                       *
      *              *-------------------------------------------------*
           if        d-vun-mag-tip-val    = 0001
                     go to dvm-100
           else if   d-vun-mag-tip-val    = 0002
                     go to dvm-200
           else if   d-vun-mag-tip-val    = 0003
                     go to dvm-300
           else if   d-vun-mag-tip-val    = 0004
                     go to dvm-400
           else if   d-vun-mag-tip-val    = 0014
                     go to dvm-410
           else if   d-vun-mag-tip-val    = 0005
                     go to dvm-500
           else      go to dvm-999.
       dvm-100.
      *              *-------------------------------------------------*
      *              * Se tipo valorizzazione richiesta : costo medio  *
      *              * ponderato                                       *
      *              *-------------------------------------------------*
           perform   det-csm-pnd-000      thru det-csm-pnd-999        .
           go to     dvm-999.
       dvm-200.
      *              *-------------------------------------------------*
      *              * Se tipo valorizzazione richiesta : costo ultimo *
      *              *-------------------------------------------------*
           perform   det-cst-ult-000      thru det-cst-ult-999        .
           go to     dvm-999.
       dvm-300.
      *              *-------------------------------------------------*
      *              * Se tipo valorizzazione richiesta : Costo stan-  *
      *              * dard                                            *
      *              *-------------------------------------------------*
           perform   det-cst-std-000      thru det-cst-std-999        .
           go to     dvm-999.
       dvm-400.
      *              *-------------------------------------------------*
      *              * Se tipo valorizzazione richiesta : Costo Lifo   *
      *              *-------------------------------------------------*
           perform   det-cst-lif-000      thru det-cst-lif-999        .
           go to     dvm-999.
       dvm-410.
      *              *-------------------------------------------------*
      *              * Se tipo valorizzazione richiesta : Costo Lifo   *
      *              * da scaglioni annuali in scheda esercizio attua- *
      *              * le                                              *
      *              *-------------------------------------------------*
           perform   det-cst-lfa-000      thru det-cst-lfa-999        .
           go to     dvm-999.
       dvm-500.
      *              *-------------------------------------------------*
      *              * Se tipo valorizzazione richiesta : Costo medio  *
      *              * continuo                                        *
      *              *-------------------------------------------------*
           perform   det-csm-con-000      thru det-csm-con-999        .
           go to     dvm-999.
       dvm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore totale                              *
      *    *-----------------------------------------------------------*
       dvt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   d-vun-mag-lif-val      .
       dvt-100.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
           if        d-vun-mag-gia-prp    =    zero or
                     d-vun-mag-val-uni    =    zero
                     go to dvt-800.
       dvt-200.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  d-vun-mag-gia-prp    by   d-vun-mag-val-uni
                                        giving d-vun-mag-lif-val      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali prezzo                *
      *                  *---------------------------------------------*
           if        d-vun-mag-num-mes    =    1
                     divide 10            into d-vun-mag-lif-val
           else if   d-vun-mag-num-mes    =    2
                     divide 100           into d-vun-mag-lif-val      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           if        d-vun-mag-tip-mag    =    1
                     divide 10            into d-vun-mag-lif-val
           else if   d-vun-mag-tip-mag    =    2
                     divide 100           into d-vun-mag-lif-val      .
       dvt-400.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione relativa all'espres-  *
      *              * sione dei costi unitari in unita' di misura     *
      *              * diversa                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N"
                     go to dvt-800.
      *                  *---------------------------------------------*
      *                  * Applicazione del coefficiente               *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "D"
                     divide 10            into d-vun-mag-lif-val
           else if   w-prs-arg-mag-ecu    =    "C"
                     divide 100           into d-vun-mag-lif-val
           else if   w-prs-arg-mag-ecu    =    "K"
                     divide 1000          into d-vun-mag-lif-val      .
       dvt-800.
       dvt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dvt-999.
       dvt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario : Costo medio ponderato    *
      *    * dell'anno                                                 *
      *    *-----------------------------------------------------------*
       det-csm-pnd-000.
      *              *-------------------------------------------------*
      *              * Determinazione rimanenza e valore iniziali      *
      *              *-------------------------------------------------*
           perform   det-rmv-ini-000      thru det-rmv-ini-999        .
      *              *-------------------------------------------------*
_ NEW_*              * Se e' stata rilevata una data, si pone intanto  *
      *              * come data di rilevazione                        *
      *              *-------------------------------------------------*
           if        w-det-csm-pnd-wcd    not  = zero
                     move  w-det-csm-pnd-wcd
                                          to   d-vun-mag-dat-val      .
      *              *-------------------------------------------------*
      *              * Normalizzazione progressivi di comodo           *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csm-pnd-wpq      .
           move      zero                 to   w-det-csm-pnd-wpv      .
           move      zero                 to   w-det-csm-pnd-wvw      .
           move      zero                 to   w-det-csm-pnd-vun      .
       det-csm-pnd-003.
      *              *-------------------------------------------------*
      *              * Lettura record [mmv] dell'anno in corso         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmv]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mmv]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-ann-ese    to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
       det-csm-pnd-005.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dell'esito della lettura e  *
      *                  * del record letto, solo per il caso in cui   *
      *                  * la determinazione del costo medio pondera-  *
      *                  * to sia stata richiamata dalla routine di    *
      *                  * determinazione del costo L.i.f.o.           *
      *                  *---------------------------------------------*
           if        d-vun-mag-tip-val    not  = 0004
                     go to det-csm-pnd-010.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione esito lettura           *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     move  spaces         to   w-det-cst-lif-flg
           else      move  "#"            to   w-det-cst-lif-flg      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione record [mmv]            *
      *                      *-----------------------------------------*
           move      rf-mmv               to   w-det-cst-lif-mmv      .
       det-csm-pnd-010.
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-pnd-200.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione per accumulo progressivi men-*
      *              * sili anno in corso fino a mese precedente a     *
      *              * quello indicato da data richiesta               *
      *              *-------------------------------------------------*
           move      w-det-vum-mes-ese    to   w-det-csm-pnd-ctr      .
       det-csm-pnd-020.
           subtract  1                    from w-det-csm-pnd-ctr      .
           if        w-det-csm-pnd-ctr    =    zero
                     go to det-csm-pnd-040.
           if        rf-mmv-qtv-mns
                    (w-det-csm-pnd-ctr)   =    zero
                     go to det-csm-pnd-020.
           add       rf-mmv-qtv-mns
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-wpq      .
           add       rf-mmv-vlv-mns
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-wpv      .
           add       rf-mmv-pca-mns
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-wpv      .
           subtract  rf-mmv-prc-mns
                    (w-det-csm-pnd-ctr)   from w-det-csm-pnd-wpv      .
           go to     det-csm-pnd-020.
       det-csm-pnd-040.
      *              *-------------------------------------------------*
      *              * Se data richiesta saldo uguale a data attuale   *
      *              * si aggiungono i progressivi del mese attuale    *
      *              *-------------------------------------------------*
           if        w-det-vum-dat-val    not  = w-det-vum-dat-att
                     go to  det-csm-pnd-060.
           add       rf-mmv-qtv-mns
                    (w-det-vum-mes-ese)   to   w-det-csm-pnd-wpq      .
           add       rf-mmv-vlv-mns
                    (w-det-vum-mes-ese)   to   w-det-csm-pnd-wpv      .
           add       rf-mmv-pca-mns
                    (w-det-vum-mes-ese)   to   w-det-csm-pnd-wpv      .
           subtract  rf-mmv-prc-mns
                    (w-det-vum-mes-ese)   from w-det-csm-pnd-wpv      .
           go to     det-csm-pnd-200.
       det-csm-pnd-060.
      *              *-------------------------------------------------*
      *              * Controllo se giorno della data richiesta e' di  *
      *              * fine mese                                       *
      *              *-------------------------------------------------*
           move      w-det-vum-dat-val    to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se la data e' corretta vuol dire che il     *
      *                  * giorno della data richiesta non e' a fine   *
      *                  * mese                                        *
      *                  *---------------------------------------------*
           if        s-sts                =    spaces
                     go to  det-csm-pnd-080.
      *                      *-----------------------------------------*
      *                      * Se fine mese si aggiungono i progressivi*
      *                      * del mese attuale                        *
      *                      *-----------------------------------------*
           add       rf-mmv-qtv-mns
                    (w-det-vum-mes-ese)   to   w-det-csm-pnd-wpq      .
           add       rf-mmv-vlv-mns
                    (w-det-vum-mes-ese)   to   w-det-csm-pnd-wpv      .
           add       rf-mmv-pca-mns
                    (w-det-vum-mes-ese)   to   w-det-csm-pnd-wpv      .
           subtract  rf-mmv-prc-mns
                    (w-det-vum-mes-ese)   from w-det-csm-pnd-wpv      .
      *                      *-----------------------------------------*
      *                      * Se mese richiesto e' quello di chiusura *
      *                      * esercizio, si aggiungono i progressivi  *
      *                      * costi aggiuntivi e rettifiche costi di  *
      *                      * fine esercizio                          *
      *                      *-----------------------------------------*
           if        w-det-vum-mes-ese    not  = w-det-vum-mes-che
                     go to  det-csm-pnd-200.
           add       rf-mmv-pca-che       to   w-det-csm-pnd-wpv      .
           subtract  rf-mmv-prc-che       from w-det-csm-pnd-wpv      .
           go to     det-csm-pnd-200.
       det-csm-pnd-080.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione file righe movimenti di ma-  *
      *              * gazzino                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mmr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      d-vun-mag-tip-mag    to   rf-mmr-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmr-num-mag         .
           move      01                   to   s-gio                  .
           move      w-det-vum-mes-ese    to   s-mes                  .
           move      w-det-vum-ann-ese    to   s-saa                  .
           move      s-dat                to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-csm-pnd-200.
       det-csm-pnd-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [mmr]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At end'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-pnd-200.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-mmr-tip-mag       not  = d-vun-mag-tip-mag or
                     rf-mmr-num-mag       not  = d-vun-mag-num-mag or
                     rf-mmr-dat-reg       >    w-det-vum-dat-val
                     go to det-csm-pnd-200.
      *                  *---------------------------------------------*
      *                  * Selezioni sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo movimento non di carico : rici- *
      *                      * clo                                     *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       not  = 01
                     go to det-csm-pnd-100.
      *                      *-----------------------------------------*
      *                      * Se movimento di nessuna valorizzazione: *
      *                      * riciclo                                 *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "N"
                     go to det-csm-pnd-100.
      *                      *-----------------------------------------*
      *                      * Se movimento di valorizzazione ad impo- *
      *                      * stazione diretta e non esiste il costo  *
      *                      * unitario : riciclo                      *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "I" and
                     rf-mmr-cun-mov       =    zero
                     go to det-csm-pnd-100.
      *                      *-----------------------------------------*
      *                      * Se movimento di valorizzazione a costo  *
      *                      * standard : riciclo                      *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "S"
                     go to det-csm-pnd-100.
      *                      *-----------------------------------------*
      *                      * Se movimento di valorizzazione di fine  *
      *                      * esercizio : riciclo                     *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "X" or
                     rf-mmr-trt-val       =    "Y"
                     go to det-csm-pnd-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivi                   *
      *                  *---------------------------------------------*
           if        rf-mmr-trt-val       =    "I"
                     add       rf-mmr-qta-mov
                                          to   w-det-csm-pnd-wpq
                     add       rf-mmr-val-mov
                                          to   w-det-csm-pnd-wpv
           else if   rf-mmr-trt-val       =    "C"
                     add       rf-mmr-val-mov
                                          to   w-det-csm-pnd-wpv
           else if   rf-mmr-trt-val       =    "R"
                     subtract  rf-mmr-val-mov
                                          from w-det-csm-pnd-wpv      .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura [mmr]                    *
      *                  *---------------------------------------------*
           go to     det-csm-pnd-100.
       det-csm-pnd-200.
      *              *-------------------------------------------------*
      *              * Da una analisi con Mario è stata by-passata     *
      *              * questa parte (03/11/2015) in quanto non teneva  *
      *              * conto del costo a inizio anno                   *
      *              *                                                 *
      *              * Sottoposto a personalizzazione                  *
      *              *-------------------------------------------------*
           if        w-prs-val-ini        =    "S"
                     go to det-csm-pnd-300.
       det-csm-pnd-220.
      *              *-------------------------------------------------*
      *              * Se e' stata trovata almeno una quantita' valo-  *
      *              * rizzata nel corso dell'esercizio attuale, si    *
      *              * calcola il costo medio ponderato e si esce      *
      *              *-------------------------------------------------*
           if        w-det-csm-pnd-wpq    =    zero
                     go to det-csm-pnd-300.
      *                  *---------------------------------------------*
      *                  * Determinazione costo medio ponderato        *
      *                  *                                             *
      *                  * Nota : la data costo rimane quella in input *
      *                  *---------------------------------------------*
           divide    w-det-csm-pnd-wpq    into w-det-csm-pnd-wpv
                                        giving w-det-csm-pnd-cst
                                               rounded                .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           multiply  100                  by   w-det-csm-pnd-cst      .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di destinazione *
      *                  *---------------------------------------------*
           move      w-det-csm-pnd-cst    to   w-det-csm-pnd-vun      .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     det-csm-pnd-800.
       det-csm-pnd-300.
      *              *-------------------------------------------------*
      *              * Se invece non e' stata trovata nessuna quanti-  *
      *              * ta' valorizzata nel corso dell'esercizio attu-  *
      *              * ale, allora il costo m.p. deve essere determi-  *
      *              * nato in base al castelletto L.i.f.o. contenuto  *
      *              * nella scheda valore attuale o al massimo in     *
      *              * quella dell'anno precedente                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore 01..10           *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-csm-pnd-ctr      .
       det-csm-pnd-310.
      *                  *---------------------------------------------*
      *                  * Incremento contatore 01..10                 *
      *                  *---------------------------------------------*
           add       1                    to   w-det-csm-pnd-ctr      .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : a lettura e scansione *
      *                  * scheda esercizio precedente                 *
      *                  *---------------------------------------------*
           if        w-det-csm-pnd-ctr    >    10
                     go to det-csm-pnd-350.
      *                  *---------------------------------------------*
      *                  * Se c.m.p. per L.i.f.o. in esame a zero :    *
      *                  * riciclo su esercizio precedente della       *
      *                  * scheda in esame                             *
      *                  *---------------------------------------------*
           if        rf-mmv-ump-pre
                    (w-det-csm-pnd-ctr)   =    zero
                     go to det-csm-pnd-310.
       det-csm-pnd-320.
      *                  *---------------------------------------------*
      *                  * Preparazione c.m.p. in uscita               *
      *                  *---------------------------------------------*
           move      rf-mmv-ump-pre
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-vun      .
       det-csm-pnd-330.
      *                  *---------------------------------------------*
      *                  * Preparazione data costo in uscita           *
      *                  *---------------------------------------------*
           move      rf-mmv-ann-ese       to   s-saa                  .
           subtract  w-det-csm-pnd-ctr    from s-saa                  .
           move      w-det-vum-mes-che    to   s-mes                  .
           move      31                   to   s-gio                  .
       det-csm-pnd-332.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-csm-pnd-332.
           move      s-dat                to   d-vun-mag-dat-val      .
       det-csm-pnd-340.
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     det-csm-pnd-800.
       det-csm-pnd-350.
      *                  *---------------------------------------------*
      *                  * Se la scheda in esame e' gia' quella rela-  *
      *                  * tiva all'esercizio precedente : uscita con  *
      *                  * c.m.p. indeterminato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        rf-mmv-ann-ese       =    w-det-vum-ann-ese
                     go to det-csm-pnd-370.
       det-csm-pnd-360.
      *                      *-----------------------------------------*
      *                      * Status di uscita : non determinato      *
      *                      *-----------------------------------------*
           move      "#"                  to   d-vun-mag-exi-sts      .
      *                      *-----------------------------------------*
      *                      * Valore in uscita : zero                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-csm-pnd-vun      .
      *                      *-----------------------------------------*
      *                      * Data in uscita : zero                   *
      *                      *-----------------------------------------*
           move      zero                 to   d-vun-mag-dat-val      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     det-csm-pnd-800.
       det-csm-pnd-370.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmv]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Lettura scheda esercizio precedente         *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-ann-ese    to   rf-mmv-ann-ese         .
           subtract  1                    from rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Se scheda non trovata : a valore indeter-   *
      *                  * minato                                      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-pnd-360.
       det-csm-pnd-400.
      *                  *---------------------------------------------*
      *                  * Esame progressivi annuali per vedere se si  *
      *                  * riesce a determinare il c.m.p. da essi      *
      *                  *---------------------------------------------*
       det-csm-pnd-405.
      *                      *-----------------------------------------*
      *                      * Normalizzazione progressivi di comodo   *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-csm-pnd-wpq      .
           move      zero                 to   w-det-csm-pnd-wpv      .
      *                      *-----------------------------------------*
      *                      * In presenza di una rimanenza iniziale,  *
_ NEW_*                      * si incrementano i progressivi come se   *
      *                      * si trattasse del primo carico in asso-  *
      *                      * luto                                    *
      *                      *-----------------------------------------*
           if        w-det-csm-pnd-wri    not  = zero
                     move  w-det-csm-pnd-wri
                                          to   w-det-csm-pnd-wpq      .
           if        w-det-csm-pnd-wci    not  = zero
                     move  w-det-csm-pnd-wci
                                          to   w-det-csm-pnd-wpv      .
           if        w-det-csm-pnd-wri    not  = zero
                     multiply w-det-csm-pnd-wri
                                          by   w-det-csm-pnd-wpv      .
      *                      *-----------------------------------------*
      *                      * Applicazione decimali valuta base       *
      *                      *-----------------------------------------*
           if        w-det-csm-pnd-wpv    not  = zero
                     divide   100         into w-det-csm-pnd-wpv      .
       det-csm-pnd-410.
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione su progressivi men-  *
      *                      * sili                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-csm-pnd-ctr      .
       det-csm-pnd-420.
           add       1                    to   w-det-csm-pnd-ctr      .
           if        w-det-csm-pnd-ctr    >    12
                     go to  det-csm-pnd-440.
           if        rf-mmv-qtv-mns
                    (w-det-csm-pnd-ctr)   =    zero
                     go to det-csm-pnd-420.
           add       rf-mmv-qtv-mns
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-wpq      .
           add       rf-mmv-vlv-mns
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-wpv      .
           add       rf-mmv-pca-mns
                    (w-det-csm-pnd-ctr)   to   w-det-csm-pnd-wpv      .
           subtract  rf-mmv-prc-mns
                    (w-det-csm-pnd-ctr)   from w-det-csm-pnd-wpv      .
           go to     det-csm-pnd-420.
       det-csm-pnd-440.
      *                      *-----------------------------------------*
      *                      * Aggiustamento con valori di aggiusta-   *
      *                      * mento di fine esercizio                 *
      *                      *-----------------------------------------*
           add       rf-mmv-pca-che       to   w-det-csm-pnd-wpv      .
           subtract  rf-mmv-prc-che       from w-det-csm-pnd-wpv      .
       det-csm-pnd-450.
      *                      *-----------------------------------------*
      *                      * Se la quantita' valorizzata totale e'   *
      *                      * ancora a zero : a tentativo di determi- *
      *                      * nazione da castelletto L.i.f.o.         *
      *                      *-----------------------------------------*
           if        w-det-csm-pnd-wpq    =    zero
                     go to det-csm-pnd-300.
       det-csm-pnd-460.
      *                  *---------------------------------------------*
      *                  * Preparazione c.m.p. in uscita               *
      *                  *---------------------------------------------*
           divide    w-det-csm-pnd-wpq    into w-det-csm-pnd-wpv
                                        giving w-det-csm-pnd-cst
                                               rounded                .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           multiply  100                  by   w-det-csm-pnd-cst      .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di destinazione *
      *                  *---------------------------------------------*
           move      w-det-csm-pnd-cst    to   w-det-csm-pnd-vun      .
       det-csm-pnd-470.
      *                  *---------------------------------------------*
_ NEW_*                  * Test se data gia' rilevata                  *
      *                  *---------------------------------------------*
           if        d-vun-mag-dat-val    not  = zero
                     go to det-csm-pnd-480.
      *                  *---------------------------------------------*
      *                  * Preparazione data costo in uscita           *
      *                  *---------------------------------------------*
           move      rf-mmv-ann-ese       to   s-saa                  .
           move      w-det-vum-mes-che    to   s-mes                  .
           move      31                   to   s-gio                  .
       det-csm-pnd-472.
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-csm-pnd-472.
           move      s-dat                to   d-vun-mag-dat-val      .
       det-csm-pnd-480.
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     det-csm-pnd-800.
       det-csm-pnd-800.
      *              *-------------------------------------------------*
      *              * Eventuale ritaratura                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N"
                     go to det-csm-pnd-850.
      *                  *---------------------------------------------*
      *                  * Applicazione del coefficiente               *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "D"
                     multiply 10          by   w-det-csm-pnd-vun
           else if   w-prs-arg-mag-ecu    =    "C"
                     multiply 100         by   w-det-csm-pnd-vun
           else if   w-prs-arg-mag-ecu    =    "K"
                     multiply 1000        by   w-det-csm-pnd-vun      .
       det-csm-pnd-850.
      *              *-------------------------------------------------*
      *              * Valore determinato in campo di destinazione     *
      *              *-------------------------------------------------*
           move      w-det-csm-pnd-vun    to   d-vun-mag-val-uni      .
       det-csm-pnd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-csm-pnd-999.
       det-csm-pnd-999.
           exit.

      *    *===========================================================*
_ NEW_*    * Determinazione rimanenza iniziale per il calcolo del      *
      *    * Costo medio ponderato dell'anno                           *
      *    *-----------------------------------------------------------*
       det-rmv-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione progressivi di comodo           *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csm-pnd-wri      .
           move      zero                 to   w-det-csm-pnd-wci      .
           move      zero                 to   w-det-csm-pnd-wcd      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per scansione            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csm-pnd-ct0      .
           move      w-det-vum-ann-ese    to   w-det-csm-pnd-ese      .
           add       1                    to   w-det-csm-pnd-ese      .
       det-rmv-ini-200.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione a ritroso per determinare la *
      *              * rimanenza ed il costo medio ponderato iniziali  *
      *              *                                                 *
      *              * Si retrocede fino ad un massimo di 10 esercizi, *
      *              * il che significa che si cercano valori non      *
      *              * oltre i 20 anni                                 *
      *              *-------------------------------------------------*
           subtract  1                    from w-det-csm-pnd-ese      .
           add       1                    to   w-det-csm-pnd-ct0      .
           if        w-det-csm-pnd-ct0    >    10
                     go to det-rmv-ini-900.
       det-rmv-ini-250.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [mmv]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *              *-------------------------------------------------*
      *              * Lettura record [mmv]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-csm-pnd-ese    to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-rmv-ini-900.
       det-rmv-ini-400.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione 10 esercizi precedenti       *
      *              * all'interno della scheda in esame               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csm-pnd-inx      .
       det-rmv-ini-420.
           add       1                    to   w-det-csm-pnd-inx      .
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
           if        w-det-csm-pnd-inx    >    10
                     go to det-rmv-ini-800.
       det-rmv-ini-440.
      *              *-------------------------------------------------*
      *              * Test su rimanenza finale dell'esercizio in      *
      *              * esame                                           *
      *              *-------------------------------------------------*
           if        rf-mmv-rim-pre
                    (w-det-csm-pnd-inx)   =    zero
                     go to det-rmv-ini-460.
      *              *-------------------------------------------------*
      *              * Trattamento rimanenza finale                    *
      *              *-------------------------------------------------*
           if        w-det-csm-pnd-wri    =    zero
                     move rf-mmv-rim-pre
                         (w-det-csm-pnd-inx)
                                          to   w-det-csm-pnd-wri      .
       det-rmv-ini-460.
      *              *-------------------------------------------------*
      *              * Test su Costo medio ponderato finale dell'eser- *
      *              * cizio in esame                                  *
      *              *-------------------------------------------------*
           if        rf-mmv-ump-pre
                    (w-det-csm-pnd-inx)   =    zero
                     go to det-rmv-ini-600.
      *              *-------------------------------------------------*
      *              * Trattamento Costo medio ponderato               *
      *              *-------------------------------------------------*
           if        w-det-csm-pnd-wci    =    zero
                     move rf-mmv-ump-pre
                         (w-det-csm-pnd-inx)
                                          to   w-det-csm-pnd-wci      .
       det-rmv-ini-500.
      *              *-------------------------------------------------*
      *              * Se entrambi i valori sono presenti, si esce con *
      *              * essi e una data di rilevazione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se determinati entrambi i valori       *
      *                  *---------------------------------------------*
           if        w-det-csm-pnd-wri    =    zero or
                     w-det-csm-pnd-wci    =    zero
                     go to det-rmv-ini-600.
      *                  *---------------------------------------------*
      *                  * Data di rilevazione                         *
      *                  *---------------------------------------------*
           move      w-det-csm-pnd-ese    to   s-saa                  .
           subtract  w-det-csm-pnd-inx    from s-saa                  .
           move      12                   to   s-mes                  .
           move      31                   to   s-gio                  .
           move      s-dat                to   w-det-csm-pnd-wcd      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-rmv-ini-900.
       det-rmv-ini-600.
      *              *-------------------------------------------------*
      *              * Riciclo a scaglione precedente                  *
      *              *-------------------------------------------------*
           go to     det-rmv-ini-420.
       det-rmv-ini-800.
      *              *-------------------------------------------------*
      *              * Riciclo a scheda 'mmv' precedente               *
      *              *-------------------------------------------------*
           go to     det-rmv-ini-200.
       det-rmv-ini-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-rmv-ini-999.
       det-rmv-ini-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario : Costo ultimo             *
      *    *-----------------------------------------------------------*
       det-cst-ult-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work di comodo                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cst-ult-dcu      .
           move      zero                 to   w-det-cst-ult-ucu      .
      *              *-------------------------------------------------*
      *              * Lettura record [mmv] dell'anno in corso         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmv]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mmv]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-ann-ese    to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cst-ult-200.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione per accumulo progressivi men-*
      *              * sili anno in corso fino a mese precedente a     *
      *              * quello indicato da data richiesta               *
      *              *-------------------------------------------------*
           move      w-det-vum-mes-ese    to   w-det-cst-ult-ctr      .
       det-cst-ult-020.
           subtract  1                    from w-det-cst-ult-ctr      .
           if        w-det-cst-ult-ctr    =    zero
                     go to det-cst-ult-040.
           if        rf-mmv-dcu-mns
                    (w-det-cst-ult-ctr)   =    zero
                     go to det-cst-ult-020.
           if        rf-mmv-dcu-mns
                    (w-det-cst-ult-ctr)   <    w-det-cst-ult-dcu
                     go to det-cst-ult-020.
           move      rf-mmv-dcu-mns
                    (w-det-cst-ult-ctr)   to   w-det-cst-ult-dcu      .
           move      rf-mmv-ucu-mns
                    (w-det-cst-ult-ctr)   to   w-det-cst-ult-ucu      .
           go to     det-cst-ult-020.
       det-cst-ult-040.
      *              *-------------------------------------------------*
      *              * Se data richiesta saldo uguale a data attuale   *
      *              * si aggiorna con i valori del mese attuale       *
      *              *-------------------------------------------------*
           if        w-det-vum-dat-val    not  = w-det-vum-dat-att
                     go to  det-cst-ult-060.
           if        rf-mmv-dcu-mns
                    (w-det-vum-mes-ese)   <    w-det-cst-ult-dcu
                     go to det-cst-ult-060.
           move      rf-mmv-dcu-mns
                    (w-det-vum-mes-ese)   to   w-det-cst-ult-dcu      .
           move      rf-mmv-ucu-mns
                    (w-det-vum-mes-ese)   to   w-det-cst-ult-ucu      .
           go to     det-cst-ult-200.
       det-cst-ult-060.
      *              *-------------------------------------------------*
      *              * Controllo se giorno della data richiesta e' di  *
      *              * fine mese                                       *
      *              *-------------------------------------------------*
           move      w-det-vum-dat-val    to   s-dat                  .
           add       1                    to   s-gio                  .
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se la data e' corretta vuol dire che il     *
      *                  * giorno della data richiesta non e' a fine   *
      *                  * mese                                        *
      *                  *---------------------------------------------*
           if        s-sts                =    spaces
                     go to  det-cst-ult-080.
      *                      *-----------------------------------------*
      *                      * Se fine mese si aggiorna con  i valori  *
      *                      * del mese attuale                        *
      *                      *-----------------------------------------*
           if        rf-mmv-dcu-mns
                    (w-det-vum-mes-ese)   <    w-det-cst-ult-dcu
                     go to det-cst-ult-080.
           move      rf-mmv-dcu-mns
                    (w-det-vum-mes-ese)   to   w-det-cst-ult-dcu      .
           move      rf-mmv-ucu-mns
                    (w-det-vum-mes-ese)   to   w-det-cst-ult-ucu      .
           go to     det-cst-ult-200.
       det-cst-ult-080.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione file righe movimenti di ma-  *
      *              * gazzino                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mmr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      d-vun-mag-tip-mag    to   rf-mmr-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmr-num-mag         .
           move      01                   to   s-gio                  .
           move      w-det-vum-mes-ese    to   s-mes                  .
           move      w-det-vum-ann-ese    to   s-saa                  .
           move      s-dat                to   rf-mmr-dat-reg         .
           move      zero                 to   rf-mmr-num-prt         .
           move      zero                 to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-cst-ult-200.
       det-cst-ult-100.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale file [mmr]              *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                      *-----------------------------------------*
      *                      * Test se 'At end'                        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cst-ult-200.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-mmr-tip-mag       not  = d-vun-mag-tip-mag or
                     rf-mmr-num-mag       not  = d-vun-mag-num-mag or
                     rf-mmr-dat-reg       >    w-det-vum-dat-val
                     go to det-cst-ult-200.
      *                  *---------------------------------------------*
      *                  * Selezioni sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo movimento non di carico : rici- *
      *                      * clo                                     *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       not  = 01
                     go to det-cst-ult-100.
      *                      *-----------------------------------------*
      *                      * Se movimento non ad impostazione diret- *
      *                      * ta : riciclo                            *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       not  = "I"
                     go to det-cst-ult-100.
      *                      *-----------------------------------------*
      *                      * Se costo unitario a zero : riciclo      *
      *                      *-----------------------------------------*
           if        rf-mmr-cun-mov       =    zero
                     go to det-cst-ult-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento valori                        *
      *                  *---------------------------------------------*
           if        rf-mmr-dat-reg       <    w-det-cst-ult-dcu
                     go to det-cst-ult-180.
           move      rf-mmr-dat-reg       to   w-det-cst-ult-dcu      .
           move      rf-mmr-cun-mov       to   w-det-cst-ult-ucu      .
       det-cst-ult-180.
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura [mmr]                    *
      *                  *---------------------------------------------*
           go to     det-cst-ult-100.
       det-cst-ult-200.
      *              *-------------------------------------------------*
      *              * Se e' stata trovato almeno un costo ultimo nel  *
      *              * corso dell'esercizio attuale                    *
      *              *-------------------------------------------------*
           if        w-det-cst-ult-dcu    =    zero
                     go to det-cst-ult-300.
      *                  *---------------------------------------------*
      *                  * Valori in uscita                            *
      *                  *---------------------------------------------*
           move      w-det-cst-ult-dcu    to   d-vun-mag-dat-val      .
           move      w-det-cst-ult-ucu    to   d-vun-mag-val-uni      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-cst-ult-800.
       det-cst-ult-300.
      *              *-------------------------------------------------*
      *              * Altrimenti si deve calcolare il costo ultimo    *
      *              * dell'esercizio precedente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work di comodo              *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-cst-ult-dcu      .
           move      zero                 to   w-det-cst-ult-ucu      .
      *                  *---------------------------------------------*
      *                  * Se si e' in periodo d'inventario            *
      *                  *---------------------------------------------*
           if        w-dnu-ife-ife-flg    =    spaces
                     go to det-cst-ult-400.
      *                      *-----------------------------------------*
      *                      * Se l'anno di inventario in corso non e' *
      *                      * quello precedente l'anno relativo alla  *
      *                      * data in cui e' richiesto il saldo : co- *
      *                      * me per periodo non di inventario        *
      *                      *-----------------------------------------*
           if        w-dnu-ife-ife-ese    not  = w-det-vum-ann-prc
                     go to det-cst-ult-400.
      *                      *-----------------------------------------*
      *                      * Esercizio da leggere : quello di inven- *
      *                      * tario                                   *
      *                      *-----------------------------------------*
           move      w-dnu-ife-ife-ese    to    w-det-vum-wrk-ann     .
       det-cst-ult-310.
      *                      *-----------------------------------------*
      *                      * Lettura record [mmv] relativo all'eser- *
      *                      * cizio precedente                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione record [mmv]        *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                          *-------------------------------------*
      *                          * Lettura record [mmv]                *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-wrk-ann    to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cst-ult-340.
      *                      *-----------------------------------------*
      *                      * Valori di inizio esercizio              *
      *                      *-----------------------------------------*
           move      rf-mmv-dcu-ies       to   w-det-cst-ult-dcu      .
           move      rf-mmv-ucu-ies       to   w-det-cst-ult-ucu      .
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione su progressivi men-  *
      *                      * sili                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-cst-ult-ctr      .
       det-cst-ult-320.
           add       1                    to   w-det-cst-ult-ctr      .
           if        w-det-cst-ult-ctr    >    12
                     go to  det-cst-ult-340.
           if        rf-mmv-dcu-mns
                    (w-det-cst-ult-ctr)   =    zero
                     go to det-cst-ult-320.
           if        rf-mmv-dcu-mns
                    (w-det-cst-ult-ctr)   <    w-det-cst-ult-dcu
                     go to det-cst-ult-320.
           move      rf-mmv-dcu-mns
                    (w-det-cst-ult-ctr)   to   w-det-cst-ult-dcu      .
           move      rf-mmv-ucu-mns
                    (w-det-cst-ult-ctr)   to   w-det-cst-ult-ucu      .
           go to     det-cst-ult-320.
       det-cst-ult-340.
      *                          *-------------------------------------*
      *                          * Se valori costo ultimo ancora inde- *
      *                          * terminati                           *
      *                          *-------------------------------------*
           if        w-det-cst-ult-dcu    not  = zero
                     go to det-cst-ult-360.
      *                              *---------------------------------*
      *                              * Status di uscita ad indetermi-  *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "#"                  to   d-vun-mag-exi-sts      .
      *                              *---------------------------------*
      *                              * Valore in uscita a zero         *
      *                              *---------------------------------*
           move      zero                 to   d-vun-mag-val-uni      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     det-cst-ult-800.
       det-cst-ult-360.
      *                          *-------------------------------------*
      *                          * Valori in uscita                    *
      *                          *-------------------------------------*
           move      w-det-cst-ult-dcu    to   d-vun-mag-dat-val      .
           move      w-det-cst-ult-ucu    to   d-vun-mag-val-uni      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-cst-ult-800.
       det-cst-ult-400.
      *                  *---------------------------------------------*
      *                  * Se non si e' in periodo d'inventario        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se si e' in fase pre-inventario bisogna *
      *                      * agire analogamente a quanto si rileva   *
      *                      * in periodo di inventario                *
      *                      *-----------------------------------------*
           if        w-det-vum-ann-prc    >    w-dnu-ife-ife-ese
                     move  w-det-vum-ann-prc
                                          to   w-det-vum-wrk-ann
                     go to det-cst-ult-310.
      *                      *-----------------------------------------*
      *                      * Test su valori costo ultimo di inizio   *
      *                      * esercizio                               *
      *                      *-----------------------------------------*
           if        rf-mmv-dcu-ies       not  = zero
                     go to det-cst-ult-420.
      *                          *-------------------------------------*
      *                          * Se a zero                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Status di uscita ad indetermi-  *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "#"                  to   d-vun-mag-exi-sts      .
      *                              *---------------------------------*
      *                              * Valore in uscita a zero         *
      *                              *---------------------------------*
           move      zero                 to   d-vun-mag-val-uni      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     det-cst-ult-800.
       det-cst-ult-420.
      *                          *-------------------------------------*
      *                          * Valori in uscita                    *
      *                          *-------------------------------------*
           move      rf-mmv-dcu-ies       to   d-vun-mag-dat-val      .
           move      rf-mmv-ucu-ies       to   d-vun-mag-val-uni      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-cst-ult-800.
       det-cst-ult-800.
      *              *-------------------------------------------------*
      *              * Eventuale ritaratura                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Al momento inibita                          *
      *                  *---------------------------------------------*
           go to     det-cst-ult-900.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N"
                     go to det-cst-ult-900.
      *                  *---------------------------------------------*
      *                  * Applicazione del coefficiente               *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "D"
                     multiply 10          by   d-vun-mag-val-uni
           else if   w-prs-arg-mag-ecu    =    "C"
                     multiply 100         by   d-vun-mag-val-uni
           else if   w-prs-arg-mag-ecu    =    "K"
                     multiply 1000        by   d-vun-mag-val-uni      .
       det-cst-ult-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cst-ult-999.
       det-cst-ult-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario : Costo ultimo             *
      *    *-----------------------------------------------------------*
       det-cst-std-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work di comodo                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cst-std-dst      .
           move      zero                 to   w-det-cst-std-ust      .
      *              *-------------------------------------------------*
      *              * Lettura record [mmv] dell'anno in corso         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmv]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mmv]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-ann-ese    to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cst-std-040.
      *              *-------------------------------------------------*
      *              * Valori globali per tutto l'anno                 *
      *              *-------------------------------------------------*
           move      rf-mmv-dst-gea       to   w-det-cst-std-dst      .
           move      rf-mmv-ust-gea       to   w-det-cst-std-ust      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione dati mensili anno in corso   *
      *              * fino al mese indicato da data richiesta         *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cst-std-ctr      .
       det-cst-std-020.
           add       1                    to   w-det-cst-std-ctr      .
           if        w-det-cst-std-ctr    >    w-det-vum-mes-ese
                     go to det-cst-std-040.
           if        rf-mmv-dst-mns
                    (w-det-cst-std-ctr)   =    zero
                     go to det-cst-std-020.
           if        rf-mmv-dst-mns
                    (w-det-cst-std-ctr)   <    w-det-cst-std-dst
                     go to det-cst-std-020.
           move      rf-mmv-dst-mns
                    (w-det-cst-std-ctr)   to   w-det-cst-std-dst      .
           move      rf-mmv-ust-mns
                    (w-det-cst-std-ctr)   to   w-det-cst-std-ust      .
           go to     det-cst-std-020.
       det-cst-std-040.
      *              *-------------------------------------------------*
      *              * Se e' stato trovato almeno un costo standard    *
      *              * nel corso dell'esercizio attuale                *
      *              *-------------------------------------------------*
           if        w-det-cst-std-dst    =    zero
                     go to det-cst-std-300.
      *                  *---------------------------------------------*
      *                  * Valori in uscita                            *
      *                  *---------------------------------------------*
           move      w-det-cst-std-dst    to   d-vun-mag-dat-val      .
           move      w-det-cst-std-ust    to   d-vun-mag-val-uni      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-cst-std-999.
       det-cst-std-300.
      *              *-------------------------------------------------*
      *              * Altrimenti si deve calcolare il costo standard  *
      *              * dell'esercizio precedente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work di comodo              *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-cst-std-dst      .
           move      zero                 to   w-det-cst-std-ust      .
      *                  *---------------------------------------------*
      *                  * Se si e' in periodo d'inventario            *
      *                  *---------------------------------------------*
           if        w-dnu-ife-ife-flg    =    spaces
                     go to det-cst-std-400.
      *                      *-----------------------------------------*
      *                      * Se l'anno di inventario in corso non e' *
      *                      * quello precedente l'anno relativo alla  *
      *                      * data in cui e' richiesto il saldo : co- *
      *                      * me per periodo non di inventario        *
      *                      *-----------------------------------------*
           if        w-dnu-ife-ife-ese    not  = w-det-vum-ann-prc
                     go to det-cst-std-400.
      *                      *-----------------------------------------*
      *                      * Esercizio da leggere : quello di inven- *
      *                      * tario                                   *
      *                      *-----------------------------------------*
           move      w-dnu-ife-ife-ese    to    w-det-vum-wrk-ann     .
       det-cst-std-310.
      *                      *-----------------------------------------*
      *                      * Lettura record [mmv] relativo all'eser- *
      *                      * cizio precedente                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione record [mmv]        *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                          *-------------------------------------*
      *                          * Lettura record [mmv]                *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-wrk-ann    to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cst-std-340.
      *                      *-----------------------------------------*
      *                      * Valori di inizio esercizio              *
      *                      *-----------------------------------------*
           move      rf-mmv-dst-ies       to   w-det-cst-std-dst      .
           move      rf-mmv-ust-ies       to   w-det-cst-std-ust      .
      *                      *-----------------------------------------*
      *                      * Test su valori globali per tutto l'anno *
      *                      *-----------------------------------------*
           if        rf-mmv-dst-gea       <    w-det-cst-std-dst
                     go to det-cst-std-315.
           move      rf-mmv-dst-gea       to   w-det-cst-std-dst      .
           move      rf-mmv-ust-gea       to   w-det-cst-std-ust      .
       det-cst-std-315.
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione su progressivi men-  *
      *                      * sili                                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-cst-std-ctr      .
       det-cst-std-320.
           add       1                    to   w-det-cst-std-ctr      .
           if        w-det-cst-std-ctr    >    12
                     go to  det-cst-std-340.
           if        rf-mmv-dst-mns
                    (w-det-cst-std-ctr)   =    zero
                     go to det-cst-std-320.
           if        rf-mmv-dst-mns
                    (w-det-cst-std-ctr)   <    w-det-cst-std-dst
                     go to det-cst-std-320.
           move      rf-mmv-dst-mns
                    (w-det-cst-std-ctr)   to   w-det-cst-std-dst      .
           move      rf-mmv-ust-mns
                    (w-det-cst-std-ctr)   to   w-det-cst-std-ust      .
           go to     det-cst-std-320.
       det-cst-std-340.
      *                          *-------------------------------------*
      *                          * Se valori costo standard ancora in- *
      *                          * determinati                         *
      *                          *-------------------------------------*
           if        w-det-cst-std-dst    not  = zero
                     go to det-cst-std-360.
      *                              *---------------------------------*
      *                              * Status di uscita ad indetermi-  *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "#"                  to   d-vun-mag-exi-sts      .
      *                              *---------------------------------*
      *                              * Valore in uscita a zero         *
      *                              *---------------------------------*
           move      zero                 to   d-vun-mag-val-uni      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     det-cst-std-999.
       det-cst-std-360.
      *                          *-------------------------------------*
      *                          * Valori in uscita                    *
      *                          *-------------------------------------*
           move      w-det-cst-std-dst    to   d-vun-mag-dat-val      .
           move      w-det-cst-std-ust    to   d-vun-mag-val-uni      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-cst-std-999.
       det-cst-std-400.
      *                  *---------------------------------------------*
      *                  * Se non si e' in periodo d'inventario        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se si e' in fase pre-inventario bisogna *
      *                      * agire analogamente a quanto si rileva   *
      *                      * in periodo di inventario                *
      *                      *-----------------------------------------*
           if        w-det-vum-ann-prc    >    w-dnu-ife-ife-ese
                     move  w-det-vum-ann-prc
                                          to   w-det-vum-wrk-ann
                     go to det-cst-std-310.
      *                      *-----------------------------------------*
      *                      * Test su valori costo ultimo di inizio   *
      *                      * esercizio                               *
      *                      *-----------------------------------------*
           if        rf-mmv-dst-ies       not  = zero
                     go to det-cst-std-420.
      *                          *-------------------------------------*
      *                          * Se a zero                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Status di uscita ad indetermi-  *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "#"                  to   d-vun-mag-exi-sts      .
      *                              *---------------------------------*
      *                              * Valore in uscita a zero         *
      *                              *---------------------------------*
           move      zero                 to   d-vun-mag-val-uni      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     det-cst-std-999.
       det-cst-std-420.
      *                          *-------------------------------------*
      *                          * Valori in uscita                    *
      *                          *-------------------------------------*
           move      rf-mmv-dst-ies       to   d-vun-mag-dat-val      .
           move      rf-mmv-ust-ies       to   d-vun-mag-val-uni      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-cst-std-999.
       det-cst-std-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario : Costo L.i.f.o.           *
      *    *-----------------------------------------------------------*
       det-cst-lif-000.
      *              *-------------------------------------------------*
      *              * Salvataggio della data di valorizzazione, in    *
      *              * quanto sara' modificata dalla successiva rou-   *
      *              * tine di determinazione del costo medio pondera- *
      *              * to                                              *
      *              *-------------------------------------------------*
           move      d-vun-mag-dat-val    to   w-dcv-lif-sav-ddv      .
      *              *-------------------------------------------------*
      *              * Richiamo della routine di determinazione del    *
      *              * costo medio ponderato alla data, che provvede   *
      *              * inoltre a bufferizzare il record [mmv] relativo *
      *              * all'anno in corso                               *
      *              *-------------------------------------------------*
           perform   det-csm-pnd-000      thru det-csm-pnd-999        .
      *              *-------------------------------------------------*
      *              * Ripristino del record [mmv] relativo all'anno   *
      *              * in corso                                        *
      *              *-------------------------------------------------*
           move      w-det-cst-lif-mmv    to   rf-mmv                 .
       det-cst-lif-005.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per il richiamo della    *
      *              * subroutine di determinazione del L.i.f.o.       *
      *              *-------------------------------------------------*
       det-cst-lif-010.
      *                  *---------------------------------------------*
      *                  * Anno di esercizio attuale, di 4 cifre       *
      *                  *---------------------------------------------*
           move      w-dcv-lif-sav-ddv    to   s-dat                  .
           move      s-saa                to   w-dcv-lif-ann-ese      .
           add       1900                 to   w-dcv-lif-ann-ese      .
       det-cst-lif-020.
      *                  *---------------------------------------------*
      *                  * Dati relativi all' anno di esercizio attua- *
      *                  * le                                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Rimanenza alla data, come da giacenza   *
      *                      * di proprieta' in input                  *
      *                      *-----------------------------------------*
           move      d-vun-mag-gia-prp    to   w-dcv-lif-rim-ese (01) .
      *                      *-----------------------------------------*
      *                      * Costo medio ponderato alla data, come   *
      *                      * appena calcolato dalla routine di de-   *
      *                      * terminazione c.m.p.                     *
      *                      *-----------------------------------------*
           move      d-vun-mag-val-uni    to   w-dcv-lif-ump-ese (01) .
      *                      *-----------------------------------------*
      *                      * Valore normale del bene nell'ultimo     *
      *                      * trimestre dell'esercizio, solo se la    *
      *                      * data corrisponde alla data di fine e-   *
      *                      * sercizio                                *
      *                      *-----------------------------------------*
           if        d-vun-mag-dat-val    =    w-det-vum-dat-fie
                     move  rf-mmv-vnb-gea to   w-dcv-lif-vnb-ese (01)
           else      move  zero           to   w-dcv-lif-vnb-ese (01) .
      *                      *-----------------------------------------*
      *                      * Flag di rivalutazione per l'esercizio   *
      *                      * attuale, solo se la data corrisponde    *
      *                      * alla data di fine esercizio             *
      *                      *-----------------------------------------*
           if        d-vun-mag-dat-val    =    w-det-vum-dat-fie
                     move  rf-mmv-fdr-gea to   w-dcv-lif-fdr-ese (01)
           else      move  spaces         to   w-dcv-lif-fdr-ese (01) .
       det-cst-lif-030.
      *                  *---------------------------------------------*
      *                  * Dati relativi agli esercizi precedenti      *
      *                  *---------------------------------------------*
       det-cst-lif-032.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatori              *
      *                      *-----------------------------------------*
           move      zero                 to   w-dcv-lif-inx-i0i      .
           move      01                   to   w-dcv-lif-inx-j0j      .
       det-cst-lif-034.
      *                      *-----------------------------------------*
      *                      * Incremento contatori                    *
      *                      *-----------------------------------------*
           add       1                    to   w-dcv-lif-inx-i0i      .
           add       1                    to   w-dcv-lif-inx-j0j      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-dcv-lif-inx-i0i    >    10
                     go to det-cst-lif-200.
       det-cst-lif-036.
      *                      *-----------------------------------------*
      *                      * Rimanenza finale dell'esercizio         *
      *                      *-----------------------------------------*
           move      rf-mmv-rim-pre
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-rim-ese
                                              (w-dcv-lif-inx-j0j)     .
      *                      *-----------------------------------------*
      *                      * Costo medio ponderato finale dell'eser- *
      *                      * cizio                                   *
      *                      *-----------------------------------------*
           move      rf-mmv-ump-pre
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-ump-ese
                                              (w-dcv-lif-inx-j0j)     .
      *                      *-----------------------------------------*
      *                      * Valore normale del bene nell'ultimo     *
      *                      * trimestre dell'esercizio, solo se la    *
      *                      * data corrisponde alla data di fine e-   *
      *                      * sercizio                                *
      *                      *-----------------------------------------*
           if        d-vun-mag-dat-val    =    w-det-vum-dat-fie
                     move  rf-mmv-vnb-pre
                          (w-dcv-lif-inx-i0i)
                                          to   w-dcv-lif-vnb-ese
                                              (w-dcv-lif-inx-j0j)
           else      move  zero           to   w-dcv-lif-vnb-ese
                                              (w-dcv-lif-inx-j0j)     .
      *                      *-----------------------------------------*
      *                      * Flag di rivalutazione per l'esercizio   *
      *                      * attuale, solo se la data corrisponde    *
      *                      * alla data di fine esercizio             *
      *                      *-----------------------------------------*
           if        d-vun-mag-dat-val    =    w-det-vum-dat-fie
                     move  rf-mmv-fdr-pre
                          (w-dcv-lif-inx-i0i)
                                          to   w-dcv-lif-fdr-ese
                                              (w-dcv-lif-inx-j0j)
           else      move  spaces         to   w-dcv-lif-fdr-ese
                                              (w-dcv-lif-inx-j0j)     .
       det-cst-lif-038.
      *                      *-----------------------------------------*
      *                      * Riciclo su elemento successivo          *
      *                      *-----------------------------------------*
           go to     det-cst-lif-034.
       det-cst-lif-200.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine di determinazione     *
      *              * del L.i.f.o.                                    *
      *              *-------------------------------------------------*
           perform   det-lif-mag-000      thru det-lif-mag-999        .
       det-cst-lif-300.
      *              *-------------------------------------------------*
      *              * Preparazione valori in uscita                   *
      *              *-------------------------------------------------*
       det-cst-lif-310.
      *                  *---------------------------------------------*
      *                  * Status di uscita                            *
      *                  *---------------------------------------------*
           move      w-dcv-lif-flg-exi    to   d-vun-mag-exi-sts      .
       det-cst-lif-320.
      *                  *---------------------------------------------*
      *                  * Valore unitario alla data, espresso come    *
      *                  * divisione tra il valore totale L.i.f.o. e   *
      *                  * la giacenza di riferimento                  *
      *                  *---------------------------------------------*
           if        w-dcv-lif-val-fin    =    zero or
                     w-dcv-lif-rim-fin    =    zero
                     move   zero          to   w-dcv-lif-rim-cst
           else      divide w-dcv-lif-rim-fin
                                          into w-dcv-lif-val-fin
                                        giving w-dcv-lif-rim-cst      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           if        w-dcv-lif-rim-cst    not  = zero
                     multiply  100        by   w-dcv-lif-rim-cst      .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di destinazione *
      *                  *---------------------------------------------*
           move      w-dcv-lif-rim-cst    to   d-vun-mag-val-uni      .
       det-cst-lif-330.
      *                  *---------------------------------------------*
      *                  * Data reale a cui si riferisce il valore u-  *
      *                  * nitario : nessuna azione in quanto gia' ap- *
      *                  * prontata dal richiamo della determinazione  *
      *                  * del costo medio ponderato                   *
      *                  *---------------------------------------------*
           go to     det-cst-lif-340.
       det-cst-lif-340.
      *                  *---------------------------------------------*
      *                  * Valore totale L.i.f.o.                      *
      *                  *---------------------------------------------*
           move      w-dcv-lif-val-fin    to   d-vun-mag-lif-val      .
       det-cst-lif-350.
      *                  *---------------------------------------------*
      *                  * Numero di scaglioni che concorrono a forma- *
      *                  * re il valore totale L.i.f.o.                *
      *                  *---------------------------------------------*
           move      w-dcv-lif-num-vlf    to   d-vun-mag-lif-nsc      .
       det-cst-lif-360.
      *                  *---------------------------------------------*
      *                  * Dati relativi ad ogni scaglione L.i.f.o.    *
      *                  *---------------------------------------------*
       det-cst-lif-370.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore              *
      *                      *-----------------------------------------*
           move      zero                 to   w-dcv-lif-inx-i0i      .
       det-cst-lif-380.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-dcv-lif-inx-i0i      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-dcv-lif-inx-i0i    >    11
                     go to det-cst-lif-900.
      *                      *-----------------------------------------*
      *                      * Quantita'                               *
      *                      *-----------------------------------------*
           move      w-dcv-lif-qta-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-qta
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Valore unitario                         *
      *                      *-----------------------------------------*
           move      w-dcv-lif-vun-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-vun
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Tipo di valore unitario                 *
      *                      *-----------------------------------------*
           move      w-dcv-lif-tpv-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-tvu
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Anno, di quattro cifre                  *
      *                      *-----------------------------------------*
           move      w-dcv-lif-ann-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-ann
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Flag di rivalutazione                   *
      *                      *-----------------------------------------*
           move      w-dcv-lif-riv-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-riv
                                              (w-dcv-lif-inx-i0i)     .
       det-cst-lif-390.
      *                      *-----------------------------------------*
      *                      * Riciclo ad elemento successivo          *
      *                      *-----------------------------------------*
           go to     det-cst-lif-380.
       det-cst-lif-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cst-lif-999.
       det-cst-lif-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario : Costo L.i.f.o. da sca-   *
      *    * glioni annuali in scheda esercizio attuale                *
      *    *-----------------------------------------------------------*
       det-cst-lfa-000.
      *              *-------------------------------------------------*
      *              * Lettura record [mmv] dell'anno in corso         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [mmv]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [mmv]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ESECOD    "         to   f-key                  .
           move      w-det-vum-dat-att    to   s-dat                  .
           move      s-saa                to   rf-mmv-ann-ese         .
           move      d-vun-mag-tip-mag    to   rf-mmv-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmv-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmmv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmv                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-cst-lfa-005.
      *                          *-------------------------------------*
      *                          * Flag di uscita                      *
      *                          *-------------------------------------*
           move      "#"                  to   d-vun-mag-exi-sts      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     det-cst-lfa-900.
       det-cst-lfa-005.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per il richiamo della    *
      *              * subroutine di determinazione del L.i.f.o.       *
      *              *-------------------------------------------------*
       det-cst-lfa-010.
      *                  *---------------------------------------------*
      *                  * Anno di esercizio attuale, di 4 cifre       *
      *                  *---------------------------------------------*
           move      w-det-vum-dat-att    to   s-dat                  .
           move      s-saa                to   w-dcv-lif-ann-ese      .
           add       1900                 to   w-dcv-lif-ann-ese      .
      *                  *---------------------------------------------*
      *                  * Anno di esercizio richiesto, di 4 cifre     *
      *                  *---------------------------------------------*
           move      d-vun-mag-dat-val    to   s-dat                  .
           move      s-saa                to   w-dcv-lif-ann-ric      .
           add       1900                 to   w-dcv-lif-ann-ric      .
       det-cst-lfa-020.
      *                  *---------------------------------------------*
      *                  * Dati relativi all' anno di esercizio ri-    *
      *                  * chiesto                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione dell'indice relativo al- *
      *                      * l'anno di cui si e' richiesto il Lifo,  *
      *                      * rispetto all'anno in corso              *
      *                      *-----------------------------------------*
           move      zero                 to   w-dcv-lif-cal-s04      .
           move      zero                 to   w-dcv-lif-inx-i1i      .
       det-cst-lfa-025.
           add       1                    to   w-dcv-lif-inx-i1i      .
           if        w-dcv-lif-inx-i1i    >    09
                     go to det-cst-lfa-028.
           subtract  w-dcv-lif-inx-i1i    from w-dcv-lif-ann-ese
                                        giving w-dcv-lif-cal-s04      .
           if        w-dcv-lif-cal-s04    =    w-dcv-lif-ann-ric
                     go to det-cst-lfa-028.
           go to     det-cst-lfa-025.
       det-cst-lfa-028.
      *                      *-----------------------------------------*
      *                      * Rimanenza alla data, come da rimanenza  *
      *                      * dello scaglione in esame                *
      *                      *-----------------------------------------*
           move      rf-mmv-rim-pre
                    (w-dcv-lif-inx-i1i)   to   w-dcv-lif-rim-ese (01) .
      *                      *-----------------------------------------*
      *                      * Costo medio ponderato alla data, da     *
      *                      * scaglione in esame                      *
      *                      *-----------------------------------------*
           move      rf-mmv-ump-pre
                    (w-dcv-lif-inx-i1i)   to   w-dcv-lif-ump-ese (01) .
      *                      *-----------------------------------------*
      *                      * Valore normale del bene nell'ultimo     *
      *                      * trimestre dell'esercizio, da scaglione  *
      *                      * in esame                                *
      *                      *-----------------------------------------*
           move      rf-mmv-vnb-pre
                    (w-dcv-lif-inx-i1i)   to   w-dcv-lif-vnb-ese (01) .
      *                      *-----------------------------------------*
      *                      * Flag di rivalutazione per l'esercizio   *
      *                      * da scaglione                            *
      *                      *-----------------------------------------*
           move      rf-mmv-fdr-pre
                    (w-dcv-lif-inx-i1i)   to   w-dcv-lif-fdr-ese (01) .
       det-cst-lfa-030.
      *                  *---------------------------------------------*
      *                  * Dati relativi agli esercizi precedenti      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare             *
      *                      *-----------------------------------------*
           move      1                    to   w-dcv-lif-inx-i0i      .
       det-cst-lfa-031.
           add       1                    to   w-dcv-lif-inx-i0i      .
           if        w-dcv-lif-inx-i0i    >    11
                     go to det-cst-lfa-032.
           move      zero                 to   w-dcv-lif-rim-ese
                                              (w-dcv-lif-inx-i0i)     .
           move      zero                 to   w-dcv-lif-ump-ese
                                              (w-dcv-lif-inx-i0i)     .
           move      zero                 to   w-dcv-lif-vnb-ese
                                              (w-dcv-lif-inx-i0i)     .
           move      spaces               to   w-dcv-lif-fdr-ese
                                              (w-dcv-lif-inx-i0i)     .
           go to     det-cst-lfa-031.
       det-cst-lfa-032.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatori              *
      *                      *-----------------------------------------*
           move      w-dcv-lif-inx-i1i    to   w-dcv-lif-inx-i0i      .
           move      01                   to   w-dcv-lif-inx-j0j      .
       det-cst-lfa-034.
      *                      *-----------------------------------------*
      *                      * Incremento contatori                    *
      *                      *-----------------------------------------*
           add       1                    to   w-dcv-lif-inx-i0i      .
           add       1                    to   w-dcv-lif-inx-j0j      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-dcv-lif-inx-i0i    >    10
                     go to det-cst-lfa-200.
       det-cst-lfa-036.
      *                      *-----------------------------------------*
      *                      * Rimanenza finale dell'esercizio         *
      *                      *-----------------------------------------*
           move      rf-mmv-rim-pre
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-rim-ese
                                              (w-dcv-lif-inx-j0j)     .
      *                      *-----------------------------------------*
      *                      * Costo medio ponderato finale dell'eser- *
      *                      * cizio                                   *
      *                      *-----------------------------------------*
           move      rf-mmv-ump-pre
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-ump-ese
                                              (w-dcv-lif-inx-j0j)     .
      *                      *-----------------------------------------*
      *                      * Valore normale del bene nell'ultimo     *
      *                      * trimestre dell'esercizio                *
      *                      *-----------------------------------------*
           move      rf-mmv-vnb-pre
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-vnb-ese
                                              (w-dcv-lif-inx-j0j)     .
      *                      *-----------------------------------------*
      *                      * Flag di rivalutazione                   *
      *                      *-----------------------------------------*
           move      rf-mmv-fdr-pre
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-fdr-ese
                                              (w-dcv-lif-inx-j0j)     .
       det-cst-lfa-038.
      *                      *-----------------------------------------*
      *                      * Riciclo su elemento successivo          *
      *                      *-----------------------------------------*
           go to     det-cst-lfa-034.
       det-cst-lfa-200.
      *              *-------------------------------------------------*
      *              * Anno di esercizio attuale, pari a quello ri-    *
      *              * chiesto                                         *
      *              *-------------------------------------------------*
           move      w-dcv-lif-ann-ric    to   w-dcv-lif-ann-ese      .
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine di determinazione     *
      *              * del L.i.f.o.                                    *
      *              *-------------------------------------------------*
           perform   det-lif-mag-000      thru det-lif-mag-999        .
       det-cst-lfa-300.
      *              *-------------------------------------------------*
      *              * Preparazione valori in uscita                   *
      *              *-------------------------------------------------*
       det-cst-lfa-310.
      *                  *---------------------------------------------*
      *                  * Status di uscita                            *
      *                  *---------------------------------------------*
           move      w-dcv-lif-flg-exi    to   d-vun-mag-exi-sts      .
       det-cst-lfa-320.
      *                  *---------------------------------------------*
      *                  * Valore unitario alla data, espresso come    *
      *                  * divisione tra il valore totale L.i.f.o. e   *
      *                  * la giacenza di riferimento                  *
      *                  *---------------------------------------------*
           if        w-dcv-lif-val-fin    =    zero or
                     w-dcv-lif-rim-fin    =    zero
                     move   zero          to   w-dcv-lif-rim-cst
           else      divide w-dcv-lif-rim-fin
                                          into w-dcv-lif-val-fin
                                        giving w-dcv-lif-rim-cst      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           if        w-dcv-lif-rim-cst    not  = zero
                     multiply  100        by   w-dcv-lif-rim-cst      .
      *                  *---------------------------------------------*
      *                  * Valore determinato in campo di destinazione *
      *                  *---------------------------------------------*
           move      w-dcv-lif-rim-cst    to   d-vun-mag-val-uni      .
       det-cst-lfa-330.
      *                  *---------------------------------------------*
      *                  * Data reale a cui si riferisce il valore u-  *
      *                  * nitario : nessuna azione in quanto gia' ap- *
      *                  * prontata dal richiamo della determinazione  *
      *                  * del costo medio ponderato                   *
      *                  *---------------------------------------------*
           go to     det-cst-lfa-340.
       det-cst-lfa-340.
      *                  *---------------------------------------------*
      *                  * Valore totale L.i.f.o.                      *
      *                  *---------------------------------------------*
           move      w-dcv-lif-val-fin    to   d-vun-mag-lif-val      .
       det-cst-lfa-350.
      *                  *---------------------------------------------*
      *                  * Numero di scaglioni che concorrono a forma- *
      *                  * re il valore totale L.i.f.o.                *
      *                  *---------------------------------------------*
           move      w-dcv-lif-num-vlf    to   d-vun-mag-lif-nsc      .
       det-cst-lfa-360.
      *                  *---------------------------------------------*
      *                  * Dati relativi ad ogni scaglione L.i.f.o.    *
      *                  *---------------------------------------------*
       det-cst-lfa-370.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore              *
      *                      *-----------------------------------------*
           move      zero                 to   w-dcv-lif-inx-i0i      .
       det-cst-lfa-380.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-dcv-lif-inx-i0i      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-dcv-lif-inx-i0i    >    11
                     go to det-cst-lfa-900.
      *                      *-----------------------------------------*
      *                      * Quantita'                               *
      *                      *-----------------------------------------*
           move      w-dcv-lif-qta-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-qta
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Valore unitario                         *
      *                      *-----------------------------------------*
           move      w-dcv-lif-vun-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-vun
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Tipo di valore unitario                 *
      *                      *-----------------------------------------*
           move      w-dcv-lif-tpv-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-tvu
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Anno, di quattro cifre                  *
      *                      *-----------------------------------------*
           move      w-dcv-lif-ann-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-ann
                                              (w-dcv-lif-inx-i0i)     .
      *                      *-----------------------------------------*
      *                      * Flag di rivalutazione                   *
      *                      *-----------------------------------------*
           move      w-dcv-lif-riv-vlf
                    (w-dcv-lif-inx-i0i)   to   d-vun-mag-lif-riv
                                              (w-dcv-lif-inx-i0i)     .
       det-cst-lfa-390.
      *                      *-----------------------------------------*
      *                      * Riciclo ad elemento successivo          *
      *                      *-----------------------------------------*
           go to     det-cst-lfa-380.
       det-cst-lfa-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cst-lfa-999.
       det-cst-lfa-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore unitario : Costo medio continuo     *
      *    * alla data richiesta                                       *
      *    *-----------------------------------------------------------*
       det-csm-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione progressivi di comodo           *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csm-con-dur      .
           move      zero                 to   w-det-csm-con-gia      .
           move      zero                 to   w-det-csm-con-cmc      .
       det-csm-con-100.
      *              *-------------------------------------------------*
      *              * Start su file [mmc] con data reverse            *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODDRE    "         to   f-key                  .
           move      d-vun-mag-tip-mag    to   rf-mmc-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmc-num-mag         .
           move      99999999             to   rf-mmc-dat-rev         .
           if        w-det-vum-dat-val    not  = zero
                     subtract  19000000   from rf-mmc-dat-rev
                     subtract  w-det-vum-dat-val
                                          from rf-mmc-dat-rev         .
           move      "pgm/mag/fls/ioc/obj/iofmmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmc                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-con-200.
      *              *-------------------------------------------------*
      *              * Lettura primo record [mmc]                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmc                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-con-200.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-mmc-tip-mag       not  = d-vun-mag-tip-mag or
                     rf-mmc-num-mag       not  = d-vun-mag-num-mag
                     go to det-csm-con-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori letti                    *
      *              *-------------------------------------------------*
           move      rf-mmc-dat-ulr       to   w-det-csm-con-dur      .
           move      rf-mmc-gia-ddc       to   w-det-csm-con-gia      .
           move      rf-mmc-cmc-ddc       to   w-det-csm-con-cmc      .
      *              *-------------------------------------------------*
      *              * Inizializzazione quantita' e valore giacenza    *
      *              * significativi                                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csm-con-qgs      .
           move      zero                 to   w-det-csm-con-vgs      .
       det-csm-con-200.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione file [mmr] a partire dalla   *
      *              * data trovata                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [mmr]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      d-vun-mag-tip-mag    to   rf-mmr-tip-mag         .
           move      d-vun-mag-num-mag    to   rf-mmr-num-mag         .
           move      rf-mmc-dat-con       to   rf-mmr-dat-reg         .
           move      99999999999          to   rf-mmr-num-prt         .
           move      99999                to   rf-mmr-num-prg         .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-con-300.
       det-csm-con-220.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [mmr]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmmr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mmr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-csm-con-300.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-mmr-tip-mag       not  = d-vun-mag-tip-mag or
                     rf-mmr-num-mag       not  = d-vun-mag-num-mag or
                     rf-mmr-dat-reg       >    w-det-vum-dat-val
                     go to det-csm-con-300.
      *                  *---------------------------------------------*
      *                  * Selezione sul record                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se movimentazione interna : riciclo     *
      *                      *-----------------------------------------*
           if        rf-mmr-tip-mdm       =    03
                     go to det-csm-con-220.
      *                      *-----------------------------------------*
      *                      * Se movimento di solo conto merce : ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-mic       =    03
                     go to det-csm-con-220.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori attuali di giacenza e    *
      *                  * costo medio                                 *
      *                  *---------------------------------------------*
           move      w-det-csm-con-gia    to   w-det-csm-con-sgi      .
           move      w-det-csm-con-cmc    to   w-det-csm-con-scm      .
       det-csm-con-230.
      *                  *---------------------------------------------*
      *                  * Aggiornamento giacenza                      *
      *                  *---------------------------------------------*
           if        rf-mmr-tip-mdm       =    01
                     add      rf-mmr-qta-mov
                                          to   w-det-csm-con-gia
           else if   rf-mmr-tip-mdm       =    02
                     subtract rf-mmr-qta-mov
                                          from w-det-csm-con-gia      .
      *                  *---------------------------------------------*
      *                  * Determinazione valore significativo di gia- *
      *                  * cenza e valore totale giacenza              *
      *                  *---------------------------------------------*
           if        w-det-csm-con-gia    >    zero and
                     w-det-csm-con-sgi    >    zero
                     go to det-csm-con-232
           else      go to det-csm-con-240.
       det-csm-con-232.
           move      w-det-csm-con-gia    to   w-det-csm-con-qgs      .
           multiply  w-det-csm-con-cmc    by   w-det-csm-con-gia
                                        giving w-det-csm-con-vgs      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           divide    100                  into w-det-csm-con-vgs      .
       det-csm-con-240.
      *                  *---------------------------------------------*
      *                  * Aggiornamento costo medio continuo          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da eseguire                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se movimento di Scarico : No        *
      *                          *-------------------------------------*
           if        rf-mmr-tip-mdm       =    02
                     go to det-csm-con-260.
      *                          *-------------------------------------*
      *                          * Se quantita' movimento non positi-  *
      *                          * va : No                             *
      *                          *-------------------------------------*
           if        rf-mmr-qta-mov       not  > zero
                     go to det-csm-con-260.
      *                      *-----------------------------------------*
      *                      * Determinazione valore movimento per ag- *
      *                      * giornamento                             *
      *                      *-----------------------------------------*
           if        rf-mmr-trt-val       =    "N" or
                     rf-mmr-trt-val       =    "S"
                     move  zero           to   w-det-csm-con-w01
           else if   rf-mmr-trt-val       =    "I"
                     move  rf-mmr-val-mov to   w-det-csm-con-w01
           else if   rf-mmr-trt-val       =    "C" or
                     rf-mmr-trt-val       =    "X"
                     move  rf-mmr-val-mov to   w-det-csm-con-w01
           else if   rf-mmr-trt-val       =    "R" or
                     rf-mmr-trt-val       =    "Y"
                     move  rf-mmr-val-mov to   w-det-csm-con-w01
                     multiply -1          by   w-det-csm-con-w01      .
      *                      *-----------------------------------------*
      *                      * Se valore movimento a zero : oltre      *
      *                      *-----------------------------------------*
           if        w-det-csm-con-w01    =    zero
                     go to det-csm-con-260.
      *                      *-----------------------------------------*
      *                      * Aggiornamento costo medio continuo      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda che la quanti- *
      *                          * ta' in giacenza precedentemente     *
      *                          * salvata sia a positiva o meno       *
      *                          *-------------------------------------*
           if        w-det-csm-con-sgi    <    zero
                     go to det-csm-con-244
           else      go to det-csm-con-242.
       det-csm-con-242.
      *                          *-------------------------------------*
      *                          * Se giacenza precedentemente salvata *
      *                          * zero o positiva                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Calcolo                         *
      *                              *---------------------------------*
           multiply  w-det-csm-con-sgi    by   w-det-csm-con-scm
                                        giving w-det-csm-con-w02      .
      *                              *---------------------------------*
      *                              * Applicazione decimali gestione  *
      *                              * magazzino                       *
      *                              *---------------------------------*
           divide    100                  into w-det-csm-con-w02      .
      *                              *---------------------------------*
      *                              * Calcolo - segue                 *
      *                              *---------------------------------*
           add       rf-mmr-val-mov       to   w-det-csm-con-w02      .
           divide    w-det-csm-con-gia    into w-det-csm-con-w02
                                        giving w-det-csm-con-cmc
                                               rounded                .
      *                              *---------------------------------*
      *                              * Applicazione decimali gestione  *
      *                              * magazzino                       *
      *                              *---------------------------------*
           multiply  100                  by   w-det-csm-con-cmc      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     det-csm-con-250.
       det-csm-con-244.
      *                          *-------------------------------------*
      *                          * Se giacenza precedentemente salvata *
      *                          * negativa                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento progressivi quan- *
      *                              * tita' e valore giacenza signi-  *
      *                              * ficativi                        *
      *                              *---------------------------------*
           add       rf-mmr-qta-mov       to   w-det-csm-con-qgs      .
           add       rf-mmr-val-mov       to   w-det-csm-con-vgs      .
      *                              *---------------------------------*
      *                              * Calcolo                         *
      *                              *---------------------------------*
           divide    w-det-csm-con-qgs    into w-det-csm-con-vgs
                                        giving w-det-csm-con-cmc
                                               rounded                .
      *                              *---------------------------------*
      *                              * Applicazione decimali gestione  *
      *                              * magazzino                       *
      *                              *---------------------------------*
           multiply  100                  by   w-det-csm-con-cmc      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     det-csm-con-250.
       det-csm-con-250.
      *                      *-----------------------------------------*
      *                      * Aggiornamento data ultimo rilevamento   *
      *                      *-----------------------------------------*
           move      rf-mmr-dat-reg       to   w-det-csm-con-dur      .
       det-csm-con-260.
      *              *-------------------------------------------------*
      *              * Riciclo su scansione file [mmr]                 *
      *              *-------------------------------------------------*
           go to     det-csm-con-220.
       det-csm-con-300.
      *              *-------------------------------------------------*
      *              * Preparazione valori in uscita                   *
      *              *-------------------------------------------------*
           move      w-det-csm-con-dur    to   d-vun-mag-dat-val      .
           move      w-det-csm-con-cmc    to   d-vun-mag-val-uni      .
       det-csm-con-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione della composizione del valore   *
      *    * L.I.F.O.                                                  *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *  - w-dcv-lif-ann-ese     : Anno di esercizio attuale, di  *
      *    *                            quattro cifre                  *
      *    *                                                           *
      *    *    Per i = 01..11 :                                       *
      *    *                                                           *
      *    *  - w-dcv-lif-rim-ese (i) : Rimanenza finale dell'eserci-  *
      *    *                            zio                            *
      *    *                                                           *
      *    *  - w-dcv-lif-ump-ese (i) : Costo medio ponderato finale   *
      *    *                            dell'esercizio                 *
      *    *                                                           *
      *    *  - w-dcv-lif-vnb-ese (i) : Valore normale del bene nel-   *
      *    *                            l'ultimo trimestre dell'eser-  *
      *    *                            cizio                          *
      *    *                                                           *
      *    *  - w-dcv-lif-fdr-ese (i) : Flag di rivalutazione per l'e- *
      *    *                            sercizio                       *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *  - w-dcv-lif-flg-exi     : Flag di uscita                 *
      *    *                                                           *
      *    *  - w-dcv-lif-rim-fin     : Rimanenza finale dell'eserci-  *
      *    *                            zio attuale, pari alla rima-   *
      *    *                            nenza finale dell'esercizio    *
      *    *                            attuale                        *
      *    *                                                           *
      *    *  - w-dcv-lif-val-fin     : Valore della rimanenza finale  *
      *    *                                                           *
      *    *  - w-dcv-lif-num-vlf     : Numero di scaglioni che con-   *
      *    *                            corrono a formare il valore    *
      *    *                            della rimanenza finale         *
      *    *                                                           *
      *    *    Per j = 01..11 :                                       *
      *    *                                                           *
      *    *  - w-dcv-lif-qta-vlf (j) : Quantita' che contribuisce a   *
      *    *                            formare il valore della rima-  *
      *    *                            nenza finale                   *
      *    *                                                           *
      *    *  - w-dcv-lif-vun-vlf (j) : Valore unitario che contribui- *
      *    *                            sce a formare il valore della  *
      *    *                            rimanenza finale               *
      *    *                                                           *
      *    *  - w-dcv-lif-tpv-vlf (j) : Tipo di valore unitario che ha *
      *    *                            contribuito a formare il valo- *
      *    *                            re della rimanenza finale      *
      *    *                                                           *
      *    *  - w-dcv-lif-ann-vlf (j) : Anno di riferimento di quanti- *
      *    *                            ta' e valore unitario che con- *
      *    *                            tribuiscono a formare il valo- *
      *    *                            re della rimanenza finale      *
      *    *                                                           *
      *    *  - w-dcv-lif-riv-vlf (j) : Flag di rivalutazione relativo *
      *    *                            all'anno di riferimento        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-lif-mag-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale dei valori in uscita   *
      *              *-------------------------------------------------*
       det-lif-mag-010.
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-dcv-lif-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Rimanenza finale dell'esercizio, pari alla  *
      *                  * rimanenza finale in input                   *
      *                  *---------------------------------------------*
           move      w-dcv-lif-rim-ese (01)
                                          to   w-dcv-lif-rim-fin      .
      *                  *---------------------------------------------*
      *                  * Valore della rimanenza finale dell'eserci-  *
      *                  * zio, a zero                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-dcv-lif-val-fin      .
      *                  *---------------------------------------------*
      *                  * Numero di scaglioni che concorrono a forma- *
      *                  * re il valore della rimanenza finale dell'e- *
      *                  * sercizio, a zero                            *
      *                  *---------------------------------------------*
           move      zero                 to   w-dcv-lif-num-vlf      .
       det-lif-mag-020.
      *                  *---------------------------------------------*
      *                  * Componenti degli scaglioni che concorrono a *
      *                  * formare il valore della rimanenza finale    *
      *                  * dell'esercizio, a zero e spaces             *
      *                  *---------------------------------------------*
           move      zero                 to   w-dcv-lif-inx-j0j      .
       det-lif-mag-022.
           add       1                    to   w-dcv-lif-inx-j0j      .
           if        w-dcv-lif-inx-j0j    >    11
                     go to det-lif-mag-100.
           move      zero                 to   w-dcv-lif-qta-vlf
                                              (w-dcv-lif-inx-j0j)     .
           move      zero                 to   w-dcv-lif-vun-vlf
                                              (w-dcv-lif-inx-j0j)     .
           move      spaces               to   w-dcv-lif-tpv-vlf
                                              (w-dcv-lif-inx-j0j)     .
           move      zero                 to   w-dcv-lif-ann-vlf
                                              (w-dcv-lif-inx-j0j)     .
           move      spaces               to   w-dcv-lif-riv-vlf
                                              (w-dcv-lif-inx-j0j)     .
           go to     det-lif-mag-022.
       det-lif-mag-100.
      *              *-------------------------------------------------*
      *              * Se la rimanenza finale e' a zero : si esce im-  *
      *              * mediatamente con il flag di uscita a 'Z'        *
      *              *-------------------------------------------------*
           if        w-dcv-lif-rim-fin    =    zero
                     move  "Z"            to   w-dcv-lif-flg-exi
                     go to det-lif-mag-900.
       det-lif-mag-200.
      *              *-------------------------------------------------*
      *              * Determinazione della differenza tra la rimanen- *
      *              * za finale di ogni esercizio e la differenza fi- *
      *              * nale dell'esercizio precedente                  *
      *              *-------------------------------------------------*
       det-lif-mag-210.
           move      zero                 to   w-dcv-lif-inx-i0i      .
           move      1                    to   w-dcv-lif-inx-j0j      .
       det-lif-mag-220.
           add       1                    to   w-dcv-lif-inx-i0i      .
           add       1                    to   w-dcv-lif-inx-j0j      .
           if        w-dcv-lif-inx-i0i    =    11
                     go to det-lif-mag-240.
       det-lif-mag-230.
           subtract  w-dcv-lif-rim-ese
                    (w-dcv-lif-inx-j0j)   from w-dcv-lif-rim-ese
                                              (w-dcv-lif-inx-i0i)
                                        giving w-dcv-lif-dif-rim
                                              (w-dcv-lif-inx-i0i)     .
           go to     det-lif-mag-220.
       det-lif-mag-240.
           move      w-dcv-lif-rim-ese
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-dif-rim
                                              (w-dcv-lif-inx-i0i)     .
       det-lif-mag-300.
      *              *-------------------------------------------------*
      *              * Accumulo sulla differenza relativa all'ultimo   *
      *              * esercizio rivalutato delle differenze relative  *
      *              * agli esercizi precedenti                        *
      *              *-------------------------------------------------*
       det-lif-mag-310.
           move      zero                 to   w-dcv-lif-inx-i0i      .
       det-lif-mag-320.
           add       1                    to   w-dcv-lif-inx-i0i      .
           if        w-dcv-lif-inx-i0i    =    11
                     go to det-lif-mag-400.
           if        w-dcv-lif-fdr-ese
                    (w-dcv-lif-inx-i0i)   =    spaces
                     go to det-lif-mag-320.
           move      w-dcv-lif-inx-i0i    to   w-dcv-lif-inx-j0j      .
       det-lif-mag-330.
           add       1                    to   w-dcv-lif-inx-j0j      .
           if        w-dcv-lif-inx-j0j    >    11
                     go to det-lif-mag-400.
           add       w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-j0j)   to   w-dcv-lif-dif-rim
                                              (w-dcv-lif-inx-i0i)     .
           move      zero                 to   w-dcv-lif-dif-rim
                                              (w-dcv-lif-inx-j0j)     .
           go to     det-lif-mag-330.
       det-lif-mag-400.
      *              *-------------------------------------------------*
      *              * Assorbimento degli incrementi negativi sugli    *
      *              * incrementi positivi degli esercizi precedenti,  *
      *              * con controllo sulla eventuale non determinabi-  *
      *              * lita' per rimanenze finali negative             *
      *              *-------------------------------------------------*
       det-lif-mag-410.
           move      zero                 to   w-dcv-lif-inx-i0i      .
       det-lif-mag-420.
           add       1                    to   w-dcv-lif-inx-i0i      .
           if        w-dcv-lif-inx-i0i    >    11
                     go to det-lif-mag-500.
           if        w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-i0i)   not  < zero
                     go to det-lif-mag-420.
           move      w-dcv-lif-inx-i0i    to   w-dcv-lif-inx-j0j      .
       det-lif-mag-430.
           add       1                    to   w-dcv-lif-inx-j0j      .
           if        w-dcv-lif-inx-j0j    >    11
                     move  "N"            to   w-dcv-lif-flg-exi
                     go to det-lif-mag-900.
           if        w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-j0j)   not  > zero
                     go to det-lif-mag-430.
           add       w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-dif-rim
                                              (w-dcv-lif-inx-j0j)     .
           move      zero                 to   w-dcv-lif-dif-rim
                                              (w-dcv-lif-inx-i0i)     .
           go to     det-lif-mag-420.
       det-lif-mag-500.
      *              *-------------------------------------------------*
      *              * Controllo sulla eventuale non determinabilita'  *
      *              * per rimanenze finali negative, cioe' che la ri- *
      *              * manenza finale piu' vecchia non sia negativa    *
      *              *-------------------------------------------------*
           move      11                   to   w-dcv-lif-inx-i0i      .
       det-lif-mag-510.
           if        w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-i0i)   >    zero
                     go to det-lif-mag-600.
           if        w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-i0i)   <    zero
                     move  "N"            to   w-dcv-lif-flg-exi
                     go to det-lif-mag-900.
           subtract  1                    from w-dcv-lif-inx-i0i      .
           if        w-dcv-lif-inx-i0i    >    zero
                     go to det-lif-mag-510.
       det-lif-mag-600.
      *              *-------------------------------------------------*
      *              * Preparazione dei valori in uscita               *
      *              *-------------------------------------------------*
       det-lif-mag-610.
           move      zero                 to   w-dcv-lif-num-vlf      .
           move      zero                 to   w-dcv-lif-inx-i0i      .
       det-lif-mag-620.
           add       1                    to   w-dcv-lif-inx-i0i      .
           if        w-dcv-lif-inx-i0i    >    11
                     go to det-lif-mag-630.
           if        w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-i0i)   not  > zero
                     go to det-lif-mag-620.
           add       1                    to   w-dcv-lif-num-vlf      .
      *
           move      w-dcv-lif-dif-rim
                    (w-dcv-lif-inx-i0i)   to   w-dcv-lif-qta-vlf
                                              (w-dcv-lif-num-vlf)     .
      *
           if        w-dcv-lif-vnb-ese
                    (w-dcv-lif-inx-i0i)   not  = zero
                     move  w-dcv-lif-vnb-ese
                          (w-dcv-lif-inx-i0i)
                                          to   w-dcv-lif-vun-vlf
                                              (w-dcv-lif-num-vlf)
                     move  "N"            to   w-dcv-lif-tpv-vlf
                                              (w-dcv-lif-num-vlf)
           else      move  w-dcv-lif-ump-ese
                          (w-dcv-lif-inx-i0i)
                                          to   w-dcv-lif-vun-vlf
                                              (w-dcv-lif-num-vlf)
                     move  "P"            to   w-dcv-lif-tpv-vlf
                                              (w-dcv-lif-num-vlf)     .
      *
           move      w-dcv-lif-ann-ese    to   w-dcv-lif-ann-vlf
                                              (w-dcv-lif-num-vlf)     .
           add       1                    to   w-dcv-lif-ann-vlf
                                              (w-dcv-lif-num-vlf)     .
           subtract  w-dcv-lif-inx-i0i    from w-dcv-lif-ann-vlf
                                              (w-dcv-lif-num-vlf)     .
      *
           if        w-dcv-lif-vnb-ese
                    (w-dcv-lif-inx-i0i)   not  = zero   and
                     w-dcv-lif-fdr-ese
                    (w-dcv-lif-inx-i0i)   not  = spaces
                     move  "S"            to   w-dcv-lif-riv-vlf
                                              (w-dcv-lif-num-vlf)
           else      move  spaces         to   w-dcv-lif-riv-vlf
                                              (w-dcv-lif-num-vlf)     .
      *
           go to     det-lif-mag-620.
       det-lif-mag-630.
           move      zero                 to   w-dcv-lif-inx-i0i      .
       det-lif-mag-640.
           add       1                    to   w-dcv-lif-inx-i0i      .
           if        w-dcv-lif-inx-i0i    >    w-dcv-lif-num-vlf
                     go to det-lif-mag-900.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-dcv-lif-qta-vlf
                    (w-dcv-lif-inx-i0i)   by   w-dcv-lif-vun-vlf
                                              (w-dcv-lif-inx-i0i)
                                        giving w-dcv-lif-cal-s11
                                                         rounded      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           divide    100                  into w-dcv-lif-cal-s11      .
      *                  *---------------------------------------------*
      *                  * Incremento del totale                       *
      *                  *---------------------------------------------*
           add       w-dcv-lif-cal-s11    to   w-dcv-lif-val-fin      .
           go to     det-lif-mag-640.
       det-lif-mag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-lif-mag-999.
       det-lif-mag-999.
           exit.
