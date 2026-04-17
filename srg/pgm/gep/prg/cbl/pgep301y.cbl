       Identification Division.
       Program-Id.                                 pgep301y           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    mov                 *
      *                                   Fase:    gep301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 30/01/92    *
      *                       Ultima revisione:    NdK del 02/01/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti per gestione   *
      *                    portafoglio                                 *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Espansione movimenti effettuati su di una   *
      *                    distinta                                    *
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
      *    * Descrizione tipo interrogazione per l'overlay             *
      *    *-----------------------------------------------------------*
       01  w-des-tit-pgm-ovy              pic  x(40)       value
                     "ESPANSIONE MOVIMENTI SU DI UNA DISTINTA "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
      *            *---------------------------------------------------*
      *            * Da accettazione campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-acc-ric-sel      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ric-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts richieste                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati richieste                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste pre esecuzione interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico interrogazione        *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento automatico interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-aut      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Si/No primo giro di esecuzione            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-prm-gir      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento qry-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-qry.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-qry-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-liv.
                   15  w-cnt-qry-sav-l05  pic  x(64)                  .
                   15  w-cnt-qry-sav-l04  pic  x(64)                  .
                   15  w-cnt-qry-sav-l03  pic  x(64)                  .
                   15  w-cnt-qry-sav-l02  pic  x(64)                  .
                   15  w-cnt-qry-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Literals                                              *
      *        *-------------------------------------------------------*
           05  w-cnt-lit.
               10  w-cnt-lit-t80          pic  x(80) value all "="    .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. eventualmente passate   *
      *    * dal chiamante                                             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per tipo di chiamante del sottoprogramma              *
      *        *-------------------------------------------------------*
           05  w-ipc-tdc-mos.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * chiamante del sottoprogramma                      *
      *            *---------------------------------------------------*
               10  w-ipc-tdc-mos-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al tipo *
      *            * di chiamante del sottoprogramma                   *
      *            * - M : Il main                                     *
      *            * - S : Un altro sottoprogramma                     *
      *            *---------------------------------------------------*
               10  w-ipc-tdc-mos-val      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per ammissibilita' tasto Slct                         *
      *        *-------------------------------------------------------*
           05  w-ipc-snx-slc.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa all'ammissibi- *
      *            * lita' del tasto Slct                              *
      *            *---------------------------------------------------*
               10  w-ipc-snx-slc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa all'am- *
      *            * missibilita' del tasto Slct                       *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-ipc-snx-slc-val      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per numero distinta                                   *
      *        *-------------------------------------------------------*
           05  w-ipc-num-ddp.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al numero di   *
      *            * distinta                                          *
      *            *---------------------------------------------------*
               10  w-ipc-num-ddp-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al nu-  *
      *            * mero di distinta                                  *
      *            *---------------------------------------------------*
               10  w-ipc-num-ddp-val      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Numero distinta                                       *
      *        *-------------------------------------------------------*
           05  rr-num-ddp                 pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Status visualizzazione dati distinta                  *
      *        * - N : Non visualizzati                                *
      *        * - V : Visualizzati                                    *
      *        *-------------------------------------------------------*
           05  rr-svd-ddp                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [ddp]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-ddp.
               10  w-fnd-arc-ddp-sns      pic  x(01)                  .
               10  w-fnd-arc-ddp-sel      pic  x(01)                  .
               10  w-fnd-arc-ddp-tip      pic  9(02)                  .
               10  w-fnd-arc-ddp-num      pic  9(11)                  .
               
      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cbp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cbp.
               10  w-let-arc-cbp-flg      pic  x(01)                  .
               10  w-let-arc-cbp-tip      pic  9(02)                  .
               10  w-let-arc-cbp-cod      pic  x(10)                  .
               10  w-let-arc-cbp-des      pic  x(40)                  .
               10  w-let-arc-cbp-csc      pic  9(07)                  .
               10  w-let-arc-cbp-csa      pic  9(07)                  .
               10  w-let-arc-cbp-abi      pic  9(05)                  .
               10  w-let-arc-cbp-cab      pic  9(05)                  .
               10  w-let-arc-cbp-ccb      pic  x(12)                  .
               10  w-let-arc-cbp-cco      pic  9(07)                  .
               10  w-let-arc-cbp-bba      pic  9(07)                  .
               10  w-let-arc-cbp-bbp      pic  9(07)                  .
               10  w-let-arc-cbp-psm      pic  9(07)                  .
               10  w-let-arc-cbp-psi      pic  9(07)                  .
               10  w-let-arc-cbp-pdi      pic  9(07)                  .
               10  w-let-arc-cbp-psc      pic  9(07)                  .
               10  w-let-arc-cbp-anv      pic  9(07)                  .
               10  w-let-arc-cbp-dfp      pic  9(07)                  .
               10  w-let-arc-cbp-dfa      pic  9(07)                  .
               10  w-let-arc-cbp-onb      pic  9(07)                  .
               10  w-let-arc-cbp-inb      pic  9(07)                  .
               10  w-let-arc-cbp-ccp      pic  x(12)                  .
               10  w-let-arc-cbp-bpa      pic  9(07)                  .
               10  w-let-arc-cbp-bpp      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per test se programma gia' attivo               *
      *    *-----------------------------------------------------------*
       01  w-pga.
      *        *-------------------------------------------------------*
      *        * Work per programma di interrogazione distinte di pre- *
      *        * sentazione in portafoglio                             *
      *        * - S : Si, gia' attivo                                 *
      *        * - N : No, non attivo                                  *
      *        *-------------------------------------------------------*
           05  w-pga-gep-303-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo distinta                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ddp.
               10  w-exp-tip-ddp-num      pic  9(02)       value 04   .
               10  w-exp-tip-ddp-lun      pic  9(02)       value 30   .
               10  w-exp-tip-ddp-tbl.
                   15  filler             pic  x(30) value
                            "distinta Incassi elettronici  "          .
                   15  filler             pic  x(30) value
                            "distinta Effetti              "          .
                   15  filler             pic  x(30) value
                            "distinta Paghero' cambiari    "          .
                   15  filler             pic  x(30) value
                            "distinta Cessioni             "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di presentazione distinta             *
      *        *-------------------------------------------------------*
           05  w-exp-tip-pre.
               10  w-exp-tip-pre-num      pic  9(02)       value 04   .
               10  w-exp-tip-pre-lun      pic  9(02)       value 40   .
               10  w-exp-tip-pre-tbl.
                   15  filler             pic  x(40) value
                            "accredito SBF a Maturazione di valuta   ".
                   15  filler             pic  x(40) value
                            "accredito Immediato SBF                 ".
                   15  filler             pic  x(40) value
                            "accredito al Dopo incasso               ".
                   15  filler             pic  x(40) value
                            "allo Sconto                             ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo avviso richiesto in presentazione     *
      *        *-------------------------------------------------------*
           05  w-exp-tav-ric.
               10  w-exp-tav-ric-num      pic  9(02)       value 05   .
               10  w-exp-tav-ric-lun      pic  9(02)       value 40   .
               10  w-exp-tav-ric-tbl.
                   15  filler             pic  x(40) value
                            "CDO, o in alternativa RIBA              ".
                   15  filler             pic  x(40) value
                            "CDO, o in alternativa MAV               ".
                   15  filler             pic  x(40) value
                            "Tutte RIBA                              ".
                   15  filler             pic  x(40) value
                            "Tutti MAV                               ".
                   15  filler             pic  x(40) value
                            "Tutti RID                               ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo scadenze da selezionare               *
      *        *-------------------------------------------------------*
           05  w-exp-tsc-das.
               10  w-exp-tsc-das-num      pic  9(02)                  .
               10  w-exp-tsc-das-lun      pic  9(02)       value 40   .
               10  w-exp-tsc-das-tbl.
                   15  w-exp-tsc-das-ele
                               occurs 05  pic  x(40)                  .
               10  w-exp-tsc-das-val.
                   15  w-exp-tsc-das-vel
                               occurs 05  pic  9(04)                  .
               10  w-exp-tsc-das-inx      pic  9(02)                  .
      *
               10  w-exp-tsc-das-n01      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t01.
                   15  filler             pic  x(40) value
                            "Solo di tipo IE                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v01.
                   15  filler             pic  9(04) value 0002       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n02      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t02.
                   15  filler             pic  x(40) value
                            "Solo di tipo IE                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v02.
                   15  filler             pic  9(04) value 0002       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n03      pic  9(02)       value 02   .
               10  w-exp-tsc-das-t03.
                   15  filler             pic  x(40) value
                            "Di tipo IE e RIBA                       ".
                   15  filler             pic  x(40) value
                            "Solo di tipo RIBA                       ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v03.
                   15  filler             pic  9(04) value 0203       .
                   15  filler             pic  9(04) value 0003       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n04      pic  9(02)       value 02   .
               10  w-exp-tsc-das-t04.
                   15  filler             pic  x(40) value
                            "Di tipo IE e MAV                        ".
                   15  filler             pic  x(40) value
                            "Solo di tipo MAV                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v04.
                   15  filler             pic  9(04) value 0205       .
                   15  filler             pic  9(04) value 0005       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n05      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t05.
                   15  filler             pic  x(40) value
                            "Solo di tipo RID                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v05.
                   15  filler             pic  9(04) value 0006       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n06      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t06.
                   15  filler             pic  x(40) value
                            "Di tipo RB e TR                         ".
                   15  filler             pic  x(40) value
                            "Solo di tipo RB                         ".
                   15  filler             pic  x(40) value
                            "Solo di tipo TR                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v06.
                   15  filler             pic  9(04) value 0910       .
                   15  filler             pic  9(04) value 0009       .
                   15  filler             pic  9(04) value 0010       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n07      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t07.
                   15  filler             pic  x(40) value
                            "Solo di tipo PC                         ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v07.
                   15  filler             pic  9(04) value 0011       .
                   15  filler             pic  9(04) value 0009       .
                   15  filler             pic  9(04) value 0010       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *
               10  w-exp-tsc-das-n08      pic  9(02)       value 01   .
               10  w-exp-tsc-das-t08.
                   15  filler             pic  x(40) value
                            "Solo di tipo PC avuti in cessione       ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
                   15  filler             pic  x(40) value
                            "                                        ".
               10  w-exp-tsc-das-v08.
                   15  filler             pic  9(04) value 0061       .
                   15  filler             pic  9(04) value 0009       .
                   15  filler             pic  9(04) value 0010       .
                   15  filler             pic  9(04) value 0000       .
                   15  filler             pic  9(04) value 0000       .
      *        *-------------------------------------------------------*
      *        * Work per : Anche scadenze a vista                     *
      *        *-------------------------------------------------------*
           05  w-exp-dsc-vis.
               10  w-exp-dsc-vis-num      pic  9(02)       value 02   .
               10  w-exp-dsc-vis-lun      pic  9(02)       value 02   .
               10  w-exp-dsc-vis-tbl.
                   15  filler             pic  x(02) value
                            "Si"                                      .
                   15  filler             pic  x(02) value
                            "No"                                      .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di invio archivio di supporto distin- *
      *        *            ta alla banca                              *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ias.
               10  w-exp-tip-ias-num      pic  9(02)       value 02   .
               10  w-exp-tip-ias-lun      pic  9(02)       value 40   .
               10  w-exp-tip-ias-tbl.
                   15  filler             pic  x(40) value
                            "tramite Dischetto                       ".
                   15  filler             pic  x(40) value
                            "tramite Home banking                    ".

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .
               10  w-err-box-err-m03      pic  x(65)                  .

      *    *===========================================================*
      *    * Work area per interrogazione distinta                     *
      *    *-----------------------------------------------------------*
       01  w-int-dis.
      *        *-------------------------------------------------------*
      *        * Flag per controllo ciclo interrogazione standard      *
      *        *-------------------------------------------------------*
           05  w-int-dis-flg-uno          pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per stampa valori distinta                      *
      *    *-----------------------------------------------------------*
       01  w-stp-ddp.
      *        *-------------------------------------------------------*
      *        * Titolo per testatina per tipo operazione              *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-tst-ope          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Prompt per la stampa                                  *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-pmt-pls          pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Data per la stampa                                    *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-dat-pls          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 10 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-a10-pls          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 40 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-a40-pls          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione di 40 caratteri per la stampa             *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-d40-pls          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Campo numerico con segno d 11 caratteri per la stampa *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-s11-pls          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Data registrazione per contabilita'                   *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-drc-ope          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo per contabilita'                    *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-npc-ope          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 1                                    *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-edt-001          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per editing 2                                    *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-edt-002          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Area per composizione contatore numero stampe         *
      *        *-------------------------------------------------------*
           05  w-stp-ddp-flc.
               10  w-stp-ddp-flc-snx      pic  x(02)                  .
               10  w-stp-ddp-flc-vrg      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-stp-ddp-flc-ctr      pic  z(02)                  .
               10  filler                 pic  x(01)                  .
               10  w-stp-ddp-flc-vlt      pic  x(05)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  filler                     pic  x(01)                  .

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

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
           05  i-ide-sap                  pic  x(03)                  .
           05  i-ide-arg                  pic  x(03)                  .
           05  i-ide-set                  pic  x(03)                  .
           05  i-ide-fas                  pic  x(06)                  .
           05  i-ide-pro                  pic  x(10)                  .
           05  i-ide-des                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per richiamo overlay per l'esecuzione effettiva *
      *    * del tipo di interrogazione                                *
      *    *-----------------------------------------------------------*
       01  w-ovy-exe.
      *        *-------------------------------------------------------*
      *        * Pathname per il richiamo della overlay                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-pat.
      *            *---------------------------------------------------*
      *            * Prefisso comune                                   *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pre          pic  x(16)                  .
      *            *---------------------------------------------------*
      *            * Postfisso variabile                               *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pos          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per il postfisso variabile                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-inx              pic  9(02)                  .
           05  w-ovy-exe-spv occurs 20    pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per tipi interrogazione                         *
      *    *-----------------------------------------------------------*
       01  w-tin.
      *        *-------------------------------------------------------*
      *        * Tabella tipi interrogazione e dati ad essi associati  *
      *        *-------------------------------------------------------*
           05  w-tin-tbl-tin.
      *            *---------------------------------------------------*
      *            * Indice per puntamento su elemento in tabella ed   *
      *            * altri indici di comodo                            *
      *            *---------------------------------------------------*
               10  w-tin-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per caricamento iniziale elementi          *
      *            *---------------------------------------------------*
               10  w-tin-ele-wci.
                   15  w-tin-ele-wci-des  pic  x(50)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-alf  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-ovy  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-tin-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi per pagina                     *
      *            *---------------------------------------------------*
               10  w-tin-ele-nep          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di pagine totali                           *
      *            *---------------------------------------------------*
               10  w-tin-ele-npt          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina attualmente visualizzata            *
      *            *---------------------------------------------------*
               10  w-tin-ele-pag          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-tin-ele-max          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Elementi per tipi interrogazione                  *
      *            *---------------------------------------------------*
               10  w-tin-ele-tin occurs 50.
      *                *-----------------------------------------------*
      *                * Codice numerico tipo interrogazione           *
      *                *-----------------------------------------------*
                   15  w-tin-num-tin      pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-tin-alf-tin      pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo interrogazione               *
      *                *-----------------------------------------------*
                   15  w-tin-des-tin      pic  x(50)                  .
      *                *-----------------------------------------------*
      *                * Overlay da richiamare per il tipo di interro- *
      *                * gazione                                       *
      *                *-----------------------------------------------*
                   15  w-tin-ovy-tin      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per sottoprogrammi attivi della fase            *
      *    *-----------------------------------------------------------*
       01  w-spg.
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, codice   *
      *        * alfanumerico del tipo di interrogazione               *
      *        *-------------------------------------------------------*
           05  w-spg-alf-gat              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, risposta *
      *        * - Spaces : No                                         *
      *        * - S      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-spg-snx-gat              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tabella dei sottoprogrammi attivi                     *
      *        *-------------------------------------------------------*
           05  w-spg-tbl-spg.
      *            *---------------------------------------------------*
      *            * Indice per puntamento in tabella                  *
      *            *---------------------------------------------------*
               10  w-spg-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-spg-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-spg-ele-max          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Elementi per sottoprogrammi attivi                *
      *            *---------------------------------------------------*
               10  w-spg-ele-spg occurs 99.
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-spg-alf-tin      pic  x(10)                  .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-ovy-exe
                                               w-tin
                                               w-spg                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Se no richieste : a esecuzione interrogazione   *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-500.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-500.
      *              *-------------------------------------------------*
      *              * Esecuzione programma di interrogazione          *
      *              *-------------------------------------------------*
           perform   qry-rou-pri-000      thru qry-rou-pri-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a fine programma              *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Test se fine esecuzione da operatore            *
      *              *-------------------------------------------------*
           if        w-cnt-qry-rou-pri    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-750
           else      go to main-250.
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-des-tit-pgm-ovy    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Routine principale                       *
      *    *-----------------------------------------------------------*
       qry-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-rou-pri      .
      *              *-------------------------------------------------*
      *              * Determinazione function-keys in Mark-points     *
      *              *-------------------------------------------------*
           perform   qry-det-fky-000      thru qry-det-fky-999        .
      *              *-------------------------------------------------*
      *              * Begin o Begin Automatico                        *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-aut    =    "S"
                     move   "BA"          to   v-ope
           else      move   "BE"          to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   qry-opn-fls-000      thru qry-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione marker di primo giro           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   qry-str-ini-000      thru qry-str-ini-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-600.
       qry-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-qry-sav-l05      .
           move      w-rot-l04            to   w-cnt-qry-sav-l04      .
           move      w-rot-l03            to   w-cnt-qry-sav-l03      .
           move      w-rot-l02            to   w-cnt-qry-sav-l02      .
           move      w-rot-l01            to   w-cnt-qry-sav-l01      .
       qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   qry-let-seq-000      thru qry-let-seq-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   qry-tst-max-000      thru qry-tst-max-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   qry-sel-rec-000      thru qry-sel-rec-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   qry-cmp-rot-000      thru qry-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-qry-mrk-uno    not  = spaces
                     go to qry-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   qry-rou-pri-790      thru qry-rou-pri-791        .
           perform   qry-rou-pri-750      thru qry-rou-pri-751        .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-qry-sav-l05
                     go to qry-rou-pri-310.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l05    to   w-rot-l05              .
           move      w-cnt-qry-sav-l04    to   w-rot-l04              .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           perform   qry-rou-pri-850      thru qry-rou-pri-851        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-750      thru qry-rou-pri-751        .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-qry-sav-l04
                     go to qry-rou-pri-320.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l04    to   w-rot-l04              .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-qry-sav-l03
                     go to qry-rou-pri-330.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-qry-sav-l02
                     go to qry-rou-pri-340.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-qry-sav-l01
                     go to qry-rou-pri-400.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
       qry-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Livello di dettaglio                            *
      *              *-------------------------------------------------*
           perform   qry-liv-det-000      thru qry-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione : fine ciclo         *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-qry-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     qry-rou-pri-100.
       qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-mrk-uno    =    spaces
                     go to qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Fine di tutti i livelli                         *
      *              *-------------------------------------------------*
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           perform   qry-rou-pri-850      thru qry-rou-pri-851        .
           perform   qry-rou-pri-890      thru qry-rou-pri-891        .
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Stop                                            *
      *              *-------------------------------------------------*
           move      "ST"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-rou-pri-900.
       qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   qry-nes-ela-000      thru qry-nes-ela-999        .
           go to     qry-rou-pri-900.
       qry-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-711.
           perform   qry-ini-lr1-000      thru qry-ini-lr1-999        .
       qry-rou-pri-711.
           exit.
       qry-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-721.
           perform   qry-ini-lr2-000      thru qry-ini-lr2-999        .
       qry-rou-pri-721.
           exit.
       qry-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-731.
           perform   qry-ini-lr3-000      thru qry-ini-lr3-999        .
       qry-rou-pri-731.
           exit.
       qry-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-741.
           perform   qry-ini-lr4-000      thru qry-ini-lr4-999        .
       qry-rou-pri-741.
           exit.
       qry-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-751.
           perform   qry-ini-lr5-000      thru qry-ini-lr5-999        .
       qry-rou-pri-751.
           exit.
       qry-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-791.
           perform   qry-ini-cic-000      thru qry-ini-cic-999        .
       qry-rou-pri-791.
           exit.
       qry-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-811.
           perform   qry-fin-lr1-000      thru qry-fin-lr1-999        .
       qry-rou-pri-811.
           exit.
       qry-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-821.
           perform   qry-fin-lr2-000      thru qry-fin-lr2-999        .
       qry-rou-pri-821.
           exit.
       qry-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-831.
           perform   qry-fin-lr3-000      thru qry-fin-lr3-999        .
       qry-rou-pri-831.
           exit.
       qry-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-841.
           perform   qry-fin-lr4-000      thru qry-fin-lr4-999        .
       qry-rou-pri-841.
           exit.
       qry-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-851.
           perform   qry-fin-lr5-000      thru qry-fin-lr5-999        .
       qry-rou-pri-851.
           exit.
       qry-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-891.
           perform   qry-fin-cic-000      thru qry-fin-cic-999        .
       qry-rou-pri-891.
           exit.
       qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   qry-cls-fls-000      thru qry-cls-fls-999        .
       qry-rou-pri-950.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Subroutine di avanzamento pagina         *
      *    *-----------------------------------------------------------*
       qry-pag-adv-000.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Test su esito interazione con operatore         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se continuazione normale                    *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata da operatore        *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "#"            to   w-cnt-qry-flg-int
                     go to qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se function-key prevista                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Trattamento function-key                *
      *                      *-----------------------------------------*
           perform   qry-trt-fun-000      thru qry-trt-fun-999        .
      *                      *-----------------------------------------*
      *                      * Test su rientro da trattamento f-key    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se fine ciclo interrogazione   *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    =    spaces
                     go to qry-pag-adv-000.
       qry-pag-adv-999.
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
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo di chiamante del sottoprogramma, se il  *
      *              * main oppure se un altro sottoprogramma          *
      *              *-------------------------------------------------*
           perform   ipc-tdc-mos-000      thru ipc-tdc-mos-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * l'ammissibilita' del tasto Slct                 *
      *              *-------------------------------------------------*
           perform   ipc-snx-slc-000      thru ipc-snx-slc-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il numero distinta                              *
      *              *-------------------------------------------------*
           perform   ipc-num-ddp-000      thru ipc-num-ddp-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "ESPDDP    "         to   w-spg-alf-gat          .
           perform   mem-spg-att-000      thru mem-spg-att-999        .
      *              *-------------------------------------------------*
      *              * Se memorizzazione non avvenuta : uscita con er- *
      *              * rore                                            *
      *              *-------------------------------------------------*
           if        w-spg-snx-gat        not  = spaces
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Eliminazione sottoprogramma in attivita'        *
      *              *-------------------------------------------------*
           move      "ESPDDP    "         to   w-spg-alf-gat          .
           perform   eli-spg-att-000      thru eli-spg-att-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di chiamante del sottoprogramma, se il main o un altro    *
      *    * sottoprogramma                                            *
      *    *-----------------------------------------------------------*
       ipc-tdc-mos-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tdc-mos' dallo      *
      *              * stesso livello di profondita'                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tdc-mos"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-tdc-mos-200
           else      go to ipc-tdc-mos-400.
       ipc-tdc-mos-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tdc-mos-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-tdc-mos-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tdc-mos-999.
       ipc-tdc-mos-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tdc-mos-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-tdc-mos-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tdc-mos-999.
       ipc-tdc-mos-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per l'ammis-  *
      *    * sibilita' del tasto Slct                                  *
      *    *-----------------------------------------------------------*
       ipc-snx-slc-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'snx-slc' dallo      *
      *              * stesso livello di profondita'                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-snx-slc-200
           else      go to ipc-snx-slc-400.
       ipc-snx-slc-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-snx-slc-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-snx-slc-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-slc-999.
       ipc-snx-slc-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-snx-slc-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-snx-slc-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-slc-999.
       ipc-snx-slc-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il nume-  *
      *    * ro di distinta                                            *
      *    *-----------------------------------------------------------*
       ipc-num-ddp-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'num-ddp' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-ddp"            to   s-var                  .
           if        w-ipc-tdc-mos-snx    =    "S" and
                     w-ipc-tdc-mos-val    =    "M"
                     move  "-"            to   s-dop
           else      move  "="            to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-num-ddp-200
           else      go to ipc-num-ddp-400.
       ipc-num-ddp-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-num-ddp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-num                to   w-ipc-num-ddp-val      .
      *                  *---------------------------------------------*
      *                  * Se valore della variabile a zero : come per *
      *                  * variabile non esistente                     *
      *                  *---------------------------------------------*
           if        w-ipc-num-ddp-val    =    zero
                     go to ipc-num-ddp-400.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di accettazione      *
      *                  *---------------------------------------------*
           move      w-ipc-num-ddp-val    to   rr-num-ddp             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-ddp-999.
       ipc-num-ddp-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-num-ddp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-num-ddp-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-num-ddp-999.
       ipc-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste un numero distinta passato dal    *
      *                  * chiamante non si eseguono le richieste,     *
      *                  * altrimenti le si eseguono                   *
      *                  *---------------------------------------------*
           if        w-ipc-num-ddp-snx    =    "S"   and
                     w-ipc-num-ddp-val    not  = zero
                     move  "N"            to   w-cnt-fun-snx-ric
           else      move  "S"            to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste un numero distinta passato dal    *
      *                  * chiamante il funzionamento non e' ciclico,  *
      *                  * altrimenti e' ciclico                       *
      *                  *---------------------------------------------*
           if        w-ipc-num-ddp-snx    =    "S"   and
                     w-ipc-num-ddp-val    not  = zero
                     move  "N"            to   w-cnt-fun-snx-cic
           else      move  "S"            to   w-cnt-fun-snx-cic      .
       pre-tip-fun-400.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre funzionamento automatico             *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
      *              *-------------------------------------------------*
      *              * Open file [ddp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * Open file [cbp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close file [ddp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * Close file [cbp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione richieste di selezione                       *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-ric-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Numero distinta                             *
      *                  *---------------------------------------------*
           perform   acc-num-ddp-000      thru acc-num-ddp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visualizzazione        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-910.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma espansione della distinta (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
           else      go to acc-ric-sel-910.
       acc-ric-sel-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     move  "S"            to   w-cnt-acc-ric-sel
                     go to acc-ric-sel-999
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-ric-sel-900.
       acc-ric-sel-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
           go to     acc-ric-sel-999.
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Numero distinta                                 *
      *              *-------------------------------------------------*
           perform   pmt-num-ddp-000      thru pmt-num-ddp-999        .
      *              *-------------------------------------------------*
      *              * Trattini di chiusura                            *
      *              *-------------------------------------------------*
           perform   pmt-trt-chs-000      thru pmt-trt-chs-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt per numero distinta                                *
      *    *-----------------------------------------------------------*
       pmt-num-ddp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero distinta            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Prompt per trattini di chiusura                           *
      *    *-----------------------------------------------------------*
       pmt-trt-chs-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-trt-chs-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero distinta                      *
      *    *-----------------------------------------------------------*
       acc-num-ddp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-ddp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se programma per interrogazione di-    *
      *                  * stinte di presentazione in portafoglio gia' *
      *                  * attivo per abilitazione tasto Find          *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep3030"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     move  "N"            to   w-pga-gep-303-snx
           else      move  "S"            to   w-pga-gep-303-snx      .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-pga-gep-303-snx    =    "N"
                     move  "FIND"         to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-num-ddp           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-num-ddp-200.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-num-ddp-999.
       acc-num-ddp-250.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-num-ddp             .
       acc-num-ddp-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-ddp-400.
      *                  *---------------------------------------------*
      *                  * Find su distinte di presentazione           *
      *                  *---------------------------------------------*
           move      "S"                  to   w-fnd-arc-ddp-sns      .
           move      zero                 to   w-fnd-arc-ddp-tip      .
           perform   fnd-arc-ddp-000      thru fnd-arc-ddp-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata : a reimposta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-fnd-arc-ddp-sel    not  = spaces
                     go to acc-num-ddp-100.
      *                  *---------------------------------------------*
      *                  * Ripresa numero distinta selezionata         *
      *                  *---------------------------------------------*
           move      w-fnd-arc-ddp-num    to   rr-num-ddp             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero distinta selezionata *
      *                  *---------------------------------------------*
           perform   vis-num-ddp-000      thru vis-num-ddp-999        .
      *                  *---------------------------------------------*
      *                  * Forzatura simulata del tasto Do             *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
           go to     acc-num-ddp-400.
       acc-num-ddp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-ddp-425.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore           *
      *                  *---------------------------------------------*
           if        rr-num-ddp           =    zero
                     go to acc-num-ddp-450
           else      go to acc-num-ddp-475.
       acc-num-ddp-450.
      *                  *---------------------------------------------*
      *                  * Se valore impostato a zero                  *
      *                  *---------------------------------------------*
       acc-num-ddp-452.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dello status di    *
      *                      * visualizzazione dati distinta           *
      *                      *-----------------------------------------*
           if        rr-svd-ddp           =    "N"
                     go to acc-num-ddp-454
           else      go to acc-num-ddp-456.
       acc-num-ddp-454.
      *                      *-----------------------------------------*
      *                      * Se dati distinta attualmente non visua- *
      *                      * lizzati                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione             *
      *                          *-------------------------------------*
           go to     acc-num-ddp-100.
       acc-num-ddp-456.
      *                      *-----------------------------------------*
      *                      * Se dati distinta attualmente visualiz-  *
      *                      * zati                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Cancellazione dati distinta         *
      *                          *-------------------------------------*
           perform   cnc-dat-ddp-000      thru cnc-dat-ddp-999        .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione             *
      *                          *-------------------------------------*
           go to     acc-num-ddp-100.
       acc-num-ddp-475.
      *                  *---------------------------------------------*
      *                  * Se valore impostato diverso da zero         *
      *                  *---------------------------------------------*
       acc-num-ddp-480.
      *                      *-----------------------------------------*
      *                      * Lettura record distinta da [ddp]        *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      rr-num-ddp           to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
       acc-num-ddp-485.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-num-ddp-500
           else      go to acc-num-ddp-490.
       acc-num-ddp-490.
      *                      *-----------------------------------------*
      *                      * Se record distinta non esistente        *
      *                      *-----------------------------------------*
       acc-num-ddp-492.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dello status   *
      *                          * di visualizzazione dati distinta    *
      *                          *-------------------------------------*
           if        rr-svd-ddp           =    "N"
                     go to acc-num-ddp-494
           else      go to acc-num-ddp-496.
       acc-num-ddp-494.
      *                          *-------------------------------------*
      *                          * Se dati distinta attualmente non    *
      *                          * visualizzati                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Numero distinta non esistente in archivio distinte
      -              " !             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione         *
      *                              *---------------------------------*
           go to     acc-num-ddp-100.
       acc-num-ddp-496.
      *                          *-------------------------------------*
      *                          * Se dati distinta attualmente visua- *
      *                          * lizzati                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Cancellazione dati distinta     *
      *                              *---------------------------------*
           perform   cnc-dat-ddp-000      thru cnc-dat-ddp-999        .
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Numero distinta non esistente in archivio distinte
      -              " !             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione         *
      *                              *---------------------------------*
           go to     acc-num-ddp-100.
       acc-num-ddp-500.
      *                      *-----------------------------------------*
      *                      * Se record distinta esistente            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione dati distinta       *
      *                          *-------------------------------------*
           perform   vis-dat-ddp-000      thru vis-dat-ddp-999        .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     acc-num-ddp-600.
       acc-num-ddp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-ddp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-num-ddp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-num-ddp-100.
       acc-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero distinta                   *
      *    *-----------------------------------------------------------*
       vis-num-ddp-000.
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-num-ddp           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-ddp-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione dati relativi alla distinta                 *
      *    *-----------------------------------------------------------*
       cnc-dat-ddp-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dai dati relativi alla    *
      *              * distinta                                        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       cnc-dat-ddp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione dati relativi alla distinta               *
      *    *-----------------------------------------------------------*
       vis-dat-ddp-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dai dati relativi alla    *
      *              * distinta                                        *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddp-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione vera e propria                  *
      *              *-------------------------------------------------*
       vis-dat-ddp-125.
      *                  *---------------------------------------------*
      *                  * Data composizione distinta                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data composizione distinta :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-ddp-dtr-com       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-ddp-150.
      *                  *---------------------------------------------*
      *                  * Tipo distinta                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo distinta              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddp-lun    to   v-car                  .
           move      w-exp-tip-ddp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddp-tbl    to   v-txt                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-ddp-tip-ddp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddp-175.
      *                  *---------------------------------------------*
      *                  * Importo distinta                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Importo distinta           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-ddp-imp-ddp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddp-200.
      *                  *---------------------------------------------*
      *                  * Nostra banca per la presentazione           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la distinta non e' stata presentata  *
      *                      * : no visualizzazione                    *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-pre       =    zero
                     go to vis-dat-ddp-225.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice nostra banca per la :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "             presentazione  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione codice banca        *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-ddp-cod-cbp       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Lettura anagrafica banca            *
      *                          *-------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-ddp-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione denominazione banca *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-let-arc-cbp-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddp-225.
      *                  *---------------------------------------------*
      *                  * Tipo di presentazione                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la distinta non e' stata presentata  *
      *                      * : no visualizzazione                    *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-pre       =    zero
                     go to vis-dat-ddp-250.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di presentazione      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pre-lun    to   v-car                  .
           move      w-exp-tip-pre-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-pre-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rf-ddp-tip-pre       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddp-250.
      *                  *---------------------------------------------*
      *                  * Fine visualizzazione dati                   *
      *                  *---------------------------------------------*
           go to     vis-dat-ddp-900.
       vis-dat-ddp-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddp-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Numero distinta                                 *
      *              *-------------------------------------------------*
           move      zero                 to   rr-num-ddp             .
      *              *-------------------------------------------------*
      *              * Status visualizzazione dati distinta            *
      *              *-------------------------------------------------*
           move      "N"                  to   rr-svd-ddp             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su tre righe          *
      *    *-----------------------------------------------------------*
       box-msg-e03-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      10                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m03    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e03-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Open files                               *
      *    *-----------------------------------------------------------*
       qry-opn-fls-000.
       qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Close files                              *
      *    *-----------------------------------------------------------*
       qry-cls-fls-000.
       qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Start iniziale                           *
      *    *-----------------------------------------------------------*
       qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
       qry-str-ini-100.
      *              *-------------------------------------------------*
      *              * Lettura record distinta da [ddp]                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      rr-num-ddp           to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to qry-str-ini-300
           else      go to qry-str-ini-200.
       qry-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se record distinta non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-300.
      *              *-------------------------------------------------*
      *              * Se record distinta esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per controllo ciclo    *
      *                  * interrogazione standard                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-int-dis-flg-uno      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Messaggio per nessuna registrazione      *
      *    *-----------------------------------------------------------*
       qry-nes-ela-000.
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
           move      "Numero distinta non esistente in archivio distinte
      -              " !             "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       qry-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Lettura sequenziale                      *
      *    *-----------------------------------------------------------*
       qry-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Test su flag per controllo ciclo interrogazione *
      *              * standard                                        *
      *              *-------------------------------------------------*
           if        w-int-dis-flg-uno    =    spaces
                     move  "#"            to   w-int-dis-flg-uno
           else      move  "#"            to   w-cnt-qry-flg-sub      .
       qry-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Test se superamento limiti massimi       *
      *    *-----------------------------------------------------------*
       qry-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
       qry-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Selezione su record letto                *
      *    *-----------------------------------------------------------*
       qry-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
       qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Composizione area per tests di rottura   *
      *    *-----------------------------------------------------------*
       qry-cmp-rot-000.
       qry-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Esecuzione per inizio ciclo              *
      *    *-----------------------------------------------------------*
       qry-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Esecuzione per fine ciclo                *
      *    *-----------------------------------------------------------*
       qry-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 5. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 5. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 4. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 4. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 3. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 3. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 2. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 2. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 1. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 1. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *-----------------------------------------------------------*
       qry-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-liv-det-050.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-100.
      *              *-------------------------------------------------*
      *              * Stampa dati per composizione distinta           *
      *              *-------------------------------------------------*
           perform   stp-dat-com-000      thru stp-dat-com-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-150.
      *              *-------------------------------------------------*
      *              * Stampa dati per presentazione distinta          *
      *              *-------------------------------------------------*
           perform   stp-dat-pre-000      thru stp-dat-pre-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa dati per accettazione distinta           *
      *              *-------------------------------------------------*
           perform   stp-dat-act-000      thru stp-dat-act-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-250.
      *              *-------------------------------------------------*
      *              * Stampa dati per accredito distinta              *
      *              *-------------------------------------------------*
           perform   stp-dat-acd-000      thru stp-dat-acd-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-300.
      *              *-------------------------------------------------*
      *              * Stampa dati per altre informazioni              *
      *              *-------------------------------------------------*
           perform   stp-dat-ain-000      thru stp-dat-ain-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per composizione distinta                     *
      *    *-----------------------------------------------------------*
       stp-dat-com-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per composizione distinta      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         Composizione distinta          "
                                          to   w-stp-ddp-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-025.
      *              *-------------------------------------------------*
      *              * Data composizione distinta                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data composizione          :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dtr-com       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-050.
      *              *-------------------------------------------------*
      *              * Tipo distinta                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo distinta              :"
                                          to   w-stp-ddp-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ddp-lun    to   v-car                  .
           move      w-exp-tip-ddp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ddp-tbl    to   v-txt                  .
           move      rf-ddp-tip-ddp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-075.
      *              *-------------------------------------------------*
      *              * Tipo avviso richiesto                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' una distinta per incassi elettro- *
      *                  * nici : no stampa                            *
      *                  *---------------------------------------------*
           if        rf-ddp-tip-ddp       not  = 01
                     go to stp-dat-com-100.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo avviso richiesto      :"
                                          to   w-stp-ddp-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tav-ric-lun    to   v-car                  .
           move      w-exp-tav-ric-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tav-ric-tbl    to   v-txt                  .
           move      rf-ddp-tav-ric       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-100.
      *              *-------------------------------------------------*
      *              * Tipi scadenza da includere in distinta          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo distinta non e' riconosciuto :   *
      *                  * no stampa                                   *
      *                  *---------------------------------------------*
           if        rf-ddp-tip-ddp       not  = 01 and
                     rf-ddp-tip-ddp       not  = 02 and
                     rf-ddp-tip-ddp       not  = 03 and
                     rf-ddp-tip-ddp       not  = 04
                     go to stp-dat-com-125.
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           perform   edt-tsc-das-000      thru edt-tsc-das-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipi scadenza da includere :"
                                          to   w-stp-ddp-pmt-pls      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-125.
      *              *-------------------------------------------------*
      *              * Data scadenza minima                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data scadenza minima       :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dsc-min       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-150.
      *              *-------------------------------------------------*
      *              * Data scadenza massima                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data scadenza massima      :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dsc-max       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-175.
      *              *-------------------------------------------------*
      *              * Anche scadenze a vista                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se distinta per incassi elettronici : no    *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-ddp-tip-ddp       =    01
                     go to stp-dat-com-200.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Anche scadenze a vista     :"
                                          to   w-stp-ddp-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-dsc-vis-lun    to   v-car                  .
           move      w-exp-dsc-vis-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-dsc-vis-tbl    to   v-txt                  .
           move      rf-ddp-dsc-vis       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-200.
      *              *-------------------------------------------------*
      *              * Circuiti interbancari                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' una distinta per incassi elettro- *
      *                  * nici : no stampa                            *
      *                  *---------------------------------------------*
           if        rf-ddp-tip-ddp       not  = 01
                     go to stp-dat-com-225.
      *                  *---------------------------------------------*
      *                  * Editing preliminare                         *
      *                  *---------------------------------------------*
           perform   edt-cir-itb-000      thru edt-cir-itb-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Circuiti interbancari      :"
                                          to   w-stp-ddp-pmt-pls      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-225.
      *              *-------------------------------------------------*
      *              * Importo massimo da selezionare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo massimo da selez.  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-imp-max       to   w-stp-ddp-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-250.
      *              *-------------------------------------------------*
      *              * Importo effettivo della distinta                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Importo effettivo distinta :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-imp-ddp       to   w-stp-ddp-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-com-999.
       stp-dat-com-275.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per composizione distinta      *
      *              *-------------------------------------------------*
           go to     stp-dat-com-999.
       stp-dat-com-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'editing per la voce : Tipi scadenza da   *
      *    * includere in distinta                                     *
      *    *-----------------------------------------------------------*
       edt-tsc-das-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo distinta        *
      *              *-------------------------------------------------*
           if        rf-ddp-tip-ddp       =    01
                     go to edt-tsc-das-100
           else if   rf-ddp-tip-ddp       =    02
                     go to edt-tsc-das-200
           else if   rf-ddp-tip-ddp       =    03
                     go to edt-tsc-das-300
           else if   rf-ddp-tip-ddp       =    04
                     go to edt-tsc-das-400
           else      go to edt-tsc-das-900.
       edt-tsc-das-100.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 01                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo avviso ri-  *
      *                  * chiesto                                     *
      *                  *---------------------------------------------*
           if        rf-ddp-tav-ric       =    01
                     go to edt-tsc-das-110
           else if   rf-ddp-tav-ric       =    02
                     go to edt-tsc-das-120
           else if   rf-ddp-tav-ric       =    03
                     go to edt-tsc-das-130
           else if   rf-ddp-tav-ric       =    04
                     go to edt-tsc-das-140
           else if   rf-ddp-tav-ric       =    05
                     go to edt-tsc-das-150
           else      go to edt-tsc-das-190.
       edt-tsc-das-110.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 01               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione editing                    *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n01    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t01    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v01    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Ad editing effettivo                    *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-120.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 02               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione editing                    *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n02    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t02    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v02    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Ad editing effettivo                    *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-130.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 03               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione editing                    *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n03    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t03    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v03    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Ad editing effettivo                    *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-140.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 04               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione editing                    *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n04    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t04    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v04    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Ad editing effettivo                    *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-150.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto : 05               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione editing                    *
      *                      *-----------------------------------------*
           move      w-exp-tsc-das-n05    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t05    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v05    to   w-exp-tsc-das-val      .
      *                      *-----------------------------------------*
      *                      * Ad editing effettivo                    *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-190.
      *                  *---------------------------------------------*
      *                  * Se tipo avviso richiesto non riconosciuto   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A normalizzazione area editata          *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-900.
       edt-tsc-das-200.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 02                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione editing                        *
      *                  *---------------------------------------------*
           move      w-exp-tsc-das-n06    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t06    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v06    to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Ad editing effettivo                        *
      *                  *---------------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-300.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 03                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione editing                        *
      *                  *---------------------------------------------*
           move      w-exp-tsc-das-n07    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t07    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v07    to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Ad editing effettivo                        *
      *                  *---------------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-400.
      *              *-------------------------------------------------*
      *              * Se tipo distinta : 04                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione editing                        *
      *                  *---------------------------------------------*
           move      w-exp-tsc-das-n08    to   w-exp-tsc-das-num      .
           move      w-exp-tsc-das-t08    to   w-exp-tsc-das-tbl      .
           move      w-exp-tsc-das-v08    to   w-exp-tsc-das-val      .
      *                  *---------------------------------------------*
      *                  * Ad editing effettivo                        *
      *                  *---------------------------------------------*
           go to     edt-tsc-das-500.
       edt-tsc-das-500.
      *              *-------------------------------------------------*
      *              * Editing effettivo                               *
      *              *-------------------------------------------------*
       edt-tsc-das-520.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di ele-   *
      *                  * menti che compongono la tabella delle pos-  *
      *                  * sibili scelte alternative                   *
      *                  *---------------------------------------------*
           if        w-exp-tsc-das-num    =    zero
                     go to edt-tsc-das-540
           else      go to edt-tsc-das-560.
       edt-tsc-das-540.
      *                  *---------------------------------------------*
      *                  * Se zero scelte possibili                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A normalizzazione area editata          *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-900.
       edt-tsc-das-560.
      *                  *---------------------------------------------*
      *                  * Se almeno una scelta possibile : si esegue  *
      *                  * l'editing vero e proprio e poi si esce      *
      *                  *---------------------------------------------*
       edt-tsc-das-570.
      *                      *-----------------------------------------*
      *                      * Preparazioni generali per l'editing     *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tsc-das-lun    to   v-car                  .
           move      w-exp-tsc-das-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tsc-das-tbl    to   v-txt                  .
       edt-tsc-das-580.
      *                      *-----------------------------------------*
      *                      * Preparazioni indice corrispondente al   *
      *                      * valore attuale                          *
      *                      *-----------------------------------------*
           if        rf-ddp-tsc-das       =    zero
                     move  zero           to   v-num
                     go to edt-tsc-das-590.
           move      zero                 to   w-exp-tsc-das-inx      .
       edt-tsc-das-582.
           add       1                    to   w-exp-tsc-das-inx      .
           if        w-exp-tsc-das-inx    >    w-exp-tsc-das-num
                     move  zero           to   v-num
                     go to edt-tsc-das-590.
           if        w-exp-tsc-das-vel
                    (w-exp-tsc-das-inx)   =    rf-ddp-tsc-das
                     move  w-exp-tsc-das-inx
                                          to   v-num
                     go to edt-tsc-das-590
           else      go to edt-tsc-das-582.
       edt-tsc-das-590.
      *                      *-----------------------------------------*
      *                      * Esecuzione                              *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     edt-tsc-das-999.
       edt-tsc-das-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione area editata                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work per accettazione       *
      *                  *---------------------------------------------*
           move      spaces               to   v-edt                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     edt-tsc-das-999.
       edt-tsc-das-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'editing per la voce : Circuiti inter-    *
      *    * bancari                                                   *
      *    *-----------------------------------------------------------*
       edt-cir-itb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare area editata        *
      *              *-------------------------------------------------*
           move      spaces               to   v-edt                  .
      *              *-------------------------------------------------*
      *              * 1. circuito                                     *
      *              *-------------------------------------------------*
           move      01                   to   v-lin                  .
           string    rf-ddp-cod-cib (1)
                                delimited by   size
                                          into v-edt
                                  with pointer v-lin                  .
      *              *-------------------------------------------------*
      *              * 2. circuito                                     *
      *              *-------------------------------------------------*
           move      04                   to   v-lin                  .
           string    rf-ddp-cod-cib (2)
                                delimited by   size
                                          into v-edt
                                  with pointer v-lin                  .
      *              *-------------------------------------------------*
      *              * 3. circuito                                     *
      *              *-------------------------------------------------*
           move      07                   to   v-lin                  .
           string    rf-ddp-cod-cib (3)
                                delimited by   size
                                          into v-edt
                                  with pointer v-lin                  .
      *              *-------------------------------------------------*
      *              * 4. circuito                                     *
      *              *-------------------------------------------------*
           move      10                   to   v-lin                  .
           string    rf-ddp-cod-cib (4)
                                delimited by   size
                                          into v-edt
                                  with pointer v-lin                  .
      *              *-------------------------------------------------*
      *              * 5. circuito                                     *
      *              *-------------------------------------------------*
           move      13                   to   v-lin                  .
           string    rf-ddp-cod-cib (5)
                                delimited by   size
                                          into v-edt
                                  with pointer v-lin                  .
       edt-cir-itb-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per presentazione distinta                    *
      *    *-----------------------------------------------------------*
       stp-dat-pre-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-ddp-dtr-pre       =    zero
                     go to stp-dat-pre-999.
       stp-dat-pre-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per presentazione distinta     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         Presentazione distinta         "
                                          to   w-stp-ddp-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-050.
      *              *-------------------------------------------------*
      *              * Data presentazione distinta                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data presentazione         :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dtr-pre       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-ddp-drc-pre       to   w-stp-ddp-drc-ope      .
           move      rf-ddp-npc-pre       to   w-stp-ddp-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-100.
      *              *-------------------------------------------------*
      *              * Tipo di presentazione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo di presentazione      :"
                                          to   w-stp-ddp-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-pre-lun    to   v-car                  .
           move      w-exp-tip-pre-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-pre-tbl    to   v-txt                  .
           move      rf-ddp-tip-pre       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-125.
      *              *-------------------------------------------------*
      *              * Banca di presentazione                          *
      *              *-------------------------------------------------*
       stp-dat-pre-130.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Banca di presentazione     :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-cod-cbp       to   w-stp-ddp-a10-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-135.
      *                  *---------------------------------------------*
      *                  * Denominazione                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica nostre banche        *
      *                      *-----------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      rf-ddp-cod-cbp       to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      w-let-arc-cbp-des    to   w-stp-ddp-d40-pls      .
           perform   stp-d40-pls-000      thru stp-d40-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-150.
      *              *-------------------------------------------------*
      *              * Richiesta stampa distinta                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Richiesta stampa distinta  :"
                                          to   w-stp-ddp-pmt-pls      .
           if        rf-ddp-snx-stp       =    "S"
                     move  "Si"           to   w-stp-ddp-a10-pls
           else if   rf-ddp-snx-stp       =    "N"
                     move  "No"           to   w-stp-ddp-a10-pls
           else      move  spaces         to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-175.
      *              *-------------------------------------------------*
      *              * Stampa distinta eseguita oppure no              *
      *              *-------------------------------------------------*
       stp-dat-pre-180.
      *                  *---------------------------------------------*
      *                  * Se non e' stata richiesta la stampa della   *
      *                  * distinta, ed inoltre il contatore di stam-  *
      *                  * pe eseguite e' a zero : no stampa           *
      *                  *---------------------------------------------*
           if        rf-ddp-snx-stp       not  = "S" and
                     rf-ddp-flc-stp       =    zero
                     go to stp-dat-pre-200.
       stp-dat-pre-185.
      *                  *---------------------------------------------*
      *                  * Preparazione literal di stampa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare area per    *
      *                      * composizione                            *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-ddp-flc          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del contatore di   *
      *                      * stampe eseguite sulla distinta          *
      *                      *-----------------------------------------*
           if        rf-ddp-flc-stp       =    zero
                     go to stp-dat-pre-186
           else if   rf-ddp-flc-stp       =    1
                     go to stp-dat-pre-187
           else      go to stp-dat-pre-188.
       stp-dat-pre-186.
      *                      *-----------------------------------------*
      *                      * Se contatore stampe a zero              *
      *                      *-----------------------------------------*
           move      "No"                 to   w-stp-ddp-flc-snx      .
           go to     stp-dat-pre-190.
       stp-dat-pre-187.
      *                      *-----------------------------------------*
      *                      * Se contatore stampe a 1                 *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-ddp-flc-snx      .
           go to     stp-dat-pre-190.
       stp-dat-pre-188.
      *                      *-----------------------------------------*
      *                      * Se contatore stampe maggiore di 1       *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-ddp-flc-snx      .
           move      ","                  to   w-stp-ddp-flc-vrg      .
           move      rf-ddp-flc-stp       to   w-stp-ddp-flc-ctr      .
           move      "volte"              to   w-stp-ddp-flc-vlt      .
           go to     stp-dat-pre-190.
       stp-dat-pre-190.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Stampa distinta eseguita   :"
                                          to   w-stp-ddp-pmt-pls      .
           move      w-stp-ddp-flc        to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-200.
      *              *-------------------------------------------------*
      *              * Richiesta archivio di supporto                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Richiesto archivio di sup. :"
                                          to   w-stp-ddp-pmt-pls      .
           if        rf-ddp-snx-ads       =    "S"
                     move  "Si"           to   w-stp-ddp-a10-pls
           else if   rf-ddp-snx-ads       =    "N"
                     move  "No"           to   w-stp-ddp-a10-pls
           else      move  spaces         to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-225.
      *              *-------------------------------------------------*
      *              * Archivio di supporto creato oppure no           *
      *              *-------------------------------------------------*
       stp-dat-pre-230.
      *                  *---------------------------------------------*
      *                  * Se non e' stata richiesta la creazione del- *
      *                  * l'archivio di supporto, ed inoltre il con-  *
      *                  * tatore di creazioni eseguite e' a zero : no *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-ddp-snx-ads       not  = "S" and
                     rf-ddp-flc-ads       =    zero
                     go to stp-dat-pre-250.
       stp-dat-pre-235.
      *                  *---------------------------------------------*
      *                  * Preparazione literal di stampa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare area per    *
      *                      * composizione                            *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-ddp-flc          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del contatore di   *
      *                      * creazioni archivio di supporto eseguite *
      *                      * per la distinta                         *
      *                      *-----------------------------------------*
           if        rf-ddp-flc-ads       =    zero
                     go to stp-dat-pre-236
           else if   rf-ddp-flc-ads       =    1
                     go to stp-dat-pre-237
           else      go to stp-dat-pre-238.
       stp-dat-pre-236.
      *                      *-----------------------------------------*
      *                      * Se contatore creazioni a zero           *
      *                      *-----------------------------------------*
           move      "No"                 to   w-stp-ddp-flc-snx      .
           go to     stp-dat-pre-240.
       stp-dat-pre-237.
      *                      *-----------------------------------------*
      *                      * Se contatore creazioni a 1              *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-ddp-flc-snx      .
           go to     stp-dat-pre-240.
       stp-dat-pre-238.
      *                      *-----------------------------------------*
      *                      * Se contatore creazioni maggiore di 1    *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-ddp-flc-snx      .
           move      ","                  to   w-stp-ddp-flc-vrg      .
           move      rf-ddp-flc-ads       to   w-stp-ddp-flc-ctr      .
           move      "volte"              to   w-stp-ddp-flc-vlt      .
           go to     stp-dat-pre-240.
       stp-dat-pre-240.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Arch. di supporto creato   :"
                                          to   w-stp-ddp-pmt-pls      .
           move      w-stp-ddp-flc        to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-250.
      *              *-------------------------------------------------*
      *              * Tipo invio archivio di supporto                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' stata richiesta la creazione del- *
      *                  * l'archivio di supporto, ed inoltre il con-  *
      *                  * tatore di creazioni eseguite e' a zero : no *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-ddp-snx-ads       not  = "S" and
                     rf-ddp-flc-ads       =    zero
                     go to stp-dat-pre-275.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Tipo invio arch. di sup.   :"
                                          to   w-stp-ddp-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ias-lun    to   v-car                  .
           move      w-exp-tip-ias-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-ias-tbl    to   v-txt                  .
           move      rf-ddp-tip-ias       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-275.
      *              *-------------------------------------------------*
      *              * Archivio di supporto inviato oppure no          *
      *              *-------------------------------------------------*
       stp-dat-pre-280.
      *                  *---------------------------------------------*
      *                  * Se non e' stata richiesta la creazione del- *
      *                  * l'archivio di supporto, ed inoltre il con-  *
      *                  * tatore di creazioni eseguite e' a zero : no *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           if        rf-ddp-snx-ads       not  = "S" and
                     rf-ddp-flc-ads       =    zero
                     go to stp-dat-pre-300.
       stp-dat-pre-285.
      *                  *---------------------------------------------*
      *                  * Preparazione literal di stampa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione preliminare area per    *
      *                      * composizione                            *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-ddp-flc          .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del contatore di   *
      *                      * invii archivio di supporto eseguiti per *
      *                      * la distinta                             *
      *                      *-----------------------------------------*
           if        rf-ddp-flc-ias       =    zero
                     go to stp-dat-pre-286
           else if   rf-ddp-flc-ias       =    1
                     go to stp-dat-pre-287
           else      go to stp-dat-pre-288.
       stp-dat-pre-286.
      *                      *-----------------------------------------*
      *                      * Se contatore invii a zero               *
      *                      *-----------------------------------------*
           move      "No"                 to   w-stp-ddp-flc-snx      .
           go to     stp-dat-pre-290.
       stp-dat-pre-287.
      *                      *-----------------------------------------*
      *                      * Se contatore invii a 1                  *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-ddp-flc-snx      .
           go to     stp-dat-pre-290.
       stp-dat-pre-288.
      *                      *-----------------------------------------*
      *                      * Se contatore invii maggiore di 1        *
      *                      *-----------------------------------------*
           move      "Si"                 to   w-stp-ddp-flc-snx      .
           move      ","                  to   w-stp-ddp-flc-vrg      .
           move      rf-ddp-flc-ias       to   w-stp-ddp-flc-ctr      .
           move      "volte"              to   w-stp-ddp-flc-vlt      .
           go to     stp-dat-pre-290.
       stp-dat-pre-290.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Arch. di supporto inviato  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      w-stp-ddp-flc        to   w-stp-ddp-a40-pls      .
           perform   stp-a40-pls-000      thru stp-a40-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pre-999.
       stp-dat-pre-300.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per presentazione distintaa    *
      *              *-------------------------------------------------*
           go to     stp-dat-pre-999.
       stp-dat-pre-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per accettazione distinta                     *
      *    *-----------------------------------------------------------*
       stp-dat-act-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-ddp-dtr-act       =    zero
                     go to stp-dat-act-999.
       stp-dat-act-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per riscossione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         Accettazione distinta          "
                                          to   w-stp-ddp-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-050.
      *              *-------------------------------------------------*
      *              * Data accettazione                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data accettazione          :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dtr-act       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-ddp-drc-act       to   w-stp-ddp-drc-ope      .
           move      rf-ddp-npc-act       to   w-stp-ddp-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-100.
      *              *-------------------------------------------------*
      *              * Data contabile bancaria                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data contabile bancaria    :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dcb-act       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-125.
      *              *-------------------------------------------------*
      *              * Numero contabile bancaria                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero contabile bancaria  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-ncb-act       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-150.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate                        *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-spe-act       to   w-stp-ddp-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-175.
      *              *-------------------------------------------------*
      *              * Importo interessi addebitati                    *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo interessi addebit. :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-int-act       to   w-stp-ddp-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-act-999.
       stp-dat-act-200.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per accettazione distinta      *
      *              *-------------------------------------------------*
           go to     stp-dat-act-999.
       stp-dat-act-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per accredito distinta                        *
      *    *-----------------------------------------------------------*
       stp-dat-acd-000.
      *              *-------------------------------------------------*
      *              * Se operazione non presente : uscita             *
      *              *-------------------------------------------------*
           if        rf-ddp-dtr-acd       =    zero
                     go to stp-dat-acd-999.
       stp-dat-acd-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina per accredito distinta         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "           Accredito distinta           "
                                          to   w-stp-ddp-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-050.
      *              *-------------------------------------------------*
      *              * Data accredito distinta                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data accredito             :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dtr-acd       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-075.
      *              *-------------------------------------------------*
      *              * Data registrazione e numero protocollo          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      rf-ddp-drc-acd       to   w-stp-ddp-drc-ope      .
           move      rf-ddp-npc-acd       to   w-stp-ddp-npc-ope      .
           perform   stp-drc-npc-000      thru stp-drc-npc-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-100.
      *              *-------------------------------------------------*
      *              * Data contabile bancaria                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data contabile bancaria    :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-dcb-acd       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-125.
      *              *-------------------------------------------------*
      *              * Numero contabile bancaria                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero contabile bancaria  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-ncb-acd       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-150.
      *              *-------------------------------------------------*
      *              * Importo spese addebitate                        *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo spese addebitate   :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-spe-acd       to   w-stp-ddp-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-175.
      *              *-------------------------------------------------*
      *              * Importo interessi addebitati                    *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Importo interessi addebit. :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-int-acd       to   w-stp-ddp-s11-pls      .
           perform   stp-s11-pls-000      thru stp-s11-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-acd-999.
       stp-dat-acd-200.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per accredito distinta         *
      *              *-------------------------------------------------*
           go to     stp-dat-acd-999.
       stp-dat-acd-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per altre informazioni                        *
      *    *-----------------------------------------------------------*
       stp-dat-ain-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per altre informazioni         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "           Altre informazioni           "
                                          to   w-stp-ddp-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-025.
      *              *-------------------------------------------------*
      *              * Flags di elaborazione bloccanti                 *
      *              *-------------------------------------------------*
       stp-dat-ain-030.
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-ddp-flg-blo       =    spaces
                     go to stp-dat-ain-050.
       stp-dat-ain-035.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags bloccanti            :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-flg-blo       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-050.
      *              *-------------------------------------------------*
      *              * Flags di elaborazione non bloccanti             *
      *              *-------------------------------------------------*
       stp-dat-ain-055.
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-ddp-flg-nbl       =    spaces
                     go to stp-dat-ain-075.
       stp-dat-ain-060.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags non bloccanti        :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-flg-nbl       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-075.
      *              *-------------------------------------------------*
      *              * Flags di sottoponibilita' a pulizia             *
      *              *-------------------------------------------------*
       stp-dat-ain-080.
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-ddp-flg-pul       =    spaces
                     go to stp-dat-ain-100.
       stp-dat-ain-085.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags per pulizia          :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-flg-pul       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-100.
      *              *-------------------------------------------------*
      *              * Data ultimo aggiornamento                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data ultimo aggiornamento  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-ide-dat       to   w-stp-ddp-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-125.
      *              *-------------------------------------------------*
      *              * Utente ultimo aggiornamento                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "     da parte dell'utente  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-ide-ute       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-150.
      *              *-------------------------------------------------*
      *              * Programma ultimo aggiornamento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         con il programma  :"
                                          to   w-stp-ddp-pmt-pls      .
           move      rf-ddp-ide-fas       to   w-stp-ddp-a10-pls      .
           perform   stp-a10-pls-000      thru stp-a10-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-175.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per altre informazioni         *
      *              *-------------------------------------------------*
           go to     stp-dat-ain-999.
       stp-dat-ain-999.
           exit.

      *    *===========================================================*
      *    * Stampa testatina per tipo operazione su distinta          *
      *    *-----------------------------------------------------------*
       stp-tst-ope-000.
      *              *-------------------------------------------------*
      *              * 1. linea                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se almeno una riga residua, con even-  *
      *                  * tuale reintestazione                        *
      *                  *---------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-ope-999.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * 40 lineette centrali                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tst-ope-200.
      *              *-------------------------------------------------*
      *              * 2. linea                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se almeno una riga residua, con even-  *
      *                  * tuale reintestazione                        *
      *                  *---------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-ope-999.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titoletto centrale                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-stp-ddp-tst-ope    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tst-ope-400.
      *              *-------------------------------------------------*
      *              * 3. linea                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se almeno una riga residua, con even-  *
      *                  * tuale reintestazione                        *
      *                  *---------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-ope-999.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * 40 lineette centrali                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tst-ope-999.
           exit.

      *    *===========================================================*
      *    * Stampa del solo prompt                                    *
      *    *-----------------------------------------------------------*
       stp-pmt-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-pmt-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-ddp-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-pmt-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di una data                                        *
      *    *-----------------------------------------------------------*
       stp-dat-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-ddp-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa data                                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-ddp-dat-pls    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-dat-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo alfanumerico di 40 caratteri           *
      *    *-----------------------------------------------------------*
       stp-a10-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-a10-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-ddp-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-ddp-a10-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a10-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo alfanumerico di 40 caratteri           *
      *    *-----------------------------------------------------------*
       stp-a40-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-a40-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-ddp-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-ddp-a40-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a40-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa descrizione di 40 caratteri                        *
      *    *-----------------------------------------------------------*
       stp-d40-pls-000.
      *              *-------------------------------------------------*
      *              * Stampa descrizione                              *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-stp-ddp-d40-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-d40-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo numerico con segno di 11 caratteri     *
      *    *-----------------------------------------------------------*
       stp-s11-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-s11-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-ddp-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa valore numerico                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<GB"                to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-ddp-s11-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-s11-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa data registrazione e numero protocollo di contabi- *
      *    * lita' generale                                            *
      *    *-----------------------------------------------------------*
       stp-drc-npc-000.
      *              *-------------------------------------------------*
      *              * Se i valori sono entrambi a zero : uscita       *
      *              *-------------------------------------------------*
           if        w-stp-ddp-drc-ope    =    zero and
                     w-stp-ddp-npc-ope    =    zero
                     go to stp-drc-npc-999.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-drc-npc-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Protocollo di contabilita' :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Editing numero protocollo                       *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-stp-ddp-npc-ope    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-edt-001      .
      *              *-------------------------------------------------*
      *              * Editing data di registrazione                   *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-stp-ddp-drc-ope    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-stp-ddp-edt-002      .
      *              *-------------------------------------------------*
      *              * Valore per numero protocollo e data di regi-    *
      *              * strazione stringati assieme                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    w-stp-ddp-edt-001
                                delimited by   spaces
                     " del "
                                delimited by   size
                     w-stp-ddp-edt-002
                                delimited by   spaces
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-drc-npc-999.
           exit.

      *    *===========================================================*
      *    * Test se almeno una riga residua, con eventuale reintesta- *
      *    * zione                                                     *
      *    *-----------------------------------------------------------*
       stp-tst-001-000.
      *              *-------------------------------------------------*
      *              * Se almeno una riga residua : uscita             *
      *              *-------------------------------------------------*
           if        v-res                >    1
                     go to stp-tst-001-999.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-001-999.
       stp-tst-001-999.
           exit.

      *    *===========================================================*
      *    * Intestazione per interrogazione                           *
      *    *-----------------------------------------------------------*
       stp-int-pag-000.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-int-pag-999.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing numero distinta                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      rf-ddp-num-ddp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titolo con numero distinta editato          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "   MOVIMENTI SU DISTINTA NR. "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Function-keys previste in Mark-points    *
      *    *-----------------------------------------------------------*
       qry-det-fky-000.
      *              *-------------------------------------------------*
      *              * Nessuna function key                            *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk(01)              .
           move      spaces               to   v-pfk(02)              .
           move      spaces               to   v-pfk(03)              .
           move      spaces               to   v-pfk(04)              .
           move      spaces               to   v-pfk(05)              .
           move      spaces               to   v-pfk(06)              .
           move      spaces               to   v-pfk(07)              .
           move      spaces               to   v-pfk(08)              .
           move      spaces               to   v-pfk(09)              .
           move      spaces               to   v-pfk(10)              .
       qry-det-fky-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key selezionata     *
      *    *-----------------------------------------------------------*
       qry-trt-fun-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-trt-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cbp]                         *
      *    *-----------------------------------------------------------*
       let-arc-cbp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-cbp-cod    =    spaces
                     go to let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP    "         to   f-key                  .
           move      w-let-arc-cbp-tip    to   rf-cbp-tip-cbp         .
           move      w-let-arc-cbp-cod    to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cbp-400.
       let-arc-cbp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cbp-des-cbp       to   w-let-arc-cbp-des      .
           move      rf-cbp-stc-csc       to   w-let-arc-cbp-csc      .
           move      rf-cbp-stc-csa       to   w-let-arc-cbp-csa      .
           move      rf-cbp-abi-ban       to   w-let-arc-cbp-abi      .
           move      rf-cbp-cab-ban       to   w-let-arc-cbp-cab      .
           move      rf-cbp-sgl-ccb       to   w-let-arc-cbp-ccb      .
           move      rf-cbp-stc-cco       to   w-let-arc-cbp-cco      .
           move      rf-cbp-stc-bba       to   w-let-arc-cbp-bba      .
           move      rf-cbp-stc-bbp       to   w-let-arc-cbp-bbp      .
           move      rf-cbp-stc-psm       to   w-let-arc-cbp-psm      .
           move      rf-cbp-stc-psi       to   w-let-arc-cbp-psi      .
           move      rf-cbp-stc-pdi       to   w-let-arc-cbp-pdi      .
           move      rf-cbp-stc-psc       to   w-let-arc-cbp-psc      .
           move      rf-cbp-stc-anv       to   w-let-arc-cbp-anv      .
           move      rf-cbp-stc-dfp       to   w-let-arc-cbp-dfp      .
           move      rf-cbp-stc-dfa       to   w-let-arc-cbp-dfa      .
           move      rf-cbp-stc-onb       to   w-let-arc-cbp-onb      .
           move      rf-cbp-stc-inb       to   w-let-arc-cbp-inb      .
           move      rf-cbp-sgl-ccp       to   w-let-arc-cbp-ccp      .
           move      rf-cbp-stc-bpa       to   w-let-arc-cbp-bpa      .
           move      rf-cbp-stc-bpp       to   w-let-arc-cbp-bpp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cbp-999.
       let-arc-cbp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cbp-flg      .
           move      all   "."            to   w-let-arc-cbp-des      .
           go to     let-arc-cbp-600.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-600.
           move      zero                 to   w-let-arc-cbp-csc      .
           move      zero                 to   w-let-arc-cbp-csa      .
           move      zero                 to   w-let-arc-cbp-abi      .
           move      zero                 to   w-let-arc-cbp-cab      .
           move      spaces               to   w-let-arc-cbp-ccb      .
           move      zero                 to   w-let-arc-cbp-cco      .
           move      zero                 to   w-let-arc-cbp-bba      .
           move      zero                 to   w-let-arc-cbp-bbp      .
           move      zero                 to   w-let-arc-cbp-psm      .
           move      zero                 to   w-let-arc-cbp-psi      .
           move      zero                 to   w-let-arc-cbp-pdi      .
           move      zero                 to   w-let-arc-cbp-psc      .
           move      zero                 to   w-let-arc-cbp-anv      .
           move      zero                 to   w-let-arc-cbp-dfp      .
           move      zero                 to   w-let-arc-cbp-dfa      .
           move      zero                 to   w-let-arc-cbp-onb      .
           move      zero                 to   w-let-arc-cbp-inb      .
           move      spaces               to   w-let-arc-cbp-ccp      .
           move      zero                 to   w-let-arc-cbp-bpa      .
           move      zero                 to   w-let-arc-cbp-bpp      .
       let-arc-cbp-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [ddp]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-ddp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      "N"                  to   w-fnd-arc-ddp-sel      .
       fnd-arc-ddp-100.
      *              *-------------------------------------------------*
      *              * Se programma di interrogazione gia' attivo :    *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep3030"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     move  "N"            to   w-pga-gep-303-snx
           else      move  "S"            to   w-pga-gep-303-snx      .
           if        w-pga-gep-303-snx    =    "S"
                     go to fnd-arc-ddp-999.
       fnd-arc-ddp-200.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non ammesso : no scrittura               *
      *                  *---------------------------------------------*
           if        w-fnd-arc-ddp-sns    not  = "S"
                     go to fnd-arc-ddp-300.
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-ddp-300.
      *              *-------------------------------------------------*
      *              * Richiamo del programma di interrogazione        *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep3030"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
       fnd-arc-ddp-350.
      *              *-------------------------------------------------*
      *              * Cancellazione del programma di interrogazione   *
      *              *-------------------------------------------------*
           cancel    s-pat                                            .
       fnd-arc-ddp-400.
      *              *-------------------------------------------------*
      *              * Determinazione selezione avvenuta               *
      *              *-------------------------------------------------*
       fnd-arc-ddp-405.
      *                  *---------------------------------------------*
      *                  * Se non era ammessa la selezioone : uscita   *
      *                  *---------------------------------------------*
           if        w-fnd-arc-ddp-sns    not  = "S"
                     go to fnd-arc-ddp-999.
       fnd-arc-ddp-410.
      *                  *---------------------------------------------*
      *                  * Lettura variabile di i.p.c. 'num-ddp' dal   *
      *                  * livello successivo per numero distinta se-  *
      *                  * lezionato                                   *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-ddp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to fnd-arc-ddp-415
           else      go to fnd-arc-ddp-420.
       fnd-arc-ddp-415.
      *                  *---------------------------------------------*
      *                  * Se variabile esistente                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di selezione avvenuta              *
      *                      *-----------------------------------------*
           move      spaces               to   w-fnd-arc-ddp-sel      .
      *                      *-----------------------------------------*
      *                      * Numero distinta selezionata             *
      *                      *-----------------------------------------*
           move      s-num                to   w-fnd-arc-ddp-num      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-ddp-999.
       fnd-arc-ddp-420.
      *                  *---------------------------------------------*
      *                  * Se variabile non esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fnd-arc-ddp-999.
       fnd-arc-ddp-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione sottoprogramma in attivita'                *
      *    *-----------------------------------------------------------*
       mem-spg-att-000.
      *              *-------------------------------------------------*
      *              * Se numero elementi in tabella gia' pari al mas- *
      *              * simo numero di elementi possibili : uscita per  *
      *              * memorizzazione non avvenuta                     *
      *              *-------------------------------------------------*
           if        w-spg-ele-num        =    w-spg-ele-max
                     move  "N"            to   w-spg-snx-gat
                     go to mem-spg-att-999.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           add       1                    to   w-spg-ele-num          .
      *              *-------------------------------------------------*
      *              * Memorizzazione codice alfanumerico del tipo di  *
      *              * interrogazione                                  *
      *              *-------------------------------------------------*
           move      w-spg-alf-gat        to   w-spg-alf-tin
                                              (w-spg-ele-num)         .
      *              *-------------------------------------------------*
      *              * Uscita per memorizzazione avvenuta              *
      *              *-------------------------------------------------*
           move      spaces               to   w-spg-snx-gat          .
       mem-spg-att-999.
           exit.

      *    *===========================================================*
      *    * Eliminazione sottoprogramma in attivita'                  *
      *    *-----------------------------------------------------------*
       eli-spg-att-000.
      *              *-------------------------------------------------*
      *              * Se numero elementi in tabella a zero : uscita   *
      *              *-------------------------------------------------*
           if        w-spg-ele-num        =    zero
                     go to eli-spg-att-999.
      *              *-------------------------------------------------*
      *              * Se l'ultimo elemento in tabella non e' pari a   *
      *              * quello da eliminare : uscita                    *
      *              *-------------------------------------------------*
           if        w-spg-alf-tin
                    (w-spg-ele-num)       not  = w-spg-alf-gat
                     go to eli-spg-att-999.
      *              *-------------------------------------------------*
      *              * Decremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           subtract  1                    from w-spg-ele-num          .
       eli-spg-att-999.
           exit.

      *    *===========================================================*
      *    * Test se sottoprogramma gia' attivo                        *
      *    *-----------------------------------------------------------*
       tst-spg-gat-000.
      *              *-------------------------------------------------*
      *              * Risposta a : non gia' attivo                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-spg-snx-gat          .
      *              *-------------------------------------------------*
      *              * Se codice alfanumerico del tipo di interroga-   *
      *              * zione a spaces : uscita                         *
      *              *-------------------------------------------------*
           if        w-spg-alf-gat        =    spaces
                     go to tst-spg-gat-999.
      *              *-------------------------------------------------*
      *              * Indice per scansione su tabella a zero          *
      *              *-------------------------------------------------*
           move      zero                 to   w-spg-ele-inx          .
       tst-spg-gat-200.
      *              *-------------------------------------------------*
      *              * Incremento indice per scansione su tabella      *
      *              *-------------------------------------------------*
           add       1                    to   w-spg-ele-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre numero elementi memorizzati : uscita   *
      *              *-------------------------------------------------*
           if        w-spg-ele-inx        >    w-spg-ele-num
                     go to tst-spg-gat-999.
      *              *-------------------------------------------------*
      *              * Se l'elemento non e' quello cercato : riciclo   *
      *              * su elemento successivo                          *
      *              *-------------------------------------------------*
           if        w-spg-alf-tin
                    (w-spg-ele-inx)       not  = w-spg-alf-gat
                     go to tst-spg-gat-200.
       tst-spg-gat-400.
      *              *-------------------------------------------------*
      *              * Risposta a : gia' attivo                        *
      *              *-------------------------------------------------*
           move      "S"                  to   w-spg-snx-gat          .
       tst-spg-gat-999.
           exit.

