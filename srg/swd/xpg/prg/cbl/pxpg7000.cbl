       Identification Division.
       Program-Id.                                 pxpg7000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    swd                 *
      *                        Area gestionale:    xpg                 *
      *                                Settore:    exp                 *
      *                                   Fase:    xpg700              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/09/02    *
      *                       Ultima revisione:    NdK del 14/04/26    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Export dati anagrafici e tabelle azienda    *
      *                                                                *
      *                   (/abd/ftx/[azi]/xxx.txt)                     *
      *                                                                *
      *                   (/abd/asc/log/AZI_TBL.log) ATTUAL. INIBITO   *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Note aggiuntive     : Inibita esportazione  *
      *                                          tabella [zmu] dei ti- *
      *                                          pi magazzino          *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Da implementare     : Lettura vriabile POST *
      *                                          con anno di esercizio *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                                                                *
      *                    _conf : File di configurazione che contiene *
      *                            l'elenco dei file da esportare in   *
      *                            modalita' 'batch'. In modalita'     *
      *                            manuale il controllo non e' attivo. *
      *                            Puo' essere presente anche un flag  *
      *                            'NOWR' che indica la generazione    *
      *                            del file di testo SENZA la costru-  *
      *                            zione della tabella MySQL.          *
      *                                                                *
      *                        !!! In fase di lettura, vengono utiliz- *
      *                            zati SOLO i primi 4 caratteri       *
      *                                                                *
      *                    MEMO  : - nel file _conf si potrebbe mette- *
      *                              re come prima riga il secolo/anno *
      *                              da cui partire nelle scansioni    *
      *                              dei movimenti che prevedono la    *
      *                              lettura per 'data' oppure aggiun- *
      *                              gere una colonna con la data      *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Da sistemare        : portare le quantita'  *
      *                                          a 10,3 (!!!)          *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Da aggiungere       : tabella [bfe] di [bfo]*
      *                                                                *
      *                                        : tabella [dbe] di [bol]*
      *                                                                *
      *                                        : tabella [xft] di [scf]*
      *                                        : tabella [xfr] di [scf]*
      *                                        : tabella [xfx] di [scf]*
      *                                                                *
      *                                        : area [___]            *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Elementi da pensare : [svs] set statistici  *
      *                                                                *
      *                                 (nuovo)  tipi movimento eva-   *
      *                                          dibili da + tipi      *
      *                                          movimento             *
      *                                                                *
      *                                 (nuovo)  relazione tra prodot- *
      *                                          ti e + classi merc.   *
      *                                                                *
      *                    ------------------------------------------- *
      *                                                                *
      *                    Aree complete (26)  : [age] [azi] [orc]     *
      *                                          [ods] [bol] [cge]     *
      *                                          [dcc] [dcp] [dpm]     *
      *                                          [dps] [mtv] [orf]     *
      *                                          [bfo] [ffo] [fat]     *
      *                                          [mag] [gep] [scf]     *
      *                                          [rda] [iic] [bil]     *
      *                                          [fab] [cdp] [vdp]     *
      *                                          [dtp] [sst]           *
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
      *    * File Control [seq]                                        *
      *    *-----------------------------------------------------------*
           select            seq   assign to input-output   f-seq-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-seq-sts .

      *    *===========================================================*
      *    * File Control [prs]                                        *
      *    *-----------------------------------------------------------*
           select  optional  prs   assign to disk           f-prs-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is prs-rck
                             file  status is f-prs-sts                .

      *    *===========================================================*
      *    * File Control [ref]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ref   assign to disk           f-ref-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is ref-rck
                             file  status is f-ref-sts                .

      *    *===========================================================*
      *    * File Control [enc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  enc   assign to disk           f-enc-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is enc-rck
                             file  status is f-enc-sts                .

      *    *===========================================================*
      *    * File Control [upr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  upr   assign to disk           f-upr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is upr-k01
                             file status  is                f-upr-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [seq]                                    *
      *    *-----------------------------------------------------------*
       fd  seq  label record omitted.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  seq-rec.
      *        *-------------------------------------------------------*
      *        * Caratteri del record                                  *
      *        *-------------------------------------------------------*
           05  seq-chr  occurs 2048       pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [prs]                                    *
      *    *-----------------------------------------------------------*
       fd  prs  label record standard.
       01  prs-rec.
           05  prs-rck.
               10  prs-tre                pic  x(04)                  .
               10  prs-cod                pic  x(40)                  .
               10  prs-prg                pic  9(03)                  .
           05  prs-rcd.
               10  prs-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  prs-r10 redefines
                   prs-r00.
                   15  prs-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  prs-r20 redefines
                   prs-r00.
                   15  prs-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  prs-r30 redefines
                   prs-r00.
                   15  prs-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  prs-r40 redefines
                   prs-r00.
                   15  prs-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ref]                                    *
      *    *-----------------------------------------------------------*
       fd  ref  label record standard.
       01  ref-rec.
           05  ref-rck.
               10  ref-tre                pic  x(04)                  .
               10  ref-cod                pic  x(40)                  .
               10  ref-prg                pic  9(03)                  .
           05  ref-rcd.
               10  ref-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  ref-r10 redefines
                   ref-r00.
                   15  ref-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  ref-r20 redefines
                   ref-r00.
                   15  ref-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  ref-r30 redefines
                   ref-r00.
                   15  ref-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  ref-r40 redefines
                   ref-r00.
                   15  ref-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [enc]                                    *
      *    *-----------------------------------------------------------*
       fd  enc  label record standard.
       01  enc-rec.
           05  enc-rck.
               10  enc-tre                pic  x(04)                  .
               10  enc-cod                pic  x(40)                  .
               10  enc-prg                pic  9(03)                  .
           05  enc-rcd.
               10  enc-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  enc-r10 redefines
                   enc-r00.
                   15  enc-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  enc-r20 redefines
                   enc-r00.
                   15  enc-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  enc-r30 redefines
                   enc-r00.
                   15  enc-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  enc-r40 redefines
                   enc-r00.
                   15  enc-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [upr]                                    *
      *    *-----------------------------------------------------------*
       fd  upr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  upr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  upr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : UTECOD                         *
      *            *---------------------------------------------------*
               10  upr-k01.
                   15  upr-cod-ute        pic  x(08)                  .
                   15  upr-cod-upr        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  upr-dat.
               10  upr-des-upr            pic  x(40)                  .
               10  upr-txt-upr.
                   15  upr-txt-rig occurs 10
                                          pic  x(40)                  .
               10  upr-mdr-upr            pic  9(02)                  .
               10  upr-mda-upr            pic  9(02)                  .
               10  upr-dat-upr            pic  9(07)       comp-3     .
               10  upr-gio-set            pic  9(02)                  .
               10  upr-gio-rif            pic  9(02)                  .
               10  upr-mes-rif            pic  9(02)                  .
               10  upr-ngp-upr            pic  9(03)       comp-3     .
               10  upr-mdp-upr            pic  9(02)                  .
               10  upr-drt-upr            pic  9(02)                  .
               10  upr-dfi-upr            pic  9(07)       comp-3     .
               10  upr-dpv-upr            pic  9(07)       comp-3     .
               10  upr-snx-acp            pic  x(01)                  .
               10  upr-snx-pvs            pic  x(01)                  .
               10  upr-snx-scf            pic  x(01)                  .
               10  upr-alx-exp.
                   15  filler occurs  78  pic  x(01)                  .

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
           05  f-seq-nam                  pic  x(04) value "seq "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-seq-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-seq-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [prs]                *
      *    *-----------------------------------------------------------*
       01  f-prs.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-prs-nam                  pic  x(04) value "prs "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-prs-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-prs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [ref]                *
      *    *-----------------------------------------------------------*
       01  f-ref.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-ref-nam                  pic  x(04) value "ref "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-ref-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-ref-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [enc]                *
      *    *-----------------------------------------------------------*
       01  f-enc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-enc-nam                  pic  x(04) value "enc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-enc-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-enc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [upr]                *
      *    *-----------------------------------------------------------*
       01  f-upr.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-upr-nam                  pic  x(04) value "upr "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-upr-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-upr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "swd"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "xpg"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "exp"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "xpg700"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pxpg7000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   ESPORTAZIONE ARCHIVI PER L'AZIENDA   "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

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
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mopsys" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mpslct"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

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
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
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
      *    * Record files per XPG                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [num]                                                 *
      *        *-------------------------------------------------------*
           copy      "swd/xpg/fls/rec/rfnum"                          .

      *    *===========================================================*
      *    * Record files per GEO                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .

      *    *===========================================================*
      *    * Record files per AZI                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .
      *        *-------------------------------------------------------*
      *        * [gva]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfgva"                          .
      *        *-------------------------------------------------------*
      *        * [gvs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfgvs"                          .
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .

      *    *===========================================================*
      *    * Record files per CGE ARC                                  *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcdb"                          .
      *        *-------------------------------------------------------*
      *        * [cea]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcea"                          .
      *        *-------------------------------------------------------*
      *        * [cer]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcer"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zc1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzc1"                          .
      *        *-------------------------------------------------------*
      *        * [zca]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzca"                          .
      *        *-------------------------------------------------------*
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [zma]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzma"                          .
      *        *-------------------------------------------------------*
      *        * [zcn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcn"                          .
      *        *-------------------------------------------------------*
      *        * [zsz]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzsz"                          .
      *        *-------------------------------------------------------*
      *        * [zsk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzsk"                          .

      *    *===========================================================*
      *    * Record files per CGE MOV                                  *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ali]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfali"                          .
      *        *-------------------------------------------------------*
      *        * [ivc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfivc"                          .
      *        *-------------------------------------------------------*
      *        * [ivp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfivp"                          .
      *        *-------------------------------------------------------*
      *        * [mgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgr"                          .
      *        *-------------------------------------------------------*
      *        * [mgs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgs"                          .
      *        *-------------------------------------------------------*
      *        * [mgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgt"                          .
      *        *-------------------------------------------------------*
      *        * [mgi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmgi"                          .
      *        *-------------------------------------------------------*
      *        * [mpu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfmpu"                          .
      *        *-------------------------------------------------------*
      *        * [rdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfrdb"                          .

      *    *===========================================================*
      *    * Record files per AGE                                      *
      *    *-----------------------------------------------------------*
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
      *        * [gpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpm"                          .
      *        *-------------------------------------------------------*
      *        * [zpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpv"                          .
      *        *-------------------------------------------------------*
      *        * [zpx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfzpx"                          .

      *    *===========================================================*
      *    * Record files per BFO                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bfe]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfe"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [bfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfs"                          .
      *        *-------------------------------------------------------*
      *        * [bfk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfk"                          .
      *        *-------------------------------------------------------*
      *        * [bfu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfu"                          .
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfx"                          .
      *        *-------------------------------------------------------*
      *        * [ybf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .

      *    *===========================================================*
      *    * Record files per FFO                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ffr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .
      *        *-------------------------------------------------------*
      *        * [fft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rffft"                          .
      *        *-------------------------------------------------------*
      *        * [ffx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffx"                          .
      *        *-------------------------------------------------------*
      *        * [yff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfyff"                          .
      *        *-------------------------------------------------------*
      *        * [ida]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfida"                          .

      *    *===========================================================*
      *    * Record files per BOL                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bie]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbie"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [bix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbix"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [zab]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzab"                          .
      *        *-------------------------------------------------------*
      *        * [zba]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzba"                          .
      *        *-------------------------------------------------------*
      *        * [zbi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .
      *        *-------------------------------------------------------*
      *        * [zct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzct"                          .
      *        *-------------------------------------------------------*
      *        * [zgc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzgc"                          .

      *    *===========================================================*
      *    * Record files per DCC ARC                                  *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cse]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfcse"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcx"                          .
      *        *-------------------------------------------------------*
      *        * [dcm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcm"                          .
      *        *-------------------------------------------------------*
      *        * [vlt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfvlt"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zdf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzdf"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfx"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zkc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzkc"                          .
      *        *-------------------------------------------------------*
      *        * [zln]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzln"                          .
      *        *-------------------------------------------------------*
      *        * [zsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsd"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [zst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzst"                          .
      *        *-------------------------------------------------------*
      *        * [zsx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsx"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .

      *    *===========================================================*
      *    * Record files per DCF ARC                                  *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [dfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdfx"                          .
      *        *-------------------------------------------------------*
      *        * [lfd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rflfd"                          .
      *        *-------------------------------------------------------*
      *        * [pdt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfpdt"                          .
      *        *-------------------------------------------------------*
      *        * [ybo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfybo"                          .
      *        *-------------------------------------------------------*
      *        * [ydf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfydf"                          .
      *        *-------------------------------------------------------*
      *        * [yfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfp"                          .
      *        *-------------------------------------------------------*
      *        * [yfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfx"                          .
      *        *-------------------------------------------------------*
      *        * [yin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyin"                          .
      *        *-------------------------------------------------------*
      *        * [ysf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfysf"                          .
      *        *-------------------------------------------------------*
      *        * [yst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyst"                          .
      *        *-------------------------------------------------------*
      *        * [yvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyvf"                          .

      *    *===========================================================*
      *    * Record files per DCP                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cpv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfcpv"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [zcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzcp"                          .
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .
      *        *-------------------------------------------------------*
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [zp2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp2"                          .
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .
      *        *-------------------------------------------------------*
      *        * [zps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzps"                          .
      *        *-------------------------------------------------------*
      *        * [ztv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfztv"                          .
      *        *-------------------------------------------------------*
      *        * [zum]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzum"                          .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dcp '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/wzosdcp0.wkl"                   .

      *    *===========================================================*
      *    * Record files per DPM                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [zm1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm1"                          .
      *        *-------------------------------------------------------*
      *        * [zm2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm2"                          .
      *        *-------------------------------------------------------*
      *        * [zm3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm3"                          .
      *        *-------------------------------------------------------*
      *        * [zms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzms"                          .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dpm '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/wzosdpm0.wkl"                   .

      *    *===========================================================*
      *    * Record files per DPS                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [zs1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs1"                          .
      *        *-------------------------------------------------------*
      *        * [zs2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs2"                          .
      *        *-------------------------------------------------------*
      *        * [zs3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs3"                          .
      *        *-------------------------------------------------------*
      *        * [zss]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzss"                          .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'dps '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/wzosdps0.wkl"                   .

      *    *===========================================================*
      *    * Record files per MTV                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [zv1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv1"                          .
      *        *-------------------------------------------------------*
      *        * [zv2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv2"                          .
      *        *-------------------------------------------------------*
      *        * [zv3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv3"                          .
      *        *-------------------------------------------------------*
      *        * [zvs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzvs"                          .

      *    *===========================================================*
      *    * Work-area per la ridefinizione dell'area libera per il    *
      *    * filtro 'mtv '                                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/wzosmtv0.wkl"                   .

      *    *===========================================================*
      *    * Record files per ORC                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ocf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocf"                          .
      *        *-------------------------------------------------------*
      *        * [ocp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocp"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocx"                          .
      *        *-------------------------------------------------------*
      *        * [zcv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzcv"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [zro]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzro"                          .

      *    *===========================================================*
      *    * Record files per ODS                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [osk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosk"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .
      *        *-------------------------------------------------------*
      *        * [osx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosx"                          .
      *        *-------------------------------------------------------*
      *        * [zsa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfzsa"                          .
      *        *-------------------------------------------------------*
      *        * [zsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfzsc"                          .

      *    *===========================================================*
      *    * Record files per ORF                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .
      *        *-------------------------------------------------------*
      *        * [ofx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofx"                          .
      *        *-------------------------------------------------------*
      *        * [yca]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyca"                          .
      *        *-------------------------------------------------------*
      *        * [yof]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyof"                          .
      *        *-------------------------------------------------------*
      *        * [yro]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfyro"                          .

      *    *===========================================================*
      *    * Record files per FAT                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ddx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfddx"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffix"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .
      *        *-------------------------------------------------------*
      *        * [idv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfidv"                          .

      *    *===========================================================*
      *    * Record files per MAG                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [mau]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmau"                          .
      *        *-------------------------------------------------------*
      *        * [mim]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmim"                          .
      *        *-------------------------------------------------------*
      *        * [miu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmiu"                          .
      *        *-------------------------------------------------------*
      *        * [mmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmc"                          .
      *        *-------------------------------------------------------*
      *        * [mmr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmr"                          .
      *        *-------------------------------------------------------*
      *        * [mms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmms"                          .
      *        *-------------------------------------------------------*
      *        * [mmt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmt"                          .
      *        *-------------------------------------------------------*
      *        * [mmv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmv"                          .
      *        *-------------------------------------------------------*
      *        * [mmz]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmmz"                          .
      *        *-------------------------------------------------------*
      *        * [zmc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmc"                          .
      *        *-------------------------------------------------------*
      *        * [zmd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmd"                          .
      *        *-------------------------------------------------------*
      *        * [zmm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmm"                          .
      *        *-------------------------------------------------------*
      *        * [zmu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzmu"                          .
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .
      *        *-------------------------------------------------------*
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .

      *    *===========================================================*
      *    * Record files per GEP                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [ccc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfccc"                          .
      *        *-------------------------------------------------------*
      *        * [cec]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcec"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [gep]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfgep"                          .
      *        *-------------------------------------------------------*
      *        * [obp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfobp"                          .
      *        *-------------------------------------------------------*
      *        * [rsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsc"                          .
      *        *-------------------------------------------------------*
      *        * [rsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsd"                          .
      *        *-------------------------------------------------------*
      *        * [scr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfscr"                          .
      *        *-------------------------------------------------------*
      *        * [sct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsct"                          .
      *        *-------------------------------------------------------*
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [zop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzop"                          .
      *        *-------------------------------------------------------*
      *        * [zpg]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzpg"                          .
      *        *-------------------------------------------------------*
      *        * [zsb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzsb"                          .
      *        *-------------------------------------------------------*
      *        * [zso]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzso"                          .

      *    *===========================================================*
      *    * Record files per SCF                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bef]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfbef"                          .
      *        *-------------------------------------------------------*
      *        * [fff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rffff"                          .
      *        *-------------------------------------------------------*
      *        * [sfa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfa"                          .
      *        *-------------------------------------------------------*
      *        * [sff]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsff"                          .
      *        *-------------------------------------------------------*
      *        * [sfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .
      *        *-------------------------------------------------------*
      *        * [sfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfs"                          .
      *        *-------------------------------------------------------*
      *        * [yop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfyop"                          .
      *        *-------------------------------------------------------*
      *        * [xfz]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfxfz"                          .

      *    *===========================================================*
      *    * Record files per RDA                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [raa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfraa"                          .
      *        *-------------------------------------------------------*
      *        * [ram]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfram"                          .
      *        *-------------------------------------------------------*
      *        * [zcr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfzcr"                          .
      *        *-------------------------------------------------------*
      *        * [zco]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfzco"                          .
      *        *-------------------------------------------------------*
      *        * [ztr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/rda/fls/rec/rfztr"                          .

      *    *===========================================================*
      *    * Record files per IIC                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [iir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiir"                          .
      *        *-------------------------------------------------------*
      *        * [iit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiit"                          .

      *    *===========================================================*
      *    * Record files per BIL                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [vbr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbr"                          .
      *        *-------------------------------------------------------*
      *        * [vbt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbt"                          .
      *        *-------------------------------------------------------*
      *        * [vbv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbv"                          .
      *        *-------------------------------------------------------*
      *        * [ztb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfztb"                          .

      *    *===========================================================*
      *    * Record files per FAB                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [fbs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rffbs"                          .
      *        *-------------------------------------------------------*
      *        * [zts]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fab/fls/rec/rfzts"                          .

      *    *===========================================================*
      *    * Record files per CDP                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cdp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfcdp"                          .
      *        *-------------------------------------------------------*
      *        * [ycp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfycp"                          .
      *        *-------------------------------------------------------*
      *        * [yrc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cdp/fls/rec/rfyrc"                          .

      *    *===========================================================*
      *    * Record files per VDP                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [vpr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfvpr"                          .
      *        *-------------------------------------------------------*
      *        * [vpt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfvpt"                          .
      *        *-------------------------------------------------------*
      *        * [yvp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/vdp/fls/rec/rfyvp"                          .

      *    *===========================================================*
      *    * Record files per DTP                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [lgr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgr"                          .
      *        *-------------------------------------------------------*
      *        * [lgt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgt"                          .
      *        *-------------------------------------------------------*
      *        * [lgv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dtp/fls/rec/rflgv"                          .

      *    *===========================================================*
      *    * Record files per SST                                      *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ssp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/sst/fls/rec/rfssp"                          .

      *    *===========================================================*
      *    * Work-area richieste per esecuzione                        *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Esportazione automatica o manuale                     *
      *        *  - A : Automatica                                     *
      *        *  - M : Manuale                                        *
      *        *-------------------------------------------------------*
           05  rr-aut-man                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data di esportazione                                  *
      *        *-------------------------------------------------------*
           05  rr-dat-exp                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Area di esportazione                                  *
      *        *-------------------------------------------------------*
           05  rr-are-exp                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Archivio di esportazione                              *
      *        *-------------------------------------------------------*
           05  rr-arc-exp                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per presa visione                              *
      *        *-------------------------------------------------------*
           05  rr-pre-vis                 pic  x(02)                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-hst                  pic  x(10)                  .
           05  f-xxx-usr                  pic  x(10)                  .
           05  f-xxx-azi                  pic  x(04)                  .
           05  f-xxx-are                  pic  x(04)                  .
           05  f-xxx-nam                  pic  x(04)                  .
           05  f-xxx-npe                  pic  x(11)                  .
           05  f-xxx-etc                  pic  x(17)                  .
           05  f-xxx-ptc                  pic  x(17)                  .
           05  f-xxx-ppb                  pic  x(40)                  .
           05  f-xxx-pat                  pic  x(40)                  .
           05  f-xxx-sts                  pic  x(02)                  .
           05  f-xxx-nrl                  pic  9(09)                  .
           05  f-xxx-nrs                  pic  9(09)                  .
           05  f-xxx-nrc                  pic  9(02)                  .
           05  f-xxx-nrd                  pic  9(01)                  .
           05  f-xxx-pfo                  pic  x(40)                  .
           05  f-xxx-log                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work generica per tutto il programma                      *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Data inizio esecuzione programma                      *
      *        *-------------------------------------------------------*
           05  w-gen-dat-iep              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data inizio ciclo notturno                            *
      *        *-------------------------------------------------------*
           05  w-gen-dat-icn              pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenze per collegamento al database                *
      *        *-------------------------------------------------------*
           05  w-ref-dtb-sql.
      *            *---------------------------------------------------*
      *            * Server                                            *
      *            *---------------------------------------------------*
               10  w-ref-dtb-sql-pwd      pic  x(20)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo esportazione                          *
      *        *-------------------------------------------------------*
           05  w-exp-aut-man.
               10  w-exp-aut-man-num      pic  9(02)       value 02   .
               10  w-exp-aut-man-lun      pic  9(02)       value 40   .
               10  w-exp-aut-man-tbl.
                   15  filler             pic  x(40) value
                          "Automatica, tutti gli archivi           "  .
                   15  filler             pic  x(40) value
                          "Manuale, un archivio alla volta         "  .
      *        *-------------------------------------------------------*
      *        * Work per : Risposta Si/No                             *
      *        *-------------------------------------------------------*
           05  w-exp-ris-snx.
               10  w-exp-ris-snx-num      pic  9(02)       value 02   .
               10  w-exp-ris-snx-lun      pic  9(02)       value 02   .
               10  w-exp-ris-snx-tbl.
                   15  filler             pic  x(02) value "Si"       .
                   15  filler             pic  x(02) value "No"       .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su tabella aree                         *
      *        *-------------------------------------------------------*
           05  w-fnd-tbl-are.
               10  w-fnd-tbl-are-sel      pic  x(01)                  .
               10  w-fnd-tbl-are-cod      pic  x(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)                  .
               10  w-let-arc-gxc-tip      pic  x(01)                  .
               10  w-let-arc-gxc-cmn      pic  9(05)                  .
               10  w-let-arc-gxc-fzn      pic  9(03)                  .
               10  w-let-arc-gxc-lct      pic  9(03)                  .
               10  w-let-arc-gxc-des      pic  x(30)                  .
               10  w-let-arc-gxc-prv      pic  x(02)                  .
               10  w-let-arc-gxc-cap      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Exe                               *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Flag di esecuzione batch                              *
      *        *-------------------------------------------------------*
           05  w-exe-flg-btc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo per il ciclo di attesa            *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-att              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Flag di esecuzione interrotta                         *
      *        *-------------------------------------------------------*
           05  w-exe-flg-ein              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per data esecuzione                            *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
           05  w-exe-saa-exe              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ora esecuzione                             *
      *        *-------------------------------------------------------*
           05  w-exe-ora-exe              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per contatori di tempo                         *
      *        *-------------------------------------------------------*
           05  w-exe-tim-ini              pic  9(04)                  .
           05  w-exe-tim-fin              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per data disponibilita'                        *
      *        *-------------------------------------------------------*
           05  w-exe-dat-dsp              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di esecuzione bloccata                           *
      *        *-------------------------------------------------------*
           05  w-exe-flg-brk              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Messaggio di esecuzione bloccata                      *
      *        *-------------------------------------------------------*
           05  w-exe-flg-brm              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per contatori fatture                          *
      *        *-------------------------------------------------------*
           05  w-exe-fit-ctr              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Flag di esito collegamento                            *
      *        *-------------------------------------------------------*
           05  w-exe-flg-png              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per verifica collegamento                      *
      *        *-------------------------------------------------------*
           05  w-exe-let-png              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio di comodo                                 *
      *        *-------------------------------------------------------*
           05  w-exe-sav-azi              pic  x(06)                  .
           05  w-exe-sav-prt              pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per esportazione contatti                      *
      *        *-------------------------------------------------------*
           05  w-exe-con.
               10  w-exe-con-arc          pic  9(02)                  .
               10  w-exe-con-cod          pic  9(07)                  .
               10  w-exe-con-prg          pic  9(05)                  .
               10  w-exe-con-dpz          pic  x(04)                  .
               10  w-exe-con-tip          pic  x(03)                  .
               10  w-exe-con-dsk          pic  x(40)                  .
               10  w-exe-con-rep          pic  x(40)                  .
               10  w-exe-con-lun          pic  9(02)                  .
               10  w-exe-con-num          pic  x(80)                  .
               10  w-exe-con-sav          pic  x(80)                  .
               10  w-exe-con-nue          pic  x(20)                  .
               10  w-exe-con-pre          pic  x(20)                  .
               10  w-exe-con-int          pic  x(30)                  .
               10  w-exe-con-idd          pic  9(07)                  .
               10  w-exe-con-agg          pic  9(07)                  .
               10  w-exe-con-idu          pic  x(08)                  .
               10  w-exe-con-idf          pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per scaglioni                                  *
      *        *-------------------------------------------------------*
           05  w-exe-scg-zpv              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per esportazione personalizzazioni e referenze *
      *        *-------------------------------------------------------*
           05  w-exe-per-tip              pic  x(01)                  .
           05  w-exe-per-val              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per esportazione valuta                        *
      *        *-------------------------------------------------------*
           05  w-exe-sgl-vlt              pic  x(03)                  .
           05  w-exe-dec-vlt              pic  9(01)                  .
           05  w-exe-tdc-vlt              pic  x(01)                  .
           05  w-exe-cdc-vlt              pic  9(06)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Comodi per esportazione legame valutario              *
      *        *-------------------------------------------------------*
           05  w-exe-sgl-lvl              pic  x(03)                  .
           05  w-exe-dec-lvl              pic  9(01)                  .
           05  w-exe-tdc-lvl              pic  x(01)                  .
           05  w-exe-prz-lvl              pic  9(09)                  .
           05  w-exe-cdc-lvl              pic  9(06)v9(05)            .
           05  w-exe-ccr-lvl              pic  9(06)v9(05)            .
           05  w-exe-plm-lvl              pic  9(01)v9(02)            .
           05  w-exe-tlm-lvl              pic  x(01)                  .
           05  w-exe-map-lvl              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per sconti e affini                            *
      *        *-------------------------------------------------------*
           05  w-exe-sco-tot              pic s9(11)                  .
           05  w-exe-sco-per              pic  9(02)v9(01)            .
           05  w-exe-sco-iva              pic  9(05)                  .
           05  w-exe-sco-cpt              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per codice IVA e contropartita                 *
      *        *-------------------------------------------------------*
           05  w-exe-iec-iva              pic  9(05)                  .
           05  w-exe-iec-cpt              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per data e numero protocollo contabilita'      *
      *        *-------------------------------------------------------*
           05  w-exe-dep-dat              pic  9(07)                  .
           05  w-exe-dep-prt              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per data e numero documento                    *
      *        *-------------------------------------------------------*
           05  w-exe-den-dat              pic  9(07)                  .
           05  w-exe-den-num              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione S.I.A.                       *
      *        *-------------------------------------------------------*
           05  w-exe-rec-sia.
               10  w-exe-daz-001          pic  x(24)                  .
               10  w-exe-daz-002          pic  x(24)                  .
               10  w-exe-daz-003          pic  x(24)                  .
               10  w-exe-daz-004          pic  x(24)                  .
               10  w-exe-fir-emi          pic  x(20)                  .
               10  w-exe-cda-sia          pic  x(05)                  .
               10  w-exe-cor-ndg          pic  x(12)                  .
               10  w-exe-pvc-aut          pic  x(15)                  .
               10  w-exe-num-aut          pic  9(10)                  .
               10  w-exe-dat-aut          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per esportazione numerazioni                   *
      *        *-------------------------------------------------------*
           05  w-exe-num-key              pic  x(08)                  .
           05  w-exe-num-saa              pic  x(03)                  .
           05  w-exe-num-dpz              pic  x(02)                  .
           05  w-exe-num-sgl              pic  x(03)                  .
           05  w-exe-num-cdn              pic  x(02)                  .
           05  w-exe-num-d01              pic  9(07)                  .
           05  w-exe-num-d02              pic  9(07)                  .
           05  w-exe-num-d03              pic  9(07)                  .
           05  w-exe-num-f01              pic  x(01)                  .
           05  w-exe-num-f02              pic  x(01)                  .
           05  w-exe-num-f03              pic  x(01)                  .
           05  w-exe-num-prt              pic  x(11)                  .
           05  w-exe-num-com              pic  x(80)                  .
           05  w-exe-num-are              pic  x(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Buf                               *
      *    *-----------------------------------------------------------*
       01  w-buf.
      *        *-------------------------------------------------------*
      *        * Work per Buf configurazione                           *
      *        *-------------------------------------------------------*
           05  w-buf-nam-cnf.
               10  w-buf-nam-cnf-ele      pic  9(03)                  .
               10  w-buf-nam-cnf-max      pic  9(03)  value 300       .
               10  w-buf-nam-cnf-ctr      pic  9(03)                  .
               10  w-buf-nam-cnf-buf occurs 300
                                 indexed  by   w-buf-nam-cnf-inx      .
                   15  w-buf-nam-cnf-nam  pic  x(04)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det numero records                           *
      *        *-------------------------------------------------------*
           05  w-det-rec-fil.
               10  w-det-rec-fil-flg      pic  x(01)                  .
               10  w-det-rec-fil-alf.
                   15  w-det-rec-fil-tst  pic  x(13)                  .
                   15  w-det-rec-fil-ler  pic  x(05)                  .
                   15  w-det-rec-fil-dat  pic  x(13)                  .
               10  w-det-rec-fil-rec      pic  9(11)                  .
               10  w-det-rec-fil-s15      pic s9(15)                  .
               10  w-det-rec-fil-v02      pic s9(03)                  .
               10  w-det-rec-fil-tpt      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione secolo anno                        *
      *        *-------------------------------------------------------*
           05  w-det-sec-ann.
               10  w-det-sec-ann-esa      pic  9(03)                  .
               10  w-det-sec-ann-csa      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione totale incassato                   *
      *        *                                                       *
      *        * PROVVISORIO ___ UTILIZZATO DA 'pxpg7000.bol'          *
      *        *-------------------------------------------------------*
           05  w-det-tot-inc.
               10  w-det-tot-inc-tot      pic  9(08)                  .
               10  w-det-tot-inc-alf      pic  x(08)                  .

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
           05  w-scr-prt-saa              pic  x(03)                  .
           05  w-scr-prt-num              pic  x(08)                  .
           05  w-scr-prt-prt              pic  9(07)                  .
           05  w-scr-prt-dat              pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per ridefinizione dati sezione                  *
      *    *-----------------------------------------------------------*
       01  w-rds.
      *        *-------------------------------------------------------*
      *        * Area di ridefinizione                                 *
      *        *-------------------------------------------------------*
           05  w-rds-ele.
               10  w-rds-ele-sez.
                   15  filler occurs 512  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Dati sezioni                                      *
      *            *---------------------------------------------------*
      *                *-----------------------------------------------*
      *                * Presentazione                                 *
      *                *-----------------------------------------------*
               10  w-rds-sez-pre redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-cbp      pic  x(10)                  .
                   15  w-rds-cod-cbp-des  pic  x(40)                  .
                   15  w-rds-fir-maa      pic  x(40)                  .
                   15  w-rds-dat-pre      pic  9(07)                  .
                   15  w-rds-cod-int      pic  9(07)                  .
                   15  w-rds-cod-int-rag  pic  x(40)                  .
                   15  filler occurs 368  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Erario                                        *
      *                *-----------------------------------------------*
               10  w-rds-sez-era redefines
                   w-rds-ele-sez.
                   15  w-rds-trb-era      pic  x(07)                  .
                   15  w-rds-trb-era-des  pic  x(40)                  .
                   15  w-rds-rtz-era      pic  x(07)                  .
                   15  w-rds-ann-era      pic  9(04)                  .
                   15  w-rds-uff-era      pic  x(07)                  .
                   15  w-rds-uff-era-des  pic  x(40)                  .
                   15  w-rds-att-era      pic  x(07)                  .
                   15  filler occurs 400  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * INPS                                          *
      *                *-----------------------------------------------*
               10  w-rds-sez-ips redefines
                   w-rds-ele-sez.
                   15  w-rds-sed-ips      pic  x(07)                  .
                   15  w-rds-sed-ips-des  pic  x(40)                  .
                   15  w-rds-trb-ips      pic  x(07)                  .
                   15  w-rds-trb-ips-des  pic  x(40)                  .
                   15  w-rds-mat-ips      pic  x(20)                  .
                   15  w-rds-mda-ips      pic  9(02)                  .
                   15  w-rds-ada-ips      pic  9(04)                  .
                   15  w-rds-maa-ips      pic  9(02)                  .
                   15  w-rds-aaa-ips      pic  9(04)                  .
                   15  filler occurs 386  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Regione                                       *
      *                *-----------------------------------------------*
               10  w-rds-sez-rgn redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-rgn      pic  x(07)                  .
                   15  w-rds-cod-rgn-des  pic  x(40)                  .
                   15  w-rds-trb-rgn      pic  x(07)                  .
                   15  w-rds-trb-rgn-des  pic  x(40)                  .
                   15  w-rds-rtz-rgn      pic  x(07)                  .
                   15  w-rds-ann-rgn      pic  9(04)                  .
                   15  filler occurs 407  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Enti locali                                   *
      *                *-----------------------------------------------*
               10  w-rds-sez-enl redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-enl      pic  x(07)                  .
                   15  w-rds-cod-enl-des  pic  x(40)                  .
                   15  w-rds-trb-enl      pic  x(07)                  .
                   15  w-rds-trb-rgn-des  pic  x(40)                  .
                   15  w-rds-rtz-enl      pic  x(07)                  .
                   15  w-rds-ann-enl      pic  9(04)                  .
      *                    *-------------------------------------------*
      *                    * Aggiunti per ICI                          *
      *                    *-------------------------------------------*
                   15  w-rds-pi1-enl      pic  x(01)                  .
                   15  w-rds-pi2-enl      pic  x(01)                  .
                   15  w-rds-pi3-enl      pic  x(01)                  .
                   15  w-rds-pi4-enl      pic  x(01)                  .
                   15  w-rds-nim-enl      pic  9(02)                  .
                   15  w-rds-dia-enl      pic  9(13)                  .
                   15  filler occurs 388  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * INAIL                                         *
      *                *-----------------------------------------------*
               10  w-rds-sez-inl redefines
                   w-rds-ele-sez.
                   15  w-rds-sed-inl      pic  x(07)                  .
                   15  w-rds-sed-inl-des  pic  x(40)                  .
                   15  w-rds-pos-inl      pic  x(20)                  .
                   15  w-rds-ccp-inl      pic  x(07)                  .
                   15  w-rds-rif-inl      pic  x(20)                  .
                   15  w-rds-cau-inl      pic  x(07)                  .
                   15  filler occurs 411  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Enti previdenziali                            *
      *                *-----------------------------------------------*
               10  w-rds-sez-enp redefines
                   w-rds-ele-sez.
                   15  w-rds-cod-enp      pic  x(07)                  .
                   15  w-rds-cod-enp-des  pic  x(40)                  .
                   15  w-rds-sed-enp      pic  x(07)                  .
                   15  w-rds-sed-enp-des  pic  x(40)                  .
                   15  w-rds-cau-enp      pic  x(07)                  .
                   15  w-rds-cau-enp-des  pic  x(40)                  .
                   15  w-rds-pos-enp      pic  x(20)                  .
                   15  w-rds-mda-enp      pic  9(02)                  .
                   15  w-rds-ada-enp      pic  9(04)                  .
                   15  w-rds-maa-enp      pic  9(02)                  .
                   15  w-rds-aaa-enp      pic  9(04)                  .
                   15  filler occurs 339  pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice CIN                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acodcin0.acl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
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
      *              * Se no richieste : a esecuzione esportazione     *
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
      *              * Esecuzione esportazione file                    *
      *              *-------------------------------------------------*
           perform   exe-exp-fil-000      thru exe-exp-fil-999        .
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
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

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
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
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
           move      i-ide-des            to   v-alf                  .
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
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Referenze per collegamento al database      *
      *                  *---------------------------------------------*
           perform   ref-dtb-sql-000      thru ref-dtb-sql-999        .
      *                  *---------------------------------------------*
      *                  * Test se password determinata                *
      *                  *---------------------------------------------*
           if        w-ref-dtb-sql-pwd    not  = spaces
                     go to pre-exe-pgm-200.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Manca la password di collegamento al database !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di errore                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-200.
       pre-exe-pgm-300.
      *              *-------------------------------------------------*
      *              * Preparazione data ed ora per esecuzione         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-exe          .
      *                  *---------------------------------------------*
      *                  * Ora                                         *
      *                  *---------------------------------------------*
           move      0005                 to   w-exe-ora-exe          .
       pre-exe-pgm-400.
      *              *-------------------------------------------------*
      *              * Preparazione nome host per il database          *
      *              *-------------------------------------------------*
           move      "localhost "         to   f-xxx-hst              .
      *              *-------------------------------------------------*
      *              * Preparazione username per il database           *
      *              *-------------------------------------------------*
           move      "root      "         to   f-xxx-usr              .
      *              *-------------------------------------------------*
      *              * Preparazione nome azienda                       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   f-xxx-azi              .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pst
                    (01 : 01)             to   w-exe-flg-btc          .
           if        w-exe-flg-btc        not  = "S" and
                     w-exe-flg-btc        not  = "X"
                     move  "N"            to   w-exe-flg-btc          .
       pre-exe-pgm-500.
      *              *-------------------------------------------------*
      *              * Preparazione pathname di base files sequenziali *
      *              *-------------------------------------------------*
           perform   pre-bas-pth-000      thru pre-bas-pth-999        .
      *              *-------------------------------------------------*
      *              * Determinazione step in funzione del flag di     *
      *              * esecuzione batch                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul flag                               *
      *                  *---------------------------------------------*
           if        w-exe-flg-btc        =    "S"
                     go to pre-exe-pgm-510
           else if   w-exe-flg-btc        =    "X"
                     go to pre-exe-pgm-520
           else      go to pre-exe-pgm-530.
       pre-exe-pgm-510.
      *                  *---------------------------------------------*
      *                  * Se batch con scrittura su database          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura file '_conf'                    *
      *                      *-----------------------------------------*
           perform   pre-exe-pgm-cnf-000  thru pre-exe-pgm-cnf-999    .
      *                      *-----------------------------------------*
      *                      * Eventuali forzature per batch           *
      *                      *-----------------------------------------*
           move      "A"                  to   rr-aut-man             .
           move      zero                 to   rr-dat-exp             .
           move      spaces               to   rr-are-exp             .
           move      spaces               to   rr-arc-exp             .
           move      spaces               to   rr-pre-vis             .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pre-exe-pgm-900.
       pre-exe-pgm-520.
      *                  *---------------------------------------------*
      *                  * Se batch senza scrittura su database        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura file '_conf'                    *
      *                      *-----------------------------------------*
           perform   pre-exe-pgm-cnf-000  thru pre-exe-pgm-cnf-999    .
      *                      *-----------------------------------------*
      *                      * Eventuali forzature per batch           *
      *                      *-----------------------------------------*
           move      "A"                  to   rr-aut-man             .
           move      zero                 to   rr-dat-exp             .
           move      spaces               to   rr-are-exp             .
           move      spaces               to   rr-arc-exp             .
           move      spaces               to   rr-pre-vis             .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pre-exe-pgm-900.
       pre-exe-pgm-530.
      *                  *---------------------------------------------*
      *                  * Se NON batch                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     pre-exe-pgm-900.
       pre-exe-pgm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *                                                           *
      *    * Lettura file di configurazione                            *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-cnf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-nam-cnf-ele      .
      *              *-------------------------------------------------*
      *              * Preparazione pathname                           *
      *              *-------------------------------------------------*
           move      spaces               to   g-pat                  .
           string    f-xxx-ppb  delimited by   spaces
                     "/"        delimited by   size
                     f-xxx-azi  delimited by   spaces
                     "/_conf"   delimited by   size
                                          into g-pat                  .
      *              *-------------------------------------------------*
      *              * Apertura del file in input                      *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "conf"               to   g-nam                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to pre-exe-pgm-cnf-800.
       pre-exe-pgm-cnf-200.
      *              *-------------------------------------------------*
      *              * Ciclo di lettura file                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura linea                               *
      *                  *---------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se at-end                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-end-fil
                     go to pre-exe-pgm-cnf-800.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-nam-cnf-ele      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      g-rec                to   w-buf-nam-cnf-nam
                                              (w-buf-nam-cnf-ele)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-cnf-200.
       pre-exe-pgm-cnf-800.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
       pre-exe-pgm-cnf-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative al collegamento al       *
      *    * database                                                  *
      *    *-----------------------------------------------------------*
       ref-dtb-sql-000.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione relativa alla   *
      *              * password per MySQL                              *
      *              *-------------------------------------------------*
           move      "PS"                 to   s-ope                  .
           move      "pwd-sql"            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-alf                to   w-ref-dtb-sql-pwd      .
       ref-dtb-sql-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Eventuali forzature                             *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     move  "N"            to   w-cnt-fun-snx-ric
                     move  "N"            to   w-cnt-fun-snx-cic
                     move  "N"            to   w-cnt-fun-snx-aut
                     go to pre-tip-fun-999.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *-------------------------------------------------*
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
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *                  *---------------------------------------------*
      *                  * Data esportazione                           *
      *                  *---------------------------------------------*
           perform   acc-dat-exp-000      thru acc-dat-exp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo esportazione                           *
      *                  *---------------------------------------------*
           perform   acc-aut-man-000      thru acc-aut-man-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Area esportazione                           *
      *                  *---------------------------------------------*
           perform   acc-are-exp-000      thru acc-are-exp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Archivio esportazione                       *
      *                  *---------------------------------------------*
           perform   acc-arc-exp-000      thru acc-arc-exp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
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
           move      "Conferma impostazioni (S/N/E) ?"
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
      *              * Tipo esecuzione                                 *
      *              *-------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Data esportazione                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data iniziale esportazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Tipo esportazione                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo conversione ..........:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Area esportazione                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Area     da esportare .....:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Archivio esportazione                           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Archivio da esportare .....:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione linea di separazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data esportazione                          *
      *    *-----------------------------------------------------------*
       acc-dat-exp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default                                     *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rr-dat-exp             .
       acc-dat-exp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-exp           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-exp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-exp-999.
       acc-dat-exp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-exp             .
       acc-dat-exp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-exp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-exp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-exp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-exp-100.
       acc-dat-exp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data esportazione                       *
      *    *-----------------------------------------------------------*
       vis-dat-exp-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-exp           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-exp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo esportazione          *
      *    *-----------------------------------------------------------*
       acc-aut-man-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default                                     *
      *                  *---------------------------------------------*
           move      "A"                  to   rr-aut-man             .
       acc-aut-man-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aut-man-lun    to   v-car                  .
           move      w-exp-aut-man-num    to   v-ldt                  .
           move      "AM#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aut-man-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-aut-man           =    "A"
                     move  01             to   v-num
           else if   rr-aut-man           =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-aut-man-999.
       acc-aut-man-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "A"            to   rr-aut-man
           else if   v-num                =    02
                     move  "M"            to   rr-aut-man
           else      move  spaces         to   rr-aut-man             .
       acc-aut-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    spaces
                     go to acc-aut-man-100.
       acc-aut-man-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-aut-man-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-aut-man-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-aut-man-100.
       acc-aut-man-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo esportazione                       *
      *    *-----------------------------------------------------------*
       vis-aut-man-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-aut-man-lun    to   v-car                  .
           move      w-exp-aut-man-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-aut-man-tbl    to   v-txt                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        rr-aut-man           =    "A"
                     move  01             to   v-num
           else if   rr-aut-man           =    "M"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-aut-man-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Area esportazione                          *
      *    *-----------------------------------------------------------*
       acc-are-exp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-are-exp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-are-exp           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-are-exp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-are-exp-999.
       acc-are-exp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-are-exp             .
       acc-are-exp-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-are-exp-400.
      *                  *---------------------------------------------*
      *                  * Find su tabella aree                        *
      *                  *---------------------------------------------*
           perform   fnd-tbl-are-000      thru fnd-tbl-are-999        .
           if        w-fnd-tbl-are-sel    not  = spaces
                     go to acc-are-exp-100.
           move      w-fnd-tbl-are-cod    to   rr-are-exp             .
           perform   vis-are-exp-000      thru vis-are-exp-999        .
           move      spaces               to   v-key                  .
       acc-are-exp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-are-exp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-are-exp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-are-exp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-are-exp-100.
       acc-are-exp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Area esportazione                       *
      *    *-----------------------------------------------------------*
       vis-are-exp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-are-exp           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-are-exp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Archivio esportazione                      *
      *    *-----------------------------------------------------------*
       acc-arc-exp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-arc-exp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-arc-exp           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-arc-exp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-arc-exp-999.
       acc-arc-exp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-arc-exp             .
       acc-arc-exp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-arc-exp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-arc-exp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-arc-exp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-arc-exp-100.
       acc-arc-exp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Archivio esportazione                   *
      *    *-----------------------------------------------------------*
       vis-arc-exp-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-arc-exp           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-arc-exp-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controllo su tipo esportazione                  *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "A" or
                     rr-aut-man           =    "M"
                     go to tdo-ric-sel-999.
           move      "ME"                 to   v-ope                  .
           move      "Tipo esportazione errato !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
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
           move      spaces               to   rr-aut-man             .
           move      zero                 to   rr-dat-exp             .
           move      spaces               to   rr-are-exp             .
           move      spaces               to   rr-arc-exp             .
           move      spaces               to   rr-pre-vis             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione routine di esportazione                        *
      *    *-----------------------------------------------------------*
       exe-exp-fil-000.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to exe-exp-fil-500.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       exe-exp-fil-100.
      *              *-------------------------------------------------*
      *              * Preliminari                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Comodo per separatori                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-scr-str-sav          .
       exe-exp-fil-400.
      *              *-------------------------------------------------*
      *              * Preparazione ora di inizio                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Date and time da segreteria                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Estrazione ora e minuti da 'time'           *
      *                  *---------------------------------------------*
           move      s-ora                to   w-exe-tim-ini          .
           multiply  100                  by   w-exe-tim-ini          .
           add       s-min                to   w-exe-tim-ini          .
      *                  *---------------------------------------------*
      *                  * Erase linee impegnate                       *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      09                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "MB"                 to   v-edm                  .
           move      "XX:XX"              to   v-msk                  .
           move      09                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-exe-tim-ini        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-fil-500.
      *              *=================================================*
      *              * Esportazioni per [xpg]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [cat]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cat-000      thru exe-exp-cat-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [prs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-prs-000      thru exe-exp-prs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ref]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ref-000      thru exe-exp-ref-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [num]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-num-000      thru exe-exp-num-999        .
      *              *=================================================*
      *              * Esportazioni per [azi] archivi                  *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ada]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ada-000      thru exe-exp-ada-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [adc] Contatti                 *
      *                  *---------------------------------------------*
           perform   exe-exp-adc-000      thru exe-exp-adc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [gva]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-gva-000      thru exe-exp-gva-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [gvs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-gvs-000      thru exe-exp-gvs-999        .
      *              *=================================================*
      *              * Esportazioni per [cge] archivi                  *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [cdb]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cdb-000      thru exe-exp-cdb-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [cea]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cea-000      thru exe-exp-cea-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [cer]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cer-000      thru exe-exp-cer-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [cli]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cli-000      thru exe-exp-cli-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lic]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lic-000      thru exe-exp-lic-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fnt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fnt-000      thru exe-exp-fnt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [pdc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-pdc-000      thru exe-exp-pdc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zma]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zma-000      thru exe-exp-zma-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zcn]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zcn-000      thru exe-exp-zcn-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsk]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsk-000      thru exe-exp-zsk-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zc1]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zc1-000      thru exe-exp-zc1-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zca]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zca-000      thru exe-exp-zca-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zcc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zcc-000      thru exe-exp-zcc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zci]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zci-000      thru exe-exp-zci-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsz]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsz-000      thru exe-exp-zsz-999        .
      *              *=================================================*
      *              * Esportazioni per [cge] movimenti                *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ali]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ali-000      thru exe-exp-ali-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ivc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ivc-000      thru exe-exp-ivc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ivp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ivp-000      thru exe-exp-ivp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mgr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mgr-000      thru exe-exp-mgr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mgs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mgs-000      thru exe-exp-mgs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mgt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mgt-000      thru exe-exp-mgt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mgi]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mgi-000      thru exe-exp-mgi-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mpu]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mpu-000      thru exe-exp-mpu-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [rdb]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-rdb-000      thru exe-exp-rdb-999        .
      *              *=================================================*
      *              * Esportazioni per [age]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [age]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-age-000      thru exe-exp-age-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ags]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ags-000      thru exe-exp-ags-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [gpc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-gpc-000      thru exe-exp-gpc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [gpm]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-gpm-000      thru exe-exp-gpm-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zpv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zpv-000      thru exe-exp-zpv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zpv] scaglioni                *
      *                  *---------------------------------------------*
           perform   exe-exp-zpy-000      thru exe-exp-zpy-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zpx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zpx-000      thru exe-exp-zpx-999        .
      *              *=================================================*
      *              * Esportazioni per [bfo]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [bfe]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bfe-000      thru exe-exp-bfe-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bfr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bfr-000      thru exe-exp-bfr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bfs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bfs-000      thru exe-exp-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bfk]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bfk-000      thru exe-exp-bfk-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bfu]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bfu-000      thru exe-exp-bfu-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bfx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bfx-000      thru exe-exp-bfx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bft]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bft-000      thru exe-exp-bft-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bft] - castelletto iva        *
      *                  *---------------------------------------------*
           perform   exe-exp-bfi-000      thru exe-exp-bfi-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bft] - cast. contropartite    *
      *                  *---------------------------------------------*
           perform   exe-exp-bfc-000      thru exe-exp-bfc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bft] - spese                  *
      *                  *                                             *
      *                  * N.B.: per risolvere un conflitto di nomi il *
      *                  *       file SQL e' stato chiamato [bfz]      *
      *                  *       anziche' [bfs] che contiene le spunte *
      *                  *       delle righe                           *
      *                  *                                             *
      *                  *---------------------------------------------*
           perform   exe-exp-bfz-000      thru exe-exp-bfz-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bft] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-bfv-000      thru exe-exp-bfv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ybf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ybf-000      thru exe-exp-ybf-999        .
      *              *=================================================*
      *              * Esportazioni per [ffo]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ffr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ffr-000      thru exe-exp-ffr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ffx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ffx-000      thru exe-exp-ffx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fft]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fft-000      thru exe-exp-fft-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fft] - castelletto iva        *
      *                  *---------------------------------------------*
           perform   exe-exp-ffi-000      thru exe-exp-ffi-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fft] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-ffs-000      thru exe-exp-ffs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fft] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-ffv-000      thru exe-exp-ffv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yff]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yff-000      thru exe-exp-yff-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ida]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ida-000      thru exe-exp-ida-999        .
      *              *=================================================*
      *              * Esportazioni per [bol]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [bie]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bie-000      thru exe-exp-bie-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bir]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bir-000      thru exe-exp-bir-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bit]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bit-000      thru exe-exp-bit-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bit] - castelletto iva        *
      *                  *---------------------------------------------*
           perform   exe-exp-bii-000      thru exe-exp-bii-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bit] - cast. contropartite    *
      *                  *---------------------------------------------*
           perform   exe-exp-bic-000      thru exe-exp-bic-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bit] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-bis-000      thru exe-exp-bis-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bit] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-biv-000      thru exe-exp-biv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [bix]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bix-000      thru exe-exp-bix-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [vet]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vet-000      thru exe-exp-vet-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zab]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zab-000      thru exe-exp-zab-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zba]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zba-000      thru exe-exp-zba-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zbi]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zbi-000      thru exe-exp-zbi-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zct]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zct-000      thru exe-exp-zct-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zgc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zgc-000      thru exe-exp-zgc-999        .
      *              *=================================================*
      *              * Esportazioni per [dcc] archivi                  *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [cse]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cse-000      thru exe-exp-cse-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dcc-000      thru exe-exp-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcd] dipendenze               *
      *                  *---------------------------------------------*
           perform   exe-exp-dcd-000      thru exe-exp-dcd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcc] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-dcs-000      thru exe-exp-dcs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcc] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-dcv-000      thru exe-exp-dcv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dcx-000      thru exe-exp-dcx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcm]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dcm-000      thru exe-exp-dcm-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [vlt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vlt-000      thru exe-exp-vlt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zbo]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zbo-000      thru exe-exp-zbo-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zbo] - scaglioni              *
      *                  *---------------------------------------------*
           perform   exe-exp-zbx-000      thru exe-exp-zbx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zcs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zcs-000      thru exe-exp-zcs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zdf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zdf-000      thru exe-exp-zdf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zfp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zfp-000      thru exe-exp-zfp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zfx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zfx-000      thru exe-exp-zfx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zin]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zin-000      thru exe-exp-zin-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zkc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zkc-000      thru exe-exp-zkc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zln]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zln-000      thru exe-exp-zln-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsd]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsd-000      thru exe-exp-zsd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsd] - scaglioni              *
      *                  *---------------------------------------------*
           perform   exe-exp-zdx-000      thru exe-exp-zdx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsf-000      thru exe-exp-zsf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zst]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zst-000      thru exe-exp-zst-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsx-000      thru exe-exp-zsx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zvf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zvf-000      thru exe-exp-zvf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zvl]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zvl-000      thru exe-exp-zvl-999        .
       exe-exp-fil-510.
      *              *=================================================*
      *              * Esportazioni per [dcf] archivi                  *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [aaf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-aaf-000      thru exe-exp-aaf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [aap] scaglioni prezzo [aaf]   *
      *                  *---------------------------------------------*
           perform   exe-exp-aap-000      thru exe-exp-aap-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [aaq]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-aaq-000      thru exe-exp-aaq-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dcf-000      thru exe-exp-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dfd] dipendenze               *
      *                  *---------------------------------------------*
           perform   exe-exp-dfd-000      thru exe-exp-dfd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcf] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-dfs-000      thru exe-exp-dfs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcf] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-dfv-000      thru exe-exp-dfv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dfx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dfx-000      thru exe-exp-dfx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lfd]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lfd-000      thru exe-exp-lfd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lfd] Scaglioni prezzo         *
      *                  *---------------------------------------------*
           perform   exe-exp-lfs-000      thru exe-exp-lfs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [pdt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-pdt-000      thru exe-exp-pdt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ybo]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ybo-000      thru exe-exp-ybo-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ybo] - scaglioni              *
      *                  *---------------------------------------------*
           perform   exe-exp-ybx-000      thru exe-exp-ybx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ydf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ydf-000      thru exe-exp-ydf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yfp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yfp-000      thru exe-exp-yfp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yfx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yfx-000      thru exe-exp-yfx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zin]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yin-000      thru exe-exp-yin-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ysf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ysf-000      thru exe-exp-ysf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yst]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yst-000      thru exe-exp-yst-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yvf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yvf-000      thru exe-exp-yvf-999        .
      *              *=================================================*
      *              * Esportazioni per [dcp]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [cpv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cpv-000      thru exe-exp-cpv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [dcp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dcp-000      thru exe-exp-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lsd]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lsd-000      thru exe-exp-lsd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lst]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lst-000      thru exe-exp-lst-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [pdk]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-pdk-000      thru exe-exp-pdk-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [pdx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-pdx-000      thru exe-exp-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zcp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zcp-000      thru exe-exp-zcp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zls]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zls-000      thru exe-exp-zls-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fsp] ex [zos]                 *
      *                  *---------------------------------------------*
           perform   exe-exp-fsp-000      thru exe-exp-fsp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zp1]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zp1-000      thru exe-exp-zp1-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zp2]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zp2-000      thru exe-exp-zp2-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zp3]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zp3-000      thru exe-exp-zp3-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zps]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zps-000      thru exe-exp-zps-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ztv]                          *
      *                  *---------------------------------------------*
______*    perform   exe-exp-ztv-000      thru exe-exp-ztv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zum]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zum-000      thru exe-exp-zum-999        .
      *              *=================================================*
      *              * Esportazioni per [dpm]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [dpm]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dpm-000      thru exe-exp-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fmp] ex [zos]                 *
      *                  *---------------------------------------------*
           perform   exe-exp-fmp-000      thru exe-exp-fmp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zm1]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zm1-000      thru exe-exp-zm1-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zm2]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zm2-000      thru exe-exp-zm2-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zm3]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zm3-000      thru exe-exp-zm3-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zms]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zms-000      thru exe-exp-zms-999        .
      *              *=================================================*
      *              * Esportazioni per [dps]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [dps]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-dps-000      thru exe-exp-dps-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fsl] ex [zos]                 *
      *                  *---------------------------------------------*
           perform   exe-exp-fsl-000      thru exe-exp-fsl-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zs1]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zs1-000      thru exe-exp-zs1-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zs2]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zs2-000      thru exe-exp-zs2-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zs3]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zs3-000      thru exe-exp-zs3-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zss]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zss-000      thru exe-exp-zss-999        .
      *              *=================================================*
      *              * Esportazioni per [mtv]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [mtv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mtv-000      thru exe-exp-mtv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fmv] ex [zos]                 *
      *                  *---------------------------------------------*
           perform   exe-exp-fmv-000      thru exe-exp-fmv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zv1]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zv1-000      thru exe-exp-zv1-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zv2]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zv2-000      thru exe-exp-zv2-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zv3]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zv3-000      thru exe-exp-zv3-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zvs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zvs-000      thru exe-exp-zvs-999        .
      *              *=================================================*
      *              * Esportazioni per [orc]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ocf]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ocf-000      thru exe-exp-ocf-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ocp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ocp-000      thru exe-exp-ocp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ocr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ocr-000      thru exe-exp-ocr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [oct]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-oct-000      thru exe-exp-oct-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [oct] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-ocs-000      thru exe-exp-ocs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [oct] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-ocv-000      thru exe-exp-ocv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ocx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ocx-000      thru exe-exp-ocx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zcv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zcv-000      thru exe-exp-zcv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zoc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zoc-000      thru exe-exp-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zro]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zro-000      thru exe-exp-zro-999        .
      *              *=================================================*
      *              * Esportazioni per [ods]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [osk]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-osk-000      thru exe-exp-osk-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [osr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-osr-000      thru exe-exp-osr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ost]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ost-000      thru exe-exp-ost-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ost] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-oss-000      thru exe-exp-oss-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ost] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-osv-000      thru exe-exp-osv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [osx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-osx-000      thru exe-exp-osx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsa]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsa-000      thru exe-exp-zsa-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsc-000      thru exe-exp-zsc-999        .
      *              *=================================================*
      *              * Esportazioni per [orf]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ofr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ofr-000      thru exe-exp-ofr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [oft]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-oft-000      thru exe-exp-oft-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [oft] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-ofs-000      thru exe-exp-ofs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [oft] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-ofv-000      thru exe-exp-ofv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ofx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ofx-000      thru exe-exp-ofx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yca]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yca-000      thru exe-exp-yca-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yof]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yof-000      thru exe-exp-yof-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yro]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yro-000      thru exe-exp-yro-999        .
      *              *=================================================*
      *              * Esportazioni per [fat]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ddx]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ddx-000      thru exe-exp-ddx-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fir]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fir-000      thru exe-exp-fir-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fit]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fit-000      thru exe-exp-fit-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fit] - castelletto iva        *
      *                  *---------------------------------------------*
           perform   exe-exp-fii-000      thru exe-exp-fii-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fit] - cast. contropartite    *
      *                  *---------------------------------------------*
           perform   exe-exp-fic-000      thru exe-exp-fic-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fit] - spese                  *
      *                  *---------------------------------------------*
           perform   exe-exp-fis-000      thru exe-exp-fis-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fit] - voci descrittive       *
      *                  *---------------------------------------------*
           perform   exe-exp-fiv-000      thru exe-exp-fiv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fix]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fix-000      thru exe-exp-fix-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zac]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zac-000      thru exe-exp-zac-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zfi]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zfi-000      thru exe-exp-zfi-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [idv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-idv-000      thru exe-exp-idv-999        .
      *              *=================================================*
      *              * Esportazioni per [mag]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [mau]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mau-000      thru exe-exp-mau-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mim]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mim-000      thru exe-exp-mim-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [miu]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-miu-000      thru exe-exp-miu-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mmc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mmc-000      thru exe-exp-mmc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mmr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mmr-000      thru exe-exp-mmr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mms]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mms-000      thru exe-exp-mms-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mms] - lifo                   *
      *                  *---------------------------------------------*
           perform   exe-exp-mml-000      thru exe-exp-mml-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mms] - mesi                   *
      *                  *---------------------------------------------*
           perform   exe-exp-mmm-000      thru exe-exp-mmm-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mmt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mmt-000      thru exe-exp-mmt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mmv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mmv-000      thru exe-exp-mmv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [mmt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-mmz-000      thru exe-exp-mmz-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zmc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zmc-000      thru exe-exp-zmc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zmd]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zmd-000      thru exe-exp-zmd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zmm]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zmm-000      thru exe-exp-zmm-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zmu]                          *
      *                  *---------------------------------------------*
______*    perform   exe-exp-zmu-000      thru exe-exp-zmu-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zrm]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zrm-000      thru exe-exp-zrm-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zub]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zub-000      thru exe-exp-zub-999        .
      *              *=================================================*
      *              * Esportazioni per [gep]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [cbp] - casse                  *
      *                  *---------------------------------------------*
           perform   exe-exp-cas-000      thru exe-exp-cas-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [cbp] - c/c postali            *
      *                  *---------------------------------------------*
           perform   exe-exp-ccp-000      thru exe-exp-ccp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [cbp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cbp-000      thru exe-exp-cbp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ccc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ccc-000      thru exe-exp-ccc-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ddp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ddp-000      thru exe-exp-ddp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sia] - ex [gep]               *
      *                  *---------------------------------------------*
           perform   exe-exp-sia-000      thru exe-exp-sia-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [obp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-obp-000      thru exe-exp-obp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [rsd]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-rsd-000      thru exe-exp-rsd-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [scr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-scr-000      thru exe-exp-scr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sct]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-sct-000      thru exe-exp-sct-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sdb]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-sdb-000      thru exe-exp-sdb-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zop]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zop-000      thru exe-exp-zop-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zpg]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zpg-000      thru exe-exp-zpg-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zsb]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zsb-000      thru exe-exp-zsb-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zbb] - scaglioni di [zsb]     *
      *                  *---------------------------------------------*
           perform   exe-exp-zbb-000      thru exe-exp-zbb-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zso]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zso-000      thru exe-exp-zso-999        .
      *              *=================================================*
      *              * Esportazioni per [scf]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [bef]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-bef-000      thru exe-exp-bef-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [fff]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fff-000      thru exe-exp-fff-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sfa]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-sfa-000      thru exe-exp-sfa-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sff]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-sff-000      thru exe-exp-sff-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sff] - castelletto iva        *
      *                  *---------------------------------------------*
           perform   exe-exp-sfi-000      thru exe-exp-sfi-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sfp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-sfp-000      thru exe-exp-sfp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [sfs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-sfs-000      thru exe-exp-sfs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yop]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yop-000      thru exe-exp-yop-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [xfz]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-xfz-000      thru exe-exp-xfz-999        .
      *              *=================================================*
      *              * Esportazioni per [rda]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [raa]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-raa-000      thru exe-exp-raa-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ram]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ram-000      thru exe-exp-ram-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zco]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zco-000      thru exe-exp-zco-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zcr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zcr-000      thru exe-exp-zcr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ztr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ztr-000      thru exe-exp-ztr-999        .
      *              *=================================================*
      *              * Esportazioni per [iic]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [iir]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-iir-000      thru exe-exp-iir-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [iit]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-iit-000      thru exe-exp-iit-999        .
      *              *=================================================*
      *              * Esportazioni per [bil]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [vbr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vbr-000      thru exe-exp-vbr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [vbt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vbt-000      thru exe-exp-vbt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [vbv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vbv-000      thru exe-exp-vbv-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ztb]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ztb-000      thru exe-exp-ztb-999        .
      *              *=================================================*
      *              * Esportazioni per [fab]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [fbs]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-fbs-000      thru exe-exp-fbs-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [zts]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-zts-000      thru exe-exp-zts-999        .
      *              *=================================================*
      *              * Esportazioni per [cdp]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [cdp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-cdp-000      thru exe-exp-cdp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [ycp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ycp-000      thru exe-exp-ycp-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yrc]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yrc-000      thru exe-exp-yrc-999        .
      *              *=================================================*
      *              * Esportazioni per [vdp]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [vpr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vpr-000      thru exe-exp-vpr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [vpt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-vpt-000      thru exe-exp-vpt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [vpt] scaglioni [vps]          *
      *                  *---------------------------------------------*
           perform   exe-exp-vps-000      thru exe-exp-vps-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [yvp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-yvp-000      thru exe-exp-yvp-999        .
      *              *=================================================*
      *              * Esportazioni per [dtp]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [lgr]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lgr-000      thru exe-exp-lgr-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lgt]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lgt-000      thru exe-exp-lgt-999        .
      *                  *---------------------------------------------*
      *                  * Esportazione [lgv]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-lgv-000      thru exe-exp-lgv-999        .
      *              *=================================================*
      *              * Esportazioni per [sst]                          *
      *              *=================================================*
      *                  *---------------------------------------------*
      *                  * Esportazione [ssp]                          *
      *                  *---------------------------------------------*
           perform   exe-exp-ssp-000      thru exe-exp-ssp-999        .
       exe-exp-fil-600.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to exe-exp-fil-980.
       exe-exp-fil-620.
      *              *-------------------------------------------------*
      *              * Preparazione ora di fine                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Date and time da segreteria                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Estrazione ora e minuti da 'time'           *
      *                  *---------------------------------------------*
           move      s-ora                to   w-exe-tim-fin          .
           multiply  100                  by   w-exe-tim-fin          .
           add       s-min                to   w-exe-tim-fin          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "MB"                 to   v-edm                  .
           move      "XX:XX"              to   v-msk                  .
           move      09                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      w-exe-tim-fin        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      09                   to   v-lin                  .
           move      59                   to   v-pos                  .
           move      s-dat                to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-exp-fil-900.
      *              *-------------------------------------------------*
      *              * Visualizzazione rullino messaggi                *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-exp-fil-980.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-exp-fil-999.
       exe-exp-fil-999.
           exit.

      *    *===========================================================*
      *    * Subroutines comuni ai programmi della serie 'xpg7xx'      *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.wks"          .

      *    *===========================================================*
      *    * Aree                                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.xpg"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.azi"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.cge"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.age"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.bfo"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.ffo"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.bol"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.dcc"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.dcf"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.dcp"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.dpm"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.dps"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.mtv"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.orc"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.ods"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.orf"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.fat"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.mag"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.gep"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.scf"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.rda"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.iic"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.bil"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.fab"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.cdp"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.vdp"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.dtp"          .
           copy      "swd/xpg/prg/cpy/pxpg7000/pxpg7000.sst"          .

      *    *===========================================================*
      *    * Richiesta di conferma, se manuale                         *
      *    *-----------------------------------------------------------*
       ric-cnf-man-000.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-flg-btc        not  = "S" and
                     w-exe-flg-btc        not  = "X"
                     go to ric-cnf-man-010.
      *                  *---------------------------------------------*
      *                  * Ricerca nel file di configurazione          *
      *                  *---------------------------------------------*
           set       w-buf-nam-cnf-inx    to   1                      .
           search    w-buf-nam-cnf-buf
                     when    w-buf-nam-cnf-nam
                            (w-buf-nam-cnf-inx)
                                          =    f-xxx-nam
                     go to   ric-cnf-man-007.
       ric-cnf-man-005.
      *                  *---------------------------------------------*
      *                  * Nome NON trovato                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag                                    *
      *                      *-----------------------------------------*
           move      "N"                  to   f-xxx-sts              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ric-cnf-man-999.
       ric-cnf-man-007.
      *                  *---------------------------------------------*
      *                  * Nome trovato                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag                                    *
      *                      *-----------------------------------------*
           move      "S"                  to   f-xxx-sts              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ric-cnf-man-999.
       ric-cnf-man-010.
      *              *-------------------------------------------------*
      *              * Test preliminare su area                        *
      *              *-------------------------------------------------*
           if        rr-are-exp           =    spaces
                     go to ric-cnf-man-020.
           if        f-xxx-are            not  = rr-are-exp
                     move  "N"            to   f-xxx-sts
                     go to ric-cnf-man-999.
       ric-cnf-man-020.
      *              *-------------------------------------------------*
      *              * Test preliminare su archivio                    *
      *              *-------------------------------------------------*
           if        rr-arc-exp           =    spaces
                     go to ric-cnf-man-050.
           if        f-xxx-nam            not  = rr-arc-exp
                     move  "N"            to   f-xxx-sts
                     go to ric-cnf-man-999.
       ric-cnf-man-050.
      *              *-------------------------------------------------*
      *              * Ricerca, comunque, nel file di configurazione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se batch                               *
      *                  *---------------------------------------------*
           if        w-exe-flg-btc        not  = "S" and
                     w-exe-flg-btc        not  = "X"
                     go to ric-cnf-man-057.
      *                  *---------------------------------------------*
      *                  * Ricerca                                     *
      *                  *---------------------------------------------*
           set       w-buf-nam-cnf-inx    to   1                      .
           search    w-buf-nam-cnf-buf
                     when    w-buf-nam-cnf-nam
                            (w-buf-nam-cnf-inx)
                                          =    f-xxx-nam
                     go to   ric-cnf-man-057.
       ric-cnf-man-055.
      *                  *---------------------------------------------*
      *                  * Nome NON trovato                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag                                    *
      *                      *-----------------------------------------*
           move      "N"                  to   f-xxx-sts              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ric-cnf-man-999.
       ric-cnf-man-057.
      *                  *---------------------------------------------*
      *                  * Nome trovato                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag                                    *
      *                      *-----------------------------------------*
           move      "S"                  to   f-xxx-sts              .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     ric-cnf-man-058.
       ric-cnf-man-058.
      *              *-------------------------------------------------*
      *              * Determinazione numero di record                 *
      *              *-------------------------------------------------*
           perform   det-rec-fil-000      thru det-rec-fil-999        .
       ric-cnf-man-060.
      *              *-------------------------------------------------*
      *              * Visualizzazione Esportazione in corso           *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Esportazione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] di {"   delimited by   size
                     f-xxx-are  delimited by   spaces
                     "}"        delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       ric-cnf-man-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione a spaces area per conferma      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt numero records letti                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero records letti       : 0"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt numero records scritti                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero records scritti     : 0"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico : uscita con Ok     *
      *              *-------------------------------------------------*
           if        rr-aut-man           not  = "M"
                     move  "S"            to   f-xxx-sts
                     go to ric-cnf-man-999.
       ric-cnf-man-110.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt per conferma             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Conferma esportazione      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       ric-cnf-man-190.
      *              *-------------------------------------------------*
      *              * Normalizzazione risposta                        *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-sts              .
       ric-cnf-man-200.
      *              *-------------------------------------------------*
      *              * Accettazione risposta Si/No                     *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-ris-snx-lun    to   v-car                  .
           move      w-exp-ris-snx-num    to   v-ldt                  .
           move      "SN#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-ris-snx-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        f-xxx-sts            =    "S"
                     move  01             to   v-num
           else if   f-xxx-sts            =    "N"
                     move  02             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       ric-cnf-man-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "S"            to   f-xxx-sts
           else if   v-num                =    02
                     move  "N"            to   f-xxx-sts
           else      move  spaces         to   f-xxx-sts              .
       ric-cnf-man-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        f-xxx-sts            not  = "S" and
                     f-xxx-sts            not  = "N"
                     go to ric-cnf-man-200.
       ric-cnf-man-999.
           exit.

      *    *===========================================================*
      *    * Scrittura rullino messaggi                                *
      *    *-----------------------------------------------------------*
       wrt-rum-msg-000.
      *              *-------------------------------------------------*
      *              * Editing data attuale                            *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-gen-dat-icn        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Rullino messaggi                                *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Trattamento archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] "       delimited by   size
                     v-edt      delimited by   spaces
                     " "        delimited by   size
                     w-exe-flg-brm
                                delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Editing record letti                            *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio record letti                          *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records letti   : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Editing record scritti                          *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio record scritti                        *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "  - Numero records scritti : "
                                delimited by   size
                     v-edt      delimited by   spaces
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *              *-------------------------------------------------*
      *              * Messaggio a spazi                               *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       wrt-rum-msg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione presa visione                                *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Comando di ricostruzione tabella                *
      *              *-------------------------------------------------*
           perform   exe-rcs-tbl-000      thru exe-rcs-tbl-999        .
       acc-pre-vis-100.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to acc-pre-vis-999.
       acc-pre-vis-200.
      *              *-------------------------------------------------*
      *              * Messaggio di fine esportazione archivio         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           string    "Fine esportazione relativa all'archivio ["
                                delimited by   size
                     f-xxx-nam  delimited by   spaces
                     "] "       delimited by   size
                     w-exe-flg-brm
                                delimited by   spaces
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se funzionamento automa-   *
      *              * tico o manuale                                  *
      *              *-------------------------------------------------*
           if        rr-aut-man           =    "M"
                     go to acc-pre-vis-600
           else      go to acc-pre-vis-400.
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento automatico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Attesa di 1 secondo                         *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mwait0"                         .
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Se funzionamento manuale                        *
      *              *-------------------------------------------------*
       acc-pre-vis-610.
      *                  *---------------------------------------------*
      *                  * Cancellazione messaggio di programma in     *
      *                  * esecuzione                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione presa visione               *
      *                  *---------------------------------------------*
           move      spaces               to   rr-pre-vis             .
      *                  *---------------------------------------------*
      *                  * Prompt per presa visione                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      63                   to   v-pos                  .
           move      "Digitare OK : [  ]" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vis-620.
      *                  *---------------------------------------------*
      *                  * Accettazione presa visione                  *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      rr-pre-vis           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   rr-pre-vis             .
           if        rr-pre-vis           not  = "OK"
                     go to acc-pre-vis-620.
       acc-pre-vis-630.
      *                  *---------------------------------------------*
      *                  * Pulizia linea 20                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-pre-vis-999.
       acc-pre-vis-999.
           exit.

      *    *===========================================================*
      *    * Comando di ricostruzione tabella                          *
      *    *-----------------------------------------------------------*
       exe-rcs-tbl-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul tipo di batch                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-btc        =    "X"
                     go to exe-rcs-tbl-800.
      *                  *---------------------------------------------*
      *                  * Ricerca nel file di configurazione          *
      *                  *---------------------------------------------*
           set       w-buf-nam-cnf-inx    to   1                      .
           search    w-buf-nam-cnf-buf
                     when    w-buf-nam-cnf-nam
                            (w-buf-nam-cnf-inx)
                                          =    "NOWR"
                     go to   exe-rcs-tbl-900.
       exe-rcs-tbl-100.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione comando di log                     *
      *              *                                                 *
      *              * Attualmente inibito                             *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      "| cat > /abd/asc/log/"
                                          to   w-all-str-cat (1)      .
           move      f-xxx-azi            to   w-all-str-cat (2)      .
           move      "_"                  to   w-all-str-cat (3)      .
           move      f-xxx-nam            to   w-all-str-cat (4)      .
           move      ".log"               to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   f-xxx-log              .
      *
           move      spaces               to   f-xxx-log              .
      *              *-------------------------------------------------*
      *              * Comando di generazione database                 *
      *              *-------------------------------------------------*
           move      17                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      f-xxx-etc            to   w-all-str-cat (1)      .
           move      "/t_mysql"           to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   f-xxx-ptc              .
      *              *-------------------------------------------------*
      *              * Comando di spedizione                           *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      f-xxx-ptc            to   w-all-str-cat (1)      .
           move      f-xxx-azi            to   w-all-str-cat (2)      .
           move      f-xxx-are            to   w-all-str-cat (3)      .
           move      f-xxx-nam            to   w-all-str-cat (4)      .
           move      f-xxx-log            to   w-all-str-cat (5)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Eventuale 'log' se NON batch                    *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to exe-rcs-tbl-200.
      *
           move      240                  to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "2>/abd/asc/log/xpg700_rt.log"
                                          to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
       exe-rcs-tbl-200.
           move      w-all-str-alf        to   o-shs                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Display per esecuzione batch                    *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        not  = "S" and
                     w-exe-flg-btc        not  = "X"
                     go to exe-rcs-tbl-900.
       exe-rcs-tbl-800.
           display   f-xxx-azi                                        .
           display   "."                                              .
           display   f-xxx-are                                        .
           display   "."                                              .
           display   f-xxx-nam                                        .
           display   H"0D"                                            .
           display   H"0A"                                            .
       exe-rcs-tbl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
            go to    exe-rcs-tbl-999.
       exe-rcs-tbl-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records letti                           *
      *    *-----------------------------------------------------------*
       inc-rec-let-000.
      *              *-------------------------------------------------*
      *              * Accettazione tasto per il break                 *
      *              *-------------------------------------------------*
           perform   inc-rec-let-brk-000  thru inc-rec-let-brk-999    .
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrl              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrl            >    100
                     go to inc-rec-let-100
           else if   f-xxx-nrl            >    10
                     go to inc-rec-let-200
           else      go to inc-rec-let-500.
       inc-rec-let-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrl            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
                     go to inc-rec-let-500
           else      go to inc-rec-let-999.
       inc-rec-let-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-let-000      thru vis-rec-let-999        .
       inc-rec-let-600.
      *              *-------------------------------------------------*
      *              * Determinazione %                                *
      *              *-------------------------------------------------*
           multiply  100                  by   f-xxx-nrl
                                        giving w-det-rec-fil-s15      .
           divide    w-det-rec-fil-rec    into w-det-rec-fil-s15
                                        giving w-det-rec-fil-v02
                                               rounded                .
           if        w-det-rec-fil-v02    >    100
                     move  100            to   w-det-rec-fil-v02      .
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to inc-rec-let-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompt                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      50                   to   v-pos                  .
           move      "% :"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione %                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<BD"                to   v-edm                  .
           move      16                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-det-rec-fil-v02    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       inc-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records letti                           *
      *    *                                                           *
      *    * Accettazione tasto per break                              *
      *    *-----------------------------------------------------------*
       inc-rec-let-brk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-brk          .
           move      spaces               to   w-exe-flg-brm          .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione del tasto per l'interruzione   *
      *                  * dell'attesa                                 *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           move      "AA"                 to   v-ope                  .
           move      21                   to   v-lin                  .
           move      80                   to   v-pos                  .
           move      "[4] "               to   v-pfk (16)             .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Se attesa disattivata : ad uscita       *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to inc-rec-let-brk-900.
       inc-rec-let-brk-200.
      *                      *-----------------------------------------*
      *                      * Se 'Pf4'                                *
      *                      *-----------------------------------------*
           if        v-key                not  = "[4] "
                     go to inc-rec-let-brk-900.
      *                      *-----------------------------------------*
      *                      * Flag                                    *
      *                      *-----------------------------------------*
           move      "#"                  to   w-exe-flg-brk          .
           move      "INTERROTTO"         to   w-exe-flg-brm          .
       inc-rec-let-brk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     inc-rec-let-brk-999.
       inc-rec-let-brk-999.
           exit.

      *    *===========================================================*
      *    * Incremento numero records scritti                         *
      *    *-----------------------------------------------------------*
       inc-rec-scr-000.
      *              *-------------------------------------------------*
      *              * Incremento                                      *
      *              *-------------------------------------------------*
           add       1                    to   f-xxx-nrs              .
      *              *-------------------------------------------------*
      *              * Deviazione                                      *
      *              *-------------------------------------------------*
           if        f-xxx-nrs            >    100
                     go to inc-rec-scr-100
           else if   f-xxx-nrs            >    10
                     go to inc-rec-scr-200
           else      go to inc-rec-scr-500.
       inc-rec-scr-100.
      *              *-------------------------------------------------*
      *              * Se maggiore di 100                              *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrc              .
           if        f-xxx-nrc            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-200.
      *              *-------------------------------------------------*
      *              * Se maggiore di 10                               *
      *              *-------------------------------------------------*
           move      f-xxx-nrs            to   f-xxx-nrd              .
           if        f-xxx-nrd            =    zero
                     go to inc-rec-scr-500
           else      go to inc-rec-scr-999.
       inc-rec-scr-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           perform   vis-rec-scr-000      thru vis-rec-scr-999        .
       inc-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records letti                      *
      *    *-----------------------------------------------------------*
       vis-rec-let-000.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to vis-rec-let-999.
       vis-rec-let-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-let-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione numero records scritti                    *
      *    *-----------------------------------------------------------*
       vis-rec-scr-000.
      *              *-------------------------------------------------*
      *              * Eventuali forzature se esecuzione batch         *
      *              *-------------------------------------------------*
           if        w-exe-flg-btc        =    "S" or
                     w-exe-flg-btc        =    "X"
                     go to vis-rec-scr-999.
       vis-rec-scr-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      f-xxx-nrs            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-rec-scr-999.
           exit.

      *    *===========================================================*
      *    * Open generica file sequenziale di output                  *
      *    *-----------------------------------------------------------*
       opn-seq-out-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname                           *
      *              *-------------------------------------------------*
           move      spaces               to   f-xxx-pat              .
           string    f-xxx-ppb  delimited by   spaces
                     "/"        delimited by   size
                     f-xxx-azi  delimited by   spaces
                     "/"        delimited by   size
______*              f-xxx-are  delimited by   spaces
______*              "_"        delimited by   size
                     f-xxx-nam  delimited by   spaces
                     ".txt"     delimited by   size
                                          into f-xxx-pat              .
      *              *-------------------------------------------------*
      *              * Apertura del file in output                     *
      *              *-------------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      "seq "               to   g-nam                  .
           move      f-xxx-pat            to   g-pat                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       opn-seq-out-999.
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
      *    * Preparazione pathnames di base                            *
      *    *-----------------------------------------------------------*
       pre-bas-pth-000.
      *              *-------------------------------------------------*
      *              * Pathname dei comandi da segreteria              *
      *              *-------------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "etc "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                      .
           move      s-pat                to   f-xxx-etc              .
      *              *-------------------------------------------------*
      *              * Pathname per 'ftx'                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se batch 'X'                           *
      *                  *---------------------------------------------*
           if        w-exe-flg-btc        =    "X"
                     move  "/abd/asc/tmp/ftx"
                                          to   f-xxx-ppb
                     go to pre-bas-pth-999.
      *                  *---------------------------------------------*
      *                  * In tutti gli altri casi                     *
      *                  *---------------------------------------------*
           move      "PB"                 to   s-ope                  .
           move      "ftx "               to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                          using s                      .
           move      s-pat                to   f-xxx-ppb              .
       pre-bas-pth-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Messaggio di input i-o error       *
      *    *-----------------------------------------------------------*
       msg-inp-err-000.
      *              *-------------------------------------------------*
      *              * Eliminazione messaggio programma in esecuzione  *
      *              *-------------------------------------------------*
           move      "PX"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      64                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore all'interno del box     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prima riga                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Errore di i-o codice "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      g-sts                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      " su file in input.   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Seconda riga                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Terminazione forzata del programma.         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Terza riga                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "        Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       msg-inp-err-600.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to msg-inp-err-800.
           if        v-alf                not  = "OK"
                     go to msg-inp-err-600.
       msg-inp-err-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-inp-err-999.
           exit.

      *    *===========================================================*
      *    * Esportazione contatti generico                            *
      *    *-----------------------------------------------------------*
       exe-exp-con-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio numero contatto intero          *
      *                  *---------------------------------------------*
           move      w-exe-con-num        to   w-exe-con-sav          .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-con-pre          .
           move      spaces               to   w-exe-con-rep          .
      *                  *---------------------------------------------*
      *                  * Test sul tipo contatto                      *
      *                  *                                             *
      *                  * - TEL : Telefono                            *
      *                  * - FAX : Fax                                 *
      *                  * - CEL : Cellulare                           *
      *                  *                                             *
      *                  * - EML : E-mail                              *
      *                  * - PEC : E-mail pec                          *
      *                  * - WWW : Sito Internet                       *
      *                  *                                             *
      *                  * - SEP : Separatore per visualizzazione      *
      *                  *        (non esportato)                      *
      *                  *---------------------------------------------*
           if        w-exe-con-tip        =    "EML" or
                     w-exe-con-tip        =    "PEC"
                     go to exe-exp-con-200
           else if   w-exe-con-tip        =    "WWW" or
                     w-exe-con-tip        =    "SEP"
                     go to exe-exp-con-700
           else      go to exe-exp-con-100.
       exe-exp-con-100.
      *              *=================================================*
      *              * Trattamento numeri telefonici                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tentativo di estrazione prefisso '/'        *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-con-pre          .
           move      zero                 to   w-scr-ctr-004          .
           inspect   w-exe-con-num    tallying w-scr-ctr-004
                                          for  all "/"                .
           if        w-scr-ctr-004        not  = 1
                     go to exe-exp-con-120.
      *                  *---------------------------------------------*
      *                  * Estrazione prefisso                         *
      *                  *---------------------------------------------*
           move      w-exe-con-num        to   w-all-str-alf          .
           move      "/"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-exe-con-pre          .
           move      w-all-str-cat (2)    to   w-exe-con-num          .
      *                  *---------------------------------------------*
      *                  * Ad esportazione                             *
      *                  *---------------------------------------------*
           go to     exe-exp-con-700.
       exe-exp-con-120.
      *                  *---------------------------------------------*
      *                  * Tentativo di estrazione prefisso '-'        *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-con-pre          .
           move      zero                 to   w-scr-ctr-004          .
           inspect   w-exe-con-num    tallying w-scr-ctr-004
                                          for  all "-"                .
           if        w-scr-ctr-004        not  = 1
                     go to exe-exp-con-150.
      *                  *---------------------------------------------*
      *                  * Estrazione prefisso                         *
      *                  *---------------------------------------------*
           move      w-exe-con-num        to   w-all-str-alf          .
           move      "-"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-exe-con-pre          .
           move      w-all-str-cat (2)    to   w-exe-con-num          .
      *                  *---------------------------------------------*
      *                  * Ad esportazione                             *
      *                  *---------------------------------------------*
           go to     exe-exp-con-700.
       exe-exp-con-150.
      *                  *---------------------------------------------*
      *                  * Tentativo di estrazione prefisso '.'        *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-con-pre          .
           move      zero                 to   w-scr-ctr-004          .
           inspect   w-exe-con-num    tallying w-scr-ctr-004
                                          for  all "."                .
           if        w-scr-ctr-004        not  = 1
                     go to exe-exp-con-180.
      *                  *---------------------------------------------*
      *                  * Estrazione prefisso                         *
      *                  *---------------------------------------------*
           move      w-exe-con-num        to   w-all-str-alf          .
           move      "."                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-exe-con-pre          .
           move      w-all-str-cat (2)    to   w-exe-con-num          .
      *                  *---------------------------------------------*
      *                  * Ad esportazione                             *
      *                  *---------------------------------------------*
           go to     exe-exp-con-700.
       exe-exp-con-180.
      *                  *---------------------------------------------*
      *                  * Tentativo di estrazione prefisso ' '        *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-con-pre          .
           move      zero                 to   w-scr-ctr-004          .
           unstring  w-exe-con-num
                                delimited by " "
                                          into w-exe-con-pre
                                    count in   w-scr-ctr-004          .
           add       2                    to   w-scr-ctr-004          .
           move      spaces               to   w-exe-con-nue          .
           unstring  w-exe-con-num        into w-exe-con-nue
                                  with pointer w-scr-ctr-004          .
           move      w-exe-con-nue        to   w-exe-con-num          .
      *                  *---------------------------------------------*
      *                  * Ad esportazione                             *
      *                  *---------------------------------------------*
           go to     exe-exp-con-700.
       exe-exp-con-200.
      *              *=================================================*
      *              * Trattamento indirizzi email o PEC               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se contengono riferimenti ai documenti *
      *                  *---------------------------------------------*
           if        w-exe-con-int
                    (02 : 03)             =    "fat"
                     move  "FAT"          to   w-exe-con-rep
                     move  spaces         to   w-exe-con-int
           else if   w-exe-con-int
                    (02 : 03)             =    "ord"
                     move  "ORD"          to   w-exe-con-rep
                     move  spaces         to   w-exe-con-int
           else if   w-exe-con-int
                    (02 : 03)             =    "bol"
                     move  "BOL"        to   w-exe-con-rep
                     move  spaces         to   w-exe-con-int          .
      *                  *---------------------------------------------*
      *                  * Ad esportazione                             *
      *                  *---------------------------------------------*
           go to     exe-exp-con-700.
       exe-exp-con-700.
      *              *=================================================*
      *              * Esportazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "iN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo archivio                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-con-arc        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      02                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-con-cod        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-dpz        to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           add       1                    to   w-scr-ctr-003          .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-scr-ctr-003        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      w-exe-con-prg        to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo contatto                               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-tip        to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Interlocutore                               *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-int        to   w-scr-str-ele          .
           move      30                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Reparto                                     *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-rep        to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Prefisso internazionale                     *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      spaces               to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Prefisso                                    *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-pre        to   w-scr-str-ele          .
      *
           if        w-exe-con-pre        =    spaces
                     move  01             to   w-scr-lun-ele
           else      move  20             to   w-scr-lun-ele          .
      *
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero contatto comunque per intero         *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-sav        to   w-scr-str-ele          .
           move      w-exe-con-lun        to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data aggiornamento                          *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      w-exe-con-agg        to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Data ultima modifica - SQL                  *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      w-exe-con-idd        to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Utente ultima modifica                      *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-idu        to   w-scr-str-ele          .
           move      08                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Fase ultima modifica                        *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-con-idf        to   w-scr-str-ele          .
           move      06                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "Af"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-exp-con-800.
      *              *-------------------------------------------------*
      *              * Composizione carattere di fine record           *
      *              *-------------------------------------------------*
           move      "fr"                 to   w-scr-str-tip          .
           move      w-scr-fso-cfr        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *              *-------------------------------------------------*
      *              * Scrittura record in output                      *
      *              *-------------------------------------------------*
           move      w-out-str-out        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti               *
      *              *-------------------------------------------------*
           perform   inc-rec-scr-000      thru inc-rec-scr-999        .
       exe-exp-con-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-exp-con-999.
       exe-exp-con-999.
           exit.

      *    *===========================================================*
      *    * Esportazione valuta generico                              *
      *    *-----------------------------------------------------------*
       exe-gen-vlt-000.
      *              *-------------------------------------------------*
      *              * Valori                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-sgl-vlt        to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali valuta                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-dec-vlt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di cambio                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-tdc-vlt        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Cambio                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-exe-cdc-vlt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-gen-vlt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-vlt-999.
       exe-gen-vlt-999.
           exit.

      *    *===========================================================*
      *    * Esportazione legame valutario generico                    *
      *    *-----------------------------------------------------------*
       exe-gen-lgv-000.
      *              *-------------------------------------------------*
      *              * Valori                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valuta per la fatturazione                  *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-sgl-lvl        to   w-scr-str-ele          .
           move      03                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Decimali valuta                             *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-dec-lvl        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di cambio                              *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-tdc-lvl        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Prezzo di riferimento                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-prz-lvl        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      09                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Cambio                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-cdc-lvl        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Cambio di riferimento                       *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-ccr-lvl        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % di limitazione                            *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-plm-lvl        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Tipo di limitazione                         *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-tlm-lvl        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "AA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Momento di applicazione                     *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-map-lvl        to   w-scr-str-ele          .
           move      01                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-gen-lgv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-lgv-999.
       exe-gen-lgv-999.
           exit.

      *    *===========================================================*
      *    * Esportazione sconto e annessi                             *
      *    *-----------------------------------------------------------*
       exe-gen-sco-000.
      *              *-------------------------------------------------*
      *              * Valori                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale sconto                               *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-sco-tot        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      12                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * % sconto chiusura                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-sco-per       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      04                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Codice Iva e contropartita                  *
      *                  *---------------------------------------------*
           move      w-exe-sco-iva        to   w-exe-iec-iva          .
           move      w-exe-sco-cpt        to   w-exe-iec-cpt          .
           perform   exe-gen-iec-000      thru exe-gen-iec-999        .
       exe-gen-sco-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-sco-999.
       exe-gen-sco-999.
           exit.

      *    *===========================================================*
      *    * Esportazione IVA e contropartita                          *
      *    *-----------------------------------------------------------*
       exe-gen-iec-000.
      *              *-------------------------------------------------*
      *              * Valori                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice Iva                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-iec-iva        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      05                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Sottoconto                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-iec-cpt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-gen-iec-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-iec-999.
       exe-gen-iec-999.
           exit.

      *    *===========================================================*
      *    * Esportazione data e protocollo di contabilita'            *
      *    *-----------------------------------------------------------*
       exe-gen-dep-000.
      *              *-------------------------------------------------*
      *              * Valori                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data movimento di contabilita'              *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      w-exe-dep-dat        to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NN"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Protocollo                                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-dep-prt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "N "                 to   w-scr-str-tip          .
           move      v-edt                to   w-scr-str-ele          .
           move      07                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-gen-dep-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-dep-999.
       exe-gen-dep-999.
           exit.

      *    *===========================================================*
      *    * Esportazione data e numero documento                      *
      *    *-----------------------------------------------------------*
       exe-gen-den-000.
      *              *-------------------------------------------------*
      *              * Valori                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "D "                 to   w-scr-str-tip          .
           move      w-exe-den-dat        to   w-scr-dat-dat          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * S-e-p-a-r-a-t-o-r-i                         *
      *                  *---------------------------------------------*
           move      "NA"                 to   w-scr-str-tip          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "A "                 to   w-scr-str-tip          .
           move      w-exe-den-num        to   w-scr-str-ele          .
           move      10                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       exe-gen-den-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-den-999.
       exe-gen-den-999.
           exit.

      *    *===========================================================*
      *    * Open file personalizzazioni                               *
      *    *-----------------------------------------------------------*
       opn-prs-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "prs"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-prs-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    prs                                       .
       opn-prs-999.
           exit.

      *    *===========================================================*
      *    * Close file personalizzazioni                              *
      *    *-----------------------------------------------------------*
       cls-prs-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-prs-pat            =    spaces
                     go to cls-prs-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      prs                                             .
       cls-prs-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-prs-pat              .
       cls-prs-999.
           exit.

      *    *===========================================================*
      *    * Open file referenze                                       *
      *    *-----------------------------------------------------------*
       opn-ref-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "ref"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-ref-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    ref                                       .
       opn-ref-999.
           exit.

      *    *===========================================================*
      *    * Close file referenze                                      *
      *    *-----------------------------------------------------------*
       cls-ref-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-ref-pat            =    spaces
                     go to cls-ref-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      ref                                             .
       cls-ref-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-ref-pat              .
       cls-ref-999.
           exit.

      *    *===========================================================*
      *    * Open file encoding automatici                             *
      *    *-----------------------------------------------------------*
       opn-enc-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "enc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-enc-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    enc                                       .
       opn-enc-999.
           exit.

      *    *===========================================================*
      *    * Close file encoding automatici                            *
      *    *-----------------------------------------------------------*
       cls-enc-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-enc-pat            =    spaces
                     go to cls-enc-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      enc                                             .
       cls-enc-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-enc-pat              .
       cls-enc-999.
           exit.

      *    *===========================================================*
      *    * Open file promemoria utente                               *
      *    *-----------------------------------------------------------*
       opn-upr-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "upr"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-upr-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    upr                                       .
       opn-upr-999.
           exit.

      *    *===========================================================*
      *    * Close file promemoria utente                              *
      *    *-----------------------------------------------------------*
       cls-upr-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-upr-pat            =    spaces
                     go to cls-upr-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      upr                                             .
       cls-upr-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-upr-pat              .
       cls-upr-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname di base per files azienda             *
      *    *-----------------------------------------------------------*
       pnb-fls-000.
           move      ".F"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       pnb-fls-999.
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
      *    * Find su tabella aree                                      *
      *    *-----------------------------------------------------------*
       fnd-tbl-are-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di selezione               *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-tbl-are-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg700a"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-tbl-are-sel
                     go to  fnd-tbl-are-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per possibili- *
      *              * ta' di function-key "SLCT" durante l'interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "swd/xpg/prg/obj/pxpg700a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-are"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-fnd-tbl-are-sel
                     move  s-alf          to   w-fnd-tbl-are-cod
           else      move  "#"            to   w-fnd-tbl-are-sel      .
       fnd-tbl-are-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di formattazione protocollo di primanota       *
      *    *-----------------------------------------------------------*
       det-prt-cge-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-scr-prt-saa          .
           move      spaces               to   w-scr-prt-num          .
      *              *-------------------------------------------------*
      *              * Editing secolo anno dedotto da data             *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-scr-prt-dat        to   s-dat                  .
           move      s-saa                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-scr-prt-saa          .
      *              *-------------------------------------------------*
      *              * Editing protocollo                              *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-scr-prt-prt        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-scr-prt-num          .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-scr-prt-saa        to   w-all-str-cat (1)      .
           move      w-scr-prt-num        to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           move      "N "                 to   w-scr-str-tip          .
           move      w-all-str-alf        to   w-scr-str-ele          .
           move      11                   to   w-scr-lun-ele          .
           perform   cmp-sng-fld-000      thru cmp-sng-fld-999        .
       det-prt-cge-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-prt-cge-999.
       det-prt-cge-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero records                             *
      *    *-----------------------------------------------------------*
       det-rec-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-rec-fil-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record determinati              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rec-fil-rec      .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Estrazione pathname file originale              *
      *              *-------------------------------------------------*
           move      "PG"                 to   s-ope                  .
           move      f-xxx-nam            to   s-nam                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-xxx-pfo              .
      *              *-------------------------------------------------*
      *              * Richiesta alla segreteria di un pathname unico  *
      *              * per file temporaneo, e sua memorizzazione       *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-det-rec-fil-tpt      .
      *              *-------------------------------------------------*
      *              * Comando di determinazione                       *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "vutil -info"        to   w-all-str-cat (1)      .
           move      f-xxx-pfo            to   w-all-str-cat (2)      .
           move      "| cat > "           to   w-all-str-cat (3)      .
           move      w-det-rec-fil-tpt    to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   o-shs                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo 'mopsys'                    *
      *              *-------------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       det-rec-fil-300.
      *              *-------------------------------------------------*
      *              * Apertura del file temporaneo                    *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "siz "               to   g-nam                  .
           move      w-det-rec-fil-tpt    to   g-pat                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se errore : uscita con flag di errore           *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to det-rec-fil-400.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvinp"                         .
      *              *-------------------------------------------------*
      *              * Flag di uscita                                  *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-rec-fil-flg      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     det-rec-fil-900.
       det-rec-fil-400.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 1                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 2                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Read file sequenziale - riga 3                  *
      *              *-------------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con flag                  *
      *              *-------------------------------------------------*
           if        g-sts                not  = e-not-err
                     go to det-rec-fil-800.
       det-rec-fil-500.
      *              *-------------------------------------------------*
      *              * Comodo per lettura file                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In campo di comodo                          *
      *                  *---------------------------------------------*
           move      g-rec                to   w-det-rec-fil-alf      .
      *                  *---------------------------------------------*
      *                  * Test su contenuto                           *
      *                  *---------------------------------------------*
           if        w-det-rec-fil-tst    not  = "# of records:"
                     go to det-rec-fil-800.
      *                  *---------------------------------------------*
      *                  * Conversione numero letto                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      w-det-rec-fil-dat    to   v-alf                  .
           move      13                   to   v-car                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-det-rec-fil-rec      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-rec-fil-flg      .
      *                  *---------------------------------------------*
      *                  * A close                                     *
      *                  *---------------------------------------------*
           go to     det-rec-fil-800.
       det-rec-fil-800.
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvinp"                         .
       det-rec-fil-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-rec-fil-999.
       det-rec-fil-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione secondo il tipo elemento             *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-tip    =    "C"
                     go to let-arc-gxc-100
           else if   w-let-arc-gxc-tip    =    "F"
                     go to let-arc-gxc-200
           else if   w-let-arc-gxc-tip    =    "L"
                     go to let-arc-gxc-300.
       let-arc-gxc-100.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Comune                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Comune                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Frazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-fzn    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Frazione                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-300.
      *              *-------------------------------------------------*
      *              * Se tipo elemento : Localita'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se codice a zero                       *
      *                  *---------------------------------------------*
           if        w-let-arc-gxc-cmn    =    zero or
                     w-let-arc-gxc-lct    =    zero
                     go to let-arc-gxc-400.
      *                  *---------------------------------------------*
      *                  * Lettura Localita'                           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cmn    to   rf-gxc-cod-cmn         .
           move      w-let-arc-gxc-fzn    to   rf-gxc-cod-fzn         .
           move      w-let-arc-gxc-lct    to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se record esistente    *
      *                  * oppure no                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-gxc-600
           else      go to let-arc-gxc-500.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Se codice elemento a zero                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           move      spaces               to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Se codice elemento non esistente                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione work area                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           move      spaces               to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non trovato                *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-600.
      *              *-------------------------------------------------*
      *              * Se codice elemento esistente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione in work area                *
      *                  *---------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
           move      rf-gxc-cap-avp       to   w-let-arc-gxc-cap      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-gxc-999.
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice CIN             *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acodcin0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

